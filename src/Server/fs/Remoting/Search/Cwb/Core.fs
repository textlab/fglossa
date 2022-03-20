module Remoting.Search.Cwb.Core

open System.IO
open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Text.RegularExpressions
open Microsoft.Data.Sqlite
open Serilog
open ClosedXML
open ServerTypes
open Shared
open Database
open Remoting.Metadata
open Remoting.Search.Cwb.Common

// If the number of running CQP processes exceeds this number, we do not allow a new
// search in a corpus that does parallel search using all cpus to be started.
let maxCqpProcesses = 8

let searchCorpus (connStr: string) (logger: ILogger) (searchParams: SearchParams) (corpus: Corpus) =
    async {
        let cqpProcs = Process.runCmdWithOutput "pgrep" "-f cqp"

        let nCqpProcs = cqpProcs.Split('\n').Length

        // If we are searching in a corpus that does parallel search with multiple cpus and
        // this is the first search step, we check that we don't already exceed the max number
        // of CQP processes before starting the search. If we are at step 2 or 3, we should finish
        // what we started. Corpora that don't use multiple cpus are assumed to be small and
        // should not cause any problems even with a lot of CQP processes.
        if corpus.Config.MultiCpuBounds.IsNone
           || nCqpProcs < maxCqpProcesses
           || searchParams.Step > 1 then

            let searchData =
                [ "CorpusCode" => searchParams.CorpusCode
                  "Queries"
                  => (searchParams.Queries
                      |> Array.map (fun q -> q.QueryString)
                      |> String.concat " | ")
                  "Metadata"
                  => searchParams.MetadataSelection.ToString() ]

            let searchId =
                if searchParams.SearchId = 0 then
                    // a SearchId of 0 means a new, unsaved search, so save it
                    // and set the database ID of the search to be the SearchId
                    use connection = new SqliteConnection(connStr)

                    let res =
                        (insert logger connection "Search" searchData)
                            .Result

                    match res with
                    | Ok id -> id
                    | Error ex -> raise ex
                else
                    searchParams.SearchId

            let searchParamsWithSearchId = { searchParams with SearchId = searchId }

            let! searchResults =
                match corpus.Config.Modality with
                | Spoken -> Spoken.runQueries logger corpus searchParamsWithSearchId None
                | Written -> Written.runQueries logger corpus searchParamsWithSearchId None

            return
                { Count = searchResults.Count
                  CpuCounts = searchResults.CpuCounts
                  SearchId = searchId
                  SearchStep = searchParams.Step
                  ResultPages =
                    match corpus.Config.Modality with
                    | Spoken ->
                        searchResults.Hits
                        |> Spoken.transformResults corpus searchParams.Queries
                        |> Array.chunkBySize searchParams.PageSize
                        |> Array.mapi (fun index results ->
                            { PageNumber = index + 1
                              Results = results })
                    | Written ->
                        searchResults.Hits
                        |> Written.transformResults searchParams.Queries
                        |> Array.chunkBySize searchParams.PageSize
                        |> Array.mapi (fun index results ->
                            { PageNumber = index + 1
                              Results =
                                results
                                |> Array.map (fun resultLines ->
                                    { AudioType = None
                                      HasVideo = false
                                      Text = resultLines }) }) }
        else
            return failwith $"TOO MANY CQP PROCESSES: {nCqpProcs}; aborting search at {System.DateTime.Now}"
    }

let getSearchResults
    (logger: ILogger)
    (searchParams: SearchParams)
    (corpus: Corpus)
    (maybeAttributes: Cwb.PositionalAttribute list option)
    (pageNumbers: ResultPageNumbers)
    =
    async {
        return!
            match corpus.Config.Modality with
            | Spoken -> Spoken.getSearchResults logger corpus searchParams maybeAttributes pageNumbers
            | Written ->
                match corpus.Config.SearchEngine with
                | Cwb -> Written.getSearchResults logger corpus searchParams maybeAttributes pageNumbers
                | Fcs -> failwith "NOT IMPLEMENTED"
    }

let getGeoDistribution (logger: ILogger) (searchParams: SearchParams) =
    let corpus = Corpora.Server.getCorpus searchParams.CorpusCode

    match corpus.Config.Modality with
    | Spoken -> Spoken.getGeoDistribution logger searchParams
    | Written -> failwith "NOT IMPLEMENTED"

let getFrequencyList
    (logger: ILogger)
    (searchParams: SearchParams)
    (attributes: Cwb.PositionalAttribute list)
    (isCaseSensitive: bool)
    (freqListTokenBoundaries: FreqListTokenBoundaries)
    : Async<string []> =
    async {
        let corpus = Corpora.Server.getCorpus searchParams.CorpusCode

        let! results =
            let caseStr = if isCaseSensitive then "" else " %c"

            let matchStr =
                match freqListTokenBoundaries.From, freqListTokenBoundaries.To with
                | Some fromToken, Some toToken -> $"match[{fromToken - 1}] .. match[{toToken - 1}]"
                | Some fromToken, None -> $"match[{fromToken - 1}] .. matchend"
                | None, Some toToken -> $"match .. match[{toToken - 1}]"
                | None, None -> "match .. matchend"

            let attrs =
                [ for attr in attributes -> $"{matchStr} {attr.Code}{caseStr}" ]
                |> String.concat ", "

            let awk =
                "|LC_ALL=C awk '{f[$0]++}END{for(k in f){print f[k], k}}' |LC_ALL=C sort -nr"

            let cmd = $"tabulate QUERY {attrs} >\" {awk}\""

            let searchParamsForFrequencyList =
                { searchParams with
                    LastCount = None
                    Step = 1
                    Start = 0L
                    End = 1000000L }

            match corpus.Config.Modality with
            | Spoken -> Spoken.runQueries logger corpus searchParamsForFrequencyList (Some cmd)
            | Written -> Written.runQueries logger corpus searchParamsForFrequencyList (Some cmd)

        return
            results.Hits
            |> Array.map (fun hit -> Regex.Replace(hit, "__UNDEF__", ""))
    }


let downloadFrequencyList
    (logger: ILogger)
    (searchParams: SearchParams)
    (attributes: Cwb.PositionalAttribute list)
    (isCaseSensitive: bool)
    (freqListTokenBoundaries: FreqListTokenBoundaries)
    (format: DownloadFormat)
    : Async<byte []> =
    async {
        let! freqList = getFrequencyList logger searchParams attributes isCaseSensitive freqListTokenBoundaries

        match format with
        | Excel ->
            use workbook = new Excel.XLWorkbook()

            let worksheet = workbook.Worksheets.Add("Frequency list")

            // Create headers
            worksheet.Cell(1, 1).Value <- "Count"

            attributes
            |> List.iteri (fun index attr -> worksheet.Cell(1, index + 2).Value <- attr.Name)

            // Create a row for each row in the frequency list
            freqList
            |> Array.iteri (fun rowIndex row ->
                let m = Regex.Match(row, "^(\d+)\s+(.+)")

                // Put the count in the first column
                worksheet.Cell(rowIndex + 2, 1).Value <- int (m.Groups[1].Value)

                // Create a column for each attribute value in the row
                m.Groups[ 2 ].Value.Split('\t')
                |> Array.iteri (fun columnIndex attrValue ->
                    worksheet.Cell(rowIndex + 2, columnIndex + 2).Value <- attrValue))

            use outputStream = new MemoryStream()
            workbook.SaveAs(outputStream)
            return outputStream.ToArray()
        | Tsv ->
            let headers =
                attributes
                |> List.map (fun attr -> attr.Name)
                |> String.concat "\t"
                |> fun s -> "Count\t" + s

            let valueRows =
                freqList
                |> Array.map (fun row -> Regex.Replace(row, "^(\d+) ", "$1\t"))

            let output =
                Array.append [| headers |] valueRows
                |> String.concat "\n"

            return System.Text.Encoding.UTF8.GetBytes(output)
        | Csv ->
            let headers =
                attributes
                |> List.map (fun attr -> $"\"{attr.Name}\"")
                |> String.concat ","
                |> fun s -> "Count," + s

            let valueRows =
                freqList
                |> Array.map (fun row ->
                    let m = Regex.Match(row, "^(\d+)\s+(.+)")

                    let attrValues =
                        m.Groups[ 2 ].Value.Split('\t')
                        |> Array.map (fun attrValue -> $"\"{attrValue}\"")
                        |> String.concat ","

                    $"{m.Groups[1].Value},{attrValues}")

            let output =
                Array.append [| headers |] valueRows
                |> String.concat "\n"

            return System.Text.Encoding.UTF8.GetBytes(output)
    }

type StringCategoryTextIdsAndTokenCount =
    { CategoryValue: string
      TextIds: string
      TokenCount: int64 }

type NumberCategoryTextIdsAndTokenCount =
    { CategoryValue: int64
      TextIds: string
      TokenCount: int64 }

let getMetadataDistribution
    (logger: ILogger)
    (searchParams: SearchParams)
    (attributeCode: string)
    (categoryCode: string)
    (categoryType: Metadata.CategoryType)
    (keepZeroValues: bool)
    (accExcludedAttrValues: Set<string>)
    : Task<MetadataDistribution> =
    task {
        let corpus = Corpora.Server.getCorpus searchParams.CorpusCode

        let textIdAttr =
            match corpus.Config.Modality with
            | Spoken -> "who_name"
            | Written -> "text_id"

        let queryName = cwbQueryName corpus searchParams.SearchId

        let namedQuery =
            match corpus.Config.Modality with
            | Spoken -> queryName
            | Written -> "QUERY"

        let cmd =
            $"group {namedQuery} match {textIdAttr} by match {sanitizeString attributeCode}"

        let searchParamsForMetadataDistribution =
            { searchParams with
                LastCount = None
                Step = 1
                Start = 0L
                End = 1000000L }

        let! attrResults =
            match corpus.Config.Modality with
            | Spoken -> Spoken.runQueries logger corpus searchParamsForMetadataDistribution (Some cmd)
            | Written -> Written.runQueries logger corpus searchParamsForMetadataDistribution (Some cmd)

        let attrDistributionMap =
            attrResults.Hits
            |> Array.fold
                (fun (distrMap: Map<string, Map<string, int64>>) hit ->
                    let parts = hit.Split("\t")
                    let attrValue = parts[0]
                    let textId = parts[1]
                    let freq = int64 parts[2]

                    // Add the mapping from textId to frequency to the map associated
                    // with the current attribute value, creating a new map with only
                    // that mapping if none existed.
                    distrMap.Change(
                        attrValue,
                        fun maybeAttrValueMap ->
                            match maybeAttrValueMap with
                            | Some attrValueMap ->
                                attrValueMap.Change(
                                    textId,
                                    fun maybeFreq ->
                                        // If a text is split into several parts that are covered by
                                        // different CPUs in multi-CPU corpora, we may already have
                                        // registered a frequency for this metadata value and text ID.
                                        // If so, add to the existing one; otherwise just register the new one.
                                        match maybeFreq with
                                        | Some existingFreq -> Some(existingFreq + freq)
                                        | None -> Some freq
                                )
                                |> Some
                            | None -> [ (textId, freq) ] |> Map.ofList |> Some
                    ))
                Map.empty
            |> Map.filter (fun attrValue _ -> not (Set.contains attrValue accExcludedAttrValues))

        //////// FOR DEBUGGING /////////
        // attrDistributionMap
        // |> Map.toArray
        // |> Array.iter (fun (attr, valueFreqs) ->
        //     printfn $"{attr}"

        //     valueFreqs
        //     |> Map.toArray
        //     |> Array.iter (fun (metadataValue, freq) -> printfn $"{metadataValue}: {freq}"))


        let connStr = getConnectionString corpus.Config.Code

        use conn = new SqliteConnection(connStr)

        let catCode = Database.sanitizeString categoryCode

        let catJoin =
            if catCode.Contains('.') then
                let catTable = catCode.Split('.')[0]
                if catTable <> "texts" then " " + createJoin catTable else ""
            else
                ""
        let excludedManyToManyCategoriesSql = generateManyToManyExclusions searchParams.MetadataSelection

        let nonExcludedManyToManyCategories =
            searchParams.MetadataSelection
            |> Map.filter (fun key value -> not (key.Contains('.') && value.ShouldExclude))

        let metadataSelectionSql =
            generateMetadataSelectionSql None nonExcludedManyToManyCategories

        let joins = generateMetadataSelectionJoins (Some catCode) nonExcludedManyToManyCategories

        let column = getQualifiedColumnName catCode

        let categorySql =
            $"SELECT {column} AS CategoryValue, GROUP_CONCAT(DISTINCT texts.tid) AS TextIds, \
              SUM(texts.endpos - texts.startpos + 1) AS TokenCount \
              FROM texts{catJoin}{joins} \
              WHERE 1 = 1{metadataSelectionSql}{excludedManyToManyCategoriesSql} GROUP BY {column} ORDER BY {column}"

        let parameters = metadataSelectionToParamDict searchParams.MetadataSelection

        // For string categories, we return the values we get from the database, but for numerical categories
        // we need to convert them to strings. Because of type checking on the data we get from the database,
        // we need to implement the query for each value type separately even though they look identical (the
        // compiler will infer different types for the values in each case).
        let categoryValuesWithTextIdsAndTokenCounts =
            match categoryType with
            | Metadata.StringCategoryType ->
                let categoryRes =
                    (query logger conn categorySql (Some parameters))
                        .Result

                match categoryRes with
                | Ok (catDist: StringCategoryTextIdsAndTokenCount seq) -> catDist
                | Error ex -> raise ex
            | Metadata.NumberCategoryType ->
                let categoryRes =
                    (query logger conn categorySql (Some parameters))
                        .Result

                match categoryRes with
                | Ok (catDist: NumberCategoryTextIdsAndTokenCount seq) ->
                    seq {
                        for d in catDist ->
                            { CategoryValue = string d.CategoryValue
                              TextIds = d.TextIds
                              TokenCount = d.TokenCount }
                    }
                | Error ex -> raise ex
            |> Seq.filter (fun categoryValueInfo ->
                (not (isNull categoryValueInfo.CategoryValue)) && categoryValueInfo.CategoryValue <> "\N")
            |> Seq.toArray

        let totalTokenCount = categoryValuesWithTextIdsAndTokenCounts |> Array.sumBy (fun c -> c.TokenCount)
        let totalTokenCountFloat = float totalTokenCount

        let multiValueSql = $"SELECT GROUP_CONCAT(DISTINCT {column}) FROM texts \
                              INNER JOIN authors_texts ON authors_texts.tid = texts.tid \
                              INNER JOIN authors on authors.id = authors_texts.authors_id \
                              GROUP BY texts.tid HAVING COUNT({column}) > 1"

        let expectedProportions =
            [| for categoryValueInfo in categoryValuesWithTextIdsAndTokenCounts ->  float categoryValueInfo.TokenCount / totalTokenCountFloat |]

        let distribution =
            [| for pair in attrDistributionMap ->
                   let attrValue = pair.Key
                   let textIdsToFreqs = pair.Value

                   let metadataValueFreqs =
                       [| for categoryValueInfo in categoryValuesWithTextIdsAndTokenCounts do
                              // For each text ID that is associated with the current metadata category value,
                              // find the frequency associated with it in the map associated with the current attribute value.
                              // Summing all those frequencies gives us the total number of occurrences of this
                              // attribute value in texts associated with the current metadata value.
                              categoryValueInfo.TextIds.Split(",")
                              |> Array.fold
                                  (fun sum textId ->
                                      textIdsToFreqs.TryFind(textId)
                                      |> function
                                          | Some freq -> sum + freq
                                          | None -> sum)
                                  0L |]

                   let attrValueTotal = metadataValueFreqs |> Array.sum
                   let attrValueTotalFloat = float attrValueTotal
                   let observedProportions =
                       [| for freq in metadataValueFreqs -> float freq / attrValueTotalFloat |]
                   // Compute the Deviation of Proportions score (Gries 2008)
                   let dp =
                       Array.zip expectedProportions observedProportions
                       |> Array.sumBy (fun (expected, observed) ->
                           abs (expected - observed))
                       |> fun sum -> sum / 2.

                   { AttributeValue = attrValue
                     MetadataValueFrequencies = metadataValueFreqs
                     AttributeValueTotal = attrValueTotal
                     Dp = dp } |]

        let totals =
            distribution
            |> Array.fold
                (fun sums attributeValueDistribution ->
                    Array.zip sums attributeValueDistribution.MetadataValueFrequencies
                    |> Array.map (fun (sum, freq) -> sum + freq))
                (Array.create categoryValuesWithTextIdsAndTokenCounts.Length 0L)

        let summedTotalsFloat = Array.sum totals |> float

        let totalsProportions =
            [| for total in totals -> float total / summedTotalsFloat |]

        let totalDp =
            Array.zip expectedProportions totalsProportions
            |> Array.sumBy (fun (expected, observed) -> abs (expected - observed))
            |> fun sum -> sum / 2.

        let distribution' =
            if keepZeroValues then
                distribution
            else
                distribution
                |> Array.map (fun attributeValueDistribution ->
                    let newMetadataValueFreqs =
                        Array.zip attributeValueDistribution.MetadataValueFrequencies totals
                        |> Array.choose (fun (freq, total) -> if total > 0L then Some freq else None)

                    { attributeValueDistribution with MetadataValueFrequencies = newMetadataValueFreqs })

        let categoryValueStats =
            [ for catVal, total in Array.zip categoryValuesWithTextIdsAndTokenCounts totals do
                if keepZeroValues || total > 0L then
                    { Value = catVal.CategoryValue
                      CategoryValueTotal = total
                      TokenCount = catVal.TokenCount } ]

        return
            { Distribution = distribution'
              CategoryValueStats = categoryValueStats
              // The total token count that will be shown in a column to to the right of the individual category
              // value columns includes the token counts of values that have zero search hits as well, so we don't
              // care about the keepZeroValues parameter here
              TotalTokenCount = totalTokenCount
              TotalDp = totalDp }
    }

let downloadMetadataDistribution
    (logger: ILogger)
    (searchParams: SearchParams)
    (attributeCode: string)
    (categoryCode: string)
    (categoryType: Metadata.CategoryType)
    (keepZeroValues: bool)
    (accExcludedAttrValues: Set<string>)
    (format: DownloadFormat)
    : Task<byte[]> =
    task {
        let! distribution =
            getMetadataDistribution
                logger searchParams attributeCode categoryCode categoryType keepZeroValues accExcludedAttrValues

        match format with
        | Excel ->
            use workbook = new Excel.XLWorkbook()

            let worksheet = workbook.Worksheets.Add("Metadata distribution")

            // Create headers
            worksheet.Cell(1, 1).Value <- "Attribute value"

            // Create header row with the different metadata category values
            distribution.CategoryValueStats
            |> List.iteri (fun index categoryValueStat ->
                worksheet.Cell(1, index + 2).Value <- $"{categoryValueStat.Value} ({categoryValueStat.TokenCount} tokens)")

            let totalsColumnNumber = distribution.CategoryValueStats.Length + 3
            let dpColumnNumber = totalsColumnNumber + 1

            // Create a header with the total token count for all category values
            worksheet.Cell(1, totalsColumnNumber).Value <- $"Total ({distribution.TotalTokenCount} tokens)"

            // Create a header for Deviation of proportions (Gries 2008)
            worksheet.Cell(1, dpColumnNumber).Value <- "Deviation of proportions"

            // Create a row for each attribute value
            distribution.Distribution
            |> Array.iteri (fun rowIndex attrValueDistribution ->
                // Put the attribute value in the first column
                worksheet.Cell(rowIndex + 2, 1).Value <- attrValueDistribution.AttributeValue

                // Create a column for each metadata value frequency
                attrValueDistribution.MetadataValueFrequencies
                |> Array.iteri (fun columnIndex metadataValueFreq ->
                    worksheet.Cell(rowIndex + 2, columnIndex + 2).Value <- metadataValueFreq)

                // Create a cell with the total number of search hits for this attribute value
                worksheet.Cell(rowIndex + 2, totalsColumnNumber).Value <- (string attrValueDistribution.AttributeValueTotal)

                // Create a cell with the Deviation of proportion value for this attribute value
                worksheet.Cell(rowIndex + 2, dpColumnNumber).Value <- $"%.2f{attrValueDistribution.Dp}")

            let totalsRowIndex = distribution.Distribution.Length + 2

            worksheet.Cell(totalsRowIndex, 1).Value <- "Total"

            distribution.CategoryValueStats
            |> List.iteri(fun colIndex categoryValueStat ->
                // Create a column for each metadata value total
                    worksheet.Cell(totalsRowIndex, colIndex + 2).Value <- categoryValueStat.CategoryValueTotal)

            // Create a cell with the total number of search hits
            worksheet.Cell(totalsRowIndex, totalsColumnNumber).Value <- (
                          distribution.CategoryValueStats
                          |> List.sumBy (fun categoryValue -> categoryValue.CategoryValueTotal)
                          |> string
                      )

            // Create a cell with the total Deviation of proportions
            worksheet.Cell(totalsRowIndex, dpColumnNumber).Value <- $"%.2f{distribution.TotalDp}"

            use outputStream = new MemoryStream()
            workbook.SaveAs(outputStream)
            return outputStream.ToArray()
        | Tsv ->
            // Create header row with the different metadata category values
            let headerRow =
                distribution.CategoryValueStats
                |> List.map (fun categoryValueStat -> $"{categoryValueStat.Value} ({categoryValueStat.TokenCount} tokens)")
                |> String.concat "\t"
                |> fun s -> $"Attribute value\t{s}\tTotal ({distribution.TotalTokenCount} tokens)\tDeviation of proportions"

            let valueRows =
                distribution.Distribution
                |> Array.map (fun attrValueDistribution ->
                    attrValueDistribution.MetadataValueFrequencies
                    |> Array.map string
                    |> String.concat "\t"
                    |> fun s ->
                        $"{attrValueDistribution.AttributeValue}\t{s}\t\
                          {attrValueDistribution.AttributeValueTotal}\t%.2f{attrValueDistribution.Dp}")

            let totalHits =
                distribution.CategoryValueStats
                |> List.sumBy (fun categoryValue -> categoryValue.CategoryValueTotal)
                |> string

            let totalsRow =
                distribution.CategoryValueStats
                |> List.map (fun stat -> string stat.CategoryValueTotal)
                |> String.concat "\t"
                |> fun s -> $"Total\t{s}\t{totalHits}\t%.2f{distribution.TotalDp}"

            let output =
               Array.concat [ [| headerRow |]
                              valueRows
                              [| totalsRow |] ]
               |> String.concat "\n"
            let fileBytes = System.Text.Encoding.UTF8.GetBytes(output)
            return fileBytes
        | Csv ->
            // Create header row with the different metadata category values
            let headerRow =
                distribution.CategoryValueStats
                |> List.map (fun categoryValueStat -> $"\"{categoryValueStat.Value} ({categoryValueStat.TokenCount} tokens)\"")
                |> String.concat ","
                |> fun s -> $"\"Attribute value\",{s},\"Total ({distribution.TotalTokenCount} tokens)\",\"Deviation of proportions\""

            let valueRows =
                distribution.Distribution
                |> Array.map (fun attrValueDistribution ->
                    attrValueDistribution.MetadataValueFrequencies
                    |> Array.map string
                    |> String.concat ","
                    |> fun s ->
                        $"\"{attrValueDistribution.AttributeValue}\",{s},\
                          {attrValueDistribution.AttributeValueTotal},%.2f{attrValueDistribution.Dp}")

            let totalHits =
                distribution.CategoryValueStats
                |> List.sumBy (fun categoryValue -> categoryValue.CategoryValueTotal)
                |> string

            let totalsRow =
                distribution.CategoryValueStats
                |> List.map (fun stat -> string stat.CategoryValueTotal)
                |> String.concat ","
                |> fun s -> $"\"Total\",{s},{totalHits},%.2f{distribution.TotalDp}"

            let output =
               Array.concat [ [| headerRow |]
                              valueRows
                              [| totalsRow |] ]
               |> String.concat "\n"
            let fileBytes = System.Text.Encoding.UTF8.GetBytes(output)
            return fileBytes
    }
