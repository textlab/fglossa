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

let getFrequencyList
    (logger: ILogger)
    (searchParams: SearchParams)
    (attributes: Cwb.PositionalAttribute list)
    (isCaseSensitive: bool)
    : Async<string []> =
    async {
        let corpus = Corpora.Server.getCorpus searchParams.CorpusCode

        let! results =
            let caseStr = if isCaseSensitive then "" else " %c"

            let attrs =
                [ for attr in attributes -> $"match .. matchend {attr.Code}{caseStr}" ]
                |> String.concat ", "

            let awk =
                "|LC_ALL=C awk '{f[$0]++}END{for(k in f){print f[k], k}}' |LC_ALL=C sort -nr"

            let cmd = $"tabulate QUERY {attrs} >\" {awk}\""

            let searchParamsForFrequencyList =
                { searchParams with
                    LastCount = None
                    Step = 1
                    Start = 0UL
                    End = 1000000UL }

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
    (format: DownloadFormat)
    : Async<string> =
    async {
        let! freqList = getFrequencyList logger searchParams attributes isCaseSensitive

        let extension =
            match format with
            | Excel -> ".xlsx"
            | Tsv -> ".tsv"
            | Csv -> ".csv"

        let downloadFilename = $"tmp/{searchParams.SearchId}_freq{extension}"

        let outputFilename = $"../Client/public/{downloadFilename}"

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

            workbook.SaveAs(outputFilename)
        | Tsv ->
            let headers =
                attributes
                |> List.map (fun attr -> attr.Name)
                |> String.concat "\t"
                |> fun s -> "Count\t" + s

            let valueRows =
                freqList
                |> Array.map (fun row -> Regex.Replace(row, "^(\d+) ", "$1\t"))

            File.WriteAllLines(outputFilename, Array.append [| headers |] valueRows)
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

            File.WriteAllLines(outputFilename, Array.append [| headers |] valueRows)

        return downloadFilename
    }

type StringCategoryTextIds =
    { CategoryValue: string
      TextIds: string }

type NumberCategoryTextIds =
    { CategoryValue: int64
      TextIds: string }

let getMetadataDistribution
    (logger: ILogger)
    (searchParams: SearchParams)
    (attributeCode: string)
    (categoryCode: string)
    (categoryType: Metadata.CategoryType)
    (keepZeroValues: bool)
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
                Start = 0UL
                End = 1000000UL }

        let! attrResults =
            match corpus.Config.Modality with
            | Spoken -> Spoken.runQueries logger corpus searchParamsForMetadataDistribution (Some cmd)
            | Written -> Written.runQueries logger corpus searchParamsForMetadataDistribution (Some cmd)

        let attrDistributionMap =
            attrResults.Hits
            |> Array.fold
                (fun (distrMap: Map<string, Map<string, uint64>>) hit ->
                    let parts = hit.Split("\t")
                    let attrValue = parts[0]
                    let textId = parts[1]
                    let freq = uint64 parts[2]

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
            $"SELECT {column} AS CategoryValue, GROUP_CONCAT(DISTINCT texts.tid) AS TextIds FROM texts{catJoin}{joins} \
             WHERE 1 = 1{metadataSelectionSql}{excludedManyToManyCategoriesSql} GROUP BY {column}"

        let parameters = metadataSelectionToParamDict searchParams.MetadataSelection

        // For string categories, we return the values we get from the database, but for numerical categories
        // we need to convert them to strings. Because of type checking on the data we get from the database,
        // we need to implement the query for each value type separately even though they look identical (the
        // compiler will infer different types for the values in each case).
        let categoryValuesToTextIds =
            match categoryType with
            | Metadata.StringCategoryType ->
                let categoryRes =
                    (query logger conn categorySql (Some parameters))
                        .Result

                match categoryRes with
                | Ok (catDist: StringCategoryTextIds seq) -> catDist
                | Error ex -> raise ex
            | Metadata.NumberCategoryType ->
                let categoryRes =
                    (query logger conn categorySql (Some parameters))
                        .Result

                match categoryRes with
                | Ok (catDist: NumberCategoryTextIds seq) ->
                    seq {
                        for d in catDist ->
                            { CategoryValue = string d.CategoryValue
                              TextIds = d.TextIds }
                    }
                | Error ex -> raise ex
            |> Seq.toArray

        let distribution =
            [| for pair in attrDistributionMap ->
                   let attrValue = pair.Key
                   let textIdsToFreqs = pair.Value

                   let metadataValueFrequencies =
                       [| for row in categoryValuesToTextIds do
                              // For each text ID that is associated with the current metadata category value,
                              // find the frequency associated with it in the map associated with the current attribute value.
                              // Summing all those frequencies gives us the total number of occurrences of this
                              // attribute value in texts associated with the current metadata value.
                              let total =
                                  row.TextIds.Split(",")
                                  |> Array.fold
                                      (fun sum textId ->
                                          textIdsToFreqs.TryFind(textId)
                                          |> function
                                              | Some freq -> sum + freq
                                              | None -> sum)
                                      0UL

                              { MetadataValue =
                                  if isNull row.CategoryValue then
                                      "Undefined"
                                  else
                                      row.CategoryValue
                                Frequency = total } |]

                   { AttributeValue = attrValue
                     MetadataValueFrequencies = metadataValueFrequencies } |]

        let totals =
            distribution
            |> Array.fold
                (fun sums attributeValueDistribution ->
                    Array.zip sums attributeValueDistribution.MetadataValueFrequencies
                    |> Array.map (fun (sum, freq) -> sum + freq.Frequency))
                (Array.create categoryValuesToTextIds.Length 0UL)

        let distribution' =
            if keepZeroValues then
                distribution
            else
                distribution
                |> Array.map (fun attributeValueDistribution ->
                    let newMetadataValueFreqs =
                        Array.zip attributeValueDistribution.MetadataValueFrequencies totals
                        |> Array.choose (fun (freq, total) -> if total > 0UL then Some freq else None)

                    { attributeValueDistribution with MetadataValueFrequencies = newMetadataValueFreqs })

        return
            { Distribution = distribution'
              CategoryValueTotals = totals }
    }

let downloadMetadataDistribution
    (logger: ILogger)
    (searchParams: SearchParams)
    (attributeCode: string)
    (categoryCode: string)
    (categoryType: Metadata.CategoryType)
    (keepZeroValues: bool)
    (format: DownloadFormat)
    : Task<string> =
    task {
        let! distribution =
            getMetadataDistribution logger searchParams attributeCode categoryCode categoryType keepZeroValues

        let extension =
            match format with
            | Excel -> ".xlsx"
            | Tsv -> ".tsv"
            | Csv -> ".csv"

        let downloadFilename = $"tmp/{searchParams.SearchId}_distr{extension}"

        let outputFilename = $"../Client/public/{downloadFilename}"

        match format with
        | Excel ->
            use workbook = new Excel.XLWorkbook()

            let worksheet = workbook.Worksheets.Add("Metadata distribution")

            // Create headers
            worksheet.Cell(1, 1).Value <- "Attribute value"

            // Get the headers as the metadata values found in the first result row
            let firstRow = distribution.Distribution |> Array.head

            firstRow.MetadataValueFrequencies
            |> Array.iteri (fun index metadataValueFreq ->
                worksheet.Cell(1, index + 2).Value <- metadataValueFreq.MetadataValue)

            // Create a row for each attribute value
            distribution.Distribution
            |> Array.iteri (fun rowIndex attributeValueDistribution ->
                // Put the attribute value in the first column
                worksheet.Cell(rowIndex + 2, 1).Value <- attributeValueDistribution.AttributeValue

                // Create a colunn for each metadata value frequency
                attributeValueDistribution.MetadataValueFrequencies
                |> Array.iteri (fun columnIndex metadataValueFreq ->
                    worksheet.Cell(rowIndex + 2, columnIndex + 2).Value <- metadataValueFreq.Frequency))

            let totalsRowIndex = distribution.Distribution.Length + 2

            worksheet.Cell(totalsRowIndex, 1).Value <- "Total"

            distribution.CategoryValueTotals
            |> Array.iteri (fun columnIndex categoryValueTotal ->
                // Create a colunn for each metadata value total
                worksheet.Cell(totalsRowIndex, columnIndex + 2).Value <- categoryValueTotal)

            workbook.SaveAs(outputFilename)
        | Tsv ->
            // Get the headers as the metadata values found in the first result row
            let headerRow =
                let firstRow = distribution.Distribution |> Array.head

                firstRow.MetadataValueFrequencies
                |> Array.map (fun metadataValueFreq -> metadataValueFreq.MetadataValue)
                |> String.concat "\t"
                |> fun s -> "Attribute value\t" + s

            let valueRows =
                distribution.Distribution
                |> Array.map (fun attributeValueDistribution ->
                    attributeValueDistribution.MetadataValueFrequencies
                    |> Array.map (fun metadataValueFreq -> string metadataValueFreq.Frequency)
                    |> String.concat "\t"
                    |> fun s -> $"{attributeValueDistribution.AttributeValue}\t{s}")

            let totalsRow =
                distribution.CategoryValueTotals
                |> Array.map string
                |> String.concat "\t"
                |> fun s -> "Total\t" + s

            File.WriteAllLines(
                outputFilename,
                Array.concat [ [| headerRow |]
                               valueRows
                               [| totalsRow |] ]
            )
        | Csv ->
            // Get the headers as the metadata values found in the first result row
            let headerRow =
                let firstRow = distribution.Distribution |> Array.head

                firstRow.MetadataValueFrequencies
                |> Array.map (fun metadataValueFreq -> $"\"{metadataValueFreq.MetadataValue}\"")
                |> String.concat ","
                |> fun s -> "\"Attribute value\"," + s

            let valueRows =
                distribution.Distribution
                |> Array.map (fun attributeValueDistribution ->
                    attributeValueDistribution.MetadataValueFrequencies
                    |> Array.map (fun metadataValueFreq -> string metadataValueFreq.Frequency)
                    |> String.concat ","
                    |> fun s -> $"\"{attributeValueDistribution.AttributeValue}\",{s}")

            let totalsRow =
                distribution.CategoryValueTotals
                |> Array.map string
                |> String.concat ","
                |> fun s -> "Total," + s

            File.WriteAllLines(
                outputFilename,
                Array.concat [ [| headerRow |]
                               valueRows
                               [| totalsRow |] ]
            )

        return downloadFilename
    }
