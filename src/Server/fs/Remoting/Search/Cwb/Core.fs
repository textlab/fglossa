module Remoting.Search.Cwb.Core

open System.IO
open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Text.RegularExpressions
open System.Data.SQLite
open Serilog
open ClosedXML
open ServerTypes
open Shared
open Database

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
                | Fcs -> failwith "NOT IMPLMENTED"
    }

// If the number of running CQP processes exceeds this number, we do not allow a new
// search in a corpus that does parallel search using all cpus to be started.
let maxCqpProcesses = 8

let searchCorpus (connStr: string) (logger: ILogger) (searchParams: SearchParams) (corpus: Corpus) =
    async {
        let cqpProcs =
            Process.runCmdWithOutput "pgrep" "-f cqp"

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
                    use connection = new SQLiteConnection(connStr)

                    let res =
                        (insert logger connection "Search" searchData)
                            .Result

                    match res with
                    | Ok id -> id
                    | Error ex -> raise ex
                else
                    searchParams.SearchId

            let searchParamsWithSearchId =
                { searchParams with SearchId = searchId }

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

let getFrequencyList
    (logger: ILogger)
    (searchParams: SearchParams)
    (attributes: Cwb.PositionalAttribute list)
    (isCaseSensitive: bool)
    : Async<string []> =
    async {
        let corpus =
            Corpora.Server.getCorpus searchParams.CorpusCode

        let! results =
            let caseStr = if isCaseSensitive then " %c" else ""

            let attrs =
                [ for attr in attributes -> $"match .. matchend {attr.Code}{caseStr}" ]
                |> String.concat ", "

            let awk =
                "|LC_ALL=C awk '{f[$0]++}END{for(k in f){print f[k], k}}' |LC_ALL=C sort -nr"

            let cmd = $"tabulate QUERY {attrs} >\" {awk}\""

            match corpus.Config.Modality with
            | Spoken -> Spoken.runQueries logger corpus searchParams (Some cmd)
            | Written -> Written.runQueries logger corpus searchParams (Some cmd)

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

        let downloadFilename =
            $"tmp/{searchParams.SearchId}_freq{extension}"

        let outputFilename = $"../Client/public/{downloadFilename}"

        match format with
        | Excel ->
            use workbook = new Excel.XLWorkbook()

            let worksheet =
                workbook.Worksheets.Add("Frequency list")

            // Create headers
            worksheet.Cell(1, 1).Value <- "Count"

            attributes
            |> List.iteri (fun index attr -> worksheet.Cell(1, index + 2).Value <- attr.Name)

            // Create a row for each row in the frequency list
            freqList
            |> Array.iteri (fun rowIndex row ->
                let m = Regex.Match(row, "^(\d+)\s+(.+)")

                // Put the count in the first column
                worksheet.Cell(rowIndex + 2, 1).Value <- int (m.Groups.[1].Value)

                // Create a colunn for each attribute value in the row
                m.Groups.[2].Value.Split('\t')
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
                        m.Groups.[2].Value.Split('\t')
                        |> Array.map (fun attrValue -> $"\"{attrValue}\"")
                        |> String.concat ","

                    $"{m.Groups.[1].Value},{attrValues}")

            File.WriteAllLines(outputFilename, Array.append [| headers |] valueRows)

        return downloadFilename
    }
