module Remoting.Search.Core

open System.IO
open System.Text.RegularExpressions
open Serilog
open ClosedXML
open Microsoft.Data.Sqlite
open ServerTypes
open Shared

let searchCorpus (connStr: string) (logger: ILogger) (searchParams: SearchParams) =
    let corpus =
        Corpora.Server.getCorpus searchParams.CorpusCode

    match corpus.Config.SearchEngine with
    | Cwb -> Cwb.Core.searchCorpus connStr logger searchParams corpus
    | Fcs -> failwith "Not implemented"


let getSearchResults (logger: ILogger) (searchParams: SearchParams) maybeAttributes (pageNumbers: ResultPageNumbers) =
    async {
        let corpus =
            Corpora.Server.getCorpus searchParams.CorpusCode

        return!
            match corpus.Config.SearchEngine with
            | Cwb -> Cwb.Core.getSearchResults logger searchParams corpus maybeAttributes pageNumbers
            | Fcs -> failwith "Not implemented"
    }

type TidAndMetadataString =
    { Tid: string
      MetadataValueString: string }

type TidAndMetadataNumber =
    { Tid: string
      MetadataValueNumber: int64 }

type DbStringOrNumberMap =
    | DbStringMap of Map<string, string>
    | DbNumberMap of Map<string, int64>

let downloadSearchResults
    (logger: ILogger)
    (searchParams: SearchParams)
    (attributes: Cwb.PositionalAttribute list)
    (categoryInfos: Metadata.CategoryInfo list)
    (format: DownloadFormat)
    (shouldCreateHeader: bool)
    : Async<byte []> =
    async {
        let corpus =
            Corpora.Server.getCorpus searchParams.CorpusCode

        let! resultPages =
            let searchParamsForDownload =
                { searchParams with
                    LastCount = None
                    Step = 1
                    Start = 0L
                    End = 1000000L }

            let pageNumbers =
                Seq.initInfinite (fun index -> index)

            match corpus.Config.SearchEngine with
            | Cwb -> Cwb.Core.getSearchResults logger searchParamsForDownload corpus (Some attributes) pageNumbers
            | Fcs -> failwith "Not implemented"

        let metadata: Map<string, DbStringOrNumberMap> =
            let connStr =
                getConnectionString corpus.Config.Code

            use connection =
                new SqliteConnection(connStr)

            [ for categoryInfo in categoryInfos ->
                  let sanitizedCode =
                      Database.sanitizeString categoryInfo.Code

                  match categoryInfo.Type with
                  | Metadata.StringCategoryType ->
                      let sql =
                          $"SELECT tid as Tid, {sanitizedCode} AS MetadataValueString FROM texts"

                      let rowsTask =
                          Database.query logger connection sql None

                      match rowsTask.Result with
                      | Ok (rows: TidAndMetadataString seq) ->
                          let valueMap =
                              [ for row in rows -> row.Tid, row.MetadataValueString ]
                              |> Map.ofList

                          categoryInfo.Code, DbStringMap valueMap
                      | Error e -> raise e
                  | Metadata.NumberCategoryType ->
                      let sql =
                          $"SELECT tid as Tid, {sanitizedCode} AS MetadataValueNumber FROM texts"

                      let rowsTask =
                          Database.query logger connection sql None

                      match rowsTask.Result with
                      | Ok (rows: TidAndMetadataNumber seq) ->
                          let valueMap =
                              [ for row in rows -> row.Tid, row.MetadataValueNumber ]
                              |> Map.ofList

                          categoryInfo.Code, DbNumberMap valueMap
                      | Error e -> raise e ]
            |> Map.ofList

        let results =
            resultPages
            // Concatenate the search results from each result page (we don't care about
            // result pages when creating export files)
            |> Array.collect (fun resultPage ->
                resultPage.Results
                |> Array.map (fun result -> result.Text))
            // We need to take num-random-hits results because the saved search results may
            // contain slightly more due to rounding (when multi-cpu, multi-step search has been used)
            |> fun r ->
                match searchParams.NumRandomHits with
                | Some numRandomHits -> r |> Array.truncate (int numRandomHits)
                | None -> r
            // Concatenate all lines (for multilingual corpora there may be more than one line
            // per search result, while for other corpora there is only one.)
            |> Array.collect (fun hitLines -> hitLines |> List.toArray)
            |> Array.map (fun line ->
                if Regex.IsMatch(line, "^\s*-->\w+:") then
                    // Non-first line of a multilingual result: Return as is
                    ("", "", line, "", "")

                // In all other cases, extract corpus position, sentence/utterance ID,
                // left context, match and right context from the result line
                elif Regex.IsMatch(line, "<who_avfile ") then
                    // For speech corpora, the who_avfile attribute is included in the
                    // PrintStructures, so make sure we ignore that
                    let m =
                        Regex.Match(
                            line,
                            "^\s*(\d+):\s*<who_name\s(.+?)><who_avfile.+?>:\s*(.*?)\s*\{\{(.+?)\}\}\s*(.*)"
                        )

                    (m.Groups.[1].Value, m.Groups.[2].Value, m.Groups.[3].Value, m.Groups.[4].Value, m.Groups.[5].Value)
                else
                    let m =
                        Regex.Match(line, "^\s*(\d+):\s*<.+?\s(.+?)>:\s*(.*?)\s*\{\{(.+?)\}\}\s*(.*)")

                    (m.Groups.[1].Value, m.Groups.[2].Value, m.Groups.[3].Value, m.Groups.[4].Value, m.Groups.[5].Value))

        let idHeader =
            match corpus.Config.Modality with
            | Spoken -> "Informant ID"
            | Written -> "Sentence ID"

        let headers =
            [| "Corpus position"
               idHeader
               "Left context"
               "Match"
               "Right context" |]

        match format with
        | Excel ->
            use workbook = new Excel.XLWorkbook()

            let worksheet =
                workbook.Worksheets.Add("Search results")

            if shouldCreateHeader then
                worksheet.Cell(1, 1).Value <- "Corpus position"

                for index, categoryInfo in categoryInfos |> List.indexed do
                    worksheet.Cell(1, 2 + index).Value <- categoryInfo.Name

                worksheet.Cell(1, categoryInfos.Length + 2).Value <- "Left context"
                worksheet.Cell(1, categoryInfos.Length + 3).Value <- "Match"
                worksheet.Cell(1, categoryInfos.Length + 4).Value <- "Right context"

            let rowDisplacement =
                if shouldCreateHeader then 2 else 1

            results
            |> Array.iteri (fun resultIndex (corpusPosition, segmentId, leftContext, theMatch, rightContext) ->
                worksheet.Cell(resultIndex + rowDisplacement, 1).Value <- corpusPosition

                for index, categoryInfo in categoryInfos |> List.indexed do
                    match metadata[categoryInfo.Code] with
                    | DbStringMap valueMap ->
                        worksheet.Cell(resultIndex + rowDisplacement, index + 2).Value <- match
                                                                                              valueMap.TryFind
                                                                                                  (segmentId)
                                                                                              with
                                                                                          | Some value -> value
                                                                                          | None -> ""
                    | DbNumberMap valueMap ->
                        worksheet.Cell(resultIndex + rowDisplacement, index + 2).Value <- match
                                                                                              valueMap.TryFind
                                                                                                  (segmentId)
                                                                                              with
                                                                                          | Some value -> value
                                                                                          | None -> 0

                worksheet.Cell(resultIndex + rowDisplacement, categoryInfos.Length + 2).Value <- leftContext
                worksheet.Cell(resultIndex + rowDisplacement, categoryInfos.Length + 3).Value <- theMatch
                worksheet.Cell(resultIndex + rowDisplacement, categoryInfos.Length + 4).Value <- rightContext)

            use outputStream = new MemoryStream()
            workbook.SaveAs(outputStream)
            return outputStream.ToArray()
        | Tsv ->
            let headerRow =
                headers |> String.concat "\t"

            let resultRows =
                [| for corpusPosition, segmentId, leftContext, theMatch, rightContext in results ->
                       [ corpusPosition
                         segmentId
                         leftContext
                         theMatch
                         rightContext ]
                       |> String.concat "\t" |]

            let output =
                Array.append [| headerRow |] resultRows
                |> String.concat "\n"

            return System.Text.Encoding.UTF8.GetBytes(output)

        | Csv ->
            let headerRow =
                headers
                |> Array.map (fun s -> $"\"{s}\"")
                |> String.concat ","

            let resultRows =
                [| for corpusPosition, segmentId, leftContext, theMatch, rightContext in results ->
                       $"\"{corpusPosition}\",\"{segmentId}\",\"{leftContext}\",\"{theMatch}\",\"{rightContext}\"" |]

            let output =
                Array.append [| headerRow |] resultRows
                |> String.concat "\n"

            return System.Text.Encoding.UTF8.GetBytes(output)
    }
