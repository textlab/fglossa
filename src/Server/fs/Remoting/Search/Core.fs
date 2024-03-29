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

            let selectedAttributes =
                match format with
                | Multiline ->
                    // If the Multiline format is selected, we want either a phon or orig attribute, which will
                    // be shown under the line with the word (default) attribute
                    let extraAttribute =
                        match corpus.Config.TryGetAttribute("phon") with
                        | Some attr -> attr
                        | None ->
                            match corpus.Config.TryGetAttribute("orig") with
                            | Some attr -> attr
                            | None -> failwith "Multiline format requires either phon or orig attribute!"

                    [ corpus.Config.GetDefaultAttribute()
                      extraAttribute ]
                | _ ->
                    // Otherwise, we want the list of attributes that the user has selected
                    attributes

            match corpus.Config.SearchEngine with
            | Cwb ->
                Cwb.Core.getSearchResults logger searchParamsForDownload corpus (Some selectedAttributes) pageNumbers
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
                  // If this is a numerical category which is not many-to-many, return numbers
                  | Metadata.NumberCategoryType when categoryInfo.Table = "texts" ->
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
                      | Error e -> raise e

                  // Otherwise, i.e., if this is a string category (regardless of whether it is many-to-many),
                  // or it is a numerical many-to-many category, return strings (in the latter case the returned string
                  // will contain the concatenated numbers).
                  | _ ->
                      let sql =
                          if categoryInfo.Table = "texts" then
                              $"SELECT tid as Tid, {sanitizedCode} AS MetadataValueString FROM texts"
                          else
                              let joinTable =
                                  $"{categoryInfo.Table}_texts"

                              $"SELECT {joinTable}.tid AS Tid, GROUP_CONCAT({sanitizedCode}, '; ') AS MetadataValueString FROM {joinTable} \
                               INNER JOIN {categoryInfo.Table} ON {categoryInfo.Table}.id = {joinTable}.{categoryInfo.Table}_id
                               WHERE {sanitizedCode} NOT IN ('', '\N') \
                               GROUP BY {joinTable}.tid"

                      let rowsTask =
                          Database.query logger connection sql None

                      match rowsTask.Result with
                      | Ok (rows: TidAndMetadataString seq) ->
                          let valueMap =
                              [ for row in rows -> row.Tid, row.MetadataValueString ]
                              |> Map.ofList

                          categoryInfo.Code, DbStringMap valueMap
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

                    (m.Groups[1].Value, m.Groups[2].Value, m.Groups[3].Value, m.Groups[4].Value, m.Groups[5].Value)
                else
                    let m =
                        Regex.Match(line, "^\s*(\d+):\s*<.+?\s(.+?)>:\s*(.*?)\s*\{\{(.+?)\}\}\s*(.*)")

                    (m.Groups[1].Value, m.Groups[2].Value, m.Groups[3].Value, m.Groups[4].Value, m.Groups[5].Value))

        let idHeader =
            match corpus.Config.Modality with
            | Spoken -> "Informant ID"
            | Written -> "Sentence ID"

        let headers =
            [ yield "Corpus position"
              yield idHeader
              for categoryInfo in categoryInfos do
                  yield categoryInfo.Name
              yield "Left context"
              yield "Match"
              yield "Right context" ]

        let getMetadataValues tid separator =
            [ for categoryInfo in categoryInfos ->
                  match metadata[categoryInfo.Code] with
                  | DbStringMap valueMap ->
                      match valueMap.TryFind(tid) with
                      | Some value ->
                          if separator = "," then
                              $"\"{value}\""
                          else
                              value
                      | None -> ""
                  | DbNumberMap valueMap ->
                      match valueMap.TryFind(tid) with
                      | Some value ->
                          if separator = "," then
                              $"\"{value}\""
                          else
                              string value
                      | None -> "" ]

        let constructExportRows (separator: string) =
            [| for corpusPosition, segmentId, leftContext, theMatch, rightContext in results do
                   // Strip everything after the dot from the segment ID to get the text ID. (Note that in spoken corpora,
                   // those are actually the same, so the segment ID will not contain any dot).
                   let tid =
                       segmentId |> StringUtils.replace "\..+" ""

                   let metadataValues =
                       getMetadataValues tid separator

                   if separator = "," then
                       [ yield corpusPosition
                         yield segmentId
                         yield! metadataValues
                         yield $"\"{leftContext}\""
                         yield $"\"{theMatch}\""
                         yield $"\"{rightContext}\"" ]
                   else
                       [ yield corpusPosition
                         yield segmentId
                         yield! metadataValues
                         yield leftContext
                         yield theMatch
                         yield rightContext ]
                   |> String.concat separator |]

        match format with
        | Multiline ->
            let output =
                [| for _, segmentId, leftContext, theMatch, rightContext in results do
                       // Strip everything after the dot from the segment ID to get the text ID. (Note that in spoken corpora,
                       // those are actually the same, so the segment ID will not contain any dot).
                       let tid =
                           segmentId |> StringUtils.replace "\..+" ""

                       let metadataValues =
                           getMetadataValues tid " "

                       // In each part of the search result (the contexts and the match), each token should be represented as its
                       // word attribute and either the phon or the orig attribute separated by a slash
                       let entireResult =
                           [ leftContext; theMatch; rightContext ]
                           |> List.filter (fun part -> (not (System.String.IsNullOrWhiteSpace(part))))
                           |> String.concat " "

                       let attrValues =
                           [| for token in entireResult.Split(' ') -> token.Split('/') |]

                       let line1 =
                           [| yield segmentId
                              yield! metadataValues
                              for token in attrValues do
                                  yield token[0] |]
                           |> String.concat " "

                       let line2 =
                           [| yield segmentId
                              yield! metadataValues
                              for token in attrValues do
                                  yield token[1] |]
                           |> String.concat " "

                       [ line1; line2 ] |> String.concat "\n" |]
                |> String.concat "\n\n"

            return System.Text.Encoding.UTF8.GetBytes(output)
        | Excel ->
            use workbook = new Excel.XLWorkbook()

            let worksheet =
                workbook.Worksheets.Add("Search results")

            if shouldCreateHeader then
                worksheet.Cell(1, 1).Value <- "Corpus position"
                worksheet.Cell(1, 2).Value <- idHeader

                for index, categoryInfo in categoryInfos |> List.indexed do
                    worksheet.Cell(1, 3 + index).Value <- categoryInfo.Name

                worksheet.Cell(1, categoryInfos.Length + 3).Value <- "Left context"
                worksheet.Cell(1, categoryInfos.Length + 4).Value <- "Match"
                worksheet.Cell(1, categoryInfos.Length + 5).Value <- "Right context"

            let rowDisplacement =
                if shouldCreateHeader then 2 else 1

            results
            |> Array.iteri (fun resultIndex (corpusPosition, segmentId, leftContext, theMatch, rightContext) ->
                // Strip everything after the dot from the segment ID to get the text ID. (Note that in spoken corpora,
                // those are actually the same, so the segment ID will not contain any dot).
                let tid =
                    segmentId |> StringUtils.replace "\..+" ""

                worksheet.Cell(resultIndex + rowDisplacement, 1).Value <- corpusPosition
                worksheet.Cell(resultIndex + rowDisplacement, 2).Value <- segmentId

                for index, categoryInfo in categoryInfos |> List.indexed do
                    match metadata[categoryInfo.Code] with
                    | DbStringMap valueMap ->
                        worksheet.Cell(resultIndex + rowDisplacement, index + 3).Value <- match valueMap.TryFind(tid)
                                                                                              with
                                                                                          | Some value -> value
                                                                                          | None -> ""
                    | DbNumberMap valueMap ->
                        worksheet.Cell(resultIndex + rowDisplacement, index + 3).Value <- match valueMap.TryFind(tid)
                                                                                              with
                                                                                          | Some value -> value
                                                                                          | None -> 0

                worksheet.Cell(resultIndex + rowDisplacement, categoryInfos.Length + 3).Value <- leftContext
                worksheet.Cell(resultIndex + rowDisplacement, categoryInfos.Length + 4).Value <- theMatch
                worksheet.Cell(resultIndex + rowDisplacement, categoryInfos.Length + 5).Value <- rightContext)

            use outputStream = new MemoryStream()
            workbook.SaveAs(outputStream)
            return outputStream.ToArray()
        | Tsv ->
            let headerRow =
                headers |> String.concat "\t"

            let resultRows = constructExportRows "\t"

            let output =
                if shouldCreateHeader then
                    Array.append [| headerRow |] resultRows
                else
                    resultRows
                |> String.concat "\n"

            return System.Text.Encoding.UTF8.GetBytes(output)

        | Csv ->
            let headerRow =
                headers
                |> List.map (fun s -> $"\"{s}\"")
                |> String.concat ","

            let resultRows = constructExportRows ","

            let output =
                if shouldCreateHeader then
                    Array.append [| headerRow |] resultRows
                else
                    resultRows
                |> String.concat "\n"

            return System.Text.Encoding.UTF8.GetBytes(output)
    }
