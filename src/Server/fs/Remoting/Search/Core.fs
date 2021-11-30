module Remoting.Search.Core

open Serilog
open ClosedXML
open Shared
open ServerTypes

let searchCorpus (connStr: string) (logger: ILogger) (searchParams: SearchParams) =
    let corpus = Corpora.Server.getCorpus searchParams.CorpusCode

    match corpus.Config.SearchEngine with
    | Cwb -> Cwb.Core.searchCorpus connStr logger searchParams corpus
    | Fcs -> failwith "Not implemented"


let getSearchResults (logger: ILogger) (searchParams: SearchParams) maybeAttributes (pageNumbers: ResultPageNumbers) =
    async {
        let corpus = Corpora.Server.getCorpus searchParams.CorpusCode

        return!
            match corpus.Config.SearchEngine with
            | Cwb -> Cwb.Core.getSearchResults logger searchParams corpus maybeAttributes pageNumbers
            | Fcs -> failwith "Not implemented"
    }


let downloadSearchResults
    (logger: ILogger)
    (searchParams: SearchParams)
    (attributes: Cwb.PositionalAttribute list)
    (format: DownloadFormat)
    (shouldCreateHeader: bool)
    =
    async {
        let corpus = Corpora.Server.getCorpus searchParams.CorpusCode

        match corpus.Config.LanguageConfig with
        | Monolingual _ ->

            let! resultPages =
                let searchParamsForDownload =
                    { searchParams with
                        LastCount = None
                        Step = 1
                        Start = 0UL
                        End = 1000000UL }

                let pageNumbers = Seq.initInfinite (fun index -> index)

                match corpus.Config.SearchEngine with
                | Cwb -> Cwb.Core.getSearchResults logger searchParamsForDownload corpus (Some attributes) pageNumbers
                | Fcs -> failwith "Not implemented"

            let extension =
                match format with
                | Excel -> ".xlsx"
                | Tsv -> ".tsv"
                | Csv -> ".csv"

            let downloadFilename = $"tmp/{searchParams.SearchId}_res{extension}"

            let outputFilename = $"../Client/public/{downloadFilename}"

            match format with
            | Excel ->
                use workbook = new Excel.XLWorkbook()

                let worksheet = workbook.Worksheets.Add("Search results")

                if shouldCreateHeader then
                    worksheet.Cell(1, 1).Value <- "Corpus position"

                    worksheet.Cell(1, 2).Value <- match corpus.Config.Modality with
                                                  | Spoken -> "Informant ID"
                                                  | Written -> "Sentence ID"

                    worksheet.Cell(1, 3).Value <- "Left context"
                    worksheet.Cell(1, 4).Value <- "Match"
                    worksheet.Cell(1, 5).Value <- "Right context"

                let rowDisplacement = if shouldCreateHeader then 2 else 1

                let results =
                    [| for resultPage in resultPages do
                           yield!
                               [| for result in resultPage.Results ->
                                      // Since we know this is a monolingual corpus, there should be
                                      // only one line for each search result, so take the list head
                                      result.Text |> List.head |] |]

                results
                |> Array.iteri (fun resultIndex result ->
                    worksheet.Cell(resultIndex + rowDisplacement, 1).Value <- result)

                workbook.SaveAs(outputFilename)

            | Tsv -> ignore ()
            | Csv -> ignore ()

            return downloadFilename

        | Multilingual _ -> return failwith "DOWNLOAD OF MULTILINGUAL RESULTS NOT IMPLEMENTED"
    }
