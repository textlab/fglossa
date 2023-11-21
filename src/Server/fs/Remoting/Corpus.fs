module Remoting.Corpus

open System
open Microsoft.Data.Sqlite
open Serilog
open ServerTypes
open Database
open Shared
open Metadata

module Spoken =
    let getTextAndTokenCount (logger: ILogger) (corpus: Corpus) (selection: Selection) =
        task {
            let connStr =
                getConnectionString corpus.Config.Code

            use conn = new SqliteConnection(connStr)

            let excludedManyToManyCategoriesSql =
                generateManyToManyExclusions selection

            let nonExcludedManyToManyCategories =
                getNonExcludedManyToManyCategories selection

            let metadataSelectionSql =
                generateMetadataSelectionSql None nonExcludedManyToManyCategories

            let joins =
                generateMetadataSelectionJoins None nonExcludedManyToManyCategories

            let parameters =
                metadataSelectionToParamDict selection

            let textSql =
                $"SELECT COUNT(DISTINCT tid) as NumTexts FROM texts{joins} WHERE 1 = 1{metadataSelectionSql}{excludedManyToManyCategoriesSql}"

            let! textRes = querySingle logger conn textSql (Some parameters)

            let numTexts =
                match textRes with
                | Ok maybeNumTexts ->
                    match maybeNumTexts with
                    | Some (num: int64) -> num
                    | None -> 0L
                | Error ex -> raise ex

            let tokenSql =
                $"SELECT bounds FROM texts{joins} WHERE 1 = 1{metadataSelectionSql}{excludedManyToManyCategoriesSql}"

            let! tokenRes = query logger conn tokenSql (Some parameters)

            let numTokens =
                match tokenRes with
                | Ok (bounds: string seq) ->
                    bounds
                    |> Seq.filter (not << String.IsNullOrWhiteSpace)
                    |> Seq.collect (fun b -> b.Split(':'))
                    |> Seq.sumBy (fun b ->
                        let parts = b.Split('-')

                        try
                            let startBound = Int64.Parse(parts[0])
                            let endBound = Int64.Parse(parts[1])
                            endBound - startBound + 1L
                        with
                        | ex -> failwith $"Error in bounds value: {b}")
                | Error ex -> raise ex

            return
                { NumTexts = numTexts
                  NumTokens = numTokens }
        }

module Written =
    let getTextAndTokenCount (logger: ILogger) (corpus: Corpus) (selection: Selection) =
        task {
            let connStr =
                getConnectionString corpus.Config.Code

            use conn = new SqliteConnection(connStr)

            let excludedManyToManyCategoriesSql =
                generateManyToManyExclusions selection

            let nonExcludedManyToManyCategories =
                getNonExcludedManyToManyCategories selection

            let metadataSelectionSql =
                generateMetadataSelectionSql None nonExcludedManyToManyCategories

            let joins =
                generateMetadataSelectionJoins None nonExcludedManyToManyCategories

            let sql =
                $"SELECT count(DISTINCT texts.tid) as NumTexts, sum(endpos - startpos + 1) as NumTokens FROM texts{joins} \
                  WHERE 1 = 1{metadataSelectionSql}{excludedManyToManyCategoriesSql}"

            let parameters =
                metadataSelectionToParamDict selection

            let! res = querySingle logger conn sql (Some parameters)

            match res with
            | Ok maybeCounts ->
                return
                    match maybeCounts with
                    | Some (counts: TextAndTokenCounts) -> counts
                    | None -> { NumTexts = 0L; NumTokens = 0L }
            | Error ex ->
                if ex.Message.Contains("no such column: texts.tid") then
                    printfn $"NOTE: No db column texts.tid found for corpus {corpus.Config.Code}."
                    return { NumTexts = 0L; NumTokens = 0L }
                else
                    return raise ex
        }

let getCorpusList () =
    async { return Corpora.Server.getCorpusList () }

let getCorpusConfig (logger: ILogger) (corpusCode: string) =
    task {
        let corpus =
            Corpora.Server.getCorpus corpusCode

        let! numTextsAndTokens =
            match corpus.Config.Modality with
            | Spoken -> Spoken.getTextAndTokenCount logger corpus Map.empty
            | Written -> Written.getTextAndTokenCount logger corpus Map.empty

        return
            { corpus.Config with
                TotalTexts = numTextsAndTokens.NumTexts
                TotalTokens = numTextsAndTokens.NumTokens }
    }

let getTextAndTokenCount (logger: ILogger) (corpusCode: string) (selection: Selection) =
    task {
        let corpus =
            Corpora.Server.getCorpus corpusCode

        let! textAndTokenCount =
            match corpus.Config.Modality with
            | Spoken -> Spoken.getTextAndTokenCount logger corpus selection
            | Written -> Written.getTextAndTokenCount logger corpus selection

        let! textSelectionInfo = corpus.GetTextSelectionInfo(logger, selection)

        return (textAndTokenCount, TextSelectionInfo(textSelectionInfo))
    }
