module Remoting.Corpus

open System.Data.SQLite
open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Text.RegularExpressions
open Serilog
open ServerTypes
open Database
open Shared
open Metadata

let getTextAndTokenCount (logger: ILogger) (corpusCode: string) (selection: Metadata.Selection) =

    task {
        let! connStr = getConnectionString corpusCode

        use conn = new SQLiteConnection(connStr)

        let metadataSelectionSql =
            generateMetadataSelectionSql None selection

        let sql =
            $"SELECT count(*) as NumTexts, sum(endpos - startpos + 1) as NumTokens FROM texts WHERE 1 = 1{metadataSelectionSql}"

        let parameters = metadataSelectionToParamDict selection

        let! res = querySingle logger conn sql (Some parameters)

        match res with
        | Ok maybeCounts ->
            return
                match maybeCounts with
                | Some (counts: TextAndTokenCounts) -> counts
                | None -> { NumTexts = 0L; NumTokens = 0L }
        | Error ex -> return raise ex
    }

let getCorpusList () =
    async { return Corpora.Server.getCorpusList () }

let getCorpusConfig (logger: ILogger) (corpusCode: string) =
    async {
        let corpus = Corpora.Server.getCorpus corpusCode

        let! numTextsAndTokens =
            getTextAndTokenCount logger corpusCode Map.empty
            |> Async.AwaitTask

        return
            { corpus.Config with
                  TotalTexts = numTextsAndTokens.NumTexts
                  TotalTokens = numTextsAndTokens.NumTokens }
    }
