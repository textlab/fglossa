module Remoting.Corpus

open System.Data.SQLite
open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Text.RegularExpressions
open Serilog
open ServerTypes
open Database
open Shared

let private metadataSelectionToParamDict (selection: Metadata.Selection) =
    selection
    |> Map.map
        (fun key sel ->
            sel.Choices
            |> Array.map (fun choice -> choice.Value))
    |> mapToParamDict

let getCorpusConfig (corpusCode: string) =
    async {
        let corpus = Corpora.Server.getCorpus corpusCode
        return corpus.Config
    }

let getCorpusList () =
    async { return Corpora.Server.getCorpusList () }

let private getConnectionString corpusCode =
    async {
        let code = sanitizeString corpusCode

        return $"DataSource=../Corpora/corpora/{code}/{code}.sqlite"
    }

let getMetadataForCategory
    (logger: ILogger)
    (corpusCode: string)
    (categoryCode: Metadata.CategoryCode)
    (selection: Metadata.Selection)
    =
    task {
        let! connStr = getConnectionString corpusCode

        use conn = new SQLiteConnection(connStr)

        let catCode = sanitizeString categoryCode

        let sql = $"SELECT distinct({catCode}) FROM texts"

        let parameters = metadataSelectionToParamDict selection

        let! res = query logger conn sql (Some parameters)

        match res with
        | Ok values -> return values
        | Error ex -> return raise ex
    }
