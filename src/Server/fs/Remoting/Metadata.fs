module Remoting.Metadata

open System.Data.SQLite
open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Text.RegularExpressions
open Serilog
open ServerTypes
open Database
open Shared

let private getConnectionString corpusCode =
    async {
        let code = sanitizeString corpusCode

        return $"DataSource=../Corpora/corpora/{code}/{code}.sqlite"
    }

let private metadataSelectionToParamDict (selection: Metadata.Selection) =
    selection
    |> Map.map
        (fun key sel ->
            sel.Choices
            |> Array.map (fun choice -> choice.Value))
    |> mapToParamDict

let generateMetadataSelectionSql (categoryCode: string) (selection: Metadata.Selection) =
    [ for category in selection do
          // Don't include the category we are fetching values for, since that would only
          // return the same values...
          if category.Key <> categoryCode then
              let column = sanitizeString category.Key

              let operator =
                  if category.Value.ShouldExclude then
                      "NOT IN"
                  else
                      "IN"

              $" AND {column} {operator} @{column}" ]
    |> String.concat ""

let getMetadataForCategory
    (logger: ILogger)
    (corpusCode: string)
    (categoryCode: string)
    (selection: Metadata.Selection)
    =
    task {
        let! connStr = getConnectionString corpusCode

        use conn = new SQLiteConnection(connStr)

        let catCode = sanitizeString categoryCode

        let metadataSelectionSql =
            generateMetadataSelectionSql catCode selection

        let sql =
            $"SELECT distinct({catCode}) FROM texts WHERE {catCode} <> '' AND {catCode} IS NOT NULL{metadataSelectionSql} ORDER BY {catCode}"

        let parameters = metadataSelectionToParamDict selection

        let! res = query logger conn sql (Some parameters)

        match res with
        | Ok values -> return values |> Seq.toArray
        | Error ex -> return raise ex
    }
