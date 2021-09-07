module Remoting.Metadata

open System.Collections.Generic
open System.Data.SQLite
open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Text.RegularExpressions
open Serilog
open ServerTypes
open Database
open Shared

let metadataSelectionToParamDict (selection: Metadata.Selection) =
    selection
    |> Map.map
        (fun key sel ->
            sel.Choices
            |> Array.map (fun choice -> choice.Value))
    |> mapToParamDict

let generateMetadataSelectionSql (maybeRequestedCategoryCode: string option) (selection: Metadata.Selection) =
    [ for category in selection do
          // Don't include the category we are fetching values for, since that would only
          // return the same values...
          let shouldInclude =
              match maybeRequestedCategoryCode with
              | Some requestedCategoryCode when category.Key <> requestedCategoryCode -> true
              | None -> true
              | _ -> false

          if shouldInclude then
              let column = sanitizeString category.Key

              // TODO: Test on subclasses of Category and make this more robust
              if category.Value.Choices
                 |> Array.exists (fun choice -> choice.Name = "glossa_interval_from") then
                  $" AND {column} BETWEEN {int category.Value.Choices.[0].Value} AND {int category.Value.Choices.[1].Value}"
              elif category.Value.ShouldExclude then
                  $" AND {column} NOT IN @{column}"
              else
                  $" AND {column} IN @{column}" ]
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
            generateMetadataSelectionSql (Some catCode) selection

        let sql =
            $"SELECT distinct({catCode}) FROM texts WHERE {catCode} <> '' AND {catCode} IS NOT NULL{metadataSelectionSql} ORDER BY {catCode}"

        let parameters = metadataSelectionToParamDict selection

        let! res = query logger conn sql (Some parameters)

        match res with
        | Ok values -> return values |> Seq.toArray
        | Error ex -> return raise ex
    }

let getMetadataForTexts
    (logger: ILogger)
    (corpusCode: string)
    (selection: Metadata.Selection)
    (columns: string list)
    (pageNumber: int)
    =

    task {
        let! connStr = getConnectionString corpusCode

        use conn = new SQLiteConnection(connStr)

        let limit = 50
        let offset = (pageNumber - 1) * limit

        let columnSql =
            columns
            |> List.map sanitizeString
            |> String.concat ", "

        let metadataSelectionSql =
            generateMetadataSelectionSql None selection

        let sql =
            $"SELECT {columnSql} FROM texts WHERE 1 = 1{metadataSelectionSql} ORDER BY tid LIMIT {limit} OFFSET {offset}"

        let parameters = metadataSelectionToParamDict selection

        // Since each corpus has a different set of metadata categories, we cannot use the 'query'
        // function, which requires the result rows to conform to a specific type. Instead, we use
        // 'queryDynamic' and cast the resulting DapperRow objects to a dictionary in order to
        // access the results (see below).
        let! res = queryDynamic logger conn sql (Some parameters)

        match res with
        | Ok maybeRows ->
            return
                match maybeRows with
                | Some rows ->
                    // Since the results of queryDynamic are DapperRow objects, which implement
                    // IDictionary<string, obj>, we cast to that in order to access the data dynamically
                    [| for (row: IDictionary<string, obj>) in rows |> Seq.cast ->
                           [| for column in columns ->
                                  let text = row.[column] |> string
                                  if text <> "\N" then text else "" |] |]
                | None -> [||]
        | Error ex -> return raise ex
    }

let getMetadataForSingleText
    (logger: ILogger)
    (corpusCode: string)
    (categories: Metadata.CategoryNameAndCode list)
    (textId: string)
    : Task<Metadata.CategoryNameAndValue list> =

    task {
        let! connStr = getConnectionString corpusCode

        use conn = new SQLiteConnection(connStr)

        let sql =
            $"SELECT * FROM texts WHERE tid = @tid LIMIT 1"

        let parameters = [ "tid" => textId ] |> dict

        // Since each corpus has a different set of metadata categories, we cannot use the 'query'
        // function, which requires the result rows to conform to a specific type. Instead, we use
        // 'queryDynamic' and cast the resulting DapperRow objects to a dictionary in order to
        // access the results (see below).
        let! res = queryDynamic logger conn sql (Some parameters)

        match res with
        | Ok maybeRows ->
            return
                match maybeRows with
                | Some rows ->
                    // Since the results of queryDynamic are DapperRow objects, which implement
                    // IDictionary<string, obj>, we cast to that in order to access the data dynamically
                    let row: IDictionary<string, obj> = rows |> Seq.cast |> Seq.head

                    [ for category in categories ->
                          let text = row.[category.Code] |> string
                          let value = if text <> "\N" then text else ""

                          { Name = category.Name; Value = value } ]
                | None -> []
        | Error ex -> return raise ex
    }
