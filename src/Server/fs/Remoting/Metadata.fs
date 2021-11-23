module Remoting.Metadata

open System.Collections.Generic
open Microsoft.Data.Sqlite
open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Text.RegularExpressions
open Serilog
open ServerTypes
open Database
open Shared

type MinAndMax = { Min: int64; Max: int64 }

let metadataSelectionToParamDict (selection: Metadata.Selection) =
    selection
    |> Map.map (fun key sel ->
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

              let choices =
                  // Remove empty values
                  category.Value.Choices
                  |> Array.filter (fun choice -> not (System.String.IsNullOrWhiteSpace(choice.Value)))

              if choices.Length > 0 then
                  // If we find 'glossa_interval_from' and/or 'glossa_interval_to' we have an interval; otherwise
                  // we options from a select list
                  let maybeFrom =
                      choices
                      |> Array.tryFind (fun choice -> choice.Name = "glossa_interval_from")

                  let maybeTo =
                      choices
                      |> Array.tryFind (fun choice -> choice.Name = "glossa_interval_to")

                  match (maybeFrom, maybeTo) with
                  | Some fromValue, Some toValue ->
                      $" AND {column} BETWEEN {int fromValue.Value} AND {int toValue.Value}"
                  | Some fromValue, None -> $" AND {column} >= {int fromValue.Value}"
                  | None, Some toValue -> $" AND {column} <= {int toValue.Value}"
                  | None, None ->
                      if category.Value.ShouldExclude then
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
        let connStr = getConnectionString corpusCode

        use conn = new SqliteConnection(connStr)

        let catCode = sanitizeString categoryCode

        let metadataSelectionSql = generateMetadataSelectionSql (Some catCode) selection

        let sql =
            $"SELECT distinct({catCode}) FROM texts WHERE {catCode} <> '' AND {catCode} IS NOT NULL{metadataSelectionSql} ORDER BY {catCode}"

        let parameters = metadataSelectionToParamDict selection

        let! res = query logger conn sql (Some parameters)

        return
            match res with
            | Ok values -> values |> Seq.toArray
            | Error ex -> raise ex
    }

let getMinAndMaxForCategory
    (logger: ILogger)
    (corpusCode: string)
    (categoryCode: string)
    (selection: Metadata.Selection)
    =
    task {
        let connStr = getConnectionString corpusCode

        use conn = new SqliteConnection(connStr)

        let catCode = sanitizeString categoryCode

        let metadataSelectionSql = generateMetadataSelectionSql (Some catCode) selection

        let sql =
            $"SELECT min({catCode}) as Min, max({catCode}) as Max FROM texts WHERE 1 = 1{metadataSelectionSql}"

        let parameters = metadataSelectionToParamDict selection

        let! res = querySingle logger conn sql (Some parameters)

        return
            match res with
            | Ok (maybeValues: MinAndMax option) ->
                match maybeValues with
                | Some values -> (values.Min, values.Max)
                | None -> failwith $"No min and max values found for category {catCode}"
            | Error ex -> raise ex
    }

let getMetadataForTexts
    (logger: ILogger)
    (corpusCode: string)
    (selection: Metadata.Selection)
    (columns: string list)
    (pageNumber: int)
    (maybeSortInfo: Metadata.SortInfo option)
    =

    task {
        let connStr = getConnectionString corpusCode

        use conn = new SqliteConnection(connStr)

        let limit = 50
        let offset = (pageNumber - 1) * limit

        let columnSql =
            columns
            |> List.map sanitizeString
            |> String.concat ", "

        let metadataSelectionSql = generateMetadataSelectionSql None selection

        let orderBy =
            match maybeSortInfo with
            | Some sortInfo -> $"{sortInfo.CategoryCode} {sortInfo.Direction}"
            | None -> "tid"

        let sql =
            $"SELECT {columnSql} FROM texts WHERE 1 = 1{metadataSelectionSql} ORDER BY {orderBy} LIMIT {limit} OFFSET {offset}"

        let parameters = metadataSelectionToParamDict selection

        // Since each corpus has a different set of metadata categories, we cannot use the 'query'
        // function, which requires the result rows to conform to a specific type. Instead, we use
        // 'queryDynamic' and cast the resulting DapperRow objects to a dictionary in order to
        // access the results (see below).
        let! res = queryDynamic logger conn sql (Some parameters)

        return
            match res with
            | Ok maybeRows ->
                match maybeRows with
                | Some rows ->
                    // Since the results of queryDynamic are DapperRow objects, which implement
                    // IDictionary<string, obj>, we cast to that in order to access the data dynamically
                    [| for (row: IDictionary<string, obj>) in rows |> Seq.cast ->
                           [| for column in columns ->
                                  let text = row.[column] |> string
                                  if text <> "\N" then text else "" |] |]
                | None -> [||]
            | Error ex -> raise ex
    }

let getMetadataForSingleText
    (logger: ILogger)
    (corpusCode: string)
    (categories: Metadata.CategoryNameAndCode list)
    (textId: string)
    : Task<Metadata.CategoryNameAndValue list> =

    task {
        let connStr = getConnectionString corpusCode

        use conn = new SqliteConnection(connStr)

        let sql = $"SELECT * FROM texts WHERE tid = @tid LIMIT 1"

        let parameters = [ "tid" => textId ] |> dict

        // Since each corpus has a different set of metadata categories, we cannot use the 'query'
        // function, which requires the result rows to conform to a specific type. Instead, we use
        // 'queryDynamic' and cast the resulting DapperRow objects to a dictionary in order to
        // access the results (see below).
        let! res = queryDynamic logger conn sql (Some parameters)

        return
            match res with
            | Ok maybeRows ->
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
            | Error ex -> raise ex
    }
