module Remoting.Metadata

open System.Collections.Generic
open Microsoft.Data.Sqlite
open System.Threading.Tasks
open FSharp.Control.Tasks
open Serilog
open ServerTypes
open Database
open Shared

type MinAndMax = { Min: int64; Max: int64 }

let getQualifiedColumnName categoryCode =
    categoryCode
    |> sanitizeString
    |> fun s ->
        if s.Contains('.') then
            s
        else
            "texts." + s

let createJoin table =
    $"INNER JOIN {table}_texts ON {table}_texts.tid = texts.tid INNER JOIN {table} ON {table}.id = {table}_texts.{table}_id"

let metadataSelectionToParamDict (selection: Metadata.Selection) =
    selection
    |> Map.toList
    |> List.map (fun (key, sel) ->
        let newKey =
            if key.Contains('.') then
                // Remove table name, since the fully qualified name is invalid as a parameter name in SQL
                key.Split('.').[1]
            else
                key

        let newValue =
            sel.Choices
            |> Array.map (fun choice -> choice.Value)

        newKey, newValue)
    |> Map.ofList
    |> mapToParamDict

// Finds categories that are NOT excluded many-to-many categories, i.e., categories that are either
// not many-to-many (which means they are found in the texts table) or not excluded
let getNonExcludedManyToManyCategories (selection: Metadata.Selection) =
    selection
    |> Map.filter (fun key value -> key.Contains("texts.") || not value.ShouldExclude)

let generateMetadataSelectionJoins (maybeRequestedCategoryCode: string option) (selection: Metadata.Selection) =
    let categoryTables =
        [ for code, _ in selection |> Map.toList do
              // Don't include the category we are fetching values for, since that will already be joined in
              // (if it is a many-to-many category)
              let shouldInclude =
                  if
                      code.Contains('.')
                      && not (code.Contains("texts."))
                  then
                      match maybeRequestedCategoryCode with
                      | Some requestedCategoryCode when not (requestedCategoryCode.Contains('.')) -> true
                      | Some requestedCategoryCode when
                          code.Split('.').[0]
                          <> requestedCategoryCode.Split('.').[0]
                          ->
                          true
                      | None -> true
                      | _ -> false
                  else
                      false

              if shouldInclude then
                  code.Split('.').[0] ]

    [ for t in categoryTables |> List.distinct -> createJoin t ]
    |> String.concat " "
    |> fun s -> if s.Length > 0 then " " + s else ""

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
              let column =
                  getQualifiedColumnName category.Key

              let choices =
                  // Remove empty values
                  category.Value.Choices
                  |> Array.filter (fun choice -> not (System.String.IsNullOrWhiteSpace(choice.Value)))

              if choices.Length > 0 then
                  // If we find 'app_interval_from' and/or 'app_interval_to' we have an interval; otherwise
                  // we options from a select list
                  let maybeFrom =
                      choices
                      |> Array.tryFind (fun choice -> choice.Name = "app_interval_from")

                  let maybeTo =
                      choices
                      |> Array.tryFind (fun choice -> choice.Name = "app_interval_to")

                  match (maybeFrom, maybeTo) with
                  | Some fromValue, Some toValue ->
                      $" AND {column} BETWEEN {int fromValue.Value} AND {int toValue.Value}"
                  | Some fromValue, None -> $" AND {column} >= {int fromValue.Value}"
                  | None, Some toValue -> $" AND {column} <= {int toValue.Value}"
                  | None, None ->
                      let paramName =
                          // if the key contains a table name in front of the column name, remove the table name,
                          // since a fully qualified name is not a valid parameter name
                          if column.Contains('.') then
                              column.Split('.').[1]
                          else
                              column

                      // If we want to exclude values from a many-to-many category, whose values are located
                      // in its own table as indicated by the presence of a dot in the category code, we cannot
                      // use a 'NOT IN'-restriction. Instead, we need to generate an 'IN'-restriction that
                      // will be part of a subquery that searches for a positive matches, the results of
                      // which will be excluded from the final results, so treat those the same as simple
                      // positive matches from other categories.
                      if
                          category.Value.ShouldExclude
                          && category.Key.Contains("texts.")
                      then
                          // Somewhat counterintuitively, the results returned by 'NOT IN' does not include NULL values
                          // (even though they are clearly not included in the set), so we need to check for that as well.
                          $" AND ({column} IS NULL OR {column} NOT IN @{paramName})"
                      else
                          $" AND {column} IN @{paramName}" ]
    |> String.concat ""

let generateManyToManyExclusions (selection: Metadata.Selection) =
    [ for category in selection do
          if category.Key.Contains('.')
             && not (category.Key.Contains("texts."))
             && category.Value.ShouldExclude then
              let parts = category.Key.Split('.')
              let table = parts.[0]
              let column = parts.[1]
              let join = createJoin table

              let subquery =
                  $"SELECT distinct(texts.tid) FROM texts {join} WHERE {table}.{column} IN @{column}"

              $" AND texts.tid NOT IN ({subquery})" ]
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

        let catJoin =
            if catCode.Contains('.') then
                let catTable = catCode.Split('.').[0]

                if catTable <> "texts" then
                    " " + createJoin catTable
                else
                    ""
            else
                ""

        let excludedManyToManyCategoriesSql =
            generateManyToManyExclusions selection

        let nonExcludedManyToManyCategories =
            getNonExcludedManyToManyCategories selection

        let metadataSelectionSql =
            generateMetadataSelectionSql (Some catCode) nonExcludedManyToManyCategories

        let joins =
            generateMetadataSelectionJoins (Some catCode) nonExcludedManyToManyCategories

        let column = getQualifiedColumnName catCode

        let sql =
            $"SELECT distinct({column}) FROM texts{catJoin}{joins} WHERE {column} <> '' AND {column} IS NOT NULL\
             {metadataSelectionSql}{excludedManyToManyCategoriesSql} ORDER BY {column}"

        let parameters =
            metadataSelectionToParamDict selection

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

        let catJoin =
            if catCode.Contains('.') then
                let catTable = catCode.Split('.').[0]

                if catTable <> "texts" then
                    " " + createJoin catTable
                else
                    ""
            else
                ""

        let excludedManyToManyCategoriesSql =
            generateManyToManyExclusions selection

        let nonExcludedManyToManyCategories =
            getNonExcludedManyToManyCategories selection

        let metadataSelectionSql =
            generateMetadataSelectionSql (Some catCode) nonExcludedManyToManyCategories

        let joins =
            generateMetadataSelectionJoins (Some catCode) nonExcludedManyToManyCategories

        let column = getQualifiedColumnName catCode

        let sql =
            $"SELECT min({column}) as Min, max({column}) as Max FROM texts{catJoin}{joins} \
              WHERE 1 = 1{metadataSelectionSql}{excludedManyToManyCategoriesSql}"

        let parameters =
            metadataSelectionToParamDict selection

        let! res = querySingle logger conn sql (Some parameters)

        return
            match res with
            | Ok (maybeValues: MinAndMax option) ->
                match maybeValues with
                | Some values -> (values.Min, values.Max)
                | None -> failwith $"No min and max values found for category {catCode}"
            | Error ex -> raise ex
    }

type ManyToManyResult = { Tid: string; CatValues: string }

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

        let limitOffsetSql =
            if pageNumber = 0 then
                // pageNumber = 0 means don't limit the number of results
                ""
            else
                let limit = 50
                let offset = (pageNumber - 1) * limit
                $" LIMIT {limit} OFFSET {offset}"

        let sanitizedColumns =
            columns |> List.map sanitizeString

        let manyToManyTables =
            [ for column in sanitizedColumns do
                  if column.Contains('.') then
                      let table = column.Split('.').[0]
                      if table <> "texts" then table ]
            |> List.distinct

        let mutable manyToManyMap: Map<string, Map<string, string>> =
            Map.empty

        for table in manyToManyTables do
            let selectColumns =
                sanitizedColumns
                |> List.filter (fun column -> column.Contains(table + "."))

            for column in selectColumns do
                let joinTable = $"{table}_texts"

                let sql =
                    $"SELECT {joinTable}.tid as Tid, GROUP_CONCAT({column}, '; ') as CatValues FROM {joinTable} \
                       INNER JOIN {table} ON {table}.id = {joinTable}.{table}_id WHERE {column} NOT IN ('', '\N') \
                       GROUP BY {joinTable}.tid"

                let res =
                    (query logger conn sql None).Result

                let results =
                    match res with
                    | Ok (resultSeq: ManyToManyResult seq) ->
                        resultSeq
                        |> Seq.map (fun r -> (r.Tid, r.CatValues))
                        |> Map.ofSeq
                    | Error ex -> raise ex

                manyToManyMap <- manyToManyMap.Add(column, results)

        let columnSql =
            sanitizedColumns
            // Don't include many-to-many categories in the SQL, since we won't be joining with those tables
            // now (we will get those values from the queries we performed in the previous step instead)
            |> List.filter (fun column ->
                (not (column.Contains('.')))
                || column.Contains("texts."))
            |> List.map (fun column ->
                if column = "texts.tid" then
                    "DISTINCT(texts.tid)"
                else
                    column)
            |> String.concat ", "

        let excludedManyToManyCategoriesSql =
            generateManyToManyExclusions selection

        let nonExcludedManyToManyCategories =
            getNonExcludedManyToManyCategories selection

        let metadataSelectionSql =
            generateMetadataSelectionSql None nonExcludedManyToManyCategories

        let joins =
            generateMetadataSelectionJoins None nonExcludedManyToManyCategories
            |> fun s ->
                match maybeSortInfo with
                // If we sort on a column in the texts table, we don't need any additional joins
                | Some sortInfo when sortInfo.CategoryCode.Contains("texts.") -> s
                // If we sort on a many-to-many category, we need to make sure it is joined in
                | Some sortInfo ->
                    let table =
                        sortInfo.CategoryCode.Split('.').[0]

                    let metadataSelectionTables =
                        [ for key in selection.Keys -> key.Split('.').[0] ]
                        |> List.distinct

                    if metadataSelectionTables |> List.contains table then
                        // The table that contains the column we are sorting on is already included in the
                        // metadata selection, so no need to join it once more
                        s
                    else
                        // The table that contains the column we are sorting on is not included in the
                        // metadata selection, so we need to join it in
                        let joinTable = $"{table}_texts"

                        $"{s} INNER JOIN {joinTable} ON {joinTable}.tid = texts.tid \
                          INNER JOIN {table} ON {table}.id = {joinTable}.{table}_id"
                | None -> s

        let orderBy =
            match maybeSortInfo with
            | Some sortInfo -> $"{sortInfo.CategoryCode} {sortInfo.Direction}"
            | None -> "texts.tid"
            |> fun s -> if s = "tid" then "texts.tid" else s

        let sql =
            $"SELECT {columnSql} FROM texts{joins} WHERE 1 = 1{metadataSelectionSql}{excludedManyToManyCategoriesSql} \
             ORDER BY {orderBy}{limitOffsetSql}"

        let parameters =
            metadataSelectionToParamDict selection

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
                    [| for row: IDictionary<string, obj> in rows |> Seq.cast ->
                           [| for column in sanitizedColumns ->
                                  let text =
                                      // If the main SQL query selected this column, use that. Otherwise, see
                                      // if our map of many-to-many values contains a value for this column and
                                      // tid. If not, we return an empty string (meaning that this text does not
                                      // have a value for this many-to-many category).
                                      let dbRowKey = column.Split('.').[1]

                                      if row.ContainsKey(dbRowKey) then
                                          row.[dbRowKey] |> string
                                      else
                                          let tid = row.["tid"] |> string

                                          manyToManyMap.[column].TryFind(tid)
                                          |> Option.defaultValue ("")

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

        let sql =
            $"SELECT * FROM texts WHERE tid = @tid LIMIT 1"

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
                    let row: IDictionary<string, obj> =
                        rows |> Seq.cast |> Seq.head

                    [ for category in categories ->
                          let text = row.[category.Code] |> string

                          let value =
                              if text <> "\N" then text else ""

                          { Name = category.Name; Value = value } ]
                | None -> []
            | Error ex -> raise ex
    }
