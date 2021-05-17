module Database

open Dapper
open System
open System.Data.Common
open System.Collections.Generic
open System.Text.RegularExpressions
open FSharp.Control.Tasks
open Serilog

let inline (=>) k v = k, box v

// https://stackoverflow.com/questions/42797288/dapper-column-to-f-option-property
type OptionHandler<'T>() =
    inherit SqlMapper.TypeHandler<option<'T>>()

    override __.SetValue(param, value) =
        let valueOrNull =
            match value with
            | Some x -> box x
            | None -> null

        param.Value <- valueOrNull

    override __.Parse value =
        if isNull value || value = box DBNull.Value then
            None
        else
            Some(value :?> 'T)

SqlMapper.AddTypeHandler(OptionHandler<string>())
SqlMapper.AddTypeHandler(OptionHandler<int>())

let toDisplayedSql (sql: string) (maybeParams: IDictionary<string, obj> option) =
    match maybeParams with
    | Some (par: IDictionary<string, obj>) ->
        par.AsList()
        |> Seq.fold
            (fun state pair ->
                let pattern = "@" + pair.Key

                let replacement =
                    match pair.Value with
                    | :? System.Collections.IEnumerable as vs when vs.GetType().Name <> "String" ->
                        // For enumerables (except strings, which are actually IEnumerable), we assume that they
                        // are used in an IN expression, so print them as quoted values in a comma-separated list
                        // surrounded by parentheses.
                        let vals = [ for v in vs -> $"'{v}'" ]

                        let valueListString = String.concat "," vals
                        $"({valueListString})"
                    | v when isNull v -> "NULL"
                    | v ->
                        // For non-enumerables (as well as strings), simply print the value quoted.
                        $"'{v}'"

                Regex.Replace(state, pattern, replacement))
            sql
    | None -> sql

let execute (logger: ILogger) (connection: #DbConnection) (sql: string) (parameters: IDictionary<string, obj> option) =
    toDisplayedSql sql parameters
    |> logger.Information

    task {
        try
            let! res =
                match parameters with
                | Some p -> connection.ExecuteAsync(sql, p)
                | None -> connection.ExecuteAsync(sql)

            return Ok res
        with ex -> return Error ex
    }

let executeScalar
    (logger: ILogger)
    (connection: #DbConnection)
    (sql: string)
    (parameters: IDictionary<string, obj> option)
    =
    toDisplayedSql sql parameters
    |> logger.Information

    task {
        try
            let! res =
                match parameters with
                | Some p -> connection.ExecuteScalarAsync<'T>(sql, p)
                | None -> connection.ExecuteScalarAsync<'T>(sql)

            return Ok res
        with ex -> return Error ex
    }

let query (logger: ILogger) (connection: #DbConnection) (sql: string) (parameters: IDictionary<string, obj> option) =
    toDisplayedSql sql parameters
    |> logger.Information

    task {
        try
            let! res =
                match parameters with
                | Some p -> connection.QueryAsync<'T>(sql, p)
                | None -> connection.QueryAsync<'T>(sql)

            return Ok res
        with ex -> return Error ex
    }

let querySingle
    (logger: ILogger)
    (connection: #DbConnection)
    (sql: string)
    (parameters: IDictionary<string, obj> option)
    =
    toDisplayedSql sql parameters
    |> logger.Information

    task {
        try
            let! res =
                match parameters with
                | Some p -> connection.QuerySingleOrDefaultAsync<'T>(sql, p)
                | None -> connection.QuerySingleOrDefaultAsync<'T>(sql)

            return
                if isNull (box res) then
                    Ok None
                else
                    Ok(Some res)
        with ex -> return Error ex
    }

let queryDynamic
    (logger: ILogger)
    (connection: #DbConnection)
    (sql: string)
    (parameters: IDictionary<string, obj> option)
    =
    toDisplayedSql sql parameters
    |> logger.Information

    task {
        try
            let! res =
                match parameters with
                | Some p -> connection.QueryAsync(sql, p)
                | None -> connection.QueryAsync(sql)

            return
                if isNull (box res) then
                    Ok None
                else
                    Ok(Some res)
        with ex -> return Error ex
    }

let insert (logger: ILogger) (connection: #DbConnection) (table: string) (data: (string * obj) seq) =
    task {
        let columns =
            data |> Seq.map fst |> String.concat ", "

        let placeholders =
            data
            |> Seq.map fst
            |> Seq.map (fun s -> $"@{s}")
            |> String.concat ", "

        let parameters = dict data

        let sql =
            $"INSERT INTO {table} ({columns}) VALUES ({placeholders})"

        printfn $"{sql}"

        try
            if connection.State = Data.ConnectionState.Closed then
                connection.Open()

            // In order to make last_insert_rowid() work, we need to run the insert and the
            // call to last_insert_rowid() inside a single transaction (according to the documentaion, simply
            // using the same db connection should be sufficient, but it doesn't work here for some reason).
            use transaction = connection.BeginTransaction()

            connection.Execute(sql, parameters, transaction)
            |> ignore

            let lastInsertId =
                unbox<int64> (connection.ExecuteScalar("SELECT last_insert_rowid()", transaction))
                |> int

            transaction.Commit()
            connection.Close()

            return Ok lastInsertId
        with ex -> return Error ex
    }
