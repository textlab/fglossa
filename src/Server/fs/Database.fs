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
                    | v ->
                        // For non-enumerables (as well as strings), simply print the value quoted.
                        $"'{v}'"

                Regex.Replace(state, pattern, replacement))
            sql
    | None -> sql

let execute (connection: #DbConnection) (sql: string) (data: _) =
    task {
        try
            let! res = connection.ExecuteAsync(sql, data)
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
