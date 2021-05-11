module Remoting.Search.Common

open FSharp.Control.Tasks
open System.Data.SQLite
open Dapper
open Shared
open Database

let createSearch (connStr: string) (searchParams: SearchParams) =
    task {
        try
            use conn = new SQLiteConnection(connStr)

            // In order to make last_insert_rowid() work, we need to run the insert and the
            // call to last_insert_rowid() inside a single transaction (according to the documentaion, simply
            // using the same db connection should be sufficient, but it doesn't work here for some reason).
            conn.Open()
            use transaction = conn.BeginTransaction()

            let sql =
                "INSERT INTO Search (CorpusCode, Queries, Metadata) VALUES (@CorpusCode, @Queries, @Metadata)"

            let parameters =
                [ "CorpusCode" => searchParams.CorpusCode
                  "Queries" => searchParams.Queries
                  "Metadata" => searchParams.Metadata ]

            conn.Execute(sql, dict (parameters), transaction)
            |> ignore

            let lastInsertId =
                unbox<int64> (conn.ExecuteScalar("SELECT last_insert_rowid()", transaction))
                |> int

            transaction.Commit()
            conn.Close()

            return lastInsertId
        with e -> return raise e

    }
