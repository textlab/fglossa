module Remoting.Search

open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Data.SQLite
open Serilog
open Database
open Shared

let runCmd (cmd: string) (args: string) =
    let proc =
        System.Diagnostics.Process.Start(cmd, args)

    proc.WaitForExit()

let runCmdWithOutput (cmd: string) (args: string) =
    let startInfo =
        System.Diagnostics.ProcessStartInfo(cmd, args)

    startInfo.UseShellExecute <- false
    startInfo.RedirectStandardOutput <- true

    let proc =
        System.Diagnostics.Process.Start(startInfo)

    let output = proc.StandardOutput.ReadToEnd()
    proc.WaitForExit()
    output

// If the number of running CQP processes exceeds this number, we do not allow a new
// search in a corpus that does parallel search using all cpus to be started.
let maxCqpProcesses = 8

open Dapper

let searchCorpus (connStr: string) (logger: ILogger) (searchParams: SearchParams) =
    let createSearch () =
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
            with e ->
                printfn $"{e}"
                return raise e

        }


    task {
        let cqpProcs = runCmdWithOutput "pgrep" "-f cqp"
        let nCqpProcs = cqpProcs.Split('\n').Length

        let corpus =
            Corpora.Server.getCorpus searchParams.CorpusCode

        // If we are searching a corpus that does parallel search with multiple cpus and
        // this is the first search step, we check that we don't already exceed the max number
        // of CQP processes before starting the search. If we are at step 2 or 3, we should finish
        // what we started. Corpora that don't use multiple cpus are assumed to be small and
        // should not cause any problems even with a lot of CQP processes.
        if corpus.Config.MultiCpuBounds.IsNone
           || nCqpProcs < maxCqpProcesses
           || searchParams.Step > 1 then

            let searchId =
                searchParams.SearchId
                |> Option.defaultWith (fun () -> createSearch().Result)

            return
                { Count = 1u
                  CpuCounts = [| 1u |]
                  SearchId = searchId
                  Results =
                      [| { HasAudio = false
                           HasVideo = false
                           Text = "heidu" } |] }
        else
            return failwith $"TOO MANY CQP PROCESSES: {nCqpProcs}; aborting search at {System.DateTime.Now}"
    }
