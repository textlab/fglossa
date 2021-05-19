module Remoting.Search.Cwb.Core

open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Data.SQLite
open Serilog
open ServerTypes
open Shared
open Database

// If the number of running CQP processes exceeds this number, we do not allow a new
// search in a corpus that does parallel search using all cpus to be started.
let maxCqpProcesses = 8

let searchCorpus (connStr: string) (logger: ILogger) (searchParams: SearchParams) (corpus: Corpus) =
    task {
        let cqpProcs =
            Process.runCmdWithOutput "pgrep" "-f cqp"

        let nCqpProcs = cqpProcs.Split('\n').Length

        // If we are searching in a corpus that does parallel search with multiple cpus and
        // this is the first search step, we check that we don't already exceed the max number
        // of CQP processes before starting the search. If we are at step 2 or 3, we should finish
        // what we started. Corpora that don't use multiple cpus are assumed to be small and
        // should not cause any problems even with a lot of CQP processes.
        if corpus.Config.MultiCpuBounds.IsNone
           || nCqpProcs < maxCqpProcesses
           || searchParams.Step > 1 then

            let searchData =
                [ "CorpusCode" => searchParams.CorpusCode
                  "Queries" => searchParams.Queries
                  "Metadata" => searchParams.Metadata ]

            use connection = new SQLiteConnection(connStr)

            let searchId =
                searchParams.SearchId
                |> Option.defaultWith
                    (fun () ->
                        let res =
                            (insert logger connection "Search" searchData)
                                .Result

                        match res with
                        | Ok id -> id
                        | Error ex -> raise ex)

            match corpus.Config.Modality with
            | Spoken -> Spoken.runQueries corpus searchId searchParams
            | Written -> Written.runQueries corpus searchId searchParams

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
