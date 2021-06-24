module Remoting.Search.Cwb.Core

open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Data.SQLite
open Serilog
open ServerTypes
open Shared
open Database

let getSearchResults (connStr: string) (logger: ILogger) (searchParams: SearchParams) (corpus: Corpus) =
    async {
        return
            [| { PageNumber = 1
                 Results =
                     [| { HasAudio = false
                          HasVideo = false
                          Text = [] } |] } |]
    }

// If the number of running CQP processes exceeds this number, we do not allow a new
// search in a corpus that does parallel search using all cpus to be started.
let maxCqpProcesses = 8

let searchCorpus (connStr: string) (logger: ILogger) (searchParams: SearchParams) (corpus: Corpus) =
    async {
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
                  "Queries"
                  => (searchParams.Queries
                      |> Array.map (fun q -> q.Query))
                  "Metadata" => searchParams.Metadata ]

            let searchId =
                if searchParams.SearchId = 0 then
                    // a SearchId of 0 means a new, unsaved search, so save it
                    // and set the database ID of the search to be the SearchId
                    use connection = new SQLiteConnection(connStr)

                    let res =
                        (insert logger connection "Search" searchData)
                            .Result

                    match res with
                    | Ok id -> id
                    | Error ex -> raise ex
                else
                    searchParams.SearchId

            let searchParamsWithSearchId =
                { searchParams with
                      SearchId = searchId }

            let! searchResults =
                match corpus.Config.Modality with
                | Spoken -> Spoken.runQueries logger corpus searchParamsWithSearchId None
                | Written -> Written.runQueries logger corpus searchParamsWithSearchId None

            return
                { Count = searchResults.Count
                  CpuCounts = searchResults.CpuCounts
                  SearchId = searchId
                  SearchStep = searchParams.Step
                  ResultPages =
                      match corpus.Config.Modality with
                      | Spoken -> failwith "NOT IMPLEMENTED"
                      | Written ->
                          searchResults.Hits
                          |> Written.transformResults searchParams.Queries
                          |> Array.chunkBySize searchParams.PageSize
                          |> Array.mapi
                              (fun index results ->
                                  { PageNumber = index + 1
                                    Results = results }) }
        else
            return failwith $"TOO MANY CQP PROCESSES: {nCqpProcs}; aborting search at {System.DateTime.Now}"
    }
