module Remoting.Search.Cwb.Spoken

open Serilog
open ServerTypes
open Shared
open Shared.StringUtils
open Remoting.Search.Cwb.Common

let runQueries (logger: ILogger) (corpus: Corpus) (searchParams: SearchParams) (maybeCommand: string option) =
    async {
        let namedQuery =
            cwbQueryName corpus searchParams.SearchId

        let startpos = 0UL

        let cwbCorpus =
            cwbCorpusName corpus searchParams.Queries

        let corpusSizes = corpus.CorpusSizes()
        let endpos = corpusSizes.[cwbCorpus.ToLower()]

        let displayedAttrsCmd =
            displayedAttrsCommand corpus searchParams.Queries None

        let commands =
            [ "set DataDirectory \"tmp\""
              cwbCorpus

              yield!
                  constructQueryCommands logger corpus searchParams namedQuery startpos endpos (Some "who_start") None

              yield!
                  match searchParams.NumRandomHits with
                  | Some numRandomHits ->
                      let seedStr =
                          match searchParams.RandomHitsSeed with
                          | Some seed -> $"randomize {seed}"
                          | None -> ""

                      [ seedStr
                        $"reduce {namedQuery} to {numRandomHits}" ]
                  | None -> []

              $"save {namedQuery}"
              "set Context 1 who_start"
              "set PrintStructures \"who_name, who_avfile\""
              "set LD \"{{\""
              "set RD \"}}\""

              if displayedAttrsCmd <> "" then
                  displayedAttrsCmd

              // Return the total number of search results...
              $"size {namedQuery}"

              // ...as well as two pages of actual results
              yield!
                  match maybeCommand with
                  | Some command ->
                      [ command |> replace "QUERY" namedQuery
                        $"cat {namedQuery} 0 {2 * searchParams.PageSize - 1}" ]
                  | None -> [] ]

        match! runCqpCommands logger corpus true commands with
        | Some hits, Some count ->
            return
                {| Count = count
                   CpuCounts = [| count |]
                   Hits = hits |}
        | _ ->
            return
                {| Count = 0UL
                   CpuCounts = [||]
                   Hits = [||] |}
    }
