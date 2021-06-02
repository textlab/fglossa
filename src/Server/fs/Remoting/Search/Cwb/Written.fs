module Remoting.Search.Cwb.Written

open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Text.RegularExpressions
open Serilog
open ServerTypes
open Shared
open Remoting.Search.Cwb.Common

let private getParts (corpus: Corpus) (step: int) (corpusSize: uint64) (maybeCommand: string option) =
    let stepIndex = step - 1

    match corpus.Config.MultiCpuBounds with
    | Some allBounds when maybeCommand.IsNone ->
        // Multicpu bounds have been defined for this corpus. The startpos for the
        // first cpu in the current step should be one above the last bounds value
        // (i.e., the last endpos) in the previous step.
        let stepBounds = allBounds.[stepIndex]

        let prevLastBounds =
            if step = 1 then
                0UL
            else
                (allBounds.[stepIndex - 1] |> Array.last)

        stepBounds
        |> Array.mapi
            (fun cpuIndex endpos ->
                let startpos =
                    if cpuIndex = 0 then
                        // If first cpu, continue where we left off in the previous step
                        if prevLastBounds = 0UL then
                            0UL
                        else
                            prevLastBounds + 1UL
                    else
                        stepBounds.[cpuIndex - 1] + 1UL

                (startpos, endpos))
    | _ ->
        // No multicpu bounds defined; in that case, we search the whole
        // corpus in one go in the first step and just return if step != 1.
        if step = 1 || maybeCommand.IsSome then
            [| (0UL, (corpusSize - 1UL)) |]
        else
            Array.empty

let private randomReduceCommand
    (corpusSize: uint64)
    (startpos: uint64)
    (endpos: uint64)
    (numRandomHits: int)
    (maybeRandomHitsSeed: int option)
    (namedQuery: string)
    =
    // Find the proportion of the total number of tokens that we are searching with this cpu in this search
    // step, and reduce the number of hits retrieved to the corresponding proportion of the number of random
    // hits we have asked for.
    let proportion =
        (float (endpos - startpos + 1UL) / float corpusSize)

    let nRandom =
        (float numRandomHits * proportion)
        |> System.Math.Ceiling
        |> int

    let seedStr =
        match maybeRandomHitsSeed with
        | Some seed -> $"randomize {seed}"
        | None -> ""

    [ seedStr
      $"reduce {namedQuery} to {nRandom}" ]


let private cqpInit
    (corpus: Corpus)
    (searchParams: SearchParams)
    (maybeAttributes: string [] option)
    (namedQuery: string)
    (constructSaveCommands: string list)
    =
    let defaultSize = 15
    let maxSize = 50
    let cs = searchParams.ContextSize

    let adjustedContextSize =
        if cs >= 0 && cs <= maxSize then cs
        elif cs > maxSize then maxSize
        else defaultSize

    let sortCmd =
        sortCommand namedQuery searchParams.SortKey

    [ "set DataDirectory \"tmp\""
      cwbCorpusName corpus searchParams.Queries
      yield! constructSaveCommands
      $"str set Context {adjustedContextSize} word"
      "set PrintStructures \"s_id\""
      "set LD \"{{\""
      "set RD \"}}\""
      $"{displayedAttrsCommand corpus searchParams.Queries maybeAttributes} +text"
      alignedLanguagesCommand corpus searchParams.Queries
      if sortCmd.IsSome then
          yield! sortCmd.Value ]

let runQueries (logger: ILogger) (corpus: Corpus) (searchParams: SearchParams) (maybeCommand: string option) =
    async {
        let numToReturn = searchParams.PageSize * 2 // number of results to return initially

        let cwbCorpus =
            (cwbCorpusName corpus searchParams.Queries)
                .ToLower()

        match corpus.Config.Sizes.TryFind(cwbCorpus) with
        | Some corpusSize ->
            let! results =
                getParts corpus searchParams.Step corpusSize maybeCommand
                |> Array.mapi
                    (fun cpu (startpos, endpos) ->
                        let queryName =
                            cwbQueryName corpus searchParams.SearchId

                        let namedQuery = $"{queryName}_{searchParams.Step}_{cpu}"

                        let cqpInitCommands =
                            [ yield! constructQueryCommands corpus searchParams namedQuery None (Some cpu)
                              match searchParams.NumRandomHits with
                              | Some numRandomHits ->
                                  yield!
                                      randomReduceCommand
                                          corpusSize
                                          startpos
                                          endpos
                                          numRandomHits
                                          searchParams.RandomHitsSeed
                                          namedQuery
                              | None -> ignore None
                              $"save {namedQuery}" ]

                        let lastCommand =
                            match maybeCommand with
                            | Some command -> Regex.Replace(command, "QUERY", namedQuery)
                            | None ->
                                match searchParams.LastCount with
                                // No LastCount means this is the first request of this
                                // search, in which case we return the first two pages of
                                // search results (or as many as we found in this first
                                // part of the corpus). If we got a LastCount value,
                                // it means this is not the first request of this search.
                                // In that case, we check to see if the previous request(s)
                                // managed to fill those two pages, and if not we return
                                // results in order to keep filling them.
                                | None -> $"cat {namedQuery} 0 {numToReturn - 1}"
                                | Some _ when searchParams.LastCount.Value < numToReturn ->
                                    $"cat {namedQuery} 0 {numToReturn - 1}"
                                | _ -> ""

                        [ yield! cqpInit corpus searchParams None namedQuery cqpInitCommands
                          // Always return the number of results, which may be
                          // either total or cut size depending on whether we
                          // restricted the corpus positions
                          $"size {namedQuery}"
                          if lastCommand <> "" then lastCommand ])
                |> Array.map (runCqpCommands logger corpus true)
                |> Async.Parallel

            let numToTake =
                match searchParams.LastCount with
                | Some lastCount -> numToReturn - lastCount
                | None -> numToReturn

            let hits =
                results
                |> Array.choose fst
                |> Array.concat
                |> Array.truncate numToTake

            // Number of hits found by each cpu in this search step
            let counts = results |> Array.choose snd

            // Sum of the number of hits found by the different cpus in this search step
            let count =
                let sum = Array.sum counts

                match searchParams.LastCount with
                | Some lastCount -> sum + lastCount
                | None -> sum

            return
                match searchParams.NumRandomHits with
                | Some numRandomHits when count > numRandomHits ->
                    // Due to rounding, we have retrieved slightly more hits than the number of
                    // random results we asked for, so remove the superfluous ones
                    let nExtra = count - numRandomHits

                    {| Hits = hits |> Array.truncate count
                       Count = count - nExtra
                       Counts = counts |}
                | _ ->
                    {| Hits = hits
                       Count = count
                       Counts = counts |}

        | None -> return failwith $"No corpus size found for {cwbCorpus} in {corpus.Config.Sizes}!"
    }
