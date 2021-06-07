module Remoting.Search.Cwb.Written

open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Text.RegularExpressions
open Serilog
open ServerTypes
open Shared
open Shared.StringUtils
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
    (numRandomHits: uint64)
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
      $"set Context {adjustedContextSize} word"
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
                            [ yield!
                                constructQueryCommands corpus searchParams namedQuery startpos endpos None (Some cpu)
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
                                // No LastCount means this is the first request of this search, in which case we return
                                // the first two pages of search results (or as many as we found in this first part of
                                // the corpus).
                                // If we got a LastCount value, it means this is not the first request of
                                // this search.  In that case, we check to see if the previous request(s) managed to fill
                                // those two pages, and if not we return results in order to keep filling them.
                                | None -> $"cat {namedQuery} 0 {numToReturn - 1}"
                                | Some lastCount when lastCount < (uint64 numToReturn) ->
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
                | Some lastCount when lastCount < (uint64 numToReturn) ->
                    // Since numToReturn is an int (i.e., int32) and we now know that lastCount is smaller, converting
                    // lastCount from uint64 to int should not be risky
                    numToReturn - (int lastCount)
                | Some _ ->
                    // If the number of hits we have fetched so far (lastCount) is already bigger than or equal to numToReturn,
                    // we don't need to return any more hits
                    0
                | None ->
                    // If there is no last count, it means that this is the first request of this search, so return
                    // numToReturn hits
                    numToReturn

            let hits =
                results
                |> Array.choose fst
                |> Array.concat
                |> Array.truncate numToTake

            // Number of hits found by each cpu in this search step
            let cpuCounts = results |> Array.choose snd

            // Sum of the number of hits found by the different cpus in this and any previous search steps
            let count =
                let sum = Array.sum cpuCounts

                match searchParams.LastCount with
                | Some lastCount -> sum + lastCount
                | None -> sum

            return
                match searchParams.NumRandomHits with
                | Some numRandomHits when count > numRandomHits ->
                    // Due to rounding, we have retrieved slightly more hits than the number of
                    // random results we asked for, so remove the superfluous ones
                    let nExtra = count - numRandomHits

                    {| Count = count - nExtra
                       CpuCounts = cpuCounts
                       Hits = hits |> Array.truncate (numToReturn - int nExtra) |}
                | _ ->
                    {| Count = count
                       CpuCounts = cpuCounts
                       Hits = hits |}

        | None -> return failwith $"No corpus size found for {cwbCorpus} in {corpus.Config.Sizes}!"
    }


let transformResults (queries: Query []) (hits: string []) =
    let queriedLangs =
        queries
        |> Array.map (fun q -> q.Language)
        |> Set.ofArray

    let numLangs = queriedLangs.Count

    [| for lines in hits |> Array.chunkBySize numLangs ->
           let ls =
               [ for line in lines ->
                     line
                     // When the match is the last token in a text, the </text> tag is
                     // included within the braces due to a CQP bug, so we need to fix it
                     |> replace "</text>\}\}" "}}</text>"
                     // Remove any material from the previous or following text
                     |> replace "^(.*\{\{.+)</text>.*" "$1"
                     |> replace "^(\s*\d+:\s*<.+?>:\s*).*<text>(.*\{\{.+)" "$1$2"
                     // Get rid of spaces in multiword expressions. Assuming that attribute values never contain spaces,
                     // we can further assume that if we find several spaces between slashes, only the first one separates
                     // tokens and the remaining ones are actually inside the token and should be replaced by underscores.
                     // Fractions containing spaces (e.g. "1 / 2") need to be handled separately because the presence of a
                     // slash confuses the normal regexes
                     |> replace " (\d+) / (\d+)" " $1/$2"
                     |> replace " ([^/<>\s]+) ([^/<>\s]+) ([^/<>\s]+)(/\S+/)" " $1_$2_$3$4"
                     |> replace " ([^/<>\s]+) ([^/<>\s]+)(/\S+/)" " $1_$2$3" ]

           { HasAudio = false
             HasVideo = false
             Text = ls } |]
