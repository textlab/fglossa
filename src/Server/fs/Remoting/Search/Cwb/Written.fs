module Remoting.Search.Cwb.Written

open ServerTypes
open Shared
open Remoting.Search.Cwb.Common

let private getParts (corpus: Corpus) (step: int) (corpusSize: uint64) (command: string option) =
    let stepIndex = step - 1

    match corpus.Config.MultiCpuBounds with
    | Some allBounds when command.IsNone ->
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
        if step = 1 || command.IsSome then
            [| (0UL, (corpusSize - 1UL)) |]
        else
            Array.empty

let private cqpInit (corpus: Corpus) (searchParams: SearchParams) (constructSaveCommands: string) =
    let defaultSize = 15
    let maxSize = 50
    let cs = searchParams.ContextSize
    let adjustedContextSize =
        if cs >= 0 && cs <= maxSize then cs elif cs > maxSize then maxSize else defaultSize

    ["set DataDirectory \"tmp\""
     cwbCorpusName corpus searchParams.Queries
     constructSaveCommands
     $"str set Context {adjustedContextSize} word"
     "set PrintStructures \"s_id\""
     "set LD \"{{\""
     "set RD \"}}\""
    (str (displayed-attrs-command corpus queries attrs) " +text")
    (aligned-languages-command corpus queries)
    (when sort-key
      (sort-command named-query sort-key))]))

let runQueries (corpus: Corpus) (searchParams: SearchParams) (command: string option) =
    let numToReturn = searchParams.PageSize * 2 // number of results to return initially

    let cwbCorpus =
        cwbCorpusName corpus searchParams.Queries

    match corpus.Config.Sizes.TryFind(cwbCorpus) with
    | Some corpusSize ->
        let scripts =
            getParts corpus searchParams.Step corpusSize command
            |> Array.mapi (fun cpu (startpos, endpos) ->
                            let queryName = cwbQueryName corpus searchParams.SearchId
                            let namedQuery = $"{queryName}_{searchParams.Step}_{cpu}")
    | None -> ()
