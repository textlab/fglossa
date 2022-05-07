module Remoting.Search.Cwb.Written

open System
open System.IO
open System.Text.RegularExpressions
open Serilog
open ServerTypes
open Shared
open Shared.StringUtils
open Remoting.Search.Cwb.Common

let getParts (corpus: Corpus) (step: int) (corpusSize: int64) (maybeCommand: string option) =
    let stepIndex = step - 1

    match corpus.Config.MultiCpuBounds with
    | Some allBounds when maybeCommand.IsNone ->
        // Multicpu bounds have been defined for this corpus. The startpos for the
        // first cpu in the current step should be one above the last bounds value
        // (i.e., the last endpos) in the previous step.
        let stepBounds = allBounds.[stepIndex]

        let prevLastBounds =
            if step = 1 then
                0L
            else
                (allBounds.[stepIndex - 1] |> Array.last)

        stepBounds
        |> Array.mapi (fun cpuIndex endpos ->
            let startpos =
                if cpuIndex = 0 then
                    // If first cpu, continue where we left off in the previous step
                    if prevLastBounds = 0L then
                        0L
                    else
                        prevLastBounds + 1L
                else
                    stepBounds.[cpuIndex - 1] + 1L

            (startpos, endpos))
    | _ ->
        // No multicpu bounds defined; in that case, we search the whole
        // corpus in one go in the first step and just return if step != 1.
        if step = 1 || maybeCommand.IsSome then
            [| (0L, (corpusSize - 1L)) |]
        else
            Array.empty


let randomReduceCommand
    (corpusSize: int64)
    (startpos: int64)
    (endpos: int64)
    (numRandomHits: int64)
    (maybeRandomHitsSeed: int option)
    (namedQuery: string)
    =
    // Find the proportion of the total number of tokens that we are searching with this cpu in this search
    // step, and reduce the number of hits retrieved to the corresponding proportion of the number of random
    // hits we have asked for.
    let proportion =
        (float (endpos - startpos + 1L) / float corpusSize)

    let nRandom =
        (float numRandomHits * proportion)
        |> Math.Ceiling
        |> int

    let seedStr =
        match maybeRandomHitsSeed with
        | Some seed -> $"randomize {seed}"
        | None -> ""

    [ seedStr
      $"reduce {namedQuery} to {nRandom}" ]


let cqpInit
    (corpus: Corpus)
    (searchParams: SearchParams)
    (maybeSortKey: SortKey option)
    (maybeAttributes: Cwb.PositionalAttribute list option)
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
        match maybeSortKey with
        | Some sortKey -> sortCommand namedQuery sortKey
        | None -> None

    [ "set DataDirectory \"/tmp/glossa\""
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
        let numToReturn =
            searchParams.End - searchParams.Start + 1L // number of results to return initially

        let cwbCorpus =
            (cwbCorpusName corpus searchParams.Queries)
                .ToLower()

        let corpusSizes = corpus.CorpusSizes()

        match corpusSizes.TryFind(cwbCorpus) with
        | Some corpusSize ->
            let! results =
                getParts corpus searchParams.Step corpusSize maybeCommand
                |> Array.mapi (fun cpu (startpos, endpos) ->
                    let queryName =
                        cwbQueryName corpus searchParams.SearchId

                    let namedQuery =
                        $"{queryName}_{searchParams.Step}_{cpu}"

                    let cqpInitCommands =
                        [ yield!
                              constructQueryCommands
                                  logger
                                  corpus
                                  searchParams
                                  namedQuery
                                  startpos
                                  endpos
                                  None
                                  (Some cpu)
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
                            | None -> $"cat {namedQuery} {searchParams.Start} {searchParams.End}"
                            | Some lastCount when lastCount < numToReturn ->
                                $"cat {namedQuery} {searchParams.Start} {searchParams.End}"
                            | _ -> ""

                    [ yield! cqpInit corpus searchParams (Some searchParams.SortKey) None namedQuery cqpInitCommands
                      // Always return the number of results, which may be
                      // either total or cut size depending on whether we
                      // restricted the corpus positions
                      $"size {namedQuery}"
                      if lastCommand <> "" then lastCommand ])
                |> Array.map (runCqpCommands logger corpus true)
                |> Async.Parallel

            let numToTake =
                match searchParams.LastCount with
                | Some lastCount when lastCount < numToReturn ->
                    // If we have already fetched a number of hits, but fewer than numToReturn, return any hits we may
                    // have found up to the numToReturn limit
                    numToReturn - lastCount
                | Some _ ->
                    // If the number of hits we have fetched so far (lastCount) is already bigger than or equal to numToReturn,
                    // we don't need to return any more hits
                    0L
                | None ->
                    // If there is no last count, it means that this is the first request of this search, so return
                    // numToReturn hits
                    numToReturn
                // We know that numToTake will be a small number, so converting to int32 should be OK
                |> int

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
                       Hits =
                        hits
                        |> Array.truncate (int (numToReturn - nExtra)) |}
                | _ ->
                    {| Count = count
                       CpuCounts = cpuCounts
                       Hits = hits |}

        | None -> return failwith $"No corpus size found for {cwbCorpus} in {corpusSizes}!"
    }


let transformResults (queries: Query []) (hits: string []) =
    let queriedLangs =
        queries
        |> Array.map (fun q -> q.LanguageCode)
        |> Set.ofArray

    let numLangs = queriedLangs.Count

    [| for lines in hits |> Array.chunkBySize numLangs ->
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
                 |> replace " ([^/<>\s]+) ([^/<>\s]+)(/\S+/)" " $1_$2$3" ] |]


let getFileStartEnd (searchParams: SearchParams) =
    match searchParams.CpuCounts with
    | Some cpuCounts ->
        let firstFileIndex, firstFileStart =
            let rec sumCountsFirst sum fileIndex =
                let newSum = sum + cpuCounts.[fileIndex]

                if newSum > searchParams.Start then
                    // We found the first result file to fetch results from. Return the index of that file as well as the first
                    // result index to fetch from this file; e.g. if we want to fetch results starting at index 100 and the sum of
                    // counts up to this file is 93, we should start fetching from index 7 in this file.
                    (fileIndex, searchParams.Start - sum)
                else
                    sumCountsFirst newSum (fileIndex + 1)

            sumCountsFirst 0L 0

        let lastFileIndex =
            let rec sumCountsLast sum fileIndex =
                let newSum = sum + cpuCounts.[fileIndex]
                // If either the end index can be found in the current file (meaning
                // that if we add the current count to the sum, we exceed the end
                // index) or there are no more files, the current file should be the
                // last one we fetch results from.
                if newSum > searchParams.End
                   || fileIndex = (cpuCounts.Length - 1) then
                    fileIndex
                else
                    // Otherwise, continue with the next file
                    sumCountsLast newSum (fileIndex + 1)

            sumCountsLast 0L 0

        (firstFileIndex, firstFileStart, lastFileIndex)
    | None -> failwith "No cpu counts found!"

// Generates the names of the files containing saved CQP queries
let getNonzeroFiles
    (corpus: Corpus)
    (searchParams: SearchParams)
    (namedQuery: string)
    (firstFile: int)
    (maybeLastFile: int option)
    =
    match (corpus.Config.MultiCpuBounds, searchParams.CpuCounts) with
    | Some multiCpuBounds, Some cpuCounts ->
        let files =
            multiCpuBounds
            |> Array.indexed
            |> Array.collect (fun (index, cpuBounds) ->
                [| for cpuIndex in 0 .. cpuBounds.Length - 1 -> $"{namedQuery}_{index + 1}_{cpuIndex}" |])

        let lastFile =
            maybeLastFile
            |> Option.defaultValue (files.Length - 1)

        let filesAndCounts =
            Array.zip files cpuCounts

        // Select the range of files that contains the range of results we are asking for
        // and remove files that don't actually contain any results
        [ for file, count in filesAndCounts.[firstFile..lastFile] do
              if count > 0L then file ]
    | _ -> []


let getFilesIndexes corpus searchParams namedQuery (maybeNumResultsMinusOne: int64 option) =
    let firstFileIndex, firstFileStart, lastFileIndex =
        getFileStartEnd searchParams

    let nonZeroFiles =
        getNonzeroFiles corpus searchParams namedQuery firstFileIndex (Some lastFileIndex)
        |> List.toSeq

    // For the first result file, we need to adjust the start and end index according to
    // the number of hits that were found in previous files. For the remaining files, we
    // set the start index to 0, and we might as well set the end index to [number of
    // desired results minus one], since CQP won't actually mind if we ask for results
    // beyond those available. If the end position is set to 0, we will return
    // all hits (typically used for exporting results to file etc.)
    let indexes =
        match maybeNumResultsMinusOne with
        | Some numResultsMinusOne ->
            Seq.append
                (Seq.singleton (firstFileStart, firstFileStart + numResultsMinusOne))
                (Seq.initInfinite (fun _ -> (0L, numResultsMinusOne)))
        | None -> Seq.initInfinite (fun _ -> (0L, 0L))

    (nonZeroFiles, indexes)


let runCqpScripts logger corpus scripts =
    async {
        let! cwbResults =
            scripts
            |> Seq.map (fun script -> runCqpCommands logger corpus false script)
            |> Async.Parallel

        return cwbResults |> Array.choose fst |> Array.concat
    }


let getSortedPositions (corpus: Corpus) (searchParams: SearchParams) =
    let namedQuery =
        cwbQueryName corpus searchParams.SearchId

    let resultPositionsFilename =
        $"/tmp/glossa/result_positions_{namedQuery}"

    if not (File.Exists(resultPositionsFilename)) then
        let nonzeroFiles =
            getNonzeroFiles corpus searchParams namedQuery 0 None

        let firstCommands =
            cqpInit corpus searchParams None None namedQuery []

        let moreCommands =
            nonzeroFiles
            |> List.mapi (fun index resultFile ->
                let redirectOperator =
                    if index = 0 then " >" else " >>"

                $"tabulate {resultFile} match[-1] word, match word, match[1] word, match, matchend {redirectOperator} '{resultPositionsFilename}'")

        let commands = firstCommands @ moreCommands

        let commandStr =
            commands
            |> Seq.filter (String.IsNullOrWhiteSpace >> not)
            |> Seq.map (fun s -> s + ";")
            |> String.concat "\n"

        if Environment.GetEnvironmentVariable("CWB_IN_DOCKER") = "1" then
            Process.runCmdWithInputAndOutput "docker" "exec -i cwb cqp -c" commandStr
        else
            Process.runCmdWithInputAndOutput "cqp" "-c" commandStr
        |> ignore

    let sortOpt =
        match searchParams.SortKey with
        | Position -> failwith "No need to call getSortedPositions when 'position' is selected!"
        | Left -> "-k1"
        | Match -> "-k2"
        | Right -> "-k3"

    let sortedResultPositions =
        $"{resultPositionsFilename}_sort_by_{searchParams.SortKey}"

    if not (File.Exists(sortedResultPositions)) then
        Process.runCmdWithOutput "util/multisort.sh" $"{resultPositionsFilename} {sortOpt}"
        |> fun output ->
            let sortedWithAllFields =
                $"{sortedResultPositions}_all_fields"

            File.WriteAllText(sortedWithAllFields, output)
            Process.runCmdWithOutput "cut" $"-f 4,5 {sortedWithAllFields}"
        |> fun output -> File.WriteAllText(sortedResultPositions, output)

    sortedResultPositions


let getSearchResults
    (logger: ILogger)
    (corpus: Corpus)
    (searchParams: SearchParams)
    (maybeAttributes: Cwb.PositionalAttribute list option)
    (pageNumbers: ResultPageNumbers)
    =
    async {
        let queryName =
            cwbQueryName corpus searchParams.SearchId

        let! rawResults =
            if corpus.Config.MultiCpuBounds.IsSome then
                // The corpus uses multiple CPUs
                match searchParams.SortKey with
                | Position ->
                    let namedQuery =
                        cwbQueryName corpus searchParams.SearchId

                    let maybeNumResultsMinusOne =
                        if searchParams.End > 0L then
                            Some(searchParams.End - searchParams.Start)
                        else
                            None

                    let nonzeroFiles, indexes =
                        getFilesIndexes corpus searchParams namedQuery maybeNumResultsMinusOne

                    let scripts =
                        (nonzeroFiles, indexes)
                        ||> Seq.map2 (fun resultFile (start, ``end``) ->
                            [ yield!
                                  cqpInit corpus searchParams (Some searchParams.SortKey) maybeAttributes namedQuery []
                              $"cat {resultFile} {start} {``end``}" ])

                    async {
                        let! allResults = runCqpScripts logger corpus scripts

                        // Since we asked for 'end' number of results even from the last file, we may have got
                        // more than we asked for (when adding up all results from all files), so make sure we
                        // only return the desired number of results if it was specified.
                        let results =
                            match maybeNumResultsMinusOne with
                            | Some numResultsMinusOne ->
                                allResults
                                |> Array.truncate (int numResultsMinusOne + 1)
                            | None -> allResults

                        return results
                    }

                | _ ->
                    let namedQuery =
                        $"{queryName}_sort_by_{searchParams.SortKey}"

                    let undumpSaveCommands =
                        if File.Exists($"tmp/{cwbCorpusName corpus searchParams.Queries}:{namedQuery}") then
                            []
                        else
                            let sortedPositions =
                                getSortedPositions corpus searchParams

                            [ $"undump {namedQuery} < '{sortedPositions}'"
                              namedQuery
                              $"save {namedQuery}" ]

                    let commands =
                        [ yield!
                              cqpInit corpus searchParams (Some searchParams.SortKey) None namedQuery undumpSaveCommands
                          $"cat {namedQuery} {searchParams.Start} {searchParams.End}" ]

                    async {
                        let! output = runCqpCommands logger corpus false commands
                        return fst output |> Option.defaultValue [||]
                    }
            else
                // The corpus uses only a single CPU (e.g. small corpora, multilingual corpora)
                let namedQuery = $"{queryName}_1_0"

                let commands =
                    [ yield! cqpInit corpus searchParams (Some searchParams.SortKey) None namedQuery []
                      $"cat {namedQuery} {searchParams.Start} {searchParams.End}" ]

                async {
                    let! output = runCqpCommands logger corpus false commands
                    return fst output |> Option.defaultValue [||]
                }

        let hits =
            rawResults
            |> transformResults searchParams.Queries

        let hitPages =
            hits |> Array.chunkBySize searchParams.PageSize

        return
            (hitPages |> Array.toSeq, pageNumbers)
            ||> Seq.map2 (fun pageHits pageNumber ->
                { PageNumber = pageNumber
                  Results =
                    [| for hitLines in pageHits ->
                           { AudioType = None
                             HasVideo = false
                             Text = hitLines } |] })
            |> Seq.toArray
    }
