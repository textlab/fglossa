module Remoting.Search.Cwb.Spoken

open System.IO
open System.Text.RegularExpressions
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
              match maybeCommand with
              | Some command -> command |> replace "QUERY" namedQuery
              | None -> $"cat {namedQuery} 0 {2 * searchParams.PageSize - 1}" ]

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

let transformResults (corpus: Corpus) (queries: Query []) (hits: string []) =
    let numLangs =
        queries
        |> Array.map (fun query -> query.LanguageCode)
        |> Array.distinct
        |> Array.length

    let audioFiles = corpus.AudioFiles()
    let videoFiles = corpus.VideoFiles()

    [| for hitLines in (hits |> Array.chunkBySize numLangs) ->
           let avFile =
               hitLines
               |> Array.head
               |> fun line ->
                   Regex.Match(line, "<who_avfile (.+?)>").Groups.[1]
                       .Value

           let maybeAudioType =
               try
                   if File.Exists($"../Corpora/corpora/{corpus.Config.Code}/audio/_.mp3") then
                       // Indicates that it should be possible to open the audio player, but
                       // with no sound
                       Some Nosound
                   elif audioFiles.Contains(avFile) then
                       Some Sound
                   else
                       None
               with
               | :? DirectoryNotFoundException -> None

           let ls =
               [ for line in hitLines ->
                     let l = line |> replace "<who_avfile .+?>" ""
                     // Get rid of spaces in multiword expressions. Assuming that
                     // attribute values never contain spaces, we can further
                     // assume that if we find several spaces between slashes,
                     // only the first one separates tokens and the remaining
                     // ones are actually inside the token and should be
                     // replaced by underscores.
                     let mutable modifiedLine = ""
                     let mutable modl = l

                     while modl <> modifiedLine do
                         modifiedLine <- modl

                         modl <-
                             modifiedLine
                             |> replace " ([^/<>\s]+) ([^/<>\s]+)(/\S+/)" " $1_$2$3"

                     modifiedLine ]

           { AudioType = maybeAudioType
             HasVideo = videoFiles.Contains(avFile)
             Text = ls } |]
