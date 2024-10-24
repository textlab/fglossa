module Remoting.Search.Cwb.Spoken

open System.IO
open System.Text.RegularExpressions
open Microsoft.Data.Sqlite
open Serilog
open ServerTypes
open Shared
open Shared.StringUtils
open Database
open Remoting.Metadata
open Remoting.Search.Cwb.Common

let runQueries (logger: ILogger) (corpus: Corpus) (searchParams: SearchParams) (maybeCommand: string option) =
    async {
        let namedQuery =
            cwbQueryName corpus searchParams.SearchId

        let startpos = 0L

        let cwbCorpus =
            cwbCorpusName corpus searchParams.Queries

        let corpusSizes = corpus.CorpusSizes()

        let endpos =
            corpusSizes[cwbCorpus.ToLower()]

        let displayedAttrsCmd =
            displayedAttrsCommand corpus searchParams.Queries None

        let commands =
            let printStructuresCmd =
                if corpus.Config.HasMessages then
                  "set PrintStructures \"who_name, who_avfile, who_messageid\""
                else
                  "set PrintStructures \"who_name, who_avfile\""

            [ "set DataDirectory \"/tmp/glossa\""
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
              printStructuresCmd
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
                {| Count = 0L
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
                   Regex.Match(line, "<who_avfile (.+?)>").Groups[1]
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
                     let l =
                         line |> replace "<who_avfile .+?>" ""
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

let sortContextWithinWho namedQuery sortKey =
    let tmpfile =
        $"\"/tmp/glossa/{namedQuery}_sort_by_{sortKey}\""

    let bound, ``match`` =
        match sortKey with
        | Left -> "lbound", "on match[-1]"
        | Right -> "rbound", "on matchend[1]"
        | _ -> failwith "This function should only be called with Left or Right sort keys"

    [ namedQuery
      "set ExternalSort on"
      $"{namedQuery}_{bound} = [{bound}(who)]"
      $"{namedQuery}_n{bound} = [!{bound}(who)] sort by word %%c {``match``}"
      $"tabulate {namedQuery}_n{bound} match, matchend > {tmpfile}"
      $"tabulate {namedQuery}_{bound} match, matchend >> {tmpfile}"
      $"undump {namedQuery} < {tmpfile}" ]

let sortWithinWho namedQuery sortKey =
    if sortKey = Match then
        [ "set ExternalSort on"
          $"sort {namedQuery} by word %%c" ]
    elif sortKey = Left || sortKey = Right then
        sortContextWithinWho namedQuery sortKey
    else
        []


let getSearchResults
    (logger: ILogger)
    (corpus: Corpus)
    (searchParams: SearchParams)
    (maybeAttributes: Cwb.PositionalAttribute list option)
    (pageNumbers: ResultPageNumbers)
    =
    async {
        let namedQuery =
            cwbQueryName corpus searchParams.SearchId

        let sortCmds =
            sortWithinWho namedQuery searchParams.SortKey

        let commands =
            let printStructuresCmd =
                if corpus.Config.HasMessages then
                  "set PrintStructures \"who_name, who_avfile, who_messageid\""
                else
                  "set PrintStructures \"who_name, who_avfile\""

            [ "set DataDirectory \"/tmp/glossa\""
              cwbCorpusName corpus searchParams.Queries
              "set Context 1 who_start"
              printStructuresCmd
              "set LD \"{{\""
              "set RD \"}}\""
              displayedAttrsCommand corpus searchParams.Queries maybeAttributes
              if sortCmds.Length > 0 then
                  yield! sortCmds
              $"cat {namedQuery} {searchParams.Start} {searchParams.End}" ]

        let! output = runCqpCommands logger corpus false commands

        let rawResults =
            fst output |> Option.defaultValue [||]

        let hits =
            rawResults
            |> transformResults corpus searchParams.Queries

        let hitPages =
            hits |> Array.chunkBySize searchParams.PageSize

        return
            (hitPages |> Array.toSeq, pageNumbers)
            ||> Seq.map2 (fun pageHits pageNumber ->
                { PageNumber = pageNumber
                  Results = pageHits })
            |> Seq.toArray
    }

let extractMediaInfo (corpus: Corpus) result =
    let result' =
        result

        // Remove any material from the previous or following transcription
        |> replace "^(.*\{\{.+)</trans>.*" "$1"
        |> replace "^.*<trans>(.*\{\{.+)" "$1"

        // If the matching word/phrase is at the beginning of the segment, CQP puts the braces
        // marking the start of the match before the starting segment tag
        // (e.g. {{<who_start 38.26><who_stop 30.34>went/go/PAST>...). Probably a
        // bug in CQP? In any case we have to fix it by moving the braces to the
        // start of the segment text instead. Similarly if the match is at the end of a segment.
        |> replace "\{\{((?:<\S+?\s+?\S+?>\s*)+)" "$1{{" // Find start tags with attributes (i.e., not the match)
        |> replace "((?:</\S+?>\s*)+)\}\}" "}}$1" // Find end tags

    let timestamps =
        [| for m in Regex.Matches(result', "<who_start\s+([\d\.]+)><who_stop\s+([\d\.]+)>.*?</who_start>") ->
               (m.Groups[1].Value, m.Groups[2].Value) |]

    let starttimes = timestamps |> Array.map fst
    let endtimes = timestamps |> Array.map snd

    let overallStarttime =
        starttimes |> Array.head

    let overallEndtime = endtimes |> Array.last

    let speakers =
        [ for m in Regex.Matches(result', "<who_name\s+(.+?)>") -> m.Groups[1].Value ]

    // If we get at hit at the beginning or end of a session, the context may include
    // material from the session before or after. Hence, we need to make sure that
    // we extract the line key from the segment containing the actual match (marked
    // by double braces).
    let movieLoc =
        let m =
            Regex.Match(result', "<who_avfile\s+([^>]+)>[^<]*\{\{")

        m.Groups[1].Value

    let result'' =
        result' |> replace "</?who_avfile ?.*?>" ""

    let mediaObjLines =
        [ for m in Regex.Matches(result'', "<who_stop.+?>(.*?)</who_stop>") -> m.Groups[1].Value ]

    // Create the data structure that is needed by jPlayer for a single search result
    let displayedAttrs =
        match corpus.Config.LanguageConfig with
        | Monolingual maybeAttributes ->
            match maybeAttributes with
            | Some attributes ->
                attributes
                |> List.map (fun attribute -> attribute.Code)
            | None -> []
        | Multilingual _ -> failwith "Multilingual spoken corpora not implemented!"

    let annotations =
        mediaObjLines
        |> List.mapi (fun index line ->
            let isMatch = Regex.IsMatch(line, "\{\{")
            let line' = line |> replace "\{\{|\}\}" ""
            let tokens = line'.Split()

            let annotation =
                { Speaker = speakers[index]
                  Line =
                    tokens
                    |> Array.mapi (fun index token ->
                        let attrValues = token.Split('/')

                        let attrNames =
                            "word" :: displayedAttrs
                            |> List.truncate attrValues.Length
                            |> List.toArray

                        let attrs =
                            Array.zip attrNames attrValues |> Map.ofArray

                        (index, attrs))
                  From = starttimes[index]
                  To = endtimes[index]
                  IsMatch = isMatch }

            (index, annotation))
        |> Map.ofList

    let matchingLineIndex =
        mediaObjLines
        |> List.findIndex (fun line -> Regex.IsMatch(line, "\{\{"))

    let lastLineIndex = mediaObjLines.Length - 1

    { Title = ""
      LastLine = lastLineIndex
      DisplayAttribute = "word"
      CorpusCode = corpus.Config.Code
      Mov =
        { Supplied = "m4v"
          Path = $"media/{corpus.Config.Code}"
          MovieLoc = movieLoc
          Start = overallStarttime
          Stop = overallEndtime }
      Divs = annotations
      StartAt = matchingLineIndex
      EndAt = matchingLineIndex
      MinStart = 0
      MaxEnd = lastLineIndex }

type TidAndPlace = { Tid: string; Place: string }

let getGeoDistribution (logger: ILogger) (searchParams: SearchParams) =
    task {
        let corpus =
            Corpora.Server.getCorpus searchParams.CorpusCode

        match corpus.Config.GeoMapConfig with
        | Some coords ->
            let namedQuery =
                cwbQueryName corpus searchParams.SearchId

            let attr =
                if corpus.Config.HasAttribute("phon") then
                    "phon"
                else
                    "word"

            let commands =
                [ "set DataDirectory \"/tmp/glossa\""
                  cwbCorpusName corpus []
                  $"group {namedQuery} match who_name by match {attr}" ]

            let! cwbResults = runCqpCommands logger corpus false commands

            // Get pairs of informant code and place name from MySQL
            let connStr =
                getConnectionString corpus.Config.Code

            use conn = new SqliteConnection(connStr)

            let sql =
                $"SELECT DISTINCT tid as Tid, {coords.LocationMetadataCategory.QualifiedColumnName} as Place FROM texts"

            let parameters =
                metadataSelectionToParamDict searchParams.MetadataSelection

            let! res = query logger conn sql (Some parameters)

            return
                match res with
                | Ok (results: TidAndPlace seq) ->
                    let informantPlaceMap =
                        results
                        |> Seq.map (fun r -> r.Tid, r.Place)
                        |> Map.ofSeq

                    // Keep the previous form in a volatile because when consecutive
                    // entries in the frequency list have the same form (but obviously
                    // different informants), the form is left blank.
                    let mutable prevForm = ""

                    cwbResults
                    // The first element contains the actual results
                    |> fst
                    |> Option.defaultValue [||]
                    |> Array.tail
                    |> Array.fold
                        (fun (state: Map<string, Map<string, int64>>) line ->
                            let parts = line.Split('\t')

                            let form =
                                let candidate = parts[0]

                                if System.String.IsNullOrWhiteSpace(candidate) then
                                    prevForm
                                else
                                    prevForm <- candidate
                                    candidate

                            match informantPlaceMap.TryFind(parts[1]) with
                            | Some place ->
                                let freq = System.Int64.Parse(parts[2])

                                state
                                |> Map.change form (fun (maybePlaceMap: Map<string, int64> option) ->
                                    match maybePlaceMap with
                                    | Some placeMap ->
                                        // We have already created a place map for this form
                                        placeMap
                                        |> Map.change place (fun (maybeAccFreq: int64 option) ->
                                            match maybeAccFreq with
                                            | Some accFreq ->
                                                // We have already registered one or more frequencies
                                                // for this place (and form), so accumulate
                                                Some(accFreq + freq)
                                            | None ->
                                                // This is the first time we see this place for the given
                                                // form, so start with the current frequency
                                                Some freq)
                                        |> Some
                                    | None ->
                                        // We have not yet created a place map for this form, so create one
                                        // with the current place and frequency
                                        [ (place, freq) ] |> Map.ofList |> Some)
                            | None -> state)
                        Map.empty
                // The first line is only decoration, so throw it away
                | Error ex -> raise ex
        | None -> return failwith "Cannot get geo distribution for corpus without geo coordinates!"
    }

let getMediaObject logger (searchParams: SearchParams) mediaPlayerType pageNumber rowIndex contextSize contextUnit =
    async {
        let corpus =
            Corpora.Server.getCorpus searchParams.CorpusCode

        let namedQuery =
            cwbQueryName corpus searchParams.SearchId

        let unitStr =
            if contextUnit = "episode" then
                "episode"
            else
                "who_start"

        let resultIndex =
            (pageNumber - 1) * searchParams.PageSize
            + rowIndex

        let showLine =
            if corpus.Config.HasMessages then
              "show +who_start +who_stop +who_name +who_avfile +trans +who_messageid"
            else
              "show +who_start +who_stop +who_name +who_avfile +trans"

        let commands =
            [ $"set DataDirectory \"/tmp/glossa\""
              corpus.Config.Code.ToUpper()
              $"set Context {contextSize} {unitStr}"
              "set LD \"{{\""
              "set RD \"}}\""
              showLine
              $"cat {namedQuery} {resultIndex} {resultIndex}" ]

        let! cqpResults = runCqpCommands logger corpus false commands

        let result =
            match cqpResults with
            | Some results, _ -> results[0]
            | _ -> failwith "Unable to fetch segment for media player"

        let mediaObject =
            extractMediaInfo corpus result

        return (mediaPlayerType, rowIndex, mediaObject)
    }
