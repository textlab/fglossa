module Remoting.Search.Cwb.Common

open System
open System.IO
open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Data.SQLite
open System.Text.RegularExpressions
open Dapper
open Serilog
open Database
open Shared
open ServerTypes
open Remoting.Search.Common

let cwbCorpusName (corpus: Corpus) (queries: Query seq) =
    let uppercaseCode = corpus.Config.Code.ToUpper()

    match corpus.Config.LanguageConfig with
    | Monolingual _ -> uppercaseCode
    | Multilingual _ ->
        // The CWB corpus we select before running our query will be the one named by the
        // code attribute of the corpus plus the name of the language of the first
        // submitted query row (e.g. RUN_EN).
        let firstLanguageCode =
            match queries |> Seq.tryHead with
            | Some head -> head.LanguageCode.ToUpper()
            | None -> failwith $"Empty query!"

        $"{uppercaseCode}_{firstLanguageCode}"

let cwbQueryName (corpus: Corpus) (searchId: int) =
    $"{corpus.Config.Code.ToUpper()}{searchId}"

let buildMonolingualQuery (queries: Query []) (sTag: string) =
    // For monolingual queries, the query expressions should be joined together with '|' (i.e., "or")
    let queryExpressions =
        queries |> Array.map (fun query -> query.Query)

    let queryStr =
        if queryExpressions.Length > 1 then
            (queryExpressions |> String.concat " | ")
        else
            match Seq.tryHead queryExpressions with
            | Some head -> head
            | None -> failwith "Empty query!"

    $"{queryStr} within {sTag}"

let buildMultilingualQuery (corpus: Corpus) (queries: Query []) (sTag: string) =
    let mainQuery =
        match Array.tryHead queries with
        | Some head -> $"{head.Query} within {sTag}"
        | None -> failwith "Empty query!"

    let alignedQueries =
        queries
        |> Array.tail
        |> Array.choose
            (fun query ->
                // TODO: In case of mandatory alignment, include even empty queries
                if String.IsNullOrWhiteSpace(query.Query) then
                    None
                else
                    Some $"{corpus.Config.Code}_{query.LanguageCode.ToUpper()} {query.Query}")

    (Array.append [| mainQuery |] alignedQueries)
    |> String.concat " :"

// Prints to file the start and stop positions of all corpus texts that are associated with the
// metadata values that have the given database ids, with an OR relationship between values within
// the same category and an AND relationship between categories. Also restricts the positions to the start
// and end positions provided in the request.
let printPositionsMatchingMetadata
    (corpus: Corpus)
    (searchParams: SearchParams)
    (startpos: uint64)
    (endpos: uint64)
    (positionsFilename: string)
    =
    File.Delete(positionsFilename)

    match searchParams.Metadata with
    | Some metadata ->
        let positionFields =
            match corpus.Config.Modality with
            | Spoken -> $"replace(replace(`bounds`, '-', '\t'), ':', '\n')"
            | Written -> $"startpos, endpos"

        let sql = $"SELECT DISTINCT({positionFields})"
        failwith "NOT IMPLEMENTED"
    | None ->
        // No metadata selected
        match corpus.Config.Modality with
        | Spoken -> failwith "NOT IMPLEMENTED"
        | Written ->
            // For written corpora, simply search the entire corpus by just printing the start and end
            // positions specified in the request, making sure that the end position does not exceed the size
            // of the corpus
            let cwbCorpus =
                (cwbCorpusName corpus searchParams.Queries)
                    .ToLower()

            match corpus.Config.Sizes.TryFind(cwbCorpus) with
            | Some corpusSize ->
                let endpos' = Math.Min(endpos, corpusSize - 1UL)
                File.WriteAllText(positionsFilename, $"{startpos}\t{endpos'}\n")
            | None -> failwith $"No corpus size found for {cwbCorpus} in {corpus.Config.Sizes}!"

let displayedAttrsCommand (corpus: Corpus) (queries: Query []) (maybeAttributes: TokenAttribute [] option) =
    let createAttrString attributes =
        attributes
        |> Array.map (fst >> fun attr -> $"+{attr}")
        |> String.concat " "

    match maybeAttributes with
    // Given an explicit array of attributes, e.g. when exporting to Excel etc.
    | Some attributes ->
        let attrString = createAttrString attributes
        $"show -word; show {attrString}"
    | None ->
        match corpus.Config.LanguageConfig with
        | Monolingual attributes ->
            let attrString = createAttrString attributes
            $"show {attrString}"
        | Multilingual languages ->
            let firstQueryLanguageCode =
                match Array.tryHead queries with
                | Some head -> head.LanguageCode
                | None -> failwith "Empty query!"

            let language =
                languages
                |> Array.find (fun lang -> lang.Code = firstQueryLanguageCode)

            let attributes = language.TokenAttributes
            let attrString = createAttrString attributes
            $"show -word; show {attrString}"
// TODO: Implement parsing of tagger attributes and corpus-specific attributes

let alignedLanguagesCommand (corpus: Corpus) (queries: Query []) =
    let languageCodes =
        queries |> Array.map (fun q -> q.LanguageCode)

    let firstLanguageCode =
        match Array.tryHead languageCodes with
        | Some head -> head
        | None -> failwith "Empty query!"

    let nonFirstLanguageCodes =
        match firstLanguageCode with
        | "org" -> [ "korr" ]
        | "korr" -> [ "org" ]
        | code when languageCodes.Length > 1 ->
            languageCodes
            |> Set.ofArray
            |> Set.remove code
            |> Set.toList
        | _ -> []
    // Only show alignment attributes if we have actually asked for aligned languages
    if not nonFirstLanguageCodes.IsEmpty then
        let codes =
            nonFirstLanguageCodes
            |> List.map (fun code -> $"+{corpus.Config.Code}_{code}")
            |> String.concat " "

        $"show ${codes}"
    else
        ""

let sortCommand (namedQuery: string) (sortKey: SortKey) =
    match sortKey with
    | Position -> None
    | Match -> Some ""
    | Left -> Some " on match[-1]"
    | Right -> Some " on matchend[1]"
    |> Option.map
        (fun c ->
            [ "set ExternalSort on"
              $"sort {namedQuery} by word %%c{c}" ])

let constructQueryCommands
    (corpus: Corpus)
    (searchParams: SearchParams)
    (namedQuery: string)
    (startpos: uint64)
    (endpos: uint64)
    (maybeSTag: string option)
    (maybeCpuIndex: int option)
    =
    let sTag = defaultArg maybeSTag "s"

    let queryStr =
        match corpus.Config.LanguageConfig with
        | Monolingual _ -> buildMonolingualQuery searchParams.Queries sTag
        | Multilingual _ -> buildMultilingualQuery corpus searchParams.Queries sTag

    let cqpIndexStr =
        maybeCpuIndex
        |> Option.map string
        |> Option.defaultValue ""

    let positionsFilename =
        $"/tmp/glossa/positions_{searchParams.SearchId}{cqpIndexStr}"

    let initCommands =
        [ $"undump {namedQuery} < '{positionsFilename}'"
          namedQuery ]

    printPositionsMatchingMetadata corpus searchParams startpos endpos positionsFilename
    initCommands @ [ $"{namedQuery} = {queryStr}" ]


let runCqpCommands (logger: ILogger) (corpus: Corpus) isCounting (commands: string seq) =
    async {
        try
            let commandStr =
                commands
                |> Seq.filter (String.IsNullOrWhiteSpace >> not)
                |> Seq.map (fun s -> $"{s};")
                |> String.concat "\n"

            let (output, error) =
                Process.runCmdWithInputOutputErrorAndEncoding "docker" "exec -i cwb cqp -c" corpus.Encoding commandStr

            let isUndumpError =
                Regex.IsMatch(error, "(?i)CQP Error:\s+Format error in undump file")

            if (not isUndumpError) && (error.Length > 0) then
                logger.Error(error)

            // Split into lines and throw away the first line, which contains the CQP version.
            // If isCounting is true (which it is when we are searching, but not when retrieving
            // results), the first line after that contains the number of results. Any following
            // lines contain actual search results (only in the first step).
            let results =
                output.Split('\n')
                |> Array.tail
                |> Array.filter (String.IsNullOrWhiteSpace >> not)

            let count =
                match isCounting with
                | true ->
                    (if not isUndumpError then
                         results |> Array.head |> uint64
                     else
                         0UL)
                    |> Some
                | false -> None

            let searchResults =
                if not isUndumpError then
                    (if isCounting then
                         (results |> Array.tail)
                     else
                         results)
                    |> Some
                else
                    None

            if
                results.Length > 1
                && Regex.IsMatch(results.[0], "PARSE ERROR|CQP Error")
            then
                return failwith $"CQP error: {results}"
            else
                return (searchResults, count)
        with
        | :? OutOfMemoryException as ex ->
            logger.Error $"Out of memory: killing all CQP processes at {DateTime.Now}"
            Process.runCmd "killall" "cqp"
            return raise ex
    }
