module Remoting.Search.Cwb.Common

open System
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
            | Some head -> head.Language.ToUpper()
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

    $"{queryStr} with {sTag}"

let buildMultilingualQuery (corpus: Corpus) (queries: Query []) (sTag: string) =
    let corpusCode = corpus.Config.Code.ToUpper()

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
                    Some $"{corpus.Config.Code}_{query.Language.ToUpper()} {query.Query}")

    (Array.append [| mainQuery |] alignedQueries)
    |> String.concat " :"

let displayedAttrsCommand (corpus: Corpus) (queries: Query []) (maybeAttributes: string [] option) =
    match maybeAttributes with
    | Some attributes ->
        let attrString =
            attributes
            |> Array.map (fun attr -> $"+{attr}")
            |> String.concat " "

        $"show -word; show {attrString}"
    | None ->
        let firstQueryLanguage =
            match Array.tryHead queries with
            | Some head -> head.Language
            | None -> failwith "Empty query!"
        // TODO: Implement parsing of tagger attributes and corpus-specific attributes
        ""

let alignedLanguagesCommand (corpus: Corpus) (queries: Query []) =
    let languageCodes =
        queries |> Array.map (fun q -> q.Language)

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
    // TODO: printPositionsMatchingMetadata
    initCommands @ [ $"{namedQuery} = {queryStr}" ]


let runCqpCommands (logger: ILogger) (corpus: Corpus) isCounting (commands: string seq) =
    async {
        try
            let commandStr =
                commands
                |> Seq.map (fun s -> $"{s};")
                |> String.concat "\n"

            let (output, error) =
                Process.runCmdWithInputOutputAndError "docker" "exec -i cwb cqp -c" commandStr

            let isUndumpError =
                Regex.IsMatch(error, "(?i)CQP Error:\s+Format error in undump file")

            if (not isUndumpError) && (error.Length > 0) then
                logger.Error(error)

            // Split into lines and throw away the first line, which contains the CQP version.
            // If isCounting is true (which it is when we are searching, but not when retrieving
            // results), the first line after that contains the number of results. Any following
            // lines contain actual search results (only in the first step).
            let results = output.Split('\n') |> Array.tail

            let count =
                match isCounting with
                | true ->
                    (if not isUndumpError then
                         results |> Array.head |> int
                     else
                         0)
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

            printfn $"OUTPUT: {output}"
            printfn $"ERROR LENGTH: {error.Length}"

            if
                results.Length > 1
                && Regex.IsMatch
                    (
                        results.[0],
                        "PARSE ERROR|CQP Error"
                    )
            then
                return failwith $"CQP error: {results}"
            else
                return (searchResults, count)
        with :? OutOfMemoryException as ex ->
            logger.Error $"Out of memory: killing all CQP processes at {DateTime.Now}"
            Process.runCmd "killall" "cqp"
            return raise ex
    }
