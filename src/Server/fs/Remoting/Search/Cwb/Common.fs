module Remoting.Search.Cwb.Common

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
    | Multilingual _ ->
        // The CWB corpus we select before running our query will be the one named by the
        // code attribute of the corpus plus the name of the language of the first
        // submitted query row (e.g. RUN_EN).
        let firstLanguageCode =
            match queries |> Seq.tryHead with
            | Some head -> head.Language.ToUpper()
            | None -> failwith $"Empty query!"

        $"{uppercaseCode}_{firstLanguageCode}"
    | Monolingual _ -> uppercaseCode

let cwbQueryName (corpus: Corpus) (searchId: int) =
    $"{corpus.Config.Code.ToUpper()}{searchId}"

let runCqpCommands (logger: ILogger) (corpus: Corpus) (commands: string seq) isCounting =
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
                     int results.[0]
                 else
                     0)
                |> Some
            | false -> None

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
            failwith $"CQP error: {results}"
    with :? System.OutOfMemoryException ->
        logger.Error $"Out of memory: killing all CQP processes at {System.DateTime.Now}"
        Process.runCmd "killall" "cqp"
