open System
open System.Text.RegularExpressions

let evalReplace pattern (evaluator: MatchEvaluator) str = Regex.Replace(str, pattern, evaluator)

let initEval =
    new MatchEvaluator(fun m -> m.Value.ToUpper())

let middleEval =
    new MatchEvaluator(fun m -> m.Groups.[1].Value.ToUpper())

let toCamelCase str =
    str |> evalReplace "^." initEval |> evalReplace "_(.)" middleEval

let corpusDirs = [ "bokmal"; "ndc2"; "nowac_1_1"; "norint_tekst" ]

let matchStrings =
    let dirStrings =
        corpusDirs
        |> List.map (fun dir ->
            let corpusModule = toCamelCase dir
            $"""    | "{dir}" -> {corpusModule}.Server.getCorpus ()""")
        |> String.concat "\n"
    dirStrings + "\n    | _ -> failwithf \"Unknown corpus: %s\" code"

printfn $"""module Corpora.Core

/////////////////////////////////////////////////////////////////////
/// !!! Auto-generated from corpus directory structure. Do not edit!
/////////////////////////////////////////////////////////////////////

open ServerTypes

let getCorpus (code: string) : Corpus =
    match code with
{matchStrings}
"""
