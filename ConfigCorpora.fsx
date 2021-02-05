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

let corpusCodeStrings =
    corpusDirs
    |> List.map (fun dir -> $"    | {toCamelCase dir}")
    |> String.concat "\n"

let matchStrings =
    corpusDirs
    |> List.map (fun dir ->
        let s = toCamelCase dir
        $"    | {s} -> {s}.Server.getCorpus ()")
    |> String.concat "\n"

printfn $"""module Corpora.Core

/////////////////////////////////////////////////////////////////////
/// !!! Auto-generated from corpus directory structure. Do not edit!
/////////////////////////////////////////////////////////////////////

type CorpusCode =
{corpusCodeStrings}

let getCorpus (code: CorpusCode) : Shared.Corpus =
    match code with
{matchStrings}
"""
