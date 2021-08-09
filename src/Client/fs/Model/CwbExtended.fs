module CwbExtended

open System.Text.RegularExpressions
open Shared.StringUtils

type AttrOperator =
    | Equals
    | Contains

type AttrExpr =
    { Attr: string
      Operator: AttrOperator
      Value: string }

type Interval = { Min: int option; Max: int option }

/// Used for e.g. morphosyntacic categories
type Subcategory =
    { Attr: string
      Values: string list }
    member this.ToCqp() =
        let valueStr = this.Values |> String.concat "|"
        $"{this.Attr} = \"{valueStr}\""

/// Used for e.g. parts of speech with their morphosyntactic categories as subcategories
type MainCategory =
    { Attr: string
      Value: string
      Subcategories: Subcategory list option }
    member this.ToCqp() =
        let mainExpr = $"{this.Attr} = \"{this.Value}\""

        let expressions =
            match this.Subcategories with
            | Some cats ->
                cats
                |> List.map (fun cat -> cat.ToCqp())
                |> String.concat " & "
                |> fun subcatExpressions -> $"{mainExpr} & {subcatExpressions}"
            | None -> mainExpr

        $"({expressions})"

type QueryTerm =
    { MainStringValue: string option
      AdditionalExpressions: AttrExpr []
      IsPhon: bool
      IsOrt: bool
      IsStart: bool
      IsEnd: bool
      IsMiddle: bool
      IsInitial: bool
      IsFinal: bool
      PrecedingInterval: Interval option }
    static member Default =
        { MainStringValue = None
          AdditionalExpressions = [||]
          IsPhon = false
          IsOrt = false
          IsStart = false
          IsEnd = false
          IsMiddle = false
          IsInitial = false
          IsFinal = false
          PrecedingInterval = None }

let handleInterval intervalStr =
    let min =
        let m = Regex.Match(intervalStr, "(\d+),")

        if m.Success then
            Some m.Groups.[1].Value |> Option.map int
        else
            None

    let max =
        let m = Regex.Match(intervalStr, ",(\d+)")

        if m.Success then
            Some m.Groups.[1].Value |> Option.map int
        else
            None

    if min.IsSome || max.IsSome then
        Some { Min = min; Max = max }
    else
        None

let handleAttributeValue (termStr: string) interval = QueryTerm.Default

let handleQuotedOrEmptyTerm (termStr: string) interval =
    if termStr.Length > 2 then
        let form = termStr.[1..(termStr.Length - 2)]
        let isStart = Regex.IsMatch(form, ".+\.\*$")
        let isEnd = Regex.IsMatch(form, "^\.\*.+")
        let isMiddle = isStart && isEnd

        { QueryTerm.Default with
              MainStringValue = Some form
              IsStart = if isMiddle then false else isStart
              IsEnd = if isMiddle then false else isEnd
              IsMiddle = isMiddle
              PrecedingInterval = interval }
    else
        QueryTerm.Default

type Query =
    { Terms: QueryTerm [] }
    static member OfCqp(cqpQuery) =
        // An interval, e.g. []{1,2}
        let intervalRx = "\[\]\{(.+?)\}"

        // A quoted string or a single unspecified token
        let quotedOrEmptyTermRx = "\".*?\"|\[\]"

        // An attribute/value expression such as [lemma="car" %c] or [(lemma="car" & pos="n")].
        // Treat quoted strings separately; they may contain right brackets
        let attributeValueRx =
            "\[\(?([^\"]+?(?:\"[^\"]*\"[^\]\"]*?)*?)(?:\s+%c)?\)?\]"

        let termsRegex =
            $"{intervalRx}|{quotedOrEmptyTermRx}|{attributeValueRx}"

        cqpQuery
        // remove all whitespace so we don't have to allow for that everywhere in our later regexes
        |> replace "\s+" ""
        |> fun s ->
            let termStrings =
                if System.String.IsNullOrWhiteSpace(s) then
                    [ ("[]", []) ]
                else
                    [ for m in Regex.Matches(s, termsRegex) ->
                          let groups = [ for group in m.Groups -> group.Value ]
                          (m.Value, groups) ]

            let mutable terms = [||]
            let mutable latestInterval = None

            termStrings
            |> List.iter
                (fun (termStr, groups) ->
                    if Regex.IsMatch(termStr, intervalRx) then
                        latestInterval <- handleInterval termStr
                    elif Regex.IsMatch(termStr, attributeValueRx) then
                        let term =
                            handleAttributeValue termStr latestInterval

                        terms <- Array.append terms [| term |]
                    elif Regex.IsMatch(termStr, quotedOrEmptyTermRx) then
                        let term =
                            handleQuotedOrEmptyTerm termStr latestInterval

                        terms <- Array.append terms [| term |])

            { Terms = terms }
