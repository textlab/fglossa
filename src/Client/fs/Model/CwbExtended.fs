module CwbExtended

open System.Text.RegularExpressions
open Shared.StringUtils

type AttrOperator =
    | Equals
    | NotEquals
    | Contains
    static member OfString(s) =
        match s with
        | "=" -> Equals
        | "!=" -> NotEquals
        | "contains" -> Contains
        | _ -> failwith $"Unrecognized operator: {s}"

type AttrExpr =
    { Attr: string
      Operator: AttrOperator
      Values: Set<string> }
    member this.ToCqp() =
        let valueStr = this.Values |> String.concat "|"
        $"{this.Attr} = \"{valueStr}\""

type Interval = { Min: int option; Max: int option }

/// Used for e.g. parts of speech with their morphosyntactic categories as subcategories
type MainCategory =
    { Attr: string
      Operator: AttrOperator
      Value: string
      Subcategories: AttrExpr list option }
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
      ExtraForms: Map<string, Set<string>>
      IsLemma: bool
      IsPhon: bool
      IsOrig: bool
      IsStart: bool
      IsEnd: bool
      IsMiddle: bool
      IsInitial: bool
      IsFinal: bool
      Categories: MainCategory list
      PrecedingInterval: Interval option }
    static member Default =
        { MainStringValue = None
          ExtraForms = Map.empty
          IsLemma = false
          IsPhon = false
          IsOrig = false
          IsStart = false
          IsEnd = false
          IsMiddle = false
          IsInitial = false
          IsFinal = false
          PrecedingInterval = None
          Categories = [] }

let handleInterval intervalStr =
    let min =
        let m = Regex.Match(intervalStr, "(\d+),")

        if m.Success then
            m.Groups.[1].Value |> int |> Some
        else
            None

    let max =
        let m = Regex.Match(intervalStr, ",(\d+)")

        if m.Success then
            m.Groups.[1].Value |> int |> Some
        else
            None

    if min.IsSome || max.IsSome then
        Some { Min = min; Max = max }
    else
        None

let handleQuotedOrEmptyTerm (termStr: string) interval =
    if termStr.Length > 2 then
        // quoted term
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
        // empty term
        QueryTerm.Default

let handleAttributeValue (inputStr: string) interval =
    /// Unescapes any escaped chars, since we don't want the backslashes to show in the text input
    let unescapeForm form =
        form
        |> (replace "\\\(.)" "$1"
            >> replace "^(?:\.\*)?(.+?)" "$1"
            >> replace "(.+?)(?:\.\*)$" "$1")

    let processFirstForm (name, operator, value) =
        let isLemma = name = "lemma"
        let isPhon = name = "phon"
        let isOrig = name = "orig"
        let isStart = Regex.IsMatch(value, ".+\.\*$")
        let isEnd = Regex.IsMatch(value, "^\.\*.+")
        let isMiddle = isStart && isEnd

        { QueryTerm.Default with
              MainStringValue = Some(unescapeForm value)
              IsLemma = isLemma
              IsPhon = isPhon
              IsOrig = isOrig
              IsStart = if isMiddle then false else isStart
              IsEnd = if isMiddle then false else isEnd
              IsMiddle = isMiddle }

    let processOtherForms (term: QueryTerm) (name, operator, value) =
        let newValues =
            match term.ExtraForms |> Map.tryFind name with
            | Some values -> values.Add(value)
            | None -> Set.singleton value

        { term with
              ExtraForms = term.ExtraForms.Add(name, newValues) }

    let processForms (term: QueryTerm) =
        // In order to distinguish between, on the one hand, standard attributes such as word form, lemma,
        // phonetic form and original form, all of which are handled by check boxes in the main extended view,
        // as well as forms included or excluded at the bottom of the attribute popup, and on the other hand,
        // (potentially hierarchical) categories like grammatical features and corpus-specific attributes presented
        // in the attribute popup, the former should be listed first in the CQP expression without parentheses,
        // while the latter should be parenthesised, e.g.
        // [lemma="han" & phon="hann" & (((pos="pron" & case="nom|acc") | (pos="noun")) & ((desc="laughing")))]
        let termWithNonCategories =
            let m = Regex.Match(inputStr, "(.+)\(?")

            if m.Success then
                let attrValuePairs =
                    m.Groups.[1].Value.Split('&')
                    |> Array.map (
                        replace "%c" ""
                        >> fun s ->
                            let m = Regex.Match(s, "(.+)(!?=)\"(.+)\"")
                            (m.Groups.[1].Value, m.Groups.[2].Value, m.Groups.[3].Value)
                    )

                let h = Array.head attrValuePairs
                let (_, firstOperator, _) = h

                let term =
                    match firstOperator with
                    | "=" ->
                        // Only set the value of the text box and checkboxes if the operator is = and not !=.
                        let t = processFirstForm h
                        Array.fold processOtherForms t (Array.tail attrValuePairs)
                    | _ ->
                        let t = QueryTerm.Default
                        Array.fold processOtherForms t attrValuePairs

                term
            else
                QueryTerm.Default

        let categories =
            // As mentioned above, categories are found within parentheses as in the following expression:
            // [lemma="han" & phon="hann" & (((pos="pron" & case="nom|acc") | (pos="noun")) & ((desc="laughing")))]
            let m = Regex.Match(inputStr, "\((.+)\)")

            if m.Success then
                let matches =
                    Regex.Matches(m.Groups.[1].Value, "\(\((.+?)\)\)")

                [ for m in matches ->
                      let attributeValuePairs =
                          [ for pair in Regex.Matches(m.Value, "(\w+)(!?=)\"(.+?)\"") ->
                                { Attr = pair.Groups.[1].Value
                                  Operator = pair.Groups.[2].Value |> AttrOperator.OfString
                                  Values = pair.Groups.[3].Value.Split('|') |> Set.ofArray } ]

                      let p = List.head attributeValuePairs

                      { Attr = p.Attr
                        Operator = p.Operator
                        Value =
                            if p.Values.Count <> 1 then
                                failwith $"Main category should not have pipe in value: {p.Values}"

                            p.Values.MinimumElement
                        Subcategories =
                            match List.tail attributeValuePairs with
                            | [] -> None
                            | pairs -> Some pairs } ]
            else
                []

        let term =
            { termWithNonCategories with
                  Categories = categories }

        printfn $"{term}"
        term

    QueryTerm.Default |> processForms

type Query =
    { Terms: QueryTerm [] }
    static member OfCqp(cqpQuery) =
        // An interval, e.g. []{1,2}
        let intervalRx = "\[\]\{(.+?)\}"

        // An attribute/value expression such as [lemma="car" %c] or [(lemma="car" & pos="n")].
        // Treat quoted strings separately; they may contain right brackets
        let attributeValueRx = "\[(.+?)\]"

        // A quoted string or a single unspecified token
        let quotedOrEmptyTermRx = "\".*?\"|\[\]"

        let termsRegex =
            $"{intervalRx}|{attributeValueRx}|{quotedOrEmptyTermRx}"

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
                        latestInterval <- handleInterval groups.[1]
                    elif Regex.IsMatch(termStr, attributeValueRx) then
                        let term =
                            handleAttributeValue (List.last groups) latestInterval

                        terms <- Array.append terms [| term |]
                    elif Regex.IsMatch(termStr, quotedOrEmptyTermRx) then
                        let term =
                            handleQuotedOrEmptyTerm termStr latestInterval

                        terms <- Array.append terms [| term |])

            { Terms = terms }
