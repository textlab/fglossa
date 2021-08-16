module CwbExtended

open System.Text.RegularExpressions
open Shared.StringUtils

type AttrOperator =
    | Equals
    | Contains

type AttrExpr =
    { Attr: string
      Operator: AttrOperator
      Values: Set<string> }

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
      ExtraForms: Map<string, Set<string>>
      IsLemma: bool
      IsPhon: bool
      IsOrig: bool
      IsStart: bool
      IsEnd: bool
      IsMiddle: bool
      IsInitial: bool
      IsFinal: bool
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
          PrecedingInterval = None }

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
            >> replace "^\"(?:\.\*)?(.+?)" "$1"
            >> replace "(.+?)(?:\.\*)\"$" "$1")

    let processFirstForm (name, value) =
        let isLemma = name = "lemma"
        let isPhon = name = "phon"
        let isOrig = name = "orig"
        let isStart = Regex.IsMatch(value, ".+\.\*\"$")
        let isEnd = Regex.IsMatch(value, "^\"\.\*.+")
        let isMiddle = isStart && isEnd

        { QueryTerm.Default with
              MainStringValue = Some(unescapeForm value)
              IsLemma = isLemma
              IsPhon = isPhon
              IsOrig = isOrig
              IsStart = if isMiddle then false else isStart
              IsEnd = if isMiddle then false else isEnd
              IsMiddle = isMiddle }

    let processOtherForms (term: QueryTerm) (name, value) =
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
        // [lemma="han" & phon="hann" & ((pos="pron" & case="nom|acc") | (pos="noun"))]
        let nonCategories =
            let m = Regex.Match(inputStr, "(.+)\(?")

            if m.Success then
                let attrValuePairs =
                    m.Groups.[1].Value.Split('&')
                    |> Array.map (
                        replace "%c" ""
                        >> fun s ->
                            let elms = s.Split("=")
                            (elms.[0], elms.[1])
                    )

                let t =
                    processFirstForm (Array.head attrValuePairs)

                let term =
                    Array.fold processOtherForms t (Array.tail attrValuePairs)

                term
            else
                QueryTerm.Default

        // let forms =
        //     [ for m in Regex.Matches(inputStr, "(word|lemma|phon|orig)\s*(!?=)\s*\"(.+?)\"") ->
        //           let name = m.Groups.[1].Value
        //           let operator = m.Groups.[2].Value

        //           let value =
        //               m.Groups.[3].Value
        //               |> fun v -> if operator = "!=" then "!" + v else v

        //           (name, operator, value) ]

        // forms
        // |> List.fold
        //     (fun acc (name, operator, value) ->
        //         // If the attribute value starts with &&, it should be a special
        //         // code (e.g. for marking errors in text, inserted as "tokens"
        //         // to ensure alignment between original and corrected text) and
        //         // should not be shown in the text input box
        //         if name.StartsWith("&&") then
        //             acc
        //         else
        //         // Only the first non-negative word/lemma/phon/orig form
        //         // goes into the main string value; the rest are additional expressions
        //         if acc.MainStringValue.IsNone && operator = "=" then
        //             processFirstForm name value
        //         else
        //             processOtherForms acc name value)
        //     term
        printfn $"{nonCategories}"
        nonCategories

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
