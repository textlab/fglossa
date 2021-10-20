module CwbExtended

open System.Text.RegularExpressions
open Shared
open Shared.StringUtils
open Model

type MinMax =
    | Min
    | Max

type Interval = { Min: int option; Max: int option }

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

    member this.ToOperatorString() =
        match this with
        | Equals -> "="
        | NotEquals -> "!="
        | Contains -> "contains"

type ExtraForm =
    { Attr: string
      Operator: AttrOperator
      Values: Set<string> }

type Subcategory =
    { Attr: string
      Operator: AttrOperator
      Values: Set<string> }
    member this.ToCqp() =
        let valueStr =
            this.Values
            |> Set.toArray
            |> Array.sort
            |> String.concat "|"

        $"{this.Attr}=\"{valueStr}\""

/// Used to represent CWB attribute selections in a query such as parts of speech
/// with their morphosyntactic categories as subcategories
type MainCategory =
    { Attr: string
      Operator: AttrOperator
      Value: string
      Subcategories: Set<Subcategory> option }
    member this.ToCqp() =
        let mainExpr =
            $"{this.Attr}{this.Operator.ToOperatorString()}\"{this.Value}\""

        let expressions =
            match this.Subcategories with
            | Some cats ->
                cats
                |> Set.toList
                |> List.sort
                |> List.map (fun cat -> cat.ToCqp())
                |> String.concat " & "
                |> fun subcatExpressions -> $"{mainExpr} & {subcatExpressions}"
            | None -> mainExpr

        $"({expressions})"

type QueryProperty =
    | IsLemma
    | IsPhonetic
    | IsOriginal
    | IsStart
    | IsEnd
    | IsMiddle
    | IsInitial
    | IsFinal

type QueryTerm =
    { MainStringValue: string option
      ExtraForms: ExtraForm list
      IsLemma: bool
      IsPhonetic: bool
      IsOriginal: bool
      IsStart: bool
      IsEnd: bool
      IsMiddle: bool
      IsInitial: bool
      IsFinal: bool
      CategorySections: Set<MainCategory> list
      PrecedingInterval: Interval option }
    static member Default =
        { MainStringValue = None
          ExtraForms = []
          IsLemma = false
          IsPhonetic = false
          IsOriginal = false
          IsStart = false
          IsEnd = false
          IsMiddle = false
          IsInitial = false
          IsFinal = false
          PrecedingInterval = None
          CategorySections = [] }

    member this.ToCqp(sTag: string) =
        let intervalString =
            match this.PrecedingInterval with
            | Some interval ->
                let min =
                    interval.Min
                    |> Option.map string
                    |> Option.defaultValue ""

                let max =
                    interval.Max
                    |> Option.map string
                    |> Option.defaultValue ""

                if min <> "" || max <> "" then
                    $"[]{{{min},{max}}} "
                else
                    ""
            | None -> ""

        let maybeMainString =
            match this.MainStringValue with
            | Some form ->
                let mainAttr =
                    if this.IsLemma then "lemma"
                    elif this.IsPhonetic then "phon"
                    elif this.IsOriginal then "orig"
                    else "word"

                let caseInsensitiveStr = if this.IsPhonetic then "" else " %c"

                let processedForm =
                    form
                    |> replace "[\.\*\+\?\^\$\{\}\(\)\|\[\]\\\]" "\\$&"
                    |> fun f ->
                        if this.IsStart then $"{f}.*"
                        elif this.IsEnd then $".*{f}"
                        elif this.IsMiddle then $".*{f}.*"
                        else f

                Some $"{mainAttr}=\"{processedForm}\"{caseInsensitiveStr}"
            | None -> None

        let maybeExtraForms =
            if this.ExtraForms.Length > 0 then
                [ for forms in this.ExtraForms ->
                      let values =
                          forms.Values
                          |> Set.toArray
                          |> Array.sort
                          |> String.concat "|"

                      $"{forms.Attr}{forms.Operator.ToOperatorString()}\"{values}\"" ]
                |> String.concat " & "
                |> Some
            else
                None

        let maybeCategoryStrings =
            if this.CategorySections
               |> List.exists (fun sectionCategories -> not sectionCategories.IsEmpty) then
                [ for categorySection in this.CategorySections ->
                      // If we want to exclude these category values rather than include them,
                      // the output expressions should be combined with '&' instead of '|'
                      // (e.g. [pos!="noun" & pos!="adj"])
                      let isExcluded =
                          categorySection.Count > 0
                          && categorySection.MinimumElement.Operator = NotEquals

                      [ for category in categorySection ->
                            // yields e.g. (pos="noun" & num="sg")
                            category.ToCqp() ]
                      // yields e.g. (pos="noun" & num="sg") | (pos="pron" & num="sg")
                      |> String.concat (if isExcluded then " & " else " | ")
                      // yields e.g. ((pos="noun" & num="sg") | (pos="pron" & num="sg"))
                      |> fun s -> $"({s})" ]
                // yields e.g. ((pos="noun" & num="sg") | (pos="pron" & num="sg")) & ((desc="laughing"))
                |> String.concat " & "
                // yields e.g. (((pos="noun" & num="sg") | (pos="pron" & num="sg")) & ((desc="laughing")))
                |> fun s -> $"({s})"
                |> Some
            else
                None

        let forms =
            [ maybeMainString
              maybeExtraForms
              maybeCategoryStrings ]
            |> List.choose id
            |> String.concat " & "
            |> fun s ->
                if this.IsInitial then $"<{sTag}>[{s}]"
                elif this.IsFinal then $"[{s}]</{sTag}>"
                else $"[{s}]"

        $"{intervalString}{forms}"

let addOrRemoveExtraForms term attrName operator attrValue =
    let mutable foundForms = false

    term.ExtraForms
    |> List.map
        (fun forms ->
            if forms.Attr = attrName then
                // A set of values already exists for this attribute name
                foundForms <- true

                let newValues = forms.Values.Add(attrValue)

                { forms with Values = newValues }
            else
                forms)
    |> fun extraFormsList ->
        if foundForms then
            extraFormsList
        else
            List.append
                extraFormsList
                [ { Attr = attrName
                    Operator = operator
                    Values = Set.singleton attrValue } ]



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

let handleQuotedOrEmptyTerm (termStr: string) interval (maybeCwbAttributeMenu: CwbAttributeMenu option) =
    let categorySections =
        match maybeCwbAttributeMenu with
        | Some cwbAttributeMenu ->
            // The corpus has an attribute menu, but no attribute category selections were
            // found the CQP expression.
            List.replicate cwbAttributeMenu.Length Set.empty
        | None ->
            // The corpus does not have an attribute menu
            []

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
              CategorySections = categorySections
              PrecedingInterval = interval }
    else
        // empty term
        { QueryTerm.Default with
              CategorySections = categorySections
              PrecedingInterval = interval }

let handleAttributeValue
    (inputStr: string)
    interval
    isSegmentInitial
    isSegmentFinal
    (maybeCwbAttributeMenu: CwbAttributeMenu option)
    =
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
              IsPhonetic = isPhon
              IsOriginal = isOrig
              IsStart = if isMiddle then false else isStart
              IsEnd = if isMiddle then false else isEnd
              IsMiddle = isMiddle }

    let processOtherForms (term: QueryTerm) (name, operator, value) =
        let newExtraForms =
            addOrRemoveExtraForms term name operator value

        { term with ExtraForms = newExtraForms }

    let processForms =
        // In order to distinguish between, on the one hand, standard attributes such as word form, lemma,
        // phonetic form and original form, all of which are handled by check boxes in the main extended view,
        // as well as forms included or excluded at the bottom of the attribute popup, and on the other hand,
        // (potentially hierarchical) categories like grammatical features and corpus-specific attributes presented
        // in the attribute popup, the former should be listed first in the CQP expression without parentheses,
        // while the latter should be parenthesised, e.g.
        // [lemma="han" & phon="hann" & (((pos="pron" & case="nom|acc") | (pos="noun")) & ((desc="laughing")))]
        let termWithNonCategories =
            let mabyeNonCatStr =
                if inputStr.Contains('(') then
                    // The term contains categories, so grab everything before that
                    let m = Regex.Match(inputStr, "^([^\(]+)&")

                    if m.Success then
                        // The term contains something besides categories
                        Some m.Groups.[1].Value
                    else
                        // The term does NOT contain anything besides categories
                        None
                else
                    // The term does not contain any categories, so use the whole string
                    Some inputStr

            match mabyeNonCatStr with
            | Some nonCatStr ->
                let attrValuePairs =
                    nonCatStr.Split('&')
                    |> Array.map (
                        replace "%c" ""
                        >> fun s ->
                            let m = Regex.Match(s, "(.+?)(!?=)\"(.+)\"")
                            (m.Groups.[1].Value, AttrOperator.OfString(m.Groups.[2].Value), m.Groups.[3].Value)
                    )

                let h = Array.head attrValuePairs
                let (_, firstOperator, _) = h

                let term =
                    match firstOperator with
                    | Equals ->
                        // Only set the value of the text box and checkboxes if the operator is = and not !=.
                        let t = processFirstForm h
                        Array.fold processOtherForms t (Array.tail attrValuePairs)
                    | _ ->
                        let t = QueryTerm.Default
                        Array.fold processOtherForms t attrValuePairs

                term
            | None -> QueryTerm.Default

        let categories =
            match maybeCwbAttributeMenu with
            | Some cwbAttributeMenu ->
                // As mentioned above, categories are found within parentheses as in the following expression:
                // [lemma="han" & phon="hann" & (((pos="pron" & case="nom|acc") | (pos="noun")) & ((desc="laughing")))]
                let m = Regex.Match(inputStr, "\((.+)\)")

                if m.Success then
                    let categorySections =
                        Regex.Matches(m.Groups.[1].Value, "\(\((.+?)\)\)")

                    let sectionsByAttributeMenuIndex =
                        [ for categorySection in categorySections ->
                              let categoryStrings =
                                  Regex.Split(categorySection.Groups.[0].Value, "\)[\|\&]\(")

                              let categories =
                                  [ for category in categoryStrings ->
                                        let attributeValuePairs =
                                            [ for pair in Regex.Matches(category, "(\w+)(!?=)\"(.+?)\"") ->
                                                  { Attr = pair.Groups.[1].Value
                                                    Operator = pair.Groups.[2].Value |> AttrOperator.OfString
                                                    Values = pair.Groups.[3].Value.Split('|') |> Set.ofArray } ]

                                        let firstPair = List.head attributeValuePairs

                                        { Attr = firstPair.Attr
                                          Operator = firstPair.Operator
                                          Value =
                                              if firstPair.Values.Count <> 1 then
                                                  failwith
                                                      $"Main category should not have pipe in value: {firstPair.Values}"

                                              firstPair.Values.MinimumElement
                                          Subcategories =
                                              match List.tail attributeValuePairs with
                                              | [] -> None
                                              | pairs -> pairs |> Set.ofList |> Some } ]

                              // Look at the first main category in this section and find the first (which should also be the
                              // only) section in the CwbAttributeMenu for this corpus that contains the same combination of
                              // attribute name and value
                              let firstCategory = categories.Head

                              let menuSectionIndex =
                                  cwbAttributeMenu
                                  |> List.findIndex
                                      (fun menuSection ->
                                          menuSection.Values
                                          |> List.exists
                                              (fun ((attr, attrValue, _, _): CwbAttributeMenu.MainCategoryValue) ->
                                                  attr.Code = firstCategory.Attr
                                                  && attrValue = firstCategory.Value))

                              (menuSectionIndex, categories |> Set.ofList) ]
                        |> Map.ofList

                    // Return a list that contains an item for each section in the CwbAttributeMenu for this corpus,
                    // where the item contains the info extracted from the CQP expression if any, and an empty set
                    // otherwise. This way, we can easily map the returned list of selected category values to the sections
                    // shown in the attribute popup in the CWB extended search view.
                    [ for i in 0 .. cwbAttributeMenu.Length - 1 ->
                          if sectionsByAttributeMenuIndex.ContainsKey(i) then
                              sectionsByAttributeMenuIndex.[i]
                          else
                              Set.empty ]
                else
                    // The corpus has an attribute menu, but no attribute category selections were
                    // found the CQP expression.
                    List.replicate cwbAttributeMenu.Length Set.empty
            | None ->
                // The corpus does not contain an attribute menu
                []

        { termWithNonCategories with
              CategorySections = categories }

    { processForms with
          PrecedingInterval = interval
          IsInitial = isSegmentInitial
          IsFinal = isSegmentFinal }

type Query =
    { Terms: QueryTerm [] }
    static member Default = { Terms = [||] }

    static member STag(corpus: Corpus) =
        match corpus.SharedInfo.Modality with
        | Spoken -> "who"
        | Written -> "s"

    static member OfCqp(corpus: Corpus, cqpQuery: string) =
        let sTag = Query.STag(corpus)

        // An interval, e.g. []{1,2}
        let intervalRx = "\[\]\{(.+?)\}"

        // An attribute/value expression such as [lemma="car" %c] or [(lemma="car" & pos="n")].
        // Treat quoted strings separately; they may contain right brackets
        let attributeValueRx = $"(?:<{sTag}>)?\[(.+?)\](?:</{sTag}>)?"

        // A quoted string or a single unspecified token
        let quotedOrEmptyTermRx = "\".*?\"|\[\]"

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
                        latestInterval <- handleInterval groups.[1]
                    elif Regex.IsMatch(termStr, attributeValueRx) then
                        let isSegmentInitial = Regex.IsMatch(termStr, $"<{sTag}>")
                        let isSegmentFinal = Regex.IsMatch(termStr, $"</{sTag}>")

                        let term =
                            handleAttributeValue
                                (List.last groups)
                                latestInterval
                                isSegmentInitial
                                isSegmentFinal
                                corpus.CwbAttributeMenu

                        terms <- Array.append terms [| term |]
                        latestInterval <- None
                    elif Regex.IsMatch(termStr, quotedOrEmptyTermRx) then
                        let term =
                            handleQuotedOrEmptyTerm termStr latestInterval corpus.CwbAttributeMenu

                        terms <- Array.append terms [| term |]
                        latestInterval <- None)

            { Terms = terms }

    member this.ToCqp(corpus: Corpus) =
        this.Terms
        |> Array.map (fun term -> term.ToCqp(Query.STag(corpus)))
        |> String.concat " "

///////////////////////////////////////
// Helpers for Elimish update function
///////////////////////////////////////

let updateQuery model query queryIndex newQueryTerms =
    let newQuery = { query with Terms = newQueryTerms }
    let newQueryCqp = newQuery.ToCqp(model.Corpus)

    let newQueries =
        model.Search.Params.Queries
        |> Array.mapi
            (fun i q ->
                if i = queryIndex then
                    { q with QueryString = newQueryCqp }
                else
                    q)

    { model with
          Search =
              { model.Search with
                    Params =
                        { model.Search.Params with
                              Queries = newQueries } } }


let updateQueryTerm model query queryIndex newTerm termIndex =
    let newQueryTerms =
        query.Terms
        |> Array.mapi (fun i t -> if i = termIndex then newTerm else t)

    updateQuery model query queryIndex newQueryTerms
