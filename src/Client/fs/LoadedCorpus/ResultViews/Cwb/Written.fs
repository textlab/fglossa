module LoadedCorpus.ResultViews.Cwb.Written

open System.Text.RegularExpressions
open Feliz
open Feliz.Bulma
open Shared
open LoadedCorpus.ResultViews.Cwb.Common

/// Processes a pre-match, match, or post-match field.
let private processField displayedAttributeIndex maybeOrigCorrIndex maybeLemmaIndex maybeFontFamily (field: string) =
    field.Split("\s+")
    |> Array.mapi
        (fun index token ->
            if token.Contains('/') then
                let attributes =
                    token.Split('/')
                    |> fun attrs ->
                        // Show the corrected or original
                        // form in italics, if present
                        match maybeOrigCorrIndex with
                        | Some origCorrIndex ->
                            attrs
                            |> Array.mapi
                                (fun attrIndex attr ->
                                    if attrIndex = origCorrIndex then
                                        $"<i>{attr}</i>"
                                    else
                                        attr)
                        | None -> attrs
                    |> fun attrs ->
                        // Show the lemma in quotes, if
                        // present
                        match maybeLemmaIndex with
                        | Some lemmaIndex ->
                            attrs
                            |> Array.mapi
                                (fun attrIndex attr ->
                                    if attrIndex = lemmaIndex then
                                        $"\"{attr}\""
                                    else
                                        attr)
                        | None -> attrs

                let tipText =
                    attributes
                    |> Array.filter
                        (fun attr ->
                            not (
                                [ "__UNDEF__"
                                  "\"__UNDEF__\""
                                  "-"
                                  "_"
                                  "\"_\""
                                  "<i>_</i>" ]
                                |> List.contains attr
                            ))
                    |> String.concat " "

                Html.span [ prop.key index
                            prop.title tipText
                            match maybeFontFamily with
                            | Some fontFamily -> prop.style [ style.fontFamily fontFamily ]
                            | None -> ignore None
                            prop.dangerouslySetInnerHTML attributes.[displayedAttributeIndex] ]
            else
                // With multi-word expressions, the non-last parts become simple strings
                // without any attributes (i.e., no slashes) when we split the text on
                // whitespace. Just print out those non-last parts and leave the tooltip
                // to be attached to the last part.
                Html.span [ prop.key index
                            prop.text $"{token} " ])


let private nonFirstMultilingualRow maybeOrigCorrIndex maybeLemmaIndex (index: int) maybeFontFamily line =
    // Extract the IDs of all s-units (typically sentences) and put them in front of their respective s-units.
    let matches =
        Regex.Matches(line, "<(\w+_id)\s*(.+?)>(.*?)</\1>")

    let components =
        if matches.Count > 0 then
            matches
            |> Seq.map (fun m -> Html.span $"{m.Groups.[2]}: {m.Groups.[3]}")
            |> Seq.toArray
        else
            processField 0 maybeOrigCorrIndex maybeLemmaIndex maybeFontFamily line

    Html.tr [ prop.key index
              prop.children [ Html.td []
                              Html.td [ prop.colSpan 3
                                        prop.children components ] ] ]


let private extractFields resultText =
    let groups =
        Regex
            .Match(
                resultText,
                "<s_id\s+(.*?)>:\s+(.*)\{\{(.+?)\}\}(.*?)$"
            )
            .Groups

    let sId = groups.[1].Value
    let pre = groups.[2].Value
    let searchWord = groups.[3].Value
    let post = groups.[4].Value

    (sId, pre, searchWord, post)


let private mainRow (resultLineFields: ResultLineFields) index =
    Html.tr [ Html.td [ prop.style [ style.textAlign.center
                                     style.verticalAlign.middle ]
                        prop.children [ idColumn resultLineFields.SId index ] ]
              yield! textColumns resultLineFields ]


let private originalRow (resultLineFields: ResultLineFields) index =
    Html.tr [ prop.key $"orig{index}"
              prop.children [ Html.td [ prop.style [ style.verticalAlign.middle ] ]
                              yield! textColumns resultLineFields ] ]


/// Returns one or more table rows representing a single search result
let private singleResultRows
    wordIndex
    maybeOrigCorrIndex
    maybeLemmaIndex
    maybeFontFamily
    (searchResult: SearchResult)
    index
    =
    match searchResult.Text with
    // Only one line per search result
    | mainLine :: otherLines when otherLines.IsEmpty ->
        let sId, pre, searchWord, post = extractFields mainLine

        let processedWordFields =
            { SId = sId
              PreMatch = processField wordIndex maybeOrigCorrIndex maybeLemmaIndex maybeFontFamily pre
              SearchWord = processField wordIndex maybeOrigCorrIndex maybeLemmaIndex maybeFontFamily searchWord
              PostMatch = processField wordIndex maybeOrigCorrIndex maybeLemmaIndex maybeFontFamily post }

        let wordRow = mainRow processedWordFields index

        match maybeOrigCorrIndex with
        | Some origCorrIndex ->
            let processedOrigFields =
                { SId = sId
                  PreMatch = processField origCorrIndex (Some wordIndex) maybeLemmaIndex maybeFontFamily pre
                  SearchWord = processField origCorrIndex (Some wordIndex) maybeLemmaIndex maybeFontFamily searchWord
                  PostMatch = processField origCorrIndex (Some wordIndex) maybeLemmaIndex maybeFontFamily post }

            [ wordRow
              originalRow processedOrigFields index
              separatorRow index ]
        | None -> [ wordRow ]

    // Multiple lines per search result
    | mainLine :: otherLines ->
        let sId, pre, searchWord, post = extractFields mainLine

        let processedWordFields =
            { SId = sId
              PreMatch = processField wordIndex maybeOrigCorrIndex maybeLemmaIndex maybeFontFamily pre
              SearchWord = processField wordIndex maybeOrigCorrIndex maybeLemmaIndex maybeFontFamily searchWord
              PostMatch = processField wordIndex maybeOrigCorrIndex maybeLemmaIndex maybeFontFamily post }

        let mainTableRow = mainRow processedWordFields index

        let otherTableRows =
            otherLines
            |> List.map (nonFirstMultilingualRow maybeOrigCorrIndex maybeLemmaIndex index maybeFontFamily)

        [ mainTableRow ] @ otherTableRows

    | [] -> []


let concordanceTable (maybeFontFamily: string option) (searchResults: SearchResults) =
    let rows =
        searchResults.Results
        |> Array.toList
        |> List.indexed
        |> List.collect (fun (index, result) -> singleResultRows 0 None None maybeFontFamily result index)

    Bulma.table [ table.isStriped
                  table.isFullWidth
                  table.isBordered
                  table.isNarrow
                  prop.children [ Html.tbody rows ] ]
