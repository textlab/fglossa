module View.LoadedCorpus.ResultViews.Cwb.Written

open System.Text.RegularExpressions
open Feliz
open Feliz.Bulma
open Shared
open Model
open Update.LoadedCorpus.ShowingResults.Concordance
open View.LoadedCorpus.ResultViews.Cwb.Common

let concordanceTable (corpus: Corpus) (pageResults: SearchResult [] option) (dispatch: Msg -> unit) =

    /// Processes a pre-match, match, or post-match field.
    let processField displayedAttributeIndex maybeOrigCorrIndex maybeLemmaIndex (field: string) =
        Regex.Split(field, "\s+")
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
                                tooltip.text tipText
                                prop.className "has-tooltip-arrow"
                                match corpus.Config.FontFamily with
                                | Some fontFamily -> prop.style [ style.fontFamily fontFamily ]
                                | None -> ignore None
                                prop.dangerouslySetInnerHTML (attributes.[displayedAttributeIndex] + " ") ]
                else
                    // With multi-word expressions, the non-last parts become simple strings
                    // without any attributes (i.e., no slashes) when we split the text on
                    // whitespace. Just print out those non-last parts and leave the tooltip
                    // to be attached to the last part.
                    Html.span [ prop.key index
                                prop.text $"{token} " ])


    let nonFirstMultilingualRow maybeOrigCorrIndex maybeLemmaIndex (index: int) line =
        // Extract the IDs of all s-units (typically sentences) and put them in front of their respective s-units.
        let matches =
            Regex.Matches(line, "<(\w+_id)\s*(.+?)>(.*?)</\1>")

        let components =
            if matches.Count > 0 then
                matches
                |> Seq.map (fun m -> Html.span $"{m.Groups.[2]}: {m.Groups.[3]}")
                |> Seq.toArray
            else
                processField 0 maybeOrigCorrIndex maybeLemmaIndex line

        Html.tr [ prop.key index
                  prop.children [ Html.td []
                                  Html.td [ prop.colSpan 3
                                            prop.children components ] ] ]


    let extractFields resultText =
        let groups =
            Regex
                .Match(
                    resultText,
                    "^<s_id\s+(.*?)>:\s+(.*)\{\{(.+?)\}\}(.*?)$"
                )
                .Groups

        let sId = groups.[1].Value
        let pre = groups.[2].Value
        let searchWord = groups.[3].Value
        let post = groups.[4].Value

        (sId, pre, searchWord, post)


    let mainRow (resultLineFields: ResultLineFields) index =
        Html.tr [ Html.td [ prop.style [ style.textAlign.center
                                         style.verticalAlign.middle ]
                            prop.children [ idColumn resultLineFields.SId index dispatch ] ]
                  yield! textColumns resultLineFields ]


    let originalRow (resultLineFields: ResultLineFields) index =
        Html.tr [ prop.key $"orig{index}"
                  prop.children [ Html.td [ prop.style [ style.verticalAlign.middle ] ]
                                  yield! textColumns resultLineFields ] ]


    /// Returns one or more table rows representing a single search result
    let singleResultRows wordIndex maybeOrigCorrIndex maybeLemmaIndex (searchResult: SearchResult) index =
        match searchResult.Text with
        // Only one line per search result
        | mainLine :: otherLines when otherLines.IsEmpty ->
            let sId, pre, searchWord, post = extractFields mainLine

            let processedWordFields =
                { SId = sId
                  PreMatch = processField wordIndex maybeOrigCorrIndex maybeLemmaIndex pre
                  SearchWord = processField wordIndex maybeOrigCorrIndex maybeLemmaIndex searchWord
                  PostMatch = processField wordIndex maybeOrigCorrIndex maybeLemmaIndex post }

            let wordRow = mainRow processedWordFields index

            match maybeOrigCorrIndex with
            | Some origCorrIndex ->
                let processedOrigFields =
                    { SId = sId
                      PreMatch = processField origCorrIndex (Some wordIndex) maybeLemmaIndex pre
                      SearchWord = processField origCorrIndex (Some wordIndex) maybeLemmaIndex searchWord
                      PostMatch = processField origCorrIndex (Some wordIndex) maybeLemmaIndex post }

                [ wordRow
                  originalRow processedOrigFields index
                  separatorRow index ]
            | None -> [ wordRow ]

        // Multiple lines per search result
        | mainLine :: otherLines ->
            let sId, pre, searchWord, post = extractFields mainLine

            let processedWordFields =
                { SId = sId
                  PreMatch = processField wordIndex maybeOrigCorrIndex maybeLemmaIndex pre
                  SearchWord = processField wordIndex maybeOrigCorrIndex maybeLemmaIndex searchWord
                  PostMatch = processField wordIndex maybeOrigCorrIndex maybeLemmaIndex post }

            let mainTableRow = mainRow processedWordFields index

            let otherTableRows =
                otherLines
                |> List.map (nonFirstMultilingualRow maybeOrigCorrIndex maybeLemmaIndex index)

            mainTableRow :: otherTableRows

        | [] -> []


    let wordIndex = 0 // word form is always the first attribute

    // We need to increment lemmaIndex and origIndex since the first attribute ('word') is
    // not in the list because it is shown by default by CQP
    let maybeLemmaIndex, maybeOrigIndex =
        match corpus.Config.LanguageConfig with
        | Monolingual maybeLangAttributes ->
            match maybeLangAttributes with
            | Some attributes ->
                let li =
                    attributes
                    |> List.tryFindIndex (fun attr -> attr.Code = "lemma")
                    |> Option.map (fun i -> i + 1)

                let oi =
                    attributes
                    |> List.tryFindIndex (fun attr -> attr.Code = "orig")
                    |> Option.map (fun i -> i + 1)

                (li, oi)
            | None -> (None, None)
        | Multilingual _ -> failwith "NOT IMPLEMENTED"

    let rows =
        match pageResults with
        | Some results ->
            results
            |> Array.toList
            |> List.indexed
            |> List.collect
                (fun (index, result) -> singleResultRows wordIndex maybeOrigIndex maybeLemmaIndex result index)
        | None -> []

    Bulma.table [ table.isStriped
                  table.isFullWidth
                  table.isBordered
                  table.isNarrow
                  prop.className "concordance-table"
                  prop.children [ Html.tbody rows ] ]
