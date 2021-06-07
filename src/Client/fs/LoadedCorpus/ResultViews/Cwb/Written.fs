module LoadedCorpus.ResultViews.Cwb.Written

open System.Text.RegularExpressions
open Feliz
open Feliz.Bulma
open Shared
open LoadedCorpus.ResultViews.Cwb.Common

/// Processes a pre-match, match, or post-match field.
let private processField displayedFieldIndex maybeOrigCorrIndex maybeLemmaIndex maybeFontFamily (field: string) =
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
                            prop.dangerouslySetInnerHTML attributes.[displayedFieldIndex] ]
            else
                // With multi-word expressions, the non-last parts become simple strings
                // without any attributes (i.e., no slashes) when we split the text on
                // whitespace. Just print out those non-last parts and leave the tooltip
                // to be attached to the last part.
                Html.span [ prop.key index
                            prop.text $"{token} " ])


let private nonFirstMultilingual maybeOrigCorrIndex maybeLemmaIndex (index: int) maybeFontFamily line =
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
                "^<s_id\s+(.*?)>:\s+(.*)\{\{(.+?)\}\}(.*?)$"
            )
            .Groups

    let sId = groups.[1].Value
    let pre = groups.[2].Value
    let searchWord = groups.[3].Value
    let post = groups.[4].Value

    (sId.Trim(), [ pre; searchWord; post ])


let private mainRow (resultInfo: ResultAttributeInfo) index =
    Html.tr [ Html.td [ prop.style [ style.textAlign.center
                                     style.verticalAlign.middle ]
                        prop.children [ idColumn resultInfo index ] ] ]


let concordanceTable (searchResults: SearchResults) =
    Bulma.table [ table.isStriped
                  table.isFullWidth
                  prop.children [ Html.tbody [ for result in searchResults.Results -> Html.tr [ Html.td result.Text ] ] ] ]
