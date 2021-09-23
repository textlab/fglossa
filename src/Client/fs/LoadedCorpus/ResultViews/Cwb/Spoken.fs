module View.LoadedCorpus.ResultViews.Cwb.Spoken

open System
open System.Text.RegularExpressions
open Feliz
open Feliz.Bulma
open Shared
open Shared.StringUtils
open Model
open Update.LoadedCorpus.ShowingResults.Concordance
open View.LoadedCorpus.ResultViews.Cwb.Common

[<ReactComponent>]
let MediaPlayerPopup (model: ConcordanceModel) resultRowIndex (dispatch: Msg -> unit) =
    let navigation =
        Bulma.buttons [ Common.iconButton
                            "step-backward"
                            (resultRowIndex <= 0)
                            (fun _ -> dispatch (SetMediaPlayerRowIndex(Some(resultRowIndex - 1))))
                        Common.iconButton
                            "step-forward"
                            (resultRowIndex >= model.SearchParams.PageSize)
                            (fun _ -> dispatch (SetMediaPlayerRowIndex(Some(resultRowIndex + 1)))) ]

    let header =
        Bulma.level [ prop.style [ style.padding 20
                                   style.marginBottom 0 ]
                      prop.children [ Bulma.levelLeft [ Bulma.levelItem [ navigation ] ]
                                      Bulma.levelRight [ Bulma.levelItem [ Bulma.delete [ delete.isMedium
                                                                                          prop.title "Close"
                                                                                          prop.style [ style.marginLeft
                                                                                                           40 ]
                                                                                          prop.onClick
                                                                                              (fun _ ->
                                                                                                  dispatch (
                                                                                                      SetMediaPlayerRowIndex
                                                                                                          None
                                                                                                  )) ] ] ] ] ]

    let mediaDisplay = Bulma.section []

    let footer =
        Bulma.level [ prop.style [ style.padding 20
                                   style.marginBottom 0 ]
                      prop.children [ Bulma.levelLeft []
                                      Bulma.levelRight [ Bulma.levelItem [ navigation ]
                                                         Bulma.levelItem [ Bulma.delete [ delete.isMedium
                                                                                          prop.title "Close"
                                                                                          prop.style [ style.marginLeft
                                                                                                           40 ]
                                                                                          prop.onClick
                                                                                              (fun _ -> printfn "hei") ] ] ] ] ]

    let elementRef = React.useElementRef ()

    let focusPopup () =
        elementRef.current
        |> Option.iter (fun popupElement -> popupElement.focus ())

    // Focus the popup when mounted to enable it to receive keyboard events
    React.useEffectOnce focusPopup

    let popup =
        Html.div [ prop.style [ style.height (length.percent 100)
                                style.top 0
                                style.left 0
                                style.width (length.percent 100)
                                style.position.absolute
                                style.zIndex 40
                                style.backgroundColor "white"
                                style.overflowX.hidden
                                style.border (1, borderStyle.solid, "black")
                                style.transitionProperty transitionProperty.height
                                style.transitionDuration (System.TimeSpan(3500000L))
                                style.transitionTimingFunction.easeOut ]
                   // Set elementRef in order to apply the focusPopup() function to this element
                   prop.ref elementRef
                   // Set tabIndex so that the lement receives keyboard events
                   prop.tabIndex 0
                   prop.onKeyUp (fun e -> if e.key = "Escape" then printfn "hei")
                   prop.children [ header
                                   mediaDisplay
                                   footer ] ]


    let root =
        Browser.Dom.document.getElementById ("media-player-popup-root")

    ReactDOM.createPortal (popup, root)

let concordanceTable
    (model: ConcordanceModel)
    (corpus: Corpus)
    (pageResults: SearchResult [] option)
    (dispatch: Msg -> unit)
    =
    let mediaPlayer =
        match model.ResultRowInMediaPlayer with
        | Some rowIndex -> MediaPlayerPopup model rowIndex dispatch
        | None -> Html.none

    let extractFields result =
        let m =
            Regex.Match(result, "^<who_name\s+(\S*?)>:\s+(.*)\{\{(.+?)\}\}(.*?)$")

        let groupValues =
            m.Groups
            |> Seq.map (fun group -> group.Value)
            |> Seq.toList

        let sId = groupValues.[1]

        let pre =
            groupValues.[2]
            // If the result begins with a who_name tag with the same ID as the one for the
            // actual match, it feels redundant (since that speaker ID is listed just
            // to the left of it), so just remove it.
            |> replace $"^<who_name\s+{sId}>" ""

        let searchWord =
            groupValues.[3]
            |> fun m ->
                // Do the same with the match if there is no left context
                if String.IsNullOrWhiteSpace(pre) then
                    Regex.Replace(m, $"^<who_name\s+{sId}>", "")
                else
                    m

        let post = groupValues.[4]

        (sId, pre, searchWord, post)


    // Returns one or more rows representing a single search result
    let singleResultRows
        ortIndex
        maybePhhonIndex
        maybeLemmaIndex
        ortTipIndexes
        phonTipIndexes
        (searchResult: SearchResult)
        index
        =
        []

    let attributes =
        match corpus.Config.LanguageConfig with
        | Monolingual (Some attrs) -> attrs
        | Monolingual None -> []
        | Multilingual languages -> failwith "NOT IMPLEMENTED!"

    let ortIndex = 0

    // We need to increment phonIndex and lemmaIndex, as well as the ranges of indexes below,
    // since the first attribute ('word') is not in the list because it is shown by
    // default by CQP
    let maybePhonIndex =
        attributes
        |> List.tryFindIndex (fun attr -> attr.Code = "phon")
        |> Option.map (fun i -> i + 1)

    let maybeLemmaIndex =
        attributes
        |> List.tryFindIndex (fun attr -> attr.Code = "lemma")
        |> Option.map (fun i -> i + 1)

    let ortTipIndexes =
        [ 0 .. attributes.Length ]
        |> List.except [ ortIndex ]

    let phonTipIndexes =
        match maybePhonIndex with
        | Some phonIndex ->
            [ 0 .. attributes.Length ]
            |> List.except [ phonIndex ]
        | None -> [ 0 .. attributes.Length ]

    let rows =
        match pageResults with
        | Some results ->
            results
            |> Array.toList
            |> List.indexed
            |> List.collect
                (fun (index, result) ->
                    singleResultRows ortIndex maybePhonIndex maybeLemmaIndex ortTipIndexes phonTipIndexes result index)
        | None -> []


    Html.span [ mediaPlayer
                Bulma.table [ table.isStriped
                              table.isFullWidth
                              table.isBordered
                              table.isNarrow
                              prop.className "concordance-table"
                              prop.children [ Html.tbody rows ] ] ]
