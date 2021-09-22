module View.LoadedCorpus.ResultViews.Cwb.Spoken

open Feliz
open Feliz.Bulma
open Shared
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

let concordanceTable (model: ConcordanceModel) (pageResults: SearchResult [] option) (dispatch: Msg -> unit) =
    let mediaPlayer =
        match model.ResultRowInMediaPlayer with
        | Some rowIndex -> MediaPlayerPopup model rowIndex dispatch
        | None -> Html.none

    let singleResultRows wordIndex maybeOrigCorrIndex maybeLemmaIndex (searchResult: SearchResult) index = []

    // let rows =
    //     match pageResults with
    //     | Some results ->
    //         results
    //         |> Array.toList
    //         |> List.indexed
    //         |> List.collect
    //             (fun (index, result) -> singleResultRows wordIndex maybeOrigIndex maybeLemmaIndex result index)
    //     | None -> []
    let rows = []


    Html.span [ mediaPlayer
                Bulma.table [ table.isStriped
                              table.isFullWidth
                              table.isBordered
                              table.isNarrow
                              prop.className "concordance-table"
                              prop.children [ Html.tbody rows ] ] ]
