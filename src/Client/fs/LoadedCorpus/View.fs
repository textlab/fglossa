module View.LoadedCorpus

open Elmish
open Feliz
open Feliz.Bulma
open Shared
open Model
open Update.LoadedCorpus
open Common

let topRowButtons =
    Bulma.buttons [ Bulma.button.button [ prop.text "Hide filters" ]
                    Bulma.button.button [ color.isInfo
                                          prop.text "Reset form" ] ]


module CorpusStartPage =
    let corpusNameBox config =
        let logo =
            match config.Logo with
            | Some logo -> Html.img [ prop.src $"corpora/{config.Code}/{logo}" ]
            | None -> Html.none

        Bulma.box [ prop.style [ style.padding 20 ]
                    prop.children [ Bulma.level [ Bulma.levelLeft [ Bulma.levelItem [ Bulma.title config.Name ]
                                                                    Bulma.levelItem logo ] ] ] ]

    let view (corpus: Corpus) (search: Search) (dispatch: Update.LoadedCorpus.Msg -> unit) =
        Html.span [ topRowButtons
                    corpusNameBox corpus.Config
                    View.SearchInterface.view search dispatch ]


module ResultsPage =
    let tabs (model: ShowingResultsModel) (dispatch: ShowingResults.Msg -> unit) =
        Bulma.tabs [ prop.style [ style.marginTop 15 ]
                     tabs.isToggle
                     tabs.isToggleRounded
                     prop.children [ Html.ul [ Html.li [ if model.ActiveTab = Concordance then
                                                             tab.isActive
                                                         prop.onClick
                                                             (fun _ ->
                                                                 dispatch (ShowingResults.SelectResultTab Concordance))
                                                         prop.children [ Html.a [ prop.text "Concordance" ] ] ]
                                               Html.li [ if model.ActiveTab = Statistics then
                                                             tab.isActive
                                                         prop.onClick
                                                             (fun _ ->
                                                                 dispatch (ShowingResults.SelectResultTab Statistics))
                                                         prop.children [ Html.a [ prop.text "Statistics" ] ] ] ] ] ]


    let view
        (model: ShowingResultsModel)
        (corpus: Corpus)
        (search: Search)
        (parentDispatch: Update.LoadedCorpus.Msg -> unit)
        (dispatch: Update.LoadedCorpus.ShowingResults.Msg -> unit)
        =
        let contextSelector =
            [ Bulma.levelItem [ prop.text "Context:" ]
              Bulma.levelItem [ Bulma.input.text [ input.isSmall
                                                   prop.style [ style.width 40 ] ] ]
              Bulma.levelItem [ prop.style [ style.marginRight 50 ]
                                prop.text "words" ] ]

        let resultsTable =
            [ Bulma.level [ Bulma.levelLeft [ Bulma.levelItem [ tabs model dispatch ] ]
                            Bulma.levelRight [ Bulma.levelItem [ prop.text "Found xxx matches (xxx pages)" ] ] ]
              Bulma.level [ Bulma.levelLeft [ Bulma.levelItem [ Bulma.buttons [ Bulma.button.button [ prop.text
                                                                                                          "Sort by position" ]
                                                                                Bulma.button.button [ prop.text
                                                                                                          "Download" ] ] ] ]
                            Bulma.levelRight [ if corpus.Config.Modality <> Spoken then
                                                   yield! contextSelector
                                               Bulma.levelItem [ Bulma.buttons [ iconButton "fa-angle-double-left"
                                                                                 iconButton "fa-angle-left" ] ]
                                               Bulma.levelItem [ Bulma.input.text [ input.isSmall
                                                                                    prop.style [ style.width 60
                                                                                                 style.textAlign.right ]
                                                                                    prop.value 1
                                                                                    prop.onChange
                                                                                        (fun (s: string) ->
                                                                                            printfn $"New value: {s}") ] ]
                                               Bulma.levelItem [ Bulma.buttons [ iconButton "fa-angle-right"
                                                                                 iconButton "fa-angle-double-right" ] ] ] ]
              match model.SearchResults with
              | Some results -> LoadedCorpus.ResultViews.Cwb.Written.concordanceTable corpus results
              | None -> Html.none ]

        let shouldShowResultsTableSpinner =
            model.IsSearching && model.SearchResults.IsNone

        Html.span [ topRowButtons
                    SearchInterface.view search parentDispatch
                    spinnerOverlay shouldShowResultsTableSpinner (Some [ style.top 75 ]) resultsTable ]


let view (model: LoadedCorpusModel) (dispatch: Update.LoadedCorpus.Msg -> unit) =
    Html.span [ Bulma.section [ prop.style [ style.paddingTop (length.em 2.5) ]
                                prop.children [ Bulma.columns [ Bulma.column [ column.isNarrow
                                                                               prop.children [ Metadata.MetadataMenu.view
                                                                                                   model
                                                                                                   (MetadataMsg
                                                                                                    >> dispatch) ] ]
                                                                Bulma.column [ match model.Substate with
                                                                               | CorpusStartPage ->
                                                                                   CorpusStartPage.view
                                                                                       model.Corpus
                                                                                       model.Search
                                                                                       dispatch
                                                                               | ShowingResults showingResultsModel ->
                                                                                   ResultsPage.view
                                                                                       showingResultsModel
                                                                                       model.Corpus
                                                                                       model.Search
                                                                                       dispatch
                                                                                       (dispatch << ShowingResultsMsg) ] ] ] ] ]
