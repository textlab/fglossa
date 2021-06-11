module LoadedCorpus.View

open Elmish
open Feliz
open Feliz.Bulma
open Shared
open Model
open LoadedCorpus.Update
open Common

let topRowButtons =
    Bulma.buttons [ Bulma.button.button [ prop.text "Hide filters" ]
                    Bulma.button.button [ color.isInfo
                                          prop.text "Reset form" ] ]


let searchInterface (search: Search) dispatch =
    let simpleHeading = "Simple"
    let extendedHeading = "Extended"
    let cqpHeading = "CQP query"

    let link title (heading: string) ``interface`` =
        Html.a [ prop.href "#"
                 prop.title title
                 prop.onClick (fun _ -> dispatch (SetSearchInterface ``interface``))
                 prop.text heading ]

    let simpleLink = link "Simple search box" "Simple" Simple

    let extendedLink =
        link "Search for grammatical categories etc." "Extended" Extended

    let cqpLink = link "CQP expressions" "CQP query" Cqp

    let separator =
        Html.span [ prop.style [ style.whitespace.pre ]
                    prop.text " | " ]

    let links =
        match search.Interface with
        | Simple ->
            [ Html.b simpleHeading
              separator
              extendedLink
              separator
              cqpLink ]
        | Extended ->
            [ simpleLink
              separator
              Html.b extendedHeading
              separator
              cqpLink ]
        | Cqp ->
            [ simpleLink
              separator
              extendedLink
              separator
              Html.b cqpHeading ]

    Html.div [ prop.style [ style.width 500 ]
               prop.children [ Bulma.level [ prop.style [ style.paddingTop 20 ]
                                             prop.children [ Bulma.levelLeft [ Bulma.levelItem links ]
                                                             Bulma.levelRight [ Bulma.button.button [ color.isSuccess
                                                                                                      prop.text "Search"
                                                                                                      prop.onClick
                                                                                                          (fun _ ->
                                                                                                              dispatch
                                                                                                                  Search) ] ] ] ]
                               Bulma.field.div [ Bulma.control.div [ Bulma.input.search [] ] ]
                               Bulma.field.div [ Bulma.control.div [ Bulma.button.button "Or..." ] ] ] ]


module CorpusStartPage =
    let corpusNameBox config =
        let logo =
            match config.Logo with
            | Some logo -> Html.img [ prop.src $"corpora/{config.Code}/{logo}" ]
            | None -> Html.none

        Bulma.box [ prop.style [ style.padding 20 ]
                    prop.children [ Bulma.level [ Bulma.levelLeft [ Bulma.levelItem [ Bulma.title config.Name ]
                                                                    Bulma.levelItem logo ] ] ] ]

    let view (corpus: Corpus) (search: Search) (dispatch: Update.Msg -> unit) =
        Html.span [ topRowButtons
                    corpusNameBox corpus.Config
                    searchInterface search dispatch ]


module ResultsPage =
    let tabs (model: ShowingResultsModel) dispatch =
        Bulma.tabs [ prop.style [ style.marginTop 15 ]
                     tabs.isToggle
                     tabs.isToggleRounded
                     prop.children [ Html.ul [ Html.li [ if model.ActiveTab = Concordance then
                                                             tab.isActive
                                                         prop.onClick (fun _ -> dispatch (SelectResultTab Concordance))
                                                         prop.children [ Html.a [ prop.text "Concordance" ] ] ]
                                               Html.li [ if model.ActiveTab = Statistics then
                                                             tab.isActive
                                                         prop.onClick (fun _ -> dispatch (SelectResultTab Statistics))
                                                         prop.children [ Html.a [ prop.text "Statistics" ] ] ] ] ] ]


    let view
        (model: ShowingResultsModel)
        (corpus: Corpus)
        (search: Search)
        (parentDispatch: Update.Msg -> unit)
        (dispatch: Update.ShowingResultsMsg -> unit)
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
              | Some results -> ResultViews.Cwb.Written.concordanceTable corpus results
              | None -> Html.none ]

        let shouldShowResultsTableSpinner =
            model.IsSearching && model.SearchResults.IsNone

        Html.span [ topRowButtons
                    searchInterface search parentDispatch
                    spinnerOverlay shouldShowResultsTableSpinner (Some [ style.top 75 ]) resultsTable ]


let view (model: LoadedCorpusModel) (dispatch: LoadedCorpus.Update.Msg -> unit) =
    Html.span [ Bulma.section [ prop.style [ style.paddingTop (length.em 2.5) ]
                                prop.children [ Bulma.columns [ Bulma.column [ column.isNarrow
                                                                               prop.children [ Metadata.View.MetadataMenu.view
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
