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


let selectSearchView (corpus: Corpus) (search: Search) dispatch =
    match corpus.Config.SearchEngine with
    | Cwb -> SearchViews.Cwb.view corpus search dispatch
    | Fcs -> SearchViews.Fcs.view corpus search dispatch


module CorpusStartView =
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
                    selectSearchView corpus search dispatch ]


module ResultsView =

    module Concordance =
        let pagination (model: ConcordanceModel) (numPages: int) (dispatch: ShowingResults.Concordance.Msg -> unit) =
            let isFetching = model.PagesBeingFetched.Length > 0

            let setPage (e: Browser.Types.MouseEvent) (pageNo: int) =
                e.preventDefault ()

                // Don't allow switching to a new page while we are in the processing of
                // fetching one or more pages, since the user may start clicking lots of
                // times, generating lots of concurrent requests
                if not isFetching
                   && pageNo >= 1
                   && pageNo <= numPages then
                    dispatch (ShowingResults.Concordance.Msg.SetPaginatorPage pageNo)

            match model.NumResults with
            | Some results when results > uint64 model.SearchParams.PageSize ->
                [ Bulma.levelItem [ Bulma.buttons [ iconButton
                                                        "fa-angle-double-left"
                                                        (model.PaginatorPageNo = 1 || isFetching)
                                                        (fun e -> setPage e 1)
                                                    iconButton
                                                        "fa-angle-left"
                                                        (model.PaginatorPageNo = 1 || isFetching)
                                                        (fun e -> setPage e (model.PaginatorPageNo - 1)) ] ]
                  Bulma.levelItem [ Bulma.input.text [ input.isSmall
                                                       prop.style [ style.width 60
                                                                    style.textAlign.right ]
                                                       prop.value 1
                                                       prop.onChange (fun (s: string) -> printfn $"New value: {s}") ] ]
                  Bulma.levelItem [ Bulma.buttons [ iconButton
                                                        "fa-angle-right"
                                                        (model.PaginatorPageNo = numPages || isFetching)
                                                        (fun e -> setPage e (model.PaginatorPageNo + 1))
                                                    iconButton
                                                        "fa-angle-double-right"
                                                        (model.PaginatorPageNo = numPages || isFetching)
                                                        (fun e -> setPage e numPages) ] ] ]
            | _ -> []


        ////////////////////////////////////////////////////
        /// View.LoadedCorpus.ResultsView.Concordance.view
        ////////////////////////////////////////////////////
        let view (model: ConcordanceModel) (corpus: Corpus) (dispatch: ShowingResults.Concordance.Msg -> unit) =
            let numPages = model.NumResultPages()

            let resultsInfo =
                let text =
                    match model.NumResults with
                    | Some numResults ->
                        if numResults > 0UL then
                            // We have received a non-zero number of results
                            let pagesStr = if numPages = 1 then "page" else "pages"

                            if model.IsSearching then
                                $"Showing {numResults} matches ({numPages} {pagesStr}); searching..."
                            else
                                $"Found {numResults} matches ({numPages} pages)"
                        else
                            // We have received results, but the count was zero
                            "No matches found"
                    | None ->
                        // We have not yet received any results
                        if model.IsSearching then
                            "Searching..."
                        else
                            ""

                // Only show the spinner when we are fetching result pages or when we are searching
                // AND have already found some results so as to avoid showing spinners both here and
                // over the result table at the same time (since we show a spinner over the result
                // table until we have found some results)
                let shouldShowSpnner =
                    model.PagesBeingFetched.Length > 0
                    || (model.IsSearching && model.ResultPages.Count > 0)

                Html.span [ Html.div [ prop.style [ style.width 400
                                                    style.textAlign.right
                                                    style.color "#555" ]
                                       prop.text text ]
                            Html.div [ prop.style [ style.position.absolute
                                                    style.top 10
                                                    style.left 350 ]
                                       prop.children (
                                           spinnerOverlay shouldShowSpnner (Some [ style.width 40; style.height 40 ]) []
                                       ) ] ]

            let contextSelector =
                [ Bulma.levelItem [ prop.text "Context:" ]
                  Bulma.levelItem [ Bulma.input.text [ input.isSmall
                                                       prop.style [ style.width 40 ] ] ]
                  Bulma.levelItem [ prop.style [ style.marginRight 50 ]
                                    prop.text "words" ] ]

            let resultPage =
                model.ResultPages.TryFind(model.ResultPageNo)

            [ Bulma.level [ Bulma.levelLeft [ Bulma.levelItem [ Bulma.buttons [ Bulma.button.button [ prop.text
                                                                                                          "Sort by position" ]
                                                                                Bulma.button.button [ prop.text
                                                                                                          "Download" ] ] ]
                                              Bulma.levelItem resultsInfo ]
                            Bulma.levelRight [ if corpus.Config.Modality <> Spoken then
                                                   yield! contextSelector
                                               yield! pagination model numPages dispatch ] ]
              LoadedCorpus.ResultViews.Cwb.Written.concordanceTable corpus resultPage ]


    let tabs (model: ShowingResultsModel) (dispatch: ShowingResults.Msg -> unit) =
        let activeTab =
            match model.ActiveTab with
            | Concordance _ -> "Concordance"
            | Statistics -> "Statistics"

        Bulma.tabs [ prop.style [ style.marginTop 15 ]
                     tabs.isToggle
                     tabs.isToggleRounded
                     prop.children [ Html.ul [ Html.li [ if activeTab = "Concordance" then
                                                             tab.isActive
                                                         prop.onClick
                                                             (fun _ ->
                                                                 dispatch (
                                                                     ShowingResults.SelectResultTab(
                                                                         Concordance(
                                                                             ConcordanceModel.Init(
                                                                                 model.SearchParams,
                                                                                 model.NumSteps
                                                                             )
                                                                         )
                                                                     )
                                                                 ))
                                                         prop.children [ Html.a [ prop.text "Concordance" ] ] ]
                                               Html.li [ if activeTab = "Statistics" then
                                                             tab.isActive
                                                         prop.onClick
                                                             (fun _ ->
                                                                 dispatch (ShowingResults.SelectResultTab Statistics))
                                                         prop.children [ Html.a [ prop.text "Statistics" ] ] ] ] ] ]

    ///////////////////////////////////////
    /// View.LoadedCorpus.ResultsView.view
    ///////////////////////////////////////
    let view (model: ShowingResultsModel) (corpus: Corpus) (search: Search) parentDispatch dispatch =
        let resultsView =
            [ Bulma.level [ Bulma.levelLeft [ Bulma.levelItem [ tabs model dispatch ] ] ]
              match model.ActiveTab with
              | Concordance concordanceModel ->
                  yield! Concordance.view concordanceModel corpus (ShowingResults.ConcordanceMsg >> dispatch)
              | Statistics -> failwith "NOT IMPLEMENTED" ]

        let shouldShowResultsTableSpinner =
            match model.ActiveTab with
            | Concordance concordanceModel when
                concordanceModel.IsSearching
                && concordanceModel.NumResults.IsNone -> true
            | _ -> false

        Html.span [ topRowButtons
                    selectSearchView corpus search parentDispatch
                    spinnerOverlay shouldShowResultsTableSpinner (Some [ style.top 75 ]) resultsView ]


//////////////////////////////
/// View.LoadedCorpus.view
//////////////////////////////
let view (model: LoadedCorpusModel) (dispatch: Update.LoadedCorpus.Msg -> unit) =
    Html.span [ Bulma.section [ prop.style [ style.paddingTop (length.em 2.5) ]
                                prop.children [ Bulma.columns [ Bulma.column [ column.isNarrow
                                                                               prop.children [ Metadata.MetadataMenu.view
                                                                                                   model
                                                                                                   (MetadataMsg
                                                                                                    >> dispatch) ] ]
                                                                Bulma.column [ match model.Substate with
                                                                               | CorpusStart ->
                                                                                   CorpusStartView.view
                                                                                       model.Corpus
                                                                                       model.Search
                                                                                       dispatch
                                                                               | ShowingResults showingResultsModel ->
                                                                                   ResultsView.view
                                                                                       showingResultsModel
                                                                                       model.Corpus
                                                                                       model.Search
                                                                                       dispatch
                                                                                       (ShowingResultsMsg >> dispatch) ] ] ] ] ]
