module View.LoadedCorpus

open System
open Elmish
open Feliz
open Feliz.Bulma
open Shared
open Model
open Update.LoadedCorpus
open Common

let shouldShowMetadataMenu model =
    if model.Corpus.MetadataMenu.IsEmpty then
        // Don't show metadata if the corpus doesn't have any (duh!)
        false
    else
        match model.ShouldShowMetadataMenu with
        | Some shouldShow ->
            // If ShouldShowMetadata is a Some, the user has explicitly chosen whether to see metadata,
            // so we respect that unconditionally
            shouldShow
        | None ->
            // Now we know that we have metadata, and that the user has not explicitly chosen
            // whether to see them. If we are showing search results, we hide the metadata if the
            // window is narrow; if instead we are showing the start page, we show the metadata
            // regardless of window size.
            match model.Substate with
            | CorpusStart -> true
            | ShowingResults _ -> not model.IsNarrowWindow

let topRowButtons (model: LoadedCorpusModel) dispatch =
    let filterButton =
        if shouldShowMetadataMenu model then
            Bulma.button.button [ prop.onClick (fun _ -> dispatch (SetShouldShowMetadataMenu false))
                                  prop.text "Hide filters" ]
        elif not model.Corpus.MetadataMenu.IsEmpty then
            Bulma.button.button [ prop.onClick (fun _ -> dispatch (SetShouldShowMetadataMenu true))
                                  prop.text "Filters" ]
        // Don't show the filter button if the corpus doesn't have any metadata
        else
            Html.none

    Bulma.buttons [ filterButton
                    Bulma.button.button [ color.isInfo
                                          prop.onClick (fun _ -> dispatch ResetForm)
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

    let corpusInfo (config: CorpusConfig) =
        match config.Info with
        | Some info ->
            Html.div [ prop.style [ style.marginTop 30
                                    style.marginBottom 10 ]
                       prop.dangerouslySetInnerHTML info ]
        | None -> Html.none

    let view (corpus: Corpus) (search: Search) topRowButtonsElement (dispatch: Update.LoadedCorpus.Msg -> unit) =
        Html.span [ topRowButtonsElement
                    corpusNameBox corpus.Config
                    selectSearchView corpus search dispatch
                    corpusInfo corpus.Config ]


module ResultsView =

    module Concordance =
        [<ReactComponent>]
        let PaginationTextInput
            (
                textValue: string,
                isDisabled: bool,
                dispatch: ShowingResults.Concordance.Msg -> unit
            ) : ReactElement =
            let inputRef = React.useInputRef ()

            let selectTextInput () =
                inputRef.current
                |> Option.iter (fun inputElement -> inputElement.select ())

            Bulma.input.text [ input.isSmall
                               prop.ref inputRef
                               prop.disabled isDisabled
                               prop.style [ style.width 60
                                            style.textAlign.right ]
                               prop.value textValue
                               prop.onClick (fun _ -> selectTextInput ())
                               prop.onChange (
                                   ShowingResults.Concordance.Msg.SetPaginatorTextValue
                                   >> dispatch
                               )
                               prop.onKeyUp (
                                   key.enter,
                                   (fun _ -> dispatch (ShowingResults.Concordance.Msg.SetPaginatorPage(None, None)))
                               ) ]

        let pagination
            (model: ConcordanceModel)
            (isSearchingOrFetching: bool)
            (numPages: int)
            (dispatch: ShowingResults.Concordance.Msg -> unit)
            =
            let setPage (e: Browser.Types.MouseEvent) (pageNo: int) =
                e.preventDefault ()

                // Don't allow switching to a new page while we are in the process of
                // fetching one or more pages, since the user may start clicking lots of
                // times, generating lots of concurrent requests
                if not isSearchingOrFetching
                   && pageNo >= 1
                   && pageNo <= numPages then
                    dispatch (ShowingResults.Concordance.Msg.SetPaginatorPage(Some pageNo, None))

            match model.NumResults with
            | Some results when results > uint64 model.SearchParams.PageSize ->
                [ Bulma.levelItem [ Bulma.buttons [ iconButton
                                                        "fa-angle-double-left"
                                                        (model.PaginatorPageNo = 1 || isSearchingOrFetching)
                                                        (fun e -> setPage e 1)
                                                    iconButton
                                                        "fa-angle-left"
                                                        (model.PaginatorPageNo = 1 || isSearchingOrFetching)
                                                        (fun e -> setPage e (model.PaginatorPageNo - 1)) ] ]
                  Bulma.levelItem [ PaginationTextInput(model.PaginatorTextValue, isSearchingOrFetching, dispatch) ]
                  Bulma.levelItem [ Bulma.buttons [ iconButton
                                                        "fa-angle-right"
                                                        (model.PaginatorPageNo = numPages
                                                         || isSearchingOrFetching)
                                                        (fun e -> setPage e (model.PaginatorPageNo + 1))
                                                    iconButton
                                                        "fa-angle-double-right"
                                                        (model.PaginatorPageNo = numPages
                                                         || isSearchingOrFetching)
                                                        (fun e -> setPage e numPages) ] ] ]
            | _ -> []

        [<ReactComponent>]
        let ContextSizeTextInput
            (
                textValue: string,
                isDisabled: bool,
                dispatch: ShowingResults.Concordance.Msg -> unit
            ) : ReactElement =
            let inputRef = React.useInputRef ()

            let selectTextInput () =
                inputRef.current
                |> Option.iter (fun inputElement -> inputElement.select ())

            Bulma.input.text [ input.isSmall
                               prop.ref inputRef
                               prop.disabled isDisabled
                               prop.style [ style.width 40
                                            style.textAlign.right ]
                               prop.value textValue
                               prop.onClick (fun _ -> selectTextInput ())
                               prop.onChange (
                                   ShowingResults.Concordance.Msg.SetContextSizeTextValue
                                   >> dispatch
                               )
                               prop.onKeyUp (
                                   key.enter,
                                   (fun _ ->
                                       match Int32.TryParse(textValue) with
                                       | (true, size) -> dispatch (ShowingResults.Concordance.Msg.SetContextSize size)
                                       | (false, _) -> ignore None)
                               ) ]

        [<ReactComponent>]
        let MetadataQuickView (model: ConcordanceModel) dispatch =
            let elementRef = React.useElementRef ()

            let focusQuickView () =
                elementRef.current
                |> Option.iter
                    (fun quickViewElement ->
                        if model.ShouldShowQuickView then
                            quickViewElement.focus ())

            // Focus the QuickView when mounted to enable it to receive keyboard events
            React.useEffect (focusQuickView, [| box model |])

            QuickView.quickview [ if model.ShouldShowQuickView then
                                      quickview.isActive
                                  // Set elementRef in order to apply the focusQuickView() function to this element
                                  prop.ref elementRef
                                  // Set tabIndex so that the lement receives keyboard events
                                  prop.tabIndex 0
                                  prop.onKeyUp
                                      (fun e ->
                                          if e.key = "Escape" then
                                              dispatch ShowingResults.Concordance.CloseQuickView)
                                  prop.children [ QuickView.header [ Html.div [ prop.style [ style.fontSize 16
                                                                                             style.fontWeight.bold ]
                                                                                prop.text "Metadata" ]
                                                                     Bulma.delete [ prop.onClick
                                                                                        (fun _ ->
                                                                                            dispatch
                                                                                                ShowingResults.Concordance.CloseQuickView) ] ]
                                                  QuickView.body [ QuickView.block [ Bulma.table [ prop.style [ style.margin
                                                                                                                    5 ]
                                                                                                   prop.children [ Html.tbody [ for category in
                                                                                                                                    model.QuickViewMetadata ->
                                                                                                                                    Html.tr [ Html.td (
                                                                                                                                                  category.Name
                                                                                                                                                  + ":"
                                                                                                                                              )
                                                                                                                                              Html.td
                                                                                                                                                  category.Value ] ] ] ] ] ] ] ]



        ////////////////////////////////////////////////////
        /// View.LoadedCorpus.ResultsView.Concordance.view
        ////////////////////////////////////////////////////
        let view (model: ConcordanceModel) (corpus: Corpus) (dispatch: ShowingResults.Concordance.Msg -> unit) =
            let numPages = model.NumResultPages()

            let isSearchingOrFetching =
                model.PagesBeingFetched.Length > 0
                || model.IsSearching

            let sortMenu =
                Bulma.select [ prop.disabled isSearchingOrFetching
                               prop.onChange
                                   (fun (s: string) ->
                                       let sortKey = SortKey.OfString(s)

                                       dispatch (ShowingResults.Concordance.SetPaginatorPage(Some 1, Some sortKey)))
                               prop.children [ Html.option [ prop.value "Position"
                                                             prop.text "Sort by position" ]
                                               Html.option [ prop.value "Match"
                                                             prop.text "Sort by match" ]
                                               Html.option [ prop.value "Left"
                                                             prop.text "Sort by left context" ]
                                               Html.option [ prop.value "Right"
                                                             prop.text "Sort by right context" ] ] ]

            let downloadButton =
                Bulma.button.button [ prop.disabled isSearchingOrFetching
                                      prop.style [ style.marginLeft 5 ]
                                      prop.text "Download" ]

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

                // Only show the spinner when we are searching AND have already found some results so as to
                // avoid showing spinners both here and over the result table at the same time (since we show
                // a spinner over the result table until we have found some results)
                let shouldShowSpnner =
                    model.IsSearching && model.ResultPages.Count > 0

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
                  Bulma.levelItem [ ContextSizeTextInput(model.ContextSizeTextValue, isSearchingOrFetching, dispatch) ]
                  Bulma.levelItem [ prop.style [ style.marginRight 50 ]
                                    prop.text "words" ] ]

            let resultPage =
                model.ResultPages.TryFind(model.ResultPageNo)

            [ MetadataQuickView model dispatch
              Bulma.level [ Bulma.levelLeft [ Bulma.levelItem [ sortMenu
                                                                downloadButton ]
                                              Bulma.levelItem resultsInfo ]
                            Bulma.levelRight [ if corpus.Config.Modality <> Spoken then
                                                   yield! contextSelector
                                               yield! pagination model isSearchingOrFetching numPages dispatch ] ]
              LoadedCorpus.ResultViews.Cwb.Written.concordanceTable corpus resultPage dispatch ]


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
                                                                                 model.NumSteps,
                                                                                 string model.SearchParams.ContextSize,
                                                                                 []
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
    let view
        (model: ShowingResultsModel)
        (corpus: Corpus)
        (search: Search)
        topRowButtonsElement
        loadedCorpusDispatch
        dispatch
        =
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
                && concordanceModel.NumResults.IsNone
                ->
                true
            | _ -> false

        Html.span [ topRowButtonsElement
                    selectSearchView corpus search loadedCorpusDispatch
                    spinnerOverlay shouldShowResultsTableSpinner (Some [ style.top 75 ]) resultsView ]


//////////////////////////////
/// View.LoadedCorpus.view
//////////////////////////////
let view (model: LoadedCorpusModel) (dispatch: Update.LoadedCorpus.Msg -> unit) =
    let topRowButtonsElement = topRowButtons model dispatch

    Html.span [ Bulma.section [ prop.style [ style.paddingTop (length.em 2.5) ]
                                prop.children [ Bulma.columns [ if shouldShowMetadataMenu model then
                                                                    Bulma.column [ column.isNarrow
                                                                                   prop.style [ style.marginRight 20 ]
                                                                                   prop.children [ Metadata.MetadataMenu.view
                                                                                                       model
                                                                                                       (MetadataMsg
                                                                                                        >> dispatch) ] ]
                                                                Bulma.column [ match model.Substate with
                                                                               | CorpusStart ->
                                                                                   CorpusStartView.view
                                                                                       model.Corpus
                                                                                       model.Search
                                                                                       topRowButtonsElement
                                                                                       dispatch
                                                                               | ShowingResults showingResultsModel ->
                                                                                   ResultsView.view
                                                                                       showingResultsModel
                                                                                       model.Corpus
                                                                                       model.Search
                                                                                       topRowButtonsElement
                                                                                       dispatch
                                                                                       (ShowingResultsMsg >> dispatch) ] ] ] ] ]
