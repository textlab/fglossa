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
    match corpus.SharedInfo.SearchEngine with
    | Cwb -> SearchViews.Cwb.view corpus search dispatch
    | Fcs -> SearchViews.Fcs.view corpus search dispatch


module CorpusStartView =
    let corpusNameBox config =
        let logo =
            match config.Logo with
            | Some logo -> Html.img [ prop.src $"corpora/{config.Code}/{logo}" ]
            | None -> Html.none

        Bulma.box [ prop.style [ style.padding 20 ]
                    prop.children [ Bulma.level [ Bulma.levelLeft [ Bulma.levelItem [ prop.style [ style.minWidth 394
                                                                                                   style.justifyContent.flexStart ]
                                                                                      prop.children (
                                                                                          Bulma.title config.Name
                                                                                      ) ]
                                                                    Bulma.levelItem logo ] ] ] ]

    let corpusInfo (config: SharedCorpusInfo) =
        match config.Info with
        | Some info ->
            Html.div [ prop.style [ style.marginTop 30
                                    style.marginBottom 10 ]
                       prop.dangerouslySetInnerHTML info ]
        | None -> Html.none

    let view (corpus: Corpus) (search: Search) topRowButtonsElement (dispatch: Update.LoadedCorpus.Msg -> unit) =
        Html.span [ topRowButtonsElement
                    corpusNameBox corpus.SharedInfo
                    selectSearchView corpus search dispatch
                    corpusInfo corpus.SharedInfo ]


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
            (loadedCorpusModel: LoadedCorpusModel)
            (concordanceModel: ConcordanceModel)
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

            match concordanceModel.NumResults with
            | Some results when results > uint64 loadedCorpusModel.Search.Params.PageSize ->
                [ Bulma.levelItem [ Bulma.buttons [ iconButton
                                                        "fa-angle-double-left"
                                                        (concordanceModel.PaginatorPageNo = 1
                                                         || isSearchingOrFetching)
                                                        (fun e -> setPage e 1)
                                                    iconButton
                                                        "fa-angle-left"
                                                        (concordanceModel.PaginatorPageNo = 1
                                                         || isSearchingOrFetching)
                                                        (fun e -> setPage e (concordanceModel.PaginatorPageNo - 1)) ] ]
                  Bulma.levelItem [ PaginationTextInput(
                                        concordanceModel.PaginatorTextValue,
                                        isSearchingOrFetching,
                                        dispatch
                                    ) ]
                  Bulma.levelItem [ Bulma.buttons [ iconButton
                                                        "fa-angle-right"
                                                        (concordanceModel.PaginatorPageNo = numPages
                                                         || isSearchingOrFetching)
                                                        (fun e -> setPage e (concordanceModel.PaginatorPageNo + 1))
                                                    iconButton
                                                        "fa-angle-double-right"
                                                        (concordanceModel.PaginatorPageNo = numPages
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
                |> Option.iter (fun quickViewElement ->
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
                                  prop.onKeyUp (fun e ->
                                      if e.key = "Escape" then
                                          dispatch ShowingResults.Concordance.CloseQuickView)
                                  prop.children [ QuickView.header [ Html.div [ prop.style [ style.fontSize 16
                                                                                             style.fontWeight.bold ]
                                                                                prop.text "Metadata" ]
                                                                     Bulma.delete [ prop.onClick (fun _ ->
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
        let view
            (loadedCorpusModel: LoadedCorpusModel)
            (concordanceModel: ConcordanceModel)
            (corpus: Corpus)
            (dispatch: ShowingResults.Concordance.Msg -> unit)
            =
            let numPages =
                concordanceModel.NumResultPages(loadedCorpusModel.Search.Params.PageSize)

            let isSearchingOrFetching =
                concordanceModel.PagesBeingFetched.Length > 0
                || concordanceModel.IsSearching

            let sortMenu =
                Bulma.select [ prop.disabled isSearchingOrFetching
                               prop.onChange (fun (s: string) ->
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
                    match concordanceModel.NumResults with
                    | Some numResults ->
                        if numResults > 0UL then
                            // We have received a non-zero number of results
                            let pagesStr = if numPages = 1 then "page" else "pages"

                            if concordanceModel.IsSearching then
                                $"Showing {numResults} matches ({numPages} {pagesStr}); searching..."
                            else
                                $"Found {numResults} matches ({numPages} pages)"
                        else
                            // We have received results, but the count was zero
                            "No matches found"
                    | None ->
                        // We have not yet received any results
                        if concordanceModel.IsSearching then
                            "Searching..."
                        else
                            ""

                // Only show the spinner when we are searching AND have already found some results so as to
                // avoid showing spinners both here and over the result table at the same time (since we show
                // a spinner over the result table until we have found some results)
                let shouldShowSpnner =
                    concordanceModel.IsSearching
                    && concordanceModel.ResultPages.Count > 0

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
                  Bulma.levelItem [ ContextSizeTextInput(
                                        concordanceModel.ContextSizeTextValue,
                                        isSearchingOrFetching,
                                        dispatch
                                    ) ]
                  Bulma.levelItem [ prop.style [ style.marginRight 50 ]
                                    prop.text "words" ] ]

            let resultPage =
                concordanceModel.ResultPages.TryFind(concordanceModel.ResultPageNo)

            [ MetadataQuickView concordanceModel dispatch
              Bulma.level [ Bulma.levelLeft [ Bulma.levelItem [ sortMenu
                                                                downloadButton ]
                                              Bulma.levelItem resultsInfo ]
                            Bulma.levelRight [ if corpus.SharedInfo.Modality <> Spoken then
                                                   yield! contextSelector
                                               yield!
                                                   pagination
                                                       loadedCorpusModel
                                                       concordanceModel
                                                       isSearchingOrFetching
                                                       numPages
                                                       dispatch ] ]
              match corpus.SharedInfo.Modality with
              | Spoken ->
                  LoadedCorpus.ResultViews.Cwb.Spoken.concordanceTable concordanceModel corpus resultPage dispatch
              | Written ->
                  LoadedCorpus.ResultViews.Cwb.Written.concordanceTable concordanceModel corpus resultPage dispatch ]


    module FrequencyLists =
        ////////////////////////////////////////////////////
        /// View.LoadedCorpus.ResultsView.FrequencyLists.view
        ////////////////////////////////////////////////////
        let view
            (model: FrequencyListsModel)
            (corpus: Corpus)
            (searchParams: SearchParams)
            (dispatch: ShowingResults.FrequencyLists.Msg -> unit)
            =

            let checkbox isChecked (attribute: Cwb.PositionalAttribute) =
                Html.label [ prop.style [ style.marginRight 15 ]
                             prop.children [ Bulma.input.checkbox [ prop.isChecked isChecked
                                                                    prop.onCheckedChange (fun isChecked ->
                                                                        dispatch (
                                                                            ShowingResults.FrequencyLists.ToggleAttribute
                                                                                attribute
                                                                        )) ]
                                             Bulma.text.span $" {attribute.Name}" ] ]

            let caseSensitiveCheckbox =
                Bulma.field.p (
                    Html.label [ prop.style [ style.marginRight 15 ]
                                 prop.children [ Bulma.input.checkbox [ prop.isChecked model.IsCaseSensitive
                                                                        prop.onCheckedChange (fun _ ->
                                                                            dispatch (
                                                                                ShowingResults.FrequencyLists.ToggleIsCaseSensitive
                                                                            )) ]
                                                 Bulma.text.span " Case sensitive" ] ]
                )

            let buttonRow =
                Bulma.level [ Bulma.levelLeft [ Bulma.levelItem [ Bulma.button.button [ color.isSuccess
                                                                                        prop.onClick (fun _ ->
                                                                                            dispatch (
                                                                                                ShowingResults.FrequencyLists.Msg.FetchFrequencyList
                                                                                                    searchParams
                                                                                            ))
                                                                                        prop.text "Update stats" ] ]
                                                Bulma.levelItem [ prop.style [ style.marginLeft 20 ]
                                                                  prop.text "Download:" ]
                                                Bulma.levelItem (
                                                    Bulma.buttons [ Bulma.button.button [ prop.onClick (fun _ ->
                                                                                              dispatch (
                                                                                                  ShowingResults.FrequencyLists.Msg.DownloadFrequencyList(
                                                                                                      searchParams,
                                                                                                      Excel
                                                                                                  )
                                                                                              ))
                                                                                          match model.DownloadingFormat
                                                                                              with
                                                                                          | Some Excel ->
                                                                                              button.isLoading
                                                                                          | Some _ -> prop.disabled true
                                                                                          | None -> ()
                                                                                          prop.text "Excel" ]
                                                                    Bulma.button.button [ prop.onClick (fun _ ->
                                                                                              dispatch (
                                                                                                  ShowingResults.FrequencyLists.Msg.DownloadFrequencyList(
                                                                                                      searchParams,
                                                                                                      Tsv
                                                                                                  )
                                                                                              ))
                                                                                          match model.DownloadingFormat
                                                                                              with
                                                                                          | Some Tsv -> button.isLoading
                                                                                          | Some _ -> prop.disabled true
                                                                                          | None -> ()
                                                                                          prop.text "Tab-separated" ]
                                                                    Bulma.button.button [ prop.onClick (fun _ ->
                                                                                              dispatch (
                                                                                                  ShowingResults.FrequencyLists.Msg.DownloadFrequencyList(
                                                                                                      searchParams,
                                                                                                      Csv
                                                                                                  )
                                                                                              ))
                                                                                          match model.DownloadingFormat
                                                                                              with
                                                                                          | Some Csv -> button.isLoading
                                                                                          | Some _ -> prop.disabled true
                                                                                          | None -> ()
                                                                                          prop.text "Commma-separated" ] ]
                                                ) ] ]

            let controls =
                match corpus.SharedInfo.LanguageConfig with
                | Monolingual maybeAttrs ->
                    match maybeAttrs with
                    | Some attrs ->
                        let checkboxes =
                            [ for attr in corpus.SharedInfo.GetDefaultAttribute() :: attrs ->
                                  Bulma.control.div [ checkbox (model.Attributes |> List.contains attr) attr ] ]

                        Html.span [ Bulma.field.div [ field.isGrouped
                                                      field.isGroupedMultiline
                                                      prop.children checkboxes ]
                                    caseSensitiveCheckbox
                                    buttonRow ]
                    | None -> Html.span caseSensitiveCheckbox
                | Multilingual languages -> failwith "NOT IMPLEMENTED"

            let frequencyTable =
                match model.Frequencies with
                | Some frequencyRows ->
                    Bulma.tableContainer [ prop.style [ style.marginTop 20 ]
                                           prop.children [ Bulma.table [ Html.thead [ Html.tr [ Html.th "Count"
                                                                                                yield!
                                                                                                    [ for attr in
                                                                                                          model.Attributes ->
                                                                                                          Html.th
                                                                                                              attr.Name ] ] ]
                                                                         Html.tbody [ for row in frequencyRows ->
                                                                                          Html.tr [ Html.td [ prop.style [ style.textAlign.right ]
                                                                                                              prop.text (
                                                                                                                  string
                                                                                                                      row.Frequency
                                                                                                              ) ]
                                                                                                    yield!
                                                                                                        [ for attrValue in
                                                                                                              row.AttributeValues ->
                                                                                                              Html.td
                                                                                                                  attrValue ] ] ] ] ] ]
                | None -> Html.none

            Html.span [ controls; frequencyTable ]

    module MetadataDistribution =
        ////////////////////////////////////////////////////////////
        /// View.LoadedCorpus.ResultsView.MetadataDistribution.view
        ////////////////////////////////////////////////////////////
        let view
            (corpus: Corpus)
            (model: MetadataDistributionModel)
            (dispatch: ShowingResults.MetadataDistribution.Msg -> unit)
            =
            let attributes =
                match corpus.SharedInfo.LanguageConfig with
                | Monolingual (Some attrs) -> corpus.SharedInfo.GetDefaultAttribute() :: attrs
                | _ -> failwith "NOT IMPLEMENTED"

            let attrOptions =
                [ for attr in attributes ->
                      Html.option [ prop.value attr.Code
                                    prop.text attr.Name ] ]

            let attrMenu =
                Bulma.select [ prop.children (
                                   Html.option [ prop.value ""
                                                 prop.text "Select attribute" ]
                                   :: attrOptions
                               )
                               prop.onChange (fun (s: string) ->
                                   if s <> "" then
                                       dispatch (ShowingResults.MetadataDistribution.SelectAttribute s)) ]

            let categoryOptions =
                [ for category in corpus.MetadataQuickView ->
                      Html.option [ prop.value category.Code
                                    prop.text category.Name ] ]

            let categoryMenu =
                Bulma.select [ prop.children (
                                   Html.option [ prop.value ""
                                                 prop.text "Select category" ]
                                   :: categoryOptions
                               )
                               prop.onChange (fun (s: string) ->
                                   if s <> "" then
                                       dispatch (ShowingResults.MetadataDistribution.SelectCategory s)) ]

            Html.span [ Bulma.level [ Bulma.levelLeft [ Bulma.levelItem attrMenu
                                                        Bulma.levelItem categoryMenu ] ] ]


    let tabs
        (loadedCorpusModel: LoadedCorpusModel)
        (showingResultsModel: ShowingResultsModel)
        (dispatch: ShowingResults.Msg -> unit)
        =
        let activeTab =
            match showingResultsModel.ActiveTab with
            | Concordance _ -> "Concordance"
            | FrequencyLists _ -> "Frequency lists"
            | MetadataDistribution _ -> "Metadata distribution"

        Bulma.tabs [ prop.style [ style.marginTop 15 ]
                     tabs.isToggle
                     tabs.isToggleRounded
                     prop.children [ Html.ul [ Html.li [ if activeTab = "Concordance" then
                                                             tab.isActive
                                                         prop.onClick (fun _ ->
                                                             dispatch (
                                                                 ShowingResults.SelectResultTab(
                                                                     Concordance(
                                                                         ConcordanceModel.Init(
                                                                             showingResultsModel.NumSteps,
                                                                             string
                                                                                 loadedCorpusModel.Search.Params.ContextSize,
                                                                             []
                                                                         )
                                                                     )
                                                                 )
                                                             ))
                                                         prop.children [ Html.a [ prop.text "Concordance" ] ] ]
                                               Html.li [ if activeTab = "Frequency lists" then
                                                             tab.isActive
                                                         prop.onClick (fun _ ->
                                                             dispatch (
                                                                 ShowingResults.SelectResultTab(
                                                                     FrequencyLists(FrequencyListsModel.Default)
                                                                 )
                                                             ))
                                                         prop.children [ Html.a [ prop.text "Frequency lists" ] ] ]
                                               Html.li [ if activeTab = "Metadata distribution" then
                                                             tab.isActive
                                                         prop.onClick (fun _ ->
                                                             dispatch (
                                                                 ShowingResults.SelectResultTab(
                                                                     MetadataDistribution(
                                                                         MetadataDistributionModel.Init(
                                                                             loadedCorpusModel.Corpus
                                                                         )
                                                                     )
                                                                 )
                                                             ))
                                                         prop.children [ Html.a [ prop.text "Metadata distribution" ] ] ] ] ] ]

    ///////////////////////////////////////
    /// View.LoadedCorpus.ResultsView.view
    ///////////////////////////////////////
    let view
        (loadedCorpusModel: LoadedCorpusModel)
        (showingResultsModel: ShowingResultsModel)
        (corpus: Corpus)
        (search: Search)
        topRowButtonsElement
        loadedCorpusDispatch
        dispatch
        =
        let resultsView =
            [ Bulma.level [ Bulma.levelLeft [ Bulma.levelItem [ tabs loadedCorpusModel showingResultsModel dispatch ] ] ]
              match showingResultsModel.ActiveTab with
              | Concordance concordanceModel ->
                  yield!
                      Concordance.view
                          loadedCorpusModel
                          concordanceModel
                          corpus
                          (ShowingResults.ConcordanceMsg >> dispatch)
              | FrequencyLists frequencyListsModel ->
                  FrequencyLists.view
                      frequencyListsModel
                      corpus
                      loadedCorpusModel.Search.Params
                      (ShowingResults.FrequencyListsMsg >> dispatch)
              | MetadataDistribution metadataDistributionModel ->
                  MetadataDistribution.view
                      corpus
                      metadataDistributionModel
                      (ShowingResults.MetadataDistributionMsg >> dispatch) ]

        let shouldShowResultsTableSpinner =
            match showingResultsModel.ActiveTab with
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
                                prop.tabIndex 0
                                prop.onClick (fun _ -> dispatch (MetadataMsg Update.Metadata.Msg.CloseMetadataMenu))
                                prop.onKeyUp (fun e ->
                                    if e.key = "Escape" then
                                        dispatch (MetadataMsg Update.Metadata.Msg.CloseMetadataMenu))
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
                                                                                       model
                                                                                       showingResultsModel
                                                                                       model.Corpus
                                                                                       model.Search
                                                                                       topRowButtonsElement
                                                                                       dispatch
                                                                                       (ShowingResultsMsg >> dispatch) ] ] ] ] ]
