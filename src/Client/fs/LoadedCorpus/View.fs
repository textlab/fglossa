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
        open ShowingResults.Concordance

        [<ReactComponent>]
        let PaginationTextInput (textValue: string, isDisabled: bool, dispatch: Msg -> unit) : ReactElement =
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
                               prop.onChange (SetPaginatorTextValue >> dispatch)
                               prop.onKeyUp (key.enter, (fun _ -> dispatch (SetPaginatorPage(None, None)))) ]

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
                    dispatch (SetPaginatorPage(Some pageNo, None))

            match concordanceModel.NumResults with
            | Some results when results > int64 loadedCorpusModel.Search.Params.PageSize ->
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
        let ContextSizeTextInput (textValue: string, isDisabled: bool, dispatch: Msg -> unit) : ReactElement =
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
                               prop.onChange (SetContextSizeTextValue >> dispatch)
                               prop.onKeyUp (
                                   key.enter,
                                   (fun _ ->
                                       match Int32.TryParse(textValue) with
                                       | true, size -> dispatch (SetContextSize size)
                                       | false, _ -> ignore None)
                               ) ]

        [<ReactComponent>]
        let MetadataQuickView (model: ConcordanceModel) (shouldShowQuickView: bool) dispatch =
            let elementRef = React.useElementRef ()

            let focusQuickView () =
                elementRef.current
                |> Option.iter
                    (fun quickViewElement ->
                        if shouldShowQuickView then
                            quickViewElement.focus ())

            // Focus the QuickView when mounted to enable it to receive keyboard events
            React.useEffect (focusQuickView, [| box model |])

            QuickView.quickview [ if shouldShowQuickView then
                                      quickview.isActive
                                  // Set elementRef in order to apply the focusQuickView() function to this element
                                  prop.ref elementRef
                                  // Set tabIndex so that the element receives keyboard events
                                  prop.tabIndex 0
                                  prop.onKeyUp
                                      (fun e ->
                                          if e.key = "Escape" then
                                              dispatch CloseQuickView)
                                  prop.children [ QuickView.header [ Html.div [ prop.style [ style.fontSize 16
                                                                                             style.fontWeight.bold ]
                                                                                prop.text "Metadata" ]
                                                                     Bulma.delete [ prop.onClick
                                                                                        (fun _ ->
                                                                                            dispatch CloseQuickView) ] ]
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

        [<ReactComponent>]
        let DownloadWindow (model: ConcordanceModel) (corpus: Corpus) dispatch =
            let elementRef = React.useElementRef ()

            let focusDownloadWindow () =
                elementRef.current
                |> Option.iter
                    (fun downloadWindowElement ->
                        if model.ShouldShowDownloadWindow then
                            downloadWindowElement.focus ())

            // Focus the QuickView when mounted to enable it to receive keyboard events
            React.useEffect (focusDownloadWindow, [| box model |])

            let checkbox isChecked (attribute: Cwb.PositionalAttribute) =
                Html.label [ prop.style [ style.marginRight 15 ]
                             prop.children [ Bulma.input.checkbox [ prop.isChecked isChecked
                                                                    prop.onCheckedChange
                                                                        (fun _ ->
                                                                            dispatch (ToggleDownloadAttribute attribute)) ]
                                             Bulma.text.span $" {attribute.Name}" ] ]

            let attributeCheckboxes =
                match corpus.SharedInfo.LanguageConfig with
                | Monolingual maybeAttrs ->
                    match maybeAttrs with
                    | Some attrs ->
                        let checkboxes =
                            [ for attr in corpus.SharedInfo.GetDefaultAttribute() :: attrs ->
                                  Bulma.control.div [ checkbox (model.DownloadAttributes |> List.contains attr) attr ] ]

                        Bulma.field.div [ field.isGrouped
                                          field.isGroupedMultiline
                                          prop.children checkboxes ]
                    | None -> Html.none
                | Multilingual _languages -> failwith "NOT IMPLEMENTED"

            let modalFooter =
                let disableDownload = model.DownloadAttributes.IsEmpty

                [ Bulma.level [ Bulma.levelLeft [ Bulma.levelItem (
                                                      Html.label [ Bulma.input.checkbox [ prop.isChecked
                                                                                              model.HeadersInDownload
                                                                                          prop.onCheckedChange
                                                                                              (fun _ ->
                                                                                                  dispatch
                                                                                                      ToggleHeadersInDownload) ]
                                                                   Bulma.text.span " Create headers" ]
                                                  )
                                                  Bulma.levelItem (
                                                      Bulma.button.button [ color.isSuccess

                                                                            match model.DownloadingFormat with
                                                                            | Some Excel -> button.isLoading
                                                                            | Some _ -> prop.disabled true
                                                                            | None -> prop.disabled disableDownload

                                                                            prop.onClick
                                                                                (fun _ ->
                                                                                    dispatch (
                                                                                        DownloadSearchResults Excel
                                                                                    ))
                                                                            prop.style [ style.marginLeft 5 ]
                                                                            prop.text "Excel" ]
                                                  )
                                                  Bulma.levelItem (
                                                      Bulma.button.button [ color.isSuccess

                                                                            match model.DownloadingFormat with
                                                                            | Some Tsv -> button.isLoading
                                                                            | Some _ -> prop.disabled true
                                                                            | None -> prop.disabled disableDownload

                                                                            prop.onClick
                                                                                (fun _ ->
                                                                                    dispatch (DownloadSearchResults Tsv))
                                                                            prop.text "Tab-separated" ]
                                                  )
                                                  Bulma.levelItem (
                                                      Bulma.button.button [ color.isSuccess

                                                                            match model.DownloadingFormat with
                                                                            | Some Csv -> button.isLoading
                                                                            | Some _ -> prop.disabled true
                                                                            | None -> prop.disabled disableDownload

                                                                            prop.onClick
                                                                                (fun _ ->
                                                                                    dispatch (DownloadSearchResults Csv))
                                                                            prop.text "Comma-separated" ]
                                                  ) ] ] ]

            Bulma.modal [ if model.ShouldShowDownloadWindow then
                              modal.isActive
                          // Set elementRef in order to apply the focusDownloadWindow() function to this element
                          prop.ref elementRef
                          // Set tabIndex so that the element receives keyboard events
                          prop.tabIndex 0
                          prop.onKeyUp
                              (fun e ->
                                  if e.key = "Escape" then
                                      dispatch CloseDownloadWindow)
                          prop.children [ Bulma.modalBackground [ prop.onClick (fun _ -> dispatch CloseDownloadWindow) ]
                                          Bulma.modalCard [ Bulma.modalCardHead [ Bulma.modalCardTitle
                                                                                      "Download results"
                                                                                  Bulma.delete [ prop.onClick
                                                                                                     (fun _ ->
                                                                                                         dispatch
                                                                                                             CloseDownloadWindow) ] ]
                                                            Bulma.modalCardBody (
                                                                Bulma.message [ color.isInfo
                                                                                prop.children [ Bulma.messageHeader [ Html.p
                                                                                                                          "Attributes" ]
                                                                                                Bulma.messageBody [ Bulma.field.div [ field.isGrouped
                                                                                                                                      field.isGroupedMultiline
                                                                                                                                      prop.children
                                                                                                                                          attributeCheckboxes ] ] ] ]
                                                            )
                                                            Bulma.modalCardFoot modalFooter ] ] ]



        ////////////////////////////////////////////////////
        /// View.LoadedCorpus.ResultsView.Concordance.view
        ////////////////////////////////////////////////////
        let view
            (loadedCorpusModel: LoadedCorpusModel)
            (concordanceModel: ConcordanceModel)
            (corpus: Corpus)
            (dispatch: Msg -> unit)
            =
            let numPages =
                concordanceModel.NumResultPages(loadedCorpusModel.Search.Params.PageSize)

            let isSearchingOrFetching =
                concordanceModel.PagesBeingFetched.Length > 0
                || concordanceModel.IsSearching

            let sortMenu =
                Bulma.select [ prop.disabled isSearchingOrFetching
                               prop.onChange
                                   (fun (s: string) ->
                                       let sortKey = SortKey.OfString(s)

                                       dispatch (SetPaginatorPage(Some 1, Some sortKey)))
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
                                      prop.onClick (fun _ -> dispatch OpenDownloadWindow)
                                      prop.text "Download" ]

            let resultsInfo =
                let text =
                    match concordanceModel.NumResults with
                    | Some numResults ->
                        if numResults > 0L then
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
                let shouldShowSpinner =
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
                                           spinnerOverlay
                                               shouldShowSpinner
                                               (Some [ style.width 40; style.height 40 ])
                                               []
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

            [ DownloadWindow concordanceModel corpus dispatch
              MetadataQuickView concordanceModel loadedCorpusModel.ShouldShowQuickView dispatch
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
        open ShowingResults.FrequencyLists

        ////////////////////////////////////////////////////
        /// View.LoadedCorpus.ResultsView.FrequencyLists.view
        ////////////////////////////////////////////////////
        let view
            (loadedCorpusModel: LoadedCorpusModel)
            (frequencyListsModel: FrequencyListsModel)
            (corpus: Corpus)
            (dispatch: Msg -> unit)
            =

            let checkbox isChecked (attribute: Cwb.PositionalAttribute) =
                Html.label [ prop.style [ style.marginRight 15 ]
                             prop.children [ Bulma.input.checkbox [ prop.isChecked isChecked
                                                                    prop.onCheckedChange
                                                                        (fun _ -> dispatch (ToggleAttribute attribute)) ]
                                             Bulma.text.span $" {attribute.Name}" ] ]

            let caseSensitiveCheckbox =
                Bulma.field.p (
                    Html.label [ prop.style [ style.marginRight 15 ]
                                 prop.children [ Bulma.input.checkbox [ prop.isChecked
                                                                            frequencyListsModel.IsCaseSensitive
                                                                        prop.onCheckedChange
                                                                            (fun _ -> dispatch ToggleIsCaseSensitive) ]
                                                 Bulma.text.span " Case sensitive" ] ]
                )

            let fromTokenInput =
                let value =
                    frequencyListsModel.TokenBoundaries.From
                    |> Option.map string
                    |> Option.defaultValue ""

                let onChange (v: string) =
                    if v = "" then
                        dispatch (SetFromToken None)
                    else
                        match Int32.TryParse(v) with
                        | true, v -> dispatch (SetFromToken(Some v))
                        | false, _ -> ignore None

                Bulma.field.div [ field.isHorizontal
                                  prop.children [ Bulma.fieldLabel [ fieldLabel.isNormal
                                                                     prop.style [ style.marginRight (length.em 0.5) ]
                                                                     prop.children [ Html.label [ Html.text
                                                                                                      "From token:" ] ] ]
                                                  Bulma.field.div (
                                                      Bulma.control.div [ Bulma.input.text [ prop.style [ style.width 60 ]
                                                                                             prop.value value
                                                                                             prop.onChange onChange ] ]
                                                  ) ] ]

            let toTokenInput =
                let value =
                    frequencyListsModel.TokenBoundaries.To
                    |> Option.map string
                    |> Option.defaultValue ""

                let onChange (v: string) =
                    if v = "" then
                        dispatch (SetToToken None)
                    else
                        match Int32.TryParse(v) with
                        | true, v -> dispatch (SetToToken(Some v))
                        | false, _ -> ignore None

                Bulma.field.div [ field.isHorizontal
                                  prop.children [ Bulma.fieldLabel [ fieldLabel.isNormal
                                                                     prop.style [ style.marginRight (length.em 0.5) ]
                                                                     prop.children [ Html.label [ Html.text "To token:" ] ] ]
                                                  Bulma.field.div (
                                                      Bulma.control.div [ Bulma.input.text [ prop.style [ style.width 60 ]
                                                                                             prop.value value
                                                                                             prop.onChange onChange ] ]
                                                  ) ] ]

            let buttonRow =
                let buttons =
                    [ Bulma.button.button [ prop.onClick (fun _ -> dispatch (DownloadFrequencyList Excel))
                                            match frequencyListsModel.DownloadingFormat with
                                            | Some Excel -> button.isLoading
                                            | Some _ -> prop.disabled true
                                            | None -> ()
                                            prop.text "Excel" ]
                      Bulma.button.button [ prop.onClick (fun _ -> dispatch (DownloadFrequencyList Tsv))
                                            match frequencyListsModel.DownloadingFormat with
                                            | Some Tsv -> button.isLoading
                                            | Some _ -> prop.disabled true
                                            | None -> ()
                                            prop.text "Tab-separated" ]
                      Bulma.button.button [ prop.onClick (fun _ -> dispatch (DownloadFrequencyList Csv))
                                            match frequencyListsModel.DownloadingFormat with
                                            | Some Csv -> button.isLoading
                                            | Some _ -> prop.disabled true
                                            | None -> ()
                                            prop.text "Comma-separated" ] ]

                Bulma.level [ Bulma.levelLeft [ Bulma.levelItem [ Bulma.button.button [ color.isSuccess
                                                                                        prop.onClick
                                                                                            (fun _ ->
                                                                                                dispatch
                                                                                                    FetchFrequencyList)
                                                                                        prop.text "Update stats" ] ]
                                                Bulma.levelItem [ prop.style [ style.marginLeft 20 ]
                                                                  prop.text "Download:" ]
                                                Bulma.levelItem [ Bulma.buttons buttons ] ] ]

            let controls =
                match corpus.SharedInfo.LanguageConfig with
                | Monolingual maybeAttrs ->
                    match maybeAttrs with
                    | Some attrs ->
                        let checkboxes =
                            [ for attr in corpus.SharedInfo.GetDefaultAttribute() :: attrs ->
                                  Bulma.control.div [ checkbox
                                                          (frequencyListsModel.Attributes
                                                           |> List.contains attr)
                                                          attr ] ]

                        Html.span [ Bulma.field.div [ field.isGrouped
                                                      field.isGroupedMultiline
                                                      prop.children checkboxes ]
                                    Bulma.level [ Bulma.levelLeft [ Bulma.levelItem caseSensitiveCheckbox
                                                                    Bulma.levelItem fromTokenInput
                                                                    Bulma.levelItem toTokenInput ] ]
                                    buttonRow ]
                    | None -> Html.span caseSensitiveCheckbox
                | Multilingual _languages -> failwith "NOT IMPLEMENTED"

            let frequencyTable =
                match frequencyListsModel.Frequencies with
                | Some frequencyRows ->
                    Bulma.tableContainer [ prop.style [ style.marginTop 20 ]
                                           prop.children [ Bulma.table [ Html.thead [ Html.tr [ Html.th "Count"
                                                                                                yield!
                                                                                                    [ for attr in
                                                                                                          frequencyListsModel.Attributes ->
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
        open ShowingResults.MetadataDistribution

        ////////////////////////////////////////////////////////////
        /// View.LoadedCorpus.ResultsView.MetadataDistribution.view
        ////////////////////////////////////////////////////////////
        let view (corpus: Corpus) (model: MetadataDistributionModel) (dispatch: Msg -> unit) =
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
                               prop.onChange
                                   (fun (s: string) ->
                                       if s <> "" then
                                           dispatch (SelectAttribute s)) ]

            let categoryOptions =
                [ for index, category in corpus.MetadataQuickView |> List.indexed ->
                      Html.option [ prop.value (string index)
                                    prop.text category.Name ] ]

            let categoryMenu =
                Bulma.select [ prop.children (
                                   Html.option [ prop.value ""
                                                 prop.text "Select category" ]
                                   :: categoryOptions
                               )
                               prop.onChange
                                   (fun (s: string) ->
                                       if s <> "" then
                                           let category = corpus.MetadataQuickView.[int s]

                                           dispatch (SelectCategory category)) ]

            let keepZeroValueButton =
                Html.label [ Bulma.input.checkbox [ prop.isChecked model.KeepZeroValues
                                                    prop.onCheckedChange (dispatch << SetKeepZero) ]
                             Html.text " Show metadata values with zero total" ]

            let downloadButtons =
                Bulma.levelItem (
                    Bulma.buttons [ Bulma.button.button [ prop.onClick
                                                              (fun _ -> dispatch (DownloadMetadataDistribution Excel))
                                                          match model.DownloadingFormat with
                                                          | Some Excel -> button.isLoading
                                                          | Some _ -> prop.disabled true
                                                          | None ->
                                                              if model.SelectedAttributeCode.IsNone
                                                                 || model.SelectedCategory.IsNone then
                                                                  prop.disabled true
                                                          prop.text "Excel" ]
                                    Bulma.button.button [ prop.onClick
                                                              (fun _ -> dispatch (DownloadMetadataDistribution Tsv))
                                                          match model.DownloadingFormat with
                                                          | Some Tsv -> button.isLoading
                                                          | Some _ -> prop.disabled true
                                                          | None ->
                                                              if model.SelectedAttributeCode.IsNone
                                                                 || model.SelectedCategory.IsNone then
                                                                  prop.disabled true
                                                          prop.text "Tab-separated" ]
                                    Bulma.button.button [ prop.onClick
                                                              (fun _ -> dispatch (DownloadMetadataDistribution Csv))
                                                          match model.DownloadingFormat with
                                                          | Some Csv -> button.isLoading
                                                          | Some _ -> prop.disabled true
                                                          | None ->
                                                              if model.SelectedAttributeCode.IsNone
                                                                 || model.SelectedCategory.IsNone then
                                                                  prop.disabled true
                                                          prop.text "Comma-separated" ] ]
                )

            let distributionTable =
                if model.MetadataDistribution.Distribution.Length > 0 then
                    let removeSelectedRowsButton =
                        Bulma.button.button [ button.isOutlined
                                              color.isDanger
                                              prop.title "Remove selected rows"
                                              prop.disabled model.ExcludedAttributeValues.New.IsEmpty
                                              prop.onClick (fun _ -> dispatch RemoveSelectedAttributeValues)
                                              prop.children [ Bulma.icon [ Html.i [ prop.className "fas fa-trash-alt" ] ] ] ]


                    let categoryValueCells =
                        [| for categoryValueStat in model.MetadataDistribution.CategoryValueStats ->
                               Html.th [ prop.key categoryValueStat.Value
                                         prop.children [ Html.text categoryValueStat.Value
                                                         Html.br []
                                                         Html.span [ prop.style [ style.fontWeight.normal ]
                                                                     prop.text
                                                                         $" ({categoryValueStat.TokenCount} tokens)" ] ] ] |]

                    let statsHeaderCells =
                        [| Html.th [ Html.span "Total"
                                     Html.span [ prop.style [ style.fontWeight.normal ]
                                                 prop.text $" ({model.MetadataDistribution.TotalTokenCount} tokens)" ] ]
                           Html.th "Deviation of proportions" |]

                    let frequencyRows =
                        [| for attrValueDistribution in model.MetadataDistribution.Distribution ->
                               let attrValue = attrValueDistribution.AttributeValue

                               let checkboxCell =
                                   Html.td [ Bulma.input.checkbox [ prop.isChecked (
                                                                        model.ExcludedAttributeValues.New.Contains(
                                                                            attrValue
                                                                        )
                                                                    )
                                                                    prop.onCheckedChange
                                                                        (fun isChecked ->
                                                                            dispatch (
                                                                                AddOrRemoveExcludedAttributeValue(
                                                                                    attrValue,
                                                                                    isChecked
                                                                                )
                                                                            )) ] ]

                               let attrValueCell =
                                   Html.td [ if attrValue = "__UNDEF__" then
                                                 Html.i "Undefined"
                                             else
                                                 Html.text attrValue ]

                               let frequencyCells =
                                   [| for valueFreq in attrValueDistribution.MetadataValueFrequencies ->
                                          Html.td (string valueFreq) |]

                               Html.tr (
                                   Array.concat [ [| checkboxCell; attrValueCell |]
                                                  frequencyCells
                                                  [| Html.td (string attrValueDistribution.AttributeValueTotal)
                                                     Html.td $"%.2f{attrValueDistribution.Dp}" |] ]
                               ) |]

                    let totalsFooter =
                        let totalsCells =
                            [| for stat in model.MetadataDistribution.CategoryValueStats ->
                                   Html.th (string stat.CategoryValueTotal) |]

                        Html.tr (
                            Array.concat [ [| Html.th ""; Html.th "Total" |]
                                           totalsCells
                                           [| Html.th (
                                               model.MetadataDistribution.CategoryValueStats
                                               |> List.sumBy (fun categoryValue -> categoryValue.CategoryValueTotal)
                                               |> string
                                              )
                                              Html.th $"%.2f{model.MetadataDistribution.TotalDp}" |] ]
                        )

                    let table =
                        Bulma.table [ prop.className "metadata-distribution-table"
                                      table.isBordered
                                      prop.children [ Html.thead [ Html.tr (
                                                                       Array.concat [ [| Html.th [ prop.key "remove-btn"
                                                                                                   prop.children
                                                                                                       removeSelectedRowsButton ]
                                                                                         Html.th [ prop.key "attr-value"
                                                                                                   prop.text "" ] |]
                                                                                      categoryValueCells
                                                                                      statsHeaderCells ]
                                                                   ) ]
                                                      Html.tbody frequencyRows
                                                      Html.tfoot [ totalsFooter ] ] ]

                    Bulma.tableContainer [ table ]
                else
                    Html.none

            Html.span [ Bulma.level [ Bulma.levelLeft [ Bulma.levelItem attrMenu
                                                        Bulma.levelItem categoryMenu
                                                        Bulma.levelItem [ prop.style [ style.marginLeft 5 ]
                                                                          prop.children [ keepZeroValueButton ] ]
                                                        Bulma.levelItem [ prop.style [ style.marginLeft 20 ]
                                                                          prop.text "Download:" ]
                                                        downloadButtons ] ]
                        spinnerOverlay model.IsFetching None [ distributionTable ] ]


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
                                                         prop.onClick
                                                             (fun _ ->
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
                                                         prop.onClick
                                                             (fun _ ->
                                                                 dispatch (
                                                                     ShowingResults.SelectResultTab(
                                                                         FrequencyLists(FrequencyListsModel.Default)
                                                                     )
                                                                 ))
                                                         prop.children [ Html.a [ prop.text "Frequency lists" ] ] ]
                                               Html.li [ if activeTab = "Metadata distribution" then
                                                             tab.isActive
                                                         prop.onClick
                                                             (fun _ ->
                                                                 dispatch (
                                                                     ShowingResults.SelectResultTab(
                                                                         MetadataDistribution(
                                                                             MetadataDistributionModel.Init()
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
                      loadedCorpusModel
                      frequencyListsModel
                      corpus
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
                                prop.onClick (fun _ -> dispatch ClosePopups)
                                prop.onKeyUp
                                    (fun e ->
                                        if e.key = "Escape" then
                                            dispatch (MetadataMsg Update.Metadata.Msg.CloseMetadataMenu))
                                prop.children [ Bulma.columns [ if shouldShowMetadataMenu model then
                                                                    Bulma.column [ column.isNarrow
                                                                                   prop.style [ style.marginRight 20 ]
                                                                                   prop.children [ Metadata.MetadataMenu.view
                                                                                                       model
                                                                                                       (MetadataMsg
                                                                                                        >> dispatch) ] ]
                                                                Bulma.column [ prop.style [ style.overflow.auto ]
                                                                               prop.children (
                                                                                   match model.Substate with
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
                                                                                           (ShowingResultsMsg
                                                                                            >> dispatch)
                                                                               ) ] ] ] ] ]
