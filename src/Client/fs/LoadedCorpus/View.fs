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


[<ReactComponent>]
let MetadataQuickView (model: LoadedCorpusModel) dispatch =
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
                          prop.style [ style.zIndex 50 ]
                          // Set elementRef in order to apply the focusQuickView() function to this element
                          prop.ref elementRef
                          // Set tabIndex so that the element receives keyboard events
                          prop.tabIndex 0
                          prop.onKeyUp (fun e ->
                              if e.key = "Escape" then
                                  dispatch CloseQuickView)
                          prop.children [ QuickView.header [ Html.div [ prop.style [ style.fontSize 16
                                                                                     style.fontWeight.bold ]
                                                                        prop.text "Metadata" ]
                                                             Bulma.delete [ prop.onClick (fun _ ->
                                                                                dispatch CloseQuickView) ] ]
                                          QuickView.body [ QuickView.block [ Bulma.table [ prop.style [ style.margin 5 ]
                                                                                           prop.children [ Html.tbody [ for category in
                                                                                                                            model.QuickViewMetadata ->
                                                                                                                            Html.tr [ Html
                                                                                                                                          .td (
                                                                                                                                              category.Name
                                                                                                                                              + ":"
                                                                                                                                          )
                                                                                                                                      Html.td [ prop.dangerouslySetInnerHTML
                                                                                                                                                    category.Value ] ] ] ] ] ] ] ] ]

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
            Html.div [ prop.className "corpus-info"
                       prop.style [ style.marginTop 30
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
        let DownloadWindow (model: ConcordanceModel) (corpus: Corpus) dispatch =
            let elementRef = React.useElementRef ()

            let focusDownloadWindow () =
                elementRef.current
                |> Option.iter (fun downloadWindowElement ->
                    if model.ShouldShowDownloadWindow then
                        downloadWindowElement.focus ())

            // Focus the QuickView when mounted to enable it to receive keyboard events
            React.useEffect (focusDownloadWindow, [| box model |])

            let attrCheckbox isChecked (attribute: Cwb.PositionalAttribute) =
                Html.label [ prop.style [ style.marginRight 15 ]
                             prop.children [ Bulma.input.checkbox [ prop.isChecked isChecked
                                                                    prop.onCheckedChange (fun _ ->
                                                                        dispatch (ToggleDownloadAttribute attribute)) ]
                                             Bulma.text.span $" {attribute.Name}" ] ]

            let categoryCheckbox isChecked (category: Metadata.Category) =
                Html.label [ prop.style [ style.marginRight 15 ]
                             prop.children [ Bulma.input.checkbox [ prop.isChecked isChecked
                                                                    prop.onCheckedChange (fun _ ->
                                                                        dispatch (ToggleDownloadCategory category)) ]
                                             Bulma.text.span $" {category.Name}" ] ]

            let attributeCheckboxes =
                match corpus.SharedInfo.LanguageConfig with
                | Monolingual maybeAttrs ->
                    match maybeAttrs with
                    | Some attrs ->
                        let checkboxes =
                            [ for attr in corpus.SharedInfo.GetDefaultAttribute() :: attrs ->
                                  Bulma.control.div [ attrCheckbox (model.DownloadAttributes |> List.contains attr) attr ] ]

                        Bulma.field.div [ field.isGrouped
                                          field.isGroupedMultiline
                                          prop.children checkboxes ]
                    | None -> Html.none
                | Multilingual _languages -> failwith "NOT IMPLEMENTED"

            let metadataCheckboxes =
                if not corpus.MetadataQuickView.IsEmpty then
                    let checkboxes =
                        [ for category in corpus.MetadataQuickView do
                              // The export always includes a "segment ID" column, and for spoken corpora that is
                              // in fact the informant ID, since we don't actually have segment IDs for such corpora.
                              // Furthermore, the informant ID is in fact the text ID in those corpora, so there is
                              // no need to show a checkbox for the tid category.
                              if category.Code <> "tid"
                                 || corpus.SharedInfo.Modality <> Spoken then
                                  Bulma.control.div [ categoryCheckbox
                                                          (model.DownLoadCategories |> List.contains category)
                                                          category ] ]

                    Bulma.field.div [ field.isGrouped
                                      field.isGroupedMultiline
                                      prop.children checkboxes ]
                else
                    Html.none

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

                                                                            prop.onClick (fun _ ->
                                                                                dispatch (DownloadSearchResults Excel))
                                                                            prop.style [ style.marginLeft 5 ]
                                                                            prop.text "Excel" ]
                                                  )
                                                  Bulma.levelItem (
                                                      Bulma.button.button [ color.isSuccess

                                                                            match model.DownloadingFormat with
                                                                            | Some Tsv -> button.isLoading
                                                                            | Some _ -> prop.disabled true
                                                                            | None -> prop.disabled disableDownload

                                                                            prop.onClick (fun _ ->
                                                                                dispatch (DownloadSearchResults Tsv))
                                                                            prop.text "Tab-separated" ]
                                                  )
                                                  Bulma.levelItem (
                                                      Bulma.button.button [ color.isSuccess

                                                                            match model.DownloadingFormat with
                                                                            | Some Csv -> button.isLoading
                                                                            | Some _ -> prop.disabled true
                                                                            | None -> prop.disabled disableDownload

                                                                            prop.onClick (fun _ ->
                                                                                dispatch (DownloadSearchResults Csv))
                                                                            prop.text "Comma-separated" ]
                                                  )
                                                  if
                                                      corpus.SharedInfo.HasAttribute("phon")
                                                      || corpus.SharedInfo.HasAttribute("orig")
                                                  then
                                                      Bulma.levelItem (
                                                          Bulma.button.button [ color.isSuccess

                                                                                match model.DownloadingFormat with
                                                                                | Some Multiline -> button.isLoading
                                                                                | Some _ -> prop.disabled true
                                                                                | None -> prop.disabled false

                                                                                prop.onClick (fun _ ->
                                                                                    dispatch (
                                                                                        DownloadSearchResults Multiline
                                                                                    ))
                                                                                prop.text "Multiline" ]
                                                      ) ] ] ]

            let modalCardBody =
                Html.span [ Bulma.message [ color.isInfo
                                            prop.children [ Bulma.messageHeader [ Html.p "Attributes" ]
                                                            Bulma.messageBody [ Bulma.field.div [ field.isGrouped
                                                                                                  field.isGroupedMultiline
                                                                                                  prop.children
                                                                                                      attributeCheckboxes ] ] ] ]
                            Bulma.message [ color.isInfo
                                            prop.children [ Bulma.messageHeader [ Html.p "Metadata categories" ]
                                                            Bulma.messageBody [ Bulma.field.div [ field.isGrouped
                                                                                                  field.isGroupedMultiline
                                                                                                  prop.children
                                                                                                      metadataCheckboxes ] ] ] ] ]

            Bulma.modal [ if model.ShouldShowDownloadWindow then
                              modal.isActive
                          // Set elementRef in order to apply the focusDownloadWindow() function to this element
                          prop.ref elementRef
                          // Set tabIndex so that the element receives keyboard events
                          prop.tabIndex 0
                          prop.onKeyUp (fun e ->
                              if e.key = "Escape" then
                                  dispatch CloseDownloadWindow)
                          prop.children [ Bulma.modalBackground [ prop.onClick (fun _ -> dispatch CloseDownloadWindow) ]
                                          Bulma.modalCard [ Bulma.modalCardHead [ Bulma.modalCardTitle
                                                                                      "Download results"
                                                                                  Bulma.delete [ prop.onClick
                                                                                                     (fun _ ->
                                                                                                         dispatch
                                                                                                             CloseDownloadWindow) ] ]
                                                            Bulma.modalCardBody modalCardBody
                                                            Bulma.modalCardFoot modalFooter ] ] ]



        ////////////////////////////////////////////////////
        /// View.LoadedCorpus.ResultsView.Concordance.view
        ////////////////////////////////////////////////////
        let view
            (loadedCorpusModel: LoadedCorpusModel)
            (concordanceModel: ConcordanceModel)
            (corpus: Corpus)
            (loadedCorpusDispatch: Update.LoadedCorpus.Msg -> unit)
            (dispatch: Msg -> unit)
            =
            let numPages =
                concordanceModel.NumResultPages(loadedCorpusModel.Search.Params.PageSize)

            let isSearchingOrFetching =
                concordanceModel.PagesBeingFetched.Length > 0
                || concordanceModel.IsSearching

            let sortMenu =
                Bulma.select [ prop.disabled isSearchingOrFetching
                               prop.value (string loadedCorpusModel.Search.Params.SortKey)
                               prop.onChange (fun (s: string) ->
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

            let resultPage = concordanceModel.ResultPages.TryFind(concordanceModel.ResultPageNo)

            [ DownloadWindow concordanceModel corpus dispatch
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
                  LoadedCorpus.ResultViews.Cwb.Spoken.concordanceTable
                      concordanceModel
                      corpus
                      resultPage
                      loadedCorpusDispatch
                      dispatch
              | Written ->
                  LoadedCorpus.ResultViews.Cwb.Written.concordanceTable
                      concordanceModel
                      corpus
                      resultPage
                      loadedCorpusDispatch
                      dispatch

              Bulma.level [ prop.style [ style.marginTop 10 ]
                            prop.children [ Bulma.levelLeft []
                                            Bulma.levelRight (
                                                pagination
                                                    loadedCorpusModel
                                                    concordanceModel
                                                    isSearchingOrFetching
                                                    numPages
                                                    dispatch
                                            ) ] ] ]


    module GeoDistrMap =
        let geoMapColors =
            [ "yellow"
              "green"
              "blue"
              "purple"
              "black"
              "white"
              "red"
              "orange" ]

        [<ReactComponent(import = "default", from = "../../react_components/result_map.jsx")>]
        let GeoDistributionMap
            (initLat: float)
            (initLng: float)
            (initZoom: float)
            (width: int)
            (height: int)
            (points: {| latitude: float
                        longitude: float
                        name: string
                        key: string
                        label: string
                        icon: string |} [])
            =
            React.imported ()

        [<ReactComponent>]
        let GeoMapController
            (allCoords: (string * float * float) [])
            (geoMapConfig: GeoMapConfig)
            (coordMap: Map<string, Map<string, int64>>)
            =
            let selectedColor, setSelectedColor = React.useState "yellow"

            // coloredPhons is a map from a color name to a set of phonetic forms whose buttons have that color
            let (coloredPhons: Map<string, Set<string>>), setColorPhons =
                React.useState Map.empty

            let colorPicker selectedColor color =
                let borderColor =
                    if [ "white"; "red"; "orange"; "yellow" ]
                       |> List.contains color then
                        "black"
                    else
                        "red"

                Bulma.button.button [ prop.className "colorpicker"
                                      prop.style [ style.backgroundColor color
                                                   style.borderColor borderColor
                                                   style.borderWidth (if selectedColor = color then 4 else 1) ]
                                      prop.onClick (fun _ -> setSelectedColor color) ]

            let phonButton phon =
                let maybeColor =
                    coloredPhons
                    |> Map.tryFindKey (fun _ v -> v |> Set.contains phon)

                let darkColors =
                    [ "green"
                      "blue"
                      "purple"
                      "black"
                      "red" ]

                let colorStyles =
                    match maybeColor with
                    | Some color ->
                        let foregroundColor =
                            if darkColors |> List.contains color then
                                "white"
                            else
                                "dark"

                        [ style.color foregroundColor
                          style.backgroundColor color ]
                    | None -> []

                let total = coordMap[phon] |> Map.toArray |> Array.sumBy snd

                let titleText =
                    coordMap[phon]
                    |> Map.toArray
                    |> Array.sortBy snd
                    |> Array.rev
                    |> Array.map (fun (location, freq) -> $"{location}: {freq}")
                    |> String.concat "; "
                    |> fun s -> $"{s}; Total: {total}"

                Bulma.button.button [ prop.style ([ style.padding 5 ] |> List.append colorStyles)
                                      prop.title titleText
                                      prop.text phon
                                      prop.onClick (fun _ ->
                                          // If the button already has a color, remove it
                                          // (regardless of whether another color picker has
                                          // been selected or not, this color should be removed)
                                          let newMap =
                                              match maybeColor with
                                              | Some color ->
                                                  coloredPhons.Change(
                                                      color,
                                                      (fun maybePhons ->
                                                          match maybePhons with
                                                          | Some phons -> Some(phons.Remove(phon))
                                                          | None -> None)
                                                  )
                                              | None -> coloredPhons

                                          // If the selected colour picker differs from the current colour
                                          // of the button, set the button to have the new button colour.
                                          let newMap' =
                                              if maybeColor.IsNone
                                                 || maybeColor.Value <> selectedColor then
                                                  newMap.Change(
                                                      selectedColor,
                                                      (fun maybePhons ->
                                                          match maybePhons with
                                                          | Some phons -> Some(phons.Add(phon))
                                                          | None -> Some(Set.singleton phon))
                                                  )
                                              else
                                                  newMap

                                          setColorPhons newMap') ]

            let locationNames =
                coordMap.Values
                |> Seq.collect (fun locationMap -> locationMap.Keys)
                |> Seq.toArray
                |> Array.distinct

            // Convert the hash map with frequency distribution over locations per phon
            // to one with the frequency distribution over phons per locations
            let locationPhonFreqs =
                coordMap
                |> Map.toArray
                |> Array.collect (fun (phon, locationMap) ->
                    locationMap
                    |> Map.toArray
                    |> Array.map (fun (location, freq) -> (location, phon, freq)))
                |> Array.groupBy (fun (location, _, _) -> location)
                |> Array.map (fun (location, phonFreqs) ->
                    let phonFreqMap =
                        [| for _, phon, freq in phonFreqs -> phon, freq |]
                        |> Map.ofArray

                    (location, phonFreqMap))
                |> Map.ofArray

            let allCoordsMap =
                allCoords
                |> Array.map (fun (locationName, lat, lng) -> locationName, (lat, lng))
                |> Map.ofArray

            let unknownLocations =
                [| for locationName in locationNames do
                       if allCoordsMap.TryFind(locationName).IsNone then
                           locationName |]

            if unknownLocations.Length > 0 then
                let msg =
                    [ "UNKNOWN LOCATIONS:"
                      unknownLocations |> String.concat "\n"
                      $"Number of unknown locations: {unknownLocations.Length}" ]
                    |> String.concat "\n"

                Fable.Core.JS.console.warn msg

            let coords =
                [| for locationName in locationNames do
                       if allCoordsMap.TryFind(locationName).IsSome then
                           yield
                               {| Name = locationName
                                  Coords = allCoordsMap[locationName]
                                  Phons =
                                   locationPhonFreqs[locationName]
                                   |> Map.toArray
                                   |> Array.map (fun (phon, freq) -> $"{phon}: {freq}")
                                   |> String.concat "; " |} |]

            // These are the small red dots that mark all locations where hits were found
            let smallDots =
                [| for coord in coords ->
                       {| latitude = fst coord.Coords
                          longitude = snd coord.Coords
                          name = coord.Name
                          key = coord.Name
                          label = $"{coord.Name}: {coord.Phons}"
                          icon = "" |} |]

            // Now find, for each color in the color picker, those locations where we
            // found one or more of the phonetic forms selected for that color, and create
            // colored markers for them.
            let selectedPoints =
                [| for color, phons in coloredPhons |> Map.toArray do
                       yield!
                           [| for phon in phons do
                                  let locationFreqs = coordMap[phon]

                                  let selectedLocations = Set.ofSeq locationFreqs.Keys

                                  let selectedCoords =
                                      smallDots
                                      |> Array.filter (fun dot -> selectedLocations.Contains(dot.name))

                                  yield!
                                      [| for coordMap in selectedCoords ->
                                             {| coordMap with
                                                 key = $"{coordMap.key}_{color}"
                                                 icon = $"speech/mm_20_{color}.png" |} |] |] |]

            let points =
                Array.append smallDots selectedPoints
                |> Array.distinct

            Html.div [ prop.className "geo-map"
                       prop.children [ Html.div [ for color in geoMapColors -> colorPicker selectedColor color ]
                                       Bulma.buttons [ prop.style [ style.marginTop 10 ]
                                                       prop.children [ for phon in coordMap.Keys -> phonButton phon ] ]
                                       Html.div [ GeoDistributionMap
                                                      geoMapConfig.CenterLat
                                                      geoMapConfig.CenterLng
                                                      geoMapConfig.ZoomLevel
                                                      640
                                                      460
                                                      points ] ] ]

    module FrequencyLists =
        open ShowingResults.FrequencyLists

        ////////////////////////////////////////////////////
        /// View.LoadedCorpus.ResultsView.FrequencyLists.view
        ////////////////////////////////////////////////////
        let view (frequencyListsModel: FrequencyListsModel) (corpus: Corpus) (dispatch: Msg -> unit) =

            let checkbox isChecked (attribute: Cwb.PositionalAttribute) =
                Html.label [ prop.style [ style.marginRight 15 ]
                             prop.children [ Bulma.input.checkbox [ prop.isChecked isChecked
                                                                    prop.onCheckedChange (fun _ ->
                                                                        dispatch (ToggleAttribute attribute)) ]
                                             Bulma.text.span $" {attribute.Name}" ] ]

            let caseSensitiveCheckbox =
                Bulma.field.p (
                    Html.label [ prop.style [ style.marginRight 15 ]
                                 prop.children [ Bulma.input.checkbox [ prop.isChecked
                                                                            frequencyListsModel.IsCaseSensitive
                                                                        prop.onCheckedChange (fun _ ->
                                                                            dispatch ToggleIsCaseSensitive) ]
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
                                                                                        prop.onClick (fun _ ->
                                                                                            dispatch FetchFrequencyList)
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
                    | None ->
                        Html.span [ Bulma.level [ Bulma.levelLeft [ Bulma.levelItem caseSensitiveCheckbox
                                                                    Bulma.levelItem fromTokenInput
                                                                    Bulma.levelItem toTokenInput ] ]
                                    buttonRow ]
                | Multilingual _languages -> failwith "NOT IMPLEMENTED"

            let frequencyTable =
                let tableOrEmpty =
                    match frequencyListsModel.Frequencies with
                    | Some frequencyRows ->
                        [ Bulma.tableContainer [ prop.style [ style.marginTop 20 ]
                                                 prop.children [ Bulma.table [ Html.thead [ Html.tr [ Html.th "Count"
                                                                                                      yield!
                                                                                                          [ for attr in
                                                                                                                frequencyListsModel.Attributes ->
                                                                                                                Html.th
                                                                                                                    attr.Name ] ] ]
                                                                               Html.tbody [ for row in frequencyRows ->
                                                                                                Html.tr [ Html.td [ prop.style [ style.textAlign.right ]
                                                                                                                    prop
                                                                                                                        .text (
                                                                                                                            string
                                                                                                                                row.Frequency
                                                                                                                        ) ]
                                                                                                          yield!
                                                                                                              [ for attrValue in
                                                                                                                    row.AttributeValues ->
                                                                                                                    Html.td
                                                                                                                        attrValue ] ] ] ] ] ] ]
                    | None -> [ Html.none ]

                spinnerOverlay frequencyListsModel.IsFetchingFrequencyList None tableOrEmpty

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
                | Monolingual None -> [ corpus.SharedInfo.GetDefaultAttribute() ]
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
                                       dispatch (SelectAttribute s)) ]

            let categoryOptions =
                let categories =
                    if corpus.MetadataDistributionCategories.IsEmpty then
                        corpus.MetadataQuickView
                    else
                        corpus.MetadataDistributionCategories

                [ for index, category in categories |> List.indexed ->
                      Html.option [ prop.value (string index)
                                    prop.text category.Name ] ]

            let categoryMenu =
                Bulma.select [ prop.children (
                                   Html.option [ prop.value ""
                                                 prop.text "Select category" ]
                                   :: categoryOptions
                               )
                               prop.onChange (fun (s: string) ->
                                   if s <> "" then
                                       let category = corpus.MetadataQuickView[int s]

                                       dispatch (SelectCategory category)) ]

            let keepZeroValueButton =
                Html.label [ Bulma.input.checkbox [ prop.isChecked model.KeepZeroValues
                                                    prop.onCheckedChange (dispatch << SetKeepZero) ]
                             Html.text " Show metadata values with zero total" ]

            let downloadButtons =
                Bulma.levelItem (
                    Bulma.buttons [ Bulma.button.button [ prop.onClick (fun _ ->
                                                              dispatch (DownloadMetadataDistribution Excel))
                                                          match model.DownloadingFormat with
                                                          | Some Excel -> button.isLoading
                                                          | Some _ -> prop.disabled true
                                                          | None ->
                                                              if model.SelectedAttributeCode.IsNone
                                                                 || model.SelectedCategory.IsNone then
                                                                  prop.disabled true
                                                          prop.text "Excel" ]
                                    Bulma.button.button [ prop.onClick (fun _ ->
                                                              dispatch (DownloadMetadataDistribution Tsv))
                                                          match model.DownloadingFormat with
                                                          | Some Tsv -> button.isLoading
                                                          | Some _ -> prop.disabled true
                                                          | None ->
                                                              if model.SelectedAttributeCode.IsNone
                                                                 || model.SelectedCategory.IsNone then
                                                                  prop.disabled true
                                                          prop.text "Tab-separated" ]
                                    Bulma.button.button [ prop.onClick (fun _ ->
                                                              dispatch (DownloadMetadataDistribution Csv))
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
                                                                    prop.onCheckedChange (fun isChecked ->
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
            | GeoDistributionMap _ -> "GeoDistributionMap"
            | FrequencyLists _ -> "Frequency lists"
            | MetadataDistribution _ -> "Metadata distribution"

        Bulma.tabs [ prop.style [ style.marginTop 15 ]
                     tabs.isToggle
                     tabs.isToggleRounded
                     prop.children [ Html.ul [ Html.li [ if activeTab = "Concordance" then
                                                             tab.isActive
                                                         prop.onClick (fun _ ->
                                                             dispatch (ShowingResults.SelectResultTab Concordance))
                                                         prop.children [ Html.a [ prop.text "Concordance" ] ] ]
                                               if loadedCorpusModel.Corpus.SharedInfo.GeoMapConfig.IsSome then
                                                   Html.li [ if activeTab = "GeoDistributionMap" then
                                                                 tab.isActive
                                                             prop.onClick (fun _ ->
                                                                 dispatch (
                                                                     ShowingResults.SelectResultTab(
                                                                         GeoDistributionMap(
                                                                             loadedCorpusModel.Corpus.SharedInfo.GeoMapConfig.Value,
                                                                             loadedCorpusModel.GeoDistribution
                                                                         )
                                                                     )
                                                                 ))
                                                             prop.children [ Html.a [ prop.text "Map" ] ] ]
                                               Html.li [ if activeTab = "Frequency lists" then
                                                             tab.isActive
                                                         prop.onClick (fun _ ->
                                                             dispatch (
                                                                 ShowingResults.SelectResultTab(
                                                                     FrequencyLists(FrequencyListsModel.Default)
                                                                 )
                                                             ))
                                                         prop.children [ Html.a [ prop.text "Frequency lists" ] ] ]
                                               if not loadedCorpusModel.Corpus.MetadataQuickView.IsEmpty then
                                                   Html.li [ if activeTab = "Metadata distribution" then
                                                                 tab.isActive
                                                             prop.onClick (fun _ ->
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
              | Concordance ->
                  yield!
                      Concordance.view
                          loadedCorpusModel
                          showingResultsModel.ConcordanceModel
                          corpus
                          loadedCorpusDispatch
                          (ShowingResults.ConcordanceMsg >> dispatch)
              | GeoDistributionMap (geoMapConfig, coordMap) ->
                  match corpus.SharedInfo.GeoCoordinates with
                  | Some coords -> GeoDistrMap.GeoMapController coords geoMapConfig coordMap
                  | None -> failwith "No geographical coordinates provided!"
              | FrequencyLists frequencyListsModel ->
                  FrequencyLists.view frequencyListsModel corpus (ShowingResults.FrequencyListsMsg >> dispatch)
              | MetadataDistribution metadataDistributionModel ->
                  MetadataDistribution.view
                      corpus
                      metadataDistributionModel
                      (ShowingResults.MetadataDistributionMsg >> dispatch) ]

        let shouldShowResultsTableSpinner =
            match showingResultsModel.ActiveTab with
            | Concordance when
                showingResultsModel.ConcordanceModel.IsSearching
                && showingResultsModel.ConcordanceModel.NumResults.IsNone
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
                                prop.onKeyUp (fun e ->
                                    if e.key = "Escape" then
                                        dispatch (MetadataMsg Update.Metadata.Msg.CloseMetadataMenu))
                                prop.children [ Bulma.columns [ if shouldShowMetadataMenu model then
                                                                    Bulma.column [ column.isNarrow
                                                                                   prop.style [ style.marginRight 20 ]
                                                                                   prop.children [ Metadata.MetadataMenu.view
                                                                                                       model
                                                                                                       dispatch
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
                                                                               ) ] ]
                                                MetadataQuickView model dispatch ] ] ]
