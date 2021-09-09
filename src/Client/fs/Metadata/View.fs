module View.Metadata

open Fable.Core.JsInterop
open Feliz
open Feliz.Bulma
open Shared.Metadata
open Shared.StringUtils
open Model
open Update.Metadata
open Common

let textAndTokenCountText (model: LoadedCorpusModel) =
    match (model.NumSelectedTexts, model.NumSelectedTokens) with
    | Some selectedTexts, selectedTokens when selectedTexts <> model.Corpus.Config.TotalTexts ->
        $"{selectedTexts} of {model.Corpus.Config.TotalTexts} texts ({selectedTokens} of {model.Corpus.Config.TotalTokens} tokens) selected"
    | _ -> $"All {model.Corpus.Config.TotalTexts} texts ({model.Corpus.Config.TotalTokens} tokens) selected"

module SelectionTable =
    [<ReactComponent>]
    let SelectionTablePopup (model: LoadedCorpusModel) dispatch =
        let pagination =
            let pageSize = 50.0

            let numPages =
                match model.NumSelectedTexts with
                | Some selectedTexts -> float selectedTexts / pageSize
                | None -> float model.Corpus.Config.TotalTexts / pageSize
                |> ceil
                |> int

            let setPage (e: Browser.Types.MouseEvent) (pageNo: int) =
                e.preventDefault ()

                if pageNo >= 1 && pageNo <= numPages then
                    dispatch (SetSelectionTablePage pageNo)

            Bulma.pagination (
                Bulma.paginationList [ if model.SelectionTablePageNumber > 1 then
                                           Bulma.paginationLink.a [ prop.onClick (fun e -> setPage e 1)
                                                                    prop.text "1" ]
                                       if model.SelectionTablePageNumber >= 4 then
                                           Html.li [ prop.key "ellipse-left"
                                                     prop.children [ Html.span [ prop.className "pagination-ellipsis"
                                                                                 prop.text "…" ] ] ]
                                       if model.SelectionTablePageNumber >= 3 then
                                           Bulma.paginationLink.a [ prop.onClick
                                                                        (fun e ->
                                                                            setPage
                                                                                e
                                                                                (model.SelectionTablePageNumber - 1))
                                                                    prop.text (model.SelectionTablePageNumber - 1) ]
                                       Bulma.paginationLink.a [ paginationLink.isCurrent
                                                                prop.text model.SelectionTablePageNumber ]
                                       if model.SelectionTablePageNumber <= numPages - 2 then
                                           Bulma.paginationLink.a [ prop.onClick
                                                                        (fun e ->
                                                                            setPage
                                                                                e
                                                                                (model.SelectionTablePageNumber + 1))
                                                                    prop.text (model.SelectionTablePageNumber + 1) ]
                                       if model.SelectionTablePageNumber <= numPages - 3 then
                                           Html.li [ prop.key "ellipse-right"
                                                     prop.children [ Html.span [ prop.className "pagination-ellipsis"
                                                                                 prop.text "…" ] ] ]
                                       if model.SelectionTablePageNumber < numPages then
                                           Bulma.paginationLink.a [ prop.onClick (fun e -> setPage e numPages)
                                                                    prop.text numPages ] ]
            )

        let header =
            Bulma.level [ prop.style [ style.padding 20
                                       style.marginBottom 0 ]
                          prop.children [ Bulma.levelLeft [ Bulma.levelItem [ Bulma.subtitle (
                                                                                  textAndTokenCountText model
                                                                              ) ] ]
                                          Bulma.levelRight [ Bulma.levelItem [ pagination ]
                                                             Bulma.levelItem [ Bulma.delete [ delete.isMedium
                                                                                              prop.title "Close"
                                                                                              prop.style [ style.marginLeft
                                                                                                               40 ]
                                                                                              prop.onClick
                                                                                                  (fun _ ->
                                                                                                      dispatch
                                                                                                          CloseSelectionTable) ] ] ] ] ]

        let table =
            Bulma.tableContainer [ Bulma.table [ table.isStriped
                                                 table.isFullWidth
                                                 prop.children [ Html.thead [ Html.tr [ for category in
                                                                                            model.Corpus.MetadataTable ->
                                                                                            Html.th category.Name ] ]
                                                                 Html.tbody [ for row in model.FetchedTextMetadata ->
                                                                                  Html.tr [ for column in row ->
                                                                                                Html.td column ] ] ] ] ]

        let footer =
            Bulma.level [ prop.style [ style.padding 20
                                       style.marginBottom 0 ]
                          prop.children [ Bulma.levelLeft []
                                          Bulma.levelRight [ Bulma.levelItem [ pagination ]
                                                             Bulma.levelItem [ Bulma.delete [ delete.isMedium
                                                                                              prop.title "Close"
                                                                                              prop.style [ style.marginLeft
                                                                                                               40 ]
                                                                                              prop.onClick
                                                                                                  (fun _ ->
                                                                                                      dispatch
                                                                                                          CloseSelectionTable) ] ] ] ] ]

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
                       prop.onKeyUp
                           (fun e ->
                               if e.key = "Escape" then
                                   dispatch CloseSelectionTable)
                       prop.children [ header; table; footer ] ]

        let root =
            Browser.Dom.document.getElementById ("metadata-selection-popup-root")

        ReactDOM.createPortal (popup, root)

module MetadataMenu =

    [<ReactComponent>]
    let MetadataSelect
        (category: Category)
        isOpen
        (metadataSelection: Shared.Metadata.Selection)
        fetchedMetadataValues
        dispatch
        =

        let (filterInputText, setFilterInputText) = React.useState ("")
        let (filterInputNumChars, setFilterInputNumChars) = React.useState (1.0)

        let filterInputRef = React.useInputRef ()

        let focusFilterInput () =
            filterInputRef.current
            |> Option.iter (fun inputFilterElement -> inputFilterElement.focus ())


        let filterSelectOptions (selectOptions: string []) =
            if System.String.IsNullOrWhiteSpace(filterInputText) then
                selectOptions
            else
                let downCasedFilterText = filterInputText.ToLower()

                selectOptions
                |> Array.filter (fun option -> option.ToLower().Contains(downCasedFilterText))

        let FixedSizeList: obj = importMember "react-window"

        // The components in react-window expect a `children` prop, which should be a
        // component function or class. However, all React functions in Fable.React and
        // Feliz (such as `ofImport`, `ofType`, and even `Fable.React.ReactBindings.React.createElement`)
        // seem to automatically call createElement on (i.e. instantiate) whatever components we pass
        // as their `children` arguments.
        // Hence, the only solution I have found is to import React ourselves and and call
        // React.createElement directly, since it allows us to provide function components as
        // the `children` argument and leaves them uninstantiated.

        let ReactBare: obj = importAll "react"

        let selectDropdown
            category
            categorySelection
            (fetchedMetadataValues: string [])
            (dispatch: Msg -> unit)
            : ReactElement =

            /// A single item in a metadata category dropdown list
            let ListItem (props: {| index: int; style: obj |}) =
                // let selectOption = fetchedMetadataValues.[props.index]
                let metadataValue = fetchedMetadataValues.[props.index]

                let selectOption =
                    { Name = metadataValue
                      Value = metadataValue }

                let isAlreadySelected =
                    categorySelection.Choices
                    |> Array.contains selectOption

                Html.div [ prop.className "metadata-menu-list-item"
                           prop.style [ style.height (int props.style?height)
                                        style.left (int props.style?left)
                                        style.position.absolute
                                        style.top (int props.style?top)
                                        style.width (length.percent 100)
                                        style.padding 6
                                        if isAlreadySelected then
                                            if categorySelection.ShouldExclude then
                                                style.color "white"
                                                style.backgroundColor "#f14668"
                                            else
                                                style.backgroundColor "#ddd" ]
                           prop.onClick
                               (fun _ ->
                                   setFilterInputText ""
                                   dispatch (SelectItem(category, selectOption)))
                           prop.text (selectOption.Name |> truncate 25)
                           prop.title selectOption.Name ]

            ReactBare?createElement (FixedSizeList,
                                     createObj [ "height" ==> 200
                                                 "itemCount" ==> fetchedMetadataValues.Length
                                                 "itemSize" ==> 32
                                                 "width" ==> 200 ],
                                     ListItem)

        let categorySelection =
            metadataSelection.TryFind(category.Code)
            |> Option.defaultValue CategorySelection.Default

        Html.li [ Html.a [ prop.key category.Code
                           if isOpen then
                               prop.style [ style.lineHeight (length.em 1.7) ]
                           prop.onClick (fun _ -> dispatch (ToggleMetadataMenuOpen category))
                           prop.children [ Html.text category.Name
                                           if isOpen || categorySelection.Choices.Length > 0 then
                                               Bulma.button.button [ button.isSmall
                                                                     if categorySelection.ShouldExclude then
                                                                         color.isDanger
                                                                     prop.title "Exclude selected values"
                                                                     prop.style [ style.marginBottom 5
                                                                                  style.marginLeft 10 ]
                                                                     prop.onClick
                                                                         (fun e ->
                                                                             dispatch (ToggleExclude category)
                                                                             e.stopPropagation ())
                                                                     prop.children [ Bulma.icon [ Html.i [ prop.className [ "fa fa-minus" ] ] ] ] ]

                                               Bulma.button.button [ button.isSmall
                                                                     color.isDanger
                                                                     prop.style [ style.marginLeft 5 ]
                                                                     prop.title "Remove selection"
                                                                     prop.onClick
                                                                         (fun _ -> dispatch (DeselectAllItems category))
                                                                     prop.children [ Bulma.icon [ Html.i [ prop.className
                                                                                                               "fa fa-times" ] ] ] ] ] ]
                  // List of already selected values
                  let choices =
                      [ for choice in categorySelection.Choices do
                            Html.div [ prop.className "metadata-choice"
                                       if categorySelection.ShouldExclude then
                                           prop.style [ style.color "white"
                                                        style.backgroundColor "#f14668" ]
                                       prop.children [ Html.span [ prop.className "metadata-choice-cross"
                                                                   if categorySelection.ShouldExclude then
                                                                       prop.style [ style.color "white" ]
                                                                   prop.children [ Html.span [ prop.onClick
                                                                                                   (fun _ ->
                                                                                                       dispatch (
                                                                                                           DeselectItem(
                                                                                                               category,
                                                                                                               choice
                                                                                                           )
                                                                                                       ))
                                                                                               prop.text "x" ] ] ]
                                                       Html.span [ prop.text choice.Name
                                                                   prop.title choice.Name ] ] ] ]

                  let filterInput =
                      Bulma.input.search [ prop.ref filterInputRef
                                           prop.autoCapitalize.off
                                           prop.autoComplete "off"
                                           prop.style [ style.width (length.em ((filterInputNumChars + 3.0) * 0.55))
                                                        style.backgroundColor color.transparent
                                                        style.borderWidth 0
                                                        style.boxShadow (0, 0, color.transparent)
                                                        style.outlineWidth 0
                                                        style.padding 0 ]
                                           prop.value filterInputText
                                           prop.onChange
                                               (fun (s: string) ->
                                                   setFilterInputText s
                                                   setFilterInputNumChars (float s.Length)) ]

                  if isOpen || categorySelection.Choices.Length > 0 then
                      // The box containing already selected values
                      Html.div [ prop.className "metadata-menu-selection"
                                 prop.onClick
                                     (fun _ ->
                                         setFilterInputText ""
                                         focusFilterInput ()
                                         dispatch (OpenMetadataMenu category))
                                 prop.children [ yield! choices
                                                 filterInput ] ]

                  if isOpen then
                      // The menu dropdown
                      Html.div [ prop.className "metadata-menu-list-container"
                                 prop.children [ selectDropdown
                                                     category
                                                     categorySelection
                                                     (filterSelectOptions fetchedMetadataValues)
                                                     dispatch ] ] ]

    let stringSelect (category: StringCategory) isOpen metadataSelection fetchedMetadataValues dispatch =
        MetadataSelect category isOpen metadataSelection fetchedMetadataValues dispatch

    let numberSelect (category: NumberCategory) isOpen metadataSelection fetchedMetadataValues dispatch =
        MetadataSelect category isOpen metadataSelection fetchedMetadataValues dispatch

    type ListOrIntervalMode =
        | ListMode
        | IntervalMode

    [<ReactComponent>]
    let SelectOrInterval
        (category: NumberCategory)
        isOpen
        (metadataSelection: Selection)
        (fetchedMetadataValues: string [])
        (fetchedMinAndMax: (int64 * int64) option)
        dispatch
        =

        let mode, setMode = React.useState (ListMode)

        let interval =
            let maybeCategorySelection = metadataSelection.TryFind(category.Code)

            let pickValue choiceName =
                // If a from or to value already exists, find it
                maybeCategorySelection
                |> Option.bind
                    (fun categorySelection ->
                        categorySelection.Choices
                        |> Array.tryPick
                            (fun choice ->
                                if choice.Name = choiceName then
                                    Some choice.Value
                                else
                                    None))

            let maybeFrom = pickValue "glossa_interval_from"
            let maybeTo = pickValue "glossa_interval_to"

            let boundaryInput
                (label: string)
                (maybeValue: string option)
                (mixOrMaxSelectionFunction: (int64 * int64) -> int64)
                onChangeMsg
                =
                Html.tr [ Html.td [ prop.style [ style.verticalAlign.middle ]
                                    prop.text label ]
                          Html.td [ Bulma.field.div (
                                        Bulma.control.div (
                                            Bulma.input.text [ prop.placeholder (
                                                                   fetchedMinAndMax
                                                                   |> Option.map (mixOrMaxSelectionFunction >> string)
                                                                   |> Option.defaultValue ""
                                                               )
                                                               prop.value (maybeValue |> Option.defaultValue "")
                                                               prop.onChange
                                                                   (fun (v: string) ->
                                                                       dispatch (onChangeMsg (category, v)))
                                                               prop.onKeyUp (
                                                                   key.enter,
                                                                   fun _ -> dispatch FetchTextAndTokenCounts
                                                               ) ]
                                        )
                                    ) ] ]

            Html.li [ Html.a [ prop.key category.Code
                               if isOpen then
                                   prop.style [ style.lineHeight (length.em 1.7) ]
                               prop.onClick (fun _ -> dispatch (ToggleIntervalOpen category))
                               prop.children [ Html.text category.Name
                                               if isOpen then
                                                   Bulma.button.button [ button.isSmall
                                                                         color.isDanger
                                                                         prop.style [ style.marginLeft 15 ]
                                                                         prop.title "Remove selection"
                                                                         prop.onClick
                                                                             (fun e ->
                                                                                 dispatch (DeselectAllItems category))
                                                                         prop.children [ Bulma.icon [ Html.i [ prop.className
                                                                                                                   "fa fa-times" ] ] ] ] ] ]

                      if isOpen then
                          Html.table [ prop.className "interval-category-table"
                                       prop.style [ style.marginTop 5
                                                    style.marginBottom 5
                                                    style.marginLeft 10 ]
                                       prop.children (
                                           Html.tbody [ boundaryInput "From:" maybeFrom fst SetIntervalFrom
                                                        boundaryInput "To:" maybeTo snd SetIntervalTo ]
                                       ) ] ]

        Html.span [ if mode = ListMode then
                        numberSelect category isOpen metadataSelection fetchedMetadataValues dispatch
                    else
                        interval

                    if isOpen then
                        Bulma.tabs [ tabs.isToggle
                                     tabs.isSmall
                                     tabs.isCentered
                                     prop.style [ style.marginTop 5 ]
                                     prop.children [ Html.ul [ Html.li [ if mode = ListMode then tab.isActive
                                                                         prop.onClick
                                                                             (fun _ ->
                                                                                 if mode <> ListMode then
                                                                                     dispatch (
                                                                                         DeselectAllItems category
                                                                                     )

                                                                                     dispatch (
                                                                                         FetchMetadataValuesForCategory
                                                                                             category
                                                                                     )

                                                                                     setMode ListMode)
                                                                         prop.children [ Html.a [ Html.span "List" ] ] ]
                                                               Html.li [ if mode = IntervalMode then tab.isActive
                                                                         prop.onClick
                                                                             (fun _ ->
                                                                                 if mode <> IntervalMode then
                                                                                     dispatch (
                                                                                         DeselectAllItems category
                                                                                     )

                                                                                     dispatch (
                                                                                         FetchMinAndMaxForCategory
                                                                                             category
                                                                                     )

                                                                                     setMode IntervalMode)
                                                                         prop.children [ Html.a [ Html.span "Interval" ] ] ] ] ] ]


                     ]

    let freeTextSearch (category: LongTextCategory) dispatch =
        Html.li (
            Html.a [ prop.key category.Code
                     prop.text category.Name
                     prop.onClick (fun _ -> dispatch (ToggleMetadataMenuOpen category)) ]
        )

    /// A collapsible section in a metadata menu
    [<ReactComponent>]
    let Section
        (props: {| StartExpanded: bool
                   Title: string
                   Items: MenuItem list
                   OpenCategoryCode: string option
                   MetadataSelection: Shared.Metadata.Selection
                   FetchedMetadataValues: string []
                   FetchedMinAndMax: (int64 * int64) option
                   Dispatch: (Msg -> unit) |})
        =
        let (isExpanded, setIsExpanded) = React.useState (props.StartExpanded)

        let children =
            props.Items
            |> List.map
                (fun item ->
                    match item with
                    | StringSelect category ->
                        let isOpen =
                            (Some category.Code = props.OpenCategoryCode)

                        stringSelect category isOpen props.MetadataSelection props.FetchedMetadataValues props.Dispatch
                    | NumberSelect category ->
                        let isOpen =
                            (Some category.Code = props.OpenCategoryCode)

                        numberSelect category isOpen props.MetadataSelection props.FetchedMetadataValues props.Dispatch
                    | Interval category ->
                        let isOpen =
                            (Some category.Code = props.OpenCategoryCode)

                        SelectOrInterval
                            category
                            isOpen
                            props.MetadataSelection
                            props.FetchedMetadataValues
                            props.FetchedMinAndMax
                            props.Dispatch
                    | FreeTextSearch category -> freeTextSearch category props.Dispatch
                    | Section _ -> failwith $"Sections are not allowed as children of other sections: {item}")

        Html.span [ if props.Title <> "" then
                        Bulma.menuLabel [ prop.style [ style.cursor "pointer"
                                                       style.marginTop 10
                                                       style.marginBottom 0 ]
                                          prop.onClick (fun _ -> setIsExpanded (not isExpanded))
                                          prop.children [ Html.text props.Title
                                                          Bulma.icon [ Html.i [ prop.className [ "fa"
                                                                                                 if isExpanded then
                                                                                                     "fa-angle-up"
                                                                                                 else
                                                                                                     "fa-angle-down" ] ] ] ] ]
                    if isExpanded then
                        Html.ul [ prop.className "menu-list"
                                  prop.style [ style.borderLeft (1, borderStyle.solid, "#dbdbdb") ]
                                  prop.children children ]

                     ]

    /// The main view of the metadata menu on the left hand side of the interface
    let view (model: LoadedCorpusModel) (dispatch: Update.Metadata.Msg -> unit) =
        let menuItems =
            [ for item in model.Corpus.MetadataMenu do
                  match item with
                  | Section (state, title, items) ->
                      Section
                          {| StartExpanded =
                                 match state with
                                 | Open -> true
                                 | Closed -> false
                             Title = title
                             Items = items
                             OpenCategoryCode = model.OpenMetadataCategoryCode
                             MetadataSelection = model.Search.Params.MetadataSelection
                             FetchedMetadataValues = model.FetchedMetadataValues
                             FetchedMinAndMax = model.FetchedMinAndMax
                             Dispatch = dispatch |}
                  | StringSelect category ->
                      let isOpen =
                          (Some category.Code = model.OpenMetadataCategoryCode)

                      stringSelect
                          category
                          isOpen
                          model.Search.Params.MetadataSelection
                          model.FetchedMetadataValues
                          dispatch
                  | NumberSelect category ->
                      let isOpen =
                          (Some category.Code = model.OpenMetadataCategoryCode)

                      numberSelect
                          category
                          isOpen
                          model.Search.Params.MetadataSelection
                          model.FetchedMetadataValues
                          dispatch
                  | Interval category ->
                      let isOpen =
                          (Some category.Code = model.OpenMetadataCategoryCode)

                      SelectOrInterval
                          category
                          isOpen
                          model.Search.Params.MetadataSelection
                          model.FetchedMetadataValues
                          model.FetchedMinAndMax
                          dispatch
                  | FreeTextSearch category -> (freeTextSearch category dispatch) ]

        let showSelectionButton =
            Bulma.button.button [ button.isSmall
                                  button.isOutlined
                                  color.isInfo
                                  prop.title "Show selection"
                                  prop.style [ style.marginLeft 10 ]
                                  prop.onClick (fun _ -> dispatch FetchMetadataForTexts)
                                  prop.children [ Bulma.icon [ Html.i [ prop.className [ "fa fa-binoculars" ] ] ]
                                                  Html.span "Show" ] ]

        Html.span [ Html.div [ prop.style [ style.width 200
                                            style.paddingLeft (length.em 0.75)
                                            style.marginBottom (length.rem 0.75) ]
                               prop.children [ Html.text (textAndTokenCountText model)
                                               showSelectionButton ] ]
                    if model.IsSelectionTableOpen then
                        SelectionTable.SelectionTablePopup model dispatch
                    Bulma.menu [ prop.style [ style.width 200
                                              style.overflowX.hidden ]
                                 prop.children [ Bulma.menuList menuItems ] ]

                     ]
