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

        let popup =
            Html.div [ prop.style [ style.height (
                                        if model.IsSelectionTableOpen then
                                            length.percent 100
                                        else
                                            (length.percent 0)
                                    )
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

    let shouldShowMetadataMenu (model: LoadedCorpusModel) =
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


    let stringSelect (category: StringCategory) isOpen metadataSelection fetchedMetadataValues dispatch =
        MetadataSelect category isOpen metadataSelection fetchedMetadataValues dispatch

    let numberSelect (category: NumberCategory) isOpen metadataSelection fetchedMetadataValues dispatch =
        MetadataSelect category isOpen metadataSelection fetchedMetadataValues dispatch

    let interval (category: NumberCategory) dispatch =
        Html.li (
            Html.a [ prop.key category.Code
                     prop.text category.Name
                     prop.onClick (fun _ -> dispatch (ToggleMetadataMenuOpen category)) ]
        )

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
                    | Interval category -> interval category props.Dispatch
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
        let sidebarWidth =
            if shouldShowMetadataMenu model then
                200
            else
                0

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
                             MetadataSelection = model.Search.MetadataSelection
                             FetchedMetadataValues = model.FetchedMetadataValues
                             Dispatch = dispatch |}
                  | StringSelect category ->
                      let isOpen =
                          (Some category.Code = model.OpenMetadataCategoryCode)

                      stringSelect category isOpen model.Search.MetadataSelection model.FetchedMetadataValues dispatch
                  | NumberSelect category ->
                      let isOpen =
                          (Some category.Code = model.OpenMetadataCategoryCode)

                      numberSelect category isOpen model.Search.MetadataSelection model.FetchedMetadataValues dispatch
                  | Interval category -> (interval category dispatch)
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

        Html.span [ Html.div [ prop.style [ style.width sidebarWidth
                                            style.paddingLeft (length.em 0.75)
                                            style.marginBottom (length.rem 0.75) ]
                               prop.children [ Html.text (textAndTokenCountText model)
                                               showSelectionButton ] ]
                    SelectionTable.SelectionTablePopup model dispatch
                    Bulma.menu [ prop.style [ style.width sidebarWidth
                                              style.overflowX.hidden ]
                                 prop.children [ Bulma.menuList menuItems ] ]

                     ]
