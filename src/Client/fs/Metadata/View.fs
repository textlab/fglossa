module View.Metadata

open Fable.Core.JsInterop
open Feliz
open Feliz.Bulma
open Shared
open Shared.Metadata
open Shared.StringUtils
open Model
open Update.Metadata

let textAndTokenCountText (model: LoadedCorpusModel) =
    let corpusInfo = model.Corpus.SharedInfo

    match (model.NumSelectedTexts, model.NumSelectedTokens) with
    | Some selectedTexts, selectedTokens when selectedTexts <> corpusInfo.TotalTexts ->
        $"{selectedTexts} of {corpusInfo.TotalTexts} {corpusInfo.TextsReferral} ({selectedTokens}
          of {corpusInfo.TotalTokens} tokens) selected {model.TextSelectionInfo}"
    | _ ->
        $"All {corpusInfo.TotalTexts} {corpusInfo.TextsReferral} ({corpusInfo.TotalTokens} tokens) selected {model.TextSelectionInfo}"

[<ReactComponent(import = "default", from = "../../react_components/metadata_geo_map/Meta.jsx")>]
let MetadataGeoMap
    (coords: obj)
    (config: {| API_KEY: string
                CENTER: {| lat: float; lng: float |}
                ZOOM: float
                LOCATOR: string
                ID: string
                CATEGORY: obj |})
    (meta: string [] [])
    (ok: obj -> unit)
    (cancel: unit -> unit)
    =
    React.imported ()

module MetadataMenu =
    [<ReactComponent>]
    let FixedSizeList: obj = importMember "react-window"

    [<ReactComponent>]
    let MetadataSelect
        (category: Category)
        isOpen
        (metadataSelection: Selection)
        fetchedMetadataValues
        isInSidebar
        dispatch
        =

        let filterInputText, setFilterInputText = React.useState ""

        let filterInputNumChars, setFilterInputNumChars = React.useState 1.0

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

        // The components in react-window expect a `children` prop, which should be a
        // component function or class. However, all React functions in Fable.React and
        // Feliz (such as `ofImport`, `ofType`, and even `Fable.React.ReactBindings.React.createElement`)
        // seem to automatically call createElement on (i.e. instantiate) whatever components we pass
        // as their `children` arguments.
        // Hence, the only solution I have found is to import React ourselves and and call
        // React.createElement directly, since it allows us to provide function components as
        // the `children` argument and leaves them uninstantiated.

        let reactBare: obj = importAll "react"

        let selectDropdown
            category
            categorySelection
            (fetchedMetadataValues: string [])
            (dispatch: Msg -> unit)
            : ReactElement =

            /// A single item in a metadata category dropdown list. Note that, unlike when we define
            /// components using the ReactComponent attribute, in this case we actually need
            /// to define the argument as a props object and not as separate arguments, since this
            /// function will be called directly by React code and not transformed by Feliz.
            let listItem (props: {| index: int; style: obj |}) =
                // let selectOption = fetchedMetadataValues[props.index]
                let metadataValue = fetchedMetadataValues[props.index]

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
                           prop.onClick (fun _ ->
                               setFilterInputText ""
                               dispatch (SelectItem(category, selectOption)))
                           prop.text (selectOption.Name |> truncate 25)
                           prop.title selectOption.Name ]

            reactBare?createElement (FixedSizeList,
                                     createObj [ "height" ==> 200
                                                 "itemCount" ==> fetchedMetadataValues.Length
                                                 "itemSize" ==> 32 ],
                                     listItem)

        let tableAndColumn = category.GetQualifiedColumnName()

        let categorySelection =
            metadataSelection.TryFind(tableAndColumn)
            |> Option.defaultValue CategorySelection.Default

        React.useEffect ((fun () -> if isOpen then focusFilterInput ()), [| box isOpen |])

        Html.li [ if isInSidebar then
                      Html.a [ prop.key tableAndColumn
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
                                                                         prop.onClick (fun e ->
                                                                             dispatch (ToggleExclude category)
                                                                             e.stopPropagation ())
                                                                         prop.children [ Bulma.icon [ Html.i [ prop.className [ "fa fa-minus" ] ] ] ] ]

                                                   Bulma.button.button [ button.isSmall
                                                                         color.isDanger
                                                                         prop.style [ style.marginLeft 5 ]
                                                                         prop.title "Remove selection"
                                                                         prop.onClick (fun _ ->
                                                                             dispatch (DeselectAllItems category))
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
                                                                   prop.children [ Html.span [ prop.onClick (fun _ ->
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
                                           prop.onChange (fun (s: string) ->
                                               setFilterInputText s
                                               setFilterInputNumChars (float s.Length)) ]

                  if isOpen || categorySelection.Choices.Length > 0 then
                      // The box containing already selected values
                      Html.div [ prop.className "metadata-menu-selection"
                                 prop.onClick (fun _ ->
                                     setFilterInputText ""
                                     focusFilterInput ()
                                     dispatch (OpenMetadataMenu category))
                                 prop.children [ yield! choices
                                                 if isInSidebar then filterInput ] ]

                  if isOpen then
                      // The menu dropdown
                      Html.div [ prop.className "metadata-menu-list-container"
                                 prop.children [ selectDropdown
                                                     category
                                                     categorySelection
                                                     (filterSelectOptions fetchedMetadataValues)
                                                     dispatch ] ] ]

    let stringSelect (category: StringCategory) isOpen metadataSelection fetchedMetadataValues isInSidebar dispatch =
        MetadataSelect category isOpen metadataSelection fetchedMetadataValues isInSidebar dispatch

    let numberSelect (category: NumberCategory) isOpen metadataSelection fetchedMetadataValues isInSidebar dispatch =
        MetadataSelect category isOpen metadataSelection fetchedMetadataValues isInSidebar dispatch

    [<ReactComponent>]
    let SelectOrInterval
        (category: NumberCategory)
        isOpen
        (mode: ListOrIntervalMode)
        (metadataSelection: Selection)
        (fetchedMetadataValues: string [])
        (fetchedMinAndMax: (int64 * int64) option)
        (isInSidebar: bool)
        dispatch
        =
        let catCode = category.GetQualifiedColumnName()

        let maybeCategorySelection = metadataSelection.TryFind(catCode)

        let pickValue choiceName =
            // If a from or to value already exists, find it
            maybeCategorySelection
            |> Option.bind (fun categorySelection ->
                categorySelection.Choices
                |> Array.tryPick (fun choice ->
                    if choice.Name = choiceName then
                        Some choice.Value
                    else
                        None))

        let maybeFrom = pickValue "app_interval_from"

        let maybeTo = pickValue "app_interval_to"

        let intervalStateFromProps =
            [ ("From:", maybeFrom)
              ("To:", maybeTo) ]
            |> Map.ofList

        let intervalState, setIntervalState =
            React.useStateWithUpdater intervalStateFromProps

        // Reset the text fields to the values in the model when the category is opened or closed
        React.useEffect ((fun () -> setIntervalState (fun _ -> intervalStateFromProps)), [| box isOpen |])

        let submitChanges label onChangeMsg =
            match intervalState[label] with
            | Some v ->
                dispatch (onChangeMsg (category, v))
                dispatch FetchTextAndTokenCounts
            | None -> ()

        let hasChanged label =
            if label = "From:" then
                intervalState["From:"] <> maybeFrom
            else
                intervalState["To:"] <> maybeTo

        let interval =
            let boundaryInput
                (label: string)
                (maybeValue: string option)
                (mixOrMaxSelectionFunction: int64 * int64 -> int64)
                onChangeMsg
                =
                let textInput =
                    Bulma.input.text [ prop.placeholder (
                                           fetchedMinAndMax
                                           |> Option.map (mixOrMaxSelectionFunction >> string)
                                           |> Option.defaultValue ""
                                       )
                                       prop.value (maybeValue |> Option.defaultValue "")
                                       prop.onChange (fun (v: string) ->
                                           setIntervalState (fun state -> state.Add(label, Some v)))
                                       prop.onKeyUp (key.enter, (fun _ -> submitChanges label onChangeMsg)) ]

                let checkButton =
                    Bulma.button.button [ prop.disabled (not (hasChanged label))
                                          prop.style [ style.width 50 ]
                                          if (hasChanged label) then
                                              // Make the button green to alert the user that it needs to be clicked in order
                                              // to register the newly input value
                                              color.isSuccess
                                          prop.onClick (fun _ -> submitChanges label onChangeMsg)
                                          if hasChanged label then
                                              prop.text "OK"
                                          else
                                              prop.children [ Bulma.icon [ Html.i [ prop.className [ "fa fa-check" ] ] ] ] ]

                Html.tr [ Html.td [ prop.style [ style.verticalAlign.middle ]
                                    prop.text label ]
                          Html.td [ Bulma.field.div [ field.hasAddons
                                                      prop.children [ Bulma.control.div [ control.isExpanded
                                                                                          prop.children textInput ]
                                                                      Bulma.control.div [ checkButton ] ] ] ] ]

            Html.li [ if isInSidebar then
                          Html.a [ prop.key catCode
                                   if isOpen then
                                       prop.style [ style.lineHeight (length.em 1.7) ]
                                   prop.onClick (fun _ -> dispatch (ToggleIntervalOpen category))
                                   prop.children [ Html.text category.Name
                                                   if isOpen then
                                                       Bulma.button.button [ button.isSmall
                                                                             color.isDanger
                                                                             prop.style [ style.marginLeft 15 ]
                                                                             prop.title "Remove selection"
                                                                             prop.onClick (fun _ ->
                                                                                 dispatch (DeselectAllItems category))
                                                                             prop.children [ Bulma.icon [ Html.i [ prop.className
                                                                                                                       "fa fa-times" ] ] ] ] ] ]

                      if isOpen then
                          Html.table [ prop.className "interval-category-table"
                                       prop.style [ style.marginTop 5
                                                    style.marginBottom 5
                                                    style.marginLeft 10 ]
                                       prop.children (
                                           Html.tbody [ boundaryInput "From:" intervalState["From:"] fst SetIntervalFrom
                                                        boundaryInput "To:" intervalState["To:"] snd SetIntervalTo ]
                                       ) ]
                      else
                          let fromText = maybeFrom |> Option.defaultValue ""

                          let toText = maybeTo |> Option.defaultValue ""

                          if fromText <> "" || toText <> "" then
                              Html.div [ prop.className "selected-interval"
                                         prop.style [ if isInSidebar then style.paddingLeft 30
                                                      style.paddingBottom 10
                                                      style.fontSize 12
                                                      style.cursor.pointer
                                                      style.whitespace.nowrap ]
                                         prop.onClick (fun _ -> dispatch (OpenMetadataMenu category))
                                         prop.text $"{fromText} \u2014 {toText}" ] ]

        Html.span [ if mode = ListMode then
                        numberSelect category isOpen metadataSelection fetchedMetadataValues isInSidebar dispatch
                    else
                        interval

                    if isOpen then
                        let listButton =
                            Html.li [ if mode = ListMode then tab.isActive
                                      prop.onClick (fun _ ->
                                          if mode <> ListMode then
                                              dispatch (DeselectAllItems category)
                                              dispatch (FetchMetadataValuesForCategory category)
                                              dispatch (SetIntervalCategoryMode(category, ListMode)))
                                      prop.children [ Html.a [ Html.span "List" ] ] ]

                        let intervalButton =
                            Html.li [ if mode = IntervalMode then tab.isActive
                                      prop.onClick (fun _ ->
                                          if mode <> IntervalMode then
                                              setIntervalState (fun _ -> intervalStateFromProps)
                                              dispatch (DeselectAllItems category)
                                              dispatch (FetchMinAndMaxForCategory category)
                                              dispatch (SetIntervalCategoryMode(category, IntervalMode)))
                                      prop.children [ Html.a [ Html.span "Interval" ] ] ]

                        Bulma.tabs [ tabs.isToggle
                                     tabs.isSmall
                                     tabs.isCentered
                                     prop.style [ style.marginTop 5 ]
                                     prop.children [ Html.ul [ listButton; intervalButton ] ] ] ]

    let freeTextSearch (category: LongTextCategory) dispatch =
        let catCode = category.GetQualifiedColumnName()

        Html.li (
            Html.a [ prop.key catCode
                     prop.text category.Name
                     prop.onClick (fun _ -> dispatch (ToggleMetadataMenuOpen category)) ]
        )

    /// A collapsible section in a metadata menu
    [<ReactComponent>]
    let Section
        (startExpanded: bool)
        (title: string)
        (items: MenuItem list)
        (openCategoryCode: string option)
        (intervalCategoryModes: Map<CategoryCode, ListOrIntervalMode>)
        (metadataSelection: Selection)
        (fetchedMetadataValues: string [])
        (fetchedMinAndMax: (int64 * int64) option)
        (dispatch: Msg -> unit)
        =
        let isExpanded, setIsExpanded = React.useState startExpanded

        let children =
            items
            |> List.map (fun item ->
                match item with
                | Section _ -> failwith $"Sections are not allowed as children of other sections: {item}"
                | CategoryMenu cat ->
                    let isOpen = (Some cat.Code = openCategoryCode)

                    match cat with
                    | :? StringCategory as c ->
                        stringSelect c isOpen metadataSelection fetchedMetadataValues true dispatch
                    | :? NumberCategory as c ->
                        SelectOrInterval
                            c
                            isOpen
                            (intervalCategoryModes.TryFind(cat.Code)
                             |> Option.defaultValue ListMode)
                            metadataSelection
                            fetchedMetadataValues
                            fetchedMinAndMax
                            true
                            dispatch
                    | :? LongTextCategory as c -> freeTextSearch c dispatch
                    | c -> failwith $"Unhandled category: {c}")

        Html.span [ if title <> "" then
                        Bulma.menuLabel [ prop.style [ style.cursor "pointer"
                                                       style.marginTop 10
                                                       style.marginBottom 0 ]
                                          prop.onClick (fun _ -> setIsExpanded (not isExpanded))
                                          prop.children [ Html.text title
                                                          Bulma.icon [ Html.i [ prop.className [ "fa"
                                                                                                 if isExpanded then
                                                                                                     "fa-angle-up"
                                                                                                 else
                                                                                                     "fa-angle-down" ] ] ] ] ]
                    if isExpanded then
                        Html.ul [ prop.className "menu-list"
                                  prop.style [ style.border (2, borderStyle.solid, "#0090d6")
                                               style.borderRadius 5
                                               style.marginTop 5
                                               style.padding 5 ]
                                  prop.children children ]

                     ]

    [<ReactComponent>]
    let SelectionTablePopup
        (model: LoadedCorpusModel)
        (loadedCorpusDispatch: Update.LoadedCorpus.Msg -> unit)
        dispatch
        =
        let pagination =
            let pageSize = 50.0

            let numPages =
                match model.NumSelectedTexts with
                | Some selectedTexts -> float selectedTexts / pageSize
                | None ->
                    float model.Corpus.SharedInfo.TotalTexts
                    / pageSize
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
                                           Bulma.paginationLink.a [ prop.onClick (fun e ->
                                                                        setPage e (model.SelectionTablePageNumber - 1))
                                                                    prop.text (model.SelectionTablePageNumber - 1) ]
                                       Bulma.paginationLink.a [ paginationLink.isCurrent
                                                                prop.text model.SelectionTablePageNumber ]
                                       if model.SelectionTablePageNumber <= numPages - 2 then
                                           Bulma.paginationLink.a [ prop.onClick (fun e ->
                                                                        setPage e (model.SelectionTablePageNumber + 1))
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
            let voyant =
                let buttons =
                    let info = model.Corpus.SharedInfo

                    Bulma.buttons [ Bulma.button.button [ color.isSuccess
                                                          prop.onClick (fun _ -> dispatch (OpenInVoyant "word"))
                                                          prop.text (
                                                              if info.HasAttribute("phon") then
                                                                  "Orthographic"
                                                              elif info.HasAttribute("orig") then
                                                                  "Corrected"
                                                              else
                                                                  "Word form"
                                                          ) ]
                                    if info.HasAttribute("phon") then
                                        Bulma.button.button [ color.isSuccess
                                                              prop.onClick (fun _ -> dispatch (OpenInVoyant "phon"))
                                                              prop.text "Phonetic" ]
                                    if info.HasAttribute("orig") then
                                        Bulma.button.button [ color.isSuccess
                                                              prop.onClick (fun _ -> dispatch (OpenInVoyant "orig"))
                                                              prop.text "Original" ]
                                    if info.HasAttribute("lemma") then
                                        Bulma.button.button [ color.isSuccess
                                                              prop.onClick (fun _ -> dispatch (OpenInVoyant "lemma"))
                                                              prop.text "Lemmas" ] ]

                Bulma.levelItem [ prop.style [ style.marginRight 50 ]
                                  prop.children [ Html.span [ prop.style [ style.marginRight 10 ]
                                                              prop.children [ Html.span "Open in "
                                                                              Html.a [ prop.href
                                                                                           "https://voyant-tools.org/"
                                                                                       prop.target.blank
                                                                                       prop.text "Voyant:" ] ] ]
                                                  buttons ] ]

            Bulma.level [ prop.style [ style.padding 20
                                       style.marginBottom 0 ]
                          prop.children [ Bulma.levelLeft [ Bulma.levelItem (
                                                                Html.span [ Bulma.subtitle (textAndTokenCountText model)
                                                                            Html.div [ prop.style [ style.marginTop 10 ]
                                                                                       prop.children (
                                                                                           Bulma.subtitle [ title.is6
                                                                                                            prop.text
                                                                                                                "Click on a column header to sort; alt/option + click to edit the metadata selection." ]
                                                                                       ) ] ]
                                                            )

                                                             ]
                                          Bulma.levelRight [ if model.Corpus.SharedInfo.ExternalTools
                                                                |> List.contains Voyant then
                                                                 voyant
                                                             Bulma.levelItem pagination
                                                             Bulma.levelItem (
                                                                 Bulma.button.button [ color.isInfo
                                                                                       prop.title "Close"
                                                                                       prop.style [ style.marginLeft 40 ]
                                                                                       prop.onClick (fun _ ->
                                                                                           dispatch CloseSelectionTable)
                                                                                       prop.text "Close" ]
                                                             ) ] ] ]

        let table =
            let columnHeader (category: Category) =
                let getMenu showAsOpen =
                    let showMenuHeader = showAsOpen

                    let menuContents =
                        match category with
                        | :? StringCategory as cat ->
                            stringSelect
                                cat
                                showAsOpen
                                model.Search.Params.MetadataSelection
                                model.FetchedMetadataValues
                                showMenuHeader
                                dispatch
                        | :? NumberCategory as cat ->
                            SelectOrInterval
                                cat
                                showAsOpen
                                (model.IntervalCategoryModes.TryFind(category.Code)
                                 |> Option.defaultValue ListMode)
                                model.Search.Params.MetadataSelection
                                model.FetchedMetadataValues
                                model.FetchedMinAndMax
                                showMenuHeader
                                dispatch
                        | :? LongTextCategory as cat -> freeTextSearch cat dispatch
                        | cat -> failwith $"Unhandled category: {cat}"

                    let closeButton =
                        Bulma.delete [ prop.onClick (fun e ->
                                           e.stopPropagation ()
                                           dispatch CloseMetadataMenu)
                                       prop.title "Close" ]

                    let menuList =
                        Bulma.menuList [ prop.onClick (fun e ->
                                             // Prevent onclick handlers further up the hierarchy from closing this menu
                                             e.stopPropagation ())
                                         prop.children menuContents ]

                    if showAsOpen then
                        Html.div [ prop.style [ style.position.absolute
                                                style.maxWidth 225
                                                style.backgroundColor "white"
                                                style.border ("1px", borderStyle.solid, "#a1a1a1")
                                                style.borderRadius 7
                                                style.padding 10
                                                style.zIndex 1 ]
                                   prop.children [ Bulma.level [ prop.style [ style.marginBottom 0 ]
                                                                 prop.children [ Bulma.levelLeft []
                                                                                 Bulma.levelRight closeButton ] ]
                                                   menuList

                                                    ] ]
                    else
                        menuList

                let isMenuOpen = (Some category.Code = model.OpenMetadataCategoryCode)

                Html.th [ prop.onClick (fun e ->
                              if e.altKey then
                                  e.stopPropagation ()

                                  match category with
                                  | :? NumberCategory as cat -> dispatch (FetchMinAndMaxForCategory cat)
                                  | _ -> ()

                                  dispatch (OpenMetadataMenu category)
                              else
                                  let direction =
                                      match model.SelectionTableSort with
                                      | Some sortInfo ->
                                          if sortInfo.CategoryCode = category.GetQualifiedColumnName() then
                                              // We were already sorting on this category, so just
                                              // change direction
                                              if sortInfo.Direction = Asc then
                                                  Desc
                                              else
                                                  Asc
                                          else
                                              Asc
                                      | None -> Asc

                                  dispatch (
                                      SetSelectionTableSort
                                          { CategoryCode = category.GetQualifiedColumnName()
                                            Direction = direction }
                                  ))
                          // Note that if the menu is open, we render it in its open form before the heading, so that it will
                          // cover and hide the heading (which is then only rendered in order to keep the column width).
                          // In addition, we render it in its closed form after the heading regardless of whether it is actually
                          // open. If the open menu is shown, showing its closed form in addition (hidden behind
                          // the open one) ensures that the column width stays the same whether or not the menu is opened,
                          // which prevents a lot of jarring layout changes as the user opens and closes menus.
                          prop.children [ if isMenuOpen then getMenu true
                                          match model.SelectionTableSort with
                                          | Some sortInfo when sortInfo.CategoryCode = category.Code ->
                                              Html.span [ prop.className "icon-text"
                                                          prop.children [ Html.span category.Name
                                                                          Bulma.icon [ Html.i [ prop.className [ "fa"
                                                                                                                 if sortInfo.Direction = Asc then
                                                                                                                     "fa-sort-down"
                                                                                                                 else
                                                                                                                     "fa-sort-up" ] ] ] ] ]
                                          | _ -> Html.text category.Name
                                          Html.div [ prop.style [ style.marginTop 5
                                                                  style.marginBottom 5 ]
                                                     prop.children (getMenu false) ] ] ]

            let tableHeader =
                let firstColumnHeader =
                    if model.Corpus.SelectionTableFirstColumn("", model.Corpus.SharedInfo) = Html.none then
                        Html.none
                    else
                        Html.th []

                let infoBtnHeader =
                    if model.Corpus.SharedInfo.ShowInfoBtnInSelectionPopup then
                        Html.th []
                    else
                        Html.none

                let metadataHeaders =
                    [ for category in model.Corpus.MetadataTable -> columnHeader category ]

                Html.thead [ Html.tr (List.append [ firstColumnHeader; infoBtnHeader ] metadataHeaders) ]

            let tableBody =
                Html.tbody [ for row in model.FetchedTextMetadata do
                                 let tid = row[0]

                                 let firstColumn =
                                     model.Corpus.SelectionTableFirstColumn(tid, model.Corpus.SharedInfo)

                                 let infoBtnColumn =
                                     if model.Corpus.SharedInfo.ShowInfoBtnInSelectionPopup then
                                         Html.td [ prop.style [ style.paddingLeft 0; style.paddingRight 0 ]
                                                   prop.children [ Html.a [ prop.href ""
                                                                            prop.children [ Bulma.button.button [ button.isSmall
                                                                                                                  prop.onClick
                                                                                                                      (fun e ->
                                                                                                                          e.preventDefault
                                                                                                                              ()

                                                                                                                          loadedCorpusDispatch (
                                                                                                                              Update
                                                                                                                                  .LoadedCorpus
                                                                                                                                  .FetchMetadataForText(
                                                                                                                                      model.Corpus,
                                                                                                                                      tid
                                                                                                                                  )
                                                                                                                          ))
                                                                                                                  prop.children [ Bulma.icon [ Html.i [ prop.className [ "fa"
                                                                                                                                                                         "fa-info" ] ] ] ] ] ] ] ] ]
                                     else
                                         Html.none

                                 let metadataColumns = [ for column in row -> Html.td column ]

                                 Html.tr (List.append [ firstColumn; infoBtnColumn ] metadataColumns) ]

            Bulma.tableContainer [ Bulma.table [ table.isStriped
                                                 table.isFullWidth
                                                 prop.children [ tableHeader; tableBody ] ] ]

        let footer =
            Bulma.level [ prop.style [ style.padding 20
                                       style.marginBottom 0 ]
                          prop.children [ Bulma.levelLeft []
                                          Bulma.levelRight [ Bulma.levelItem [ pagination ]
                                                             Bulma.levelItem (
                                                                 Bulma.button.button [ color.isInfo
                                                                                       prop.title "Close"
                                                                                       prop.style [ style.marginLeft 40 ]
                                                                                       prop.onClick (fun _ ->
                                                                                           dispatch CloseSelectionTable)
                                                                                       prop.text "Close" ]
                                                             ) ] ] ]

        let elementRef = React.useElementRef ()

        let focusPopup () =
            elementRef.current
            |> Option.iter (fun popupElement -> popupElement.focus ())

        // Focus the popup when mounted to enable it to receive keyboard events
        React.useEffectOnce focusPopup

        let popup =
            Html.div [ prop.className "overscroll-contain"
                       prop.style [ style.height (length.percent 100)
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
                       // Set tabIndex so that the element receives keyboard events
                       prop.tabIndex 0
                       prop.onKeyUp (fun e ->
                           if e.key = "Escape" then
                               dispatch CloseSelectionTable)
                       prop.children [ header; table; footer ] ]

        let root = Browser.Dom.document.getElementById "metadata-selection-popup-root"

        ReactDOM.createPortal (popup, root)


    [<ReactComponent>]
    let MetadataGeoMapModal
        (googleMapsApiKey: string)
        (geoCoords: (string * float * float) [])
        (geoMapConfig: GeoMapConfig)
        (metadata: string [] [])
        (allCategories: Category list)
        (dispatch: Msg -> unit)
        =

        let elementRef = React.useElementRef ()

        let focusModal () =
            elementRef.current
            |> Option.iter (fun modalElement -> modalElement.focus ())

        // Focus the modal when mounted to enable it to receive keyboard events
        React.useEffectOnce focusModal

        let coords =
            createObj [| for place, lat, lng in geoCoords -> (place, [| lat; lng |]) |]

        let config =
            {| API_KEY = googleMapsApiKey
               CENTER =
                {| lat = geoMapConfig.CenterLat
                   lng = geoMapConfig.CenterLng |}
               ZOOM = geoMapConfig.ZoomLevel
               LOCATOR = geoMapConfig.LocationMetadataCategory.QualifiedColumnName
               ID = "texts.tid"
               CATEGORY =
                createObj (
                    geoMapConfig.MetadataCategories
                    |> List.mapi (fun index cat ->
                        cat.QualifiedColumnName,
                        {| COLUMN = index
                           CODE = cat.QualifiedColumnName
                           TYPE = cat.ControlType.ToString()
                           NAME = cat.Name |})
                ) |}

        let okHandler (results: obj) =
            let catCodes = Fable.Core.JS.Constructors.Object.keys results

            let values = Fable.Core.JS.Constructors.Object.values results

            for catCode, value in Seq.zip catCodes values do
                let categoryObj =
                    allCategories
                    |> List.find (fun c -> c.GetQualifiedColumnName() = catCode)

                let catControlType =
                    geoMapConfig.MetadataCategories
                    |> List.pick (fun c ->
                        if c.QualifiedColumnName = catCode then
                            Some c.ControlType
                        else
                            None)

                match catControlType with
                | IntervalControl ->
                    match categoryObj with
                    | :? NumberCategory as numberCat ->
                        let valueArray = value :?> string []
                        let min = valueArray[0]
                        let max = valueArray[1]
                        dispatch (SetIntervalCategoryMode(numberCat, IntervalMode))
                        dispatch (SetIntervalFrom(numberCat, min))
                        dispatch (SetIntervalTo(numberCat, max))
                    | _ -> failwith $"Non-numerical category defined as interval: {catCode}"
                | DiscreteControl ->
                    let choices = [| for v in (value :?> string []) -> { Name = v; Value = v } |]

                    dispatch (SetSelection(categoryObj, choices, false))

            dispatch FetchTextAndTokenCounts
            dispatch CloseMetadataGeoMap

        Bulma.modal [ modal.isActive
                      // Set elementRef in order to apply the focusModal() function to this element
                      prop.ref elementRef
                      // Set tabIndex so that the element receives keyboard events
                      prop.tabIndex 0
                      prop.onKeyUp (fun e ->
                          if e.key = "Escape" then
                              dispatch CloseMetadataGeoMap)
                      prop.children [ Bulma.modalBackground [ prop.onClick (fun _ -> dispatch CloseMetadataGeoMap) ]
                                      Bulma.modalContent [ prop.style [ style.width 1440 ]
                                                           prop.children [ MetadataGeoMap
                                                                               coords
                                                                               config
                                                                               metadata
                                                                               okHandler
                                                                               (fun () -> dispatch CloseMetadataGeoMap) ] ]

                                      Bulma.modalClose [ button.isLarge
                                                         prop.onClick (fun _ -> dispatch CloseMetadataGeoMap) ] ] ]


    /// The main view of the metadata menu on the left hand side of the interface
    let view
        (model: LoadedCorpusModel)
        (loadedCorpusDispatch: Update.LoadedCorpus.Msg -> unit)
        (dispatch: Update.Metadata.Msg -> unit)
        =
        let voyantSelect =
            let info = model.Corpus.SharedInfo

            let selectOptions =
                [ Html.option [ prop.value ""
                                prop.text "Open in Voyant" ]
                  Html.option [ prop.value "word"
                                prop.text (
                                    if info.HasAttribute("phon") then
                                        "Orthographic"
                                    elif info.HasAttribute("orig") then
                                        "Corrected"
                                    else
                                        "Word form"
                                ) ]
                  if info.HasAttribute("phon") then
                      Html.option [ prop.value "phon"
                                    prop.text "Phonetic" ]
                  if info.HasAttribute("orig") then
                      Html.option [ prop.value "orig"
                                    prop.text "Original" ]
                  if info.HasAttribute("lemma") then
                      Html.option [ prop.value "lemma"
                                    prop.text "Lemmas" ] ]

            let dropdown =
                [ Bulma.select [ select.isSmall
                                 color.isInfo
                                 prop.value ""
                                 prop.onChange (fun (attr: string) ->
                                     if attr <> "" then
                                         dispatch (OpenInVoyant attr))
                                 prop.children selectOptions ]
                  Bulma.icon [ icon.isLeft
                               prop.children [ Html.i [ prop.className "fa fa-chart-simple"
                                                        prop.style [ style.color "#3e8ed0" ] ] ] ] ]

            Bulma.control.div [ control.hasIconsLeft
                                prop.style [ style.marginBottom 5 ]
                                prop.children (
                                    if
                                        info.HasAttribute("phon")
                                        || info.HasAttribute("orig")
                                        || info.HasAttribute("lemma")
                                    then
                                        dropdown
                                    else
                                        [ Bulma.button.button [ button.isSmall
                                                                button.isOutlined
                                                                color.isInfo
                                                                prop.style [ style.marginTop 5 ]
                                                                prop.title "Show selection"
                                                                prop.onClick (fun _ -> dispatch (OpenInVoyant "word"))
                                                                prop.children [ Html.span "Open in Voyant" ] ] ]
                                ) ]

        let menuItems =
            [ for item in model.Corpus.MetadataMenu do
                  match item with
                  | Section (state, title, items) ->
                      let startExpanded =
                          match state with
                          | Open -> true
                          | Closed -> false

                      Section
                          startExpanded
                          title
                          items
                          model.OpenMetadataCategoryCode
                          model.IntervalCategoryModes
                          model.Search.Params.MetadataSelection
                          model.FetchedMetadataValues
                          model.FetchedMinAndMax
                          dispatch
                  | CategoryMenu category ->
                      let isOpen = (Some category.Code = model.OpenMetadataCategoryCode)

                      match category with
                      | :? StringCategory as cat ->
                          stringSelect
                              cat
                              isOpen
                              model.Search.Params.MetadataSelection
                              model.FetchedMetadataValues
                              true
                              dispatch
                      | :? NumberCategory as cat ->
                          SelectOrInterval
                              cat
                              isOpen
                              (model.IntervalCategoryModes.TryFind(category.Code)
                               |> Option.defaultValue ListMode)
                              model.Search.Params.MetadataSelection
                              model.FetchedMetadataValues
                              model.FetchedMinAndMax
                              true
                              dispatch
                      | :? LongTextCategory as cat -> freeTextSearch cat dispatch
                      | cat -> failwith $"Unhandled category: {cat}" ]

        let selectionButtons =
            Bulma.buttons [ prop.style [ style.marginTop 5 ]
                            prop.children [ Bulma.button.button [ button.isSmall
                                                                  button.isOutlined
                                                                  color.isInfo
                                                                  prop.title "Show selection"
                                                                  prop.onClick (fun _ -> dispatch FetchMetadataForTexts)
                                                                  prop.children [ Bulma.icon [ Html.i [ prop.className [ "fa fa-binoculars" ] ] ]
                                                                                  Html.span "Show" ] ]
                                            if model.Corpus.SharedInfo.GeoMapConfig.IsSome then
                                                Bulma.button.button [ button.isSmall
                                                                      button.isOutlined
                                                                      color.isInfo
                                                                      prop.title "Show map"
                                                                      prop.onClick (fun _ ->
                                                                          dispatch FetchMetadataForGeoMap)
                                                                      prop.children [ Bulma.icon [ Html.i [ prop.className [ "fa fa-globe" ] ] ]
                                                                                      Html.span "Map" ] ]
                                            if model.Corpus.SharedInfo.ExternalTools
                                               |> List.contains Voyant then
                                                voyantSelect ] ]

        Html.span [ if model.IsMetadataGeoMapOpen then
                        match model.Corpus.SharedInfo.GeoMapConfig with
                        | Some config ->
                            match model.Corpus.SharedInfo.GoogleMapsApiKey with
                            | Some key ->
                                match model.Corpus.SharedInfo.GeoCoordinates with
                                | Some coords ->
                                    let metadata =
                                        [| for informants in model.FetchedTextMetadata ->
                                               [| for informantCategory in informants ->
                                                      if informantCategory = ""
                                                         || informantCategory = "NULL" then
                                                          "null"
                                                      else
                                                          informantCategory |] |]

                                    MetadataGeoMapModal
                                        key
                                        coords
                                        config
                                        metadata
                                        model.Corpus.MetadataQuickView
                                        dispatch
                                | None -> failwith "No geographical coordinates provided!"
                            | None ->
                                failwith
                                    "No Google Maps API key provided! Set it in the environment variable GOOGLE_MAPS_API_KEY."
                        | None -> Html.none
                    Html.div [ prop.style [ style.width 200
                                            style.paddingLeft (length.em 0.75)
                                            style.marginBottom (length.rem 0.75) ]
                               prop.children [ Html.text (textAndTokenCountText model)
                                               selectionButtons ] ]
                    if model.IsSelectionTableOpen then
                        SelectionTablePopup model loadedCorpusDispatch dispatch
                    Bulma.menu [ prop.style [ style.width 200
                                              style.overflowX.hidden ]
                                 prop.onClick (fun e ->
                                     // Prevent onclick handlers further up the hierarchy from closing this menu
                                     e.stopPropagation ())
                                 prop.children [ Bulma.menuList menuItems ] ]

                     ]
