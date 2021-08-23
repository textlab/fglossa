module View.Metadata

open Fable.Core.JsInterop
open Feliz
open Feliz.Bulma
open Shared.Metadata
open Model
open Update.Metadata
open Common

let fetchedMetadataValues =
    [| { Name = "Adresseavisen"
         Value = "Adresseavisen" }
       { Name = "Aftenposten"
         Value = "Aftenposten" }
       { Name = "Bellona"; Value = "Bellona" }
       { Name = "Bergens Tidende"
         Value = "Bergens Tidende" }
       { Name = "Dagbladet"
         Value = "Dagbladet" }
       { Name = "Det Nye"; Value = "Det Nye" }
       { Name = "Familien"
         Value = "Familien" }
       { Name = "Filmmagasinet"
         Value = "Filmmagasinet" }
       { Name = "Gateavisa"
         Value = "Gateavisa" }
       { Name = "KK"; Value = "KK" } |]

let dummyMetadataTableData =
    [| "SA00BaJo04"
       "Årboka Litteratur for barn og unge 2004: Nyskapende faglitteratur - to fulltreffere og ett godt forsøk"
       "Årboka Litteratur for barn og unge 2004"
       "SA"
       "SA00"
       "Samlaget"
       "2004"
       "Oslo"
       "original"
       "HUM%: Humaniora"
       "HUM02"
       "Bakken, Jonas"
       "M"
       "1976" |]
    |> Array.create 25

module SelectionTable =
    [<ReactComponent>]
    let SelectionTablePopup model dispatch =
        let pagination =
            [ Bulma.levelItem [ Bulma.buttons [ iconButton "fa-angle-double-left" false (fun e -> ())
                                                iconButton "fa-angle-left" false (fun e -> ()) ] ]
              Bulma.levelItem [ Bulma.input.number [ input.isSmall
                                                     prop.style [ style.width 60
                                                                  style.textAlign.right ]
                                                     prop.value 1
                                                     prop.onChange (fun (s: string) -> printfn $"New value: {s}") ] ]
              Bulma.levelItem [ Bulma.buttons [ iconButton "fa-angle-right" false (fun e -> ())
                                                iconButton "fa-angle-double-right" false (fun e -> ()) ] ]
              Bulma.levelItem [ Bulma.delete [ delete.isMedium
                                               prop.title "Close"
                                               prop.style [ style.marginLeft 40 ]
                                               prop.onClick (fun _ -> dispatch ToggleShowSelectionOpen) ] ] ]

        let header =
            Bulma.level [ prop.style [ style.padding 20
                                       style.marginBottom 0 ]
                          prop.children [ Bulma.levelLeft [ Bulma.levelItem [ Bulma.subtitle
                                                                                  "All 123 texts (123,456,789 tokens) selected" ] ]
                                          Bulma.levelRight pagination ] ]

        let table =
            Bulma.tableContainer [ Bulma.table [ table.isStriped
                                                 table.isFullWidth
                                                 prop.children [ Html.thead [ Html.tr [ for category in
                                                                                            model.Corpus.MetadataTable ->
                                                                                            Html.th category.Name ] ]
                                                                 Html.tbody [ for row in dummyMetadataTableData ->
                                                                                  Html.tr [ for column in row ->
                                                                                                Html.td column ] ] ] ] ]

        let popup =
            Html.div [ prop.style [ style.height (
                                        if model.IsShowSelectionOpen then
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
                       prop.children [ header; table ] ]

        let root =
            Browser.Dom.document.getElementById ("metadata-selection-popup-root")

        ReactDOM.createPortal (popup, root)

module MetadataMenu =
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

    let selectDropdown category categorySelection (dispatch: Msg -> unit) : ReactElement =
        /// A single item in a metadata category dropdown list
        let ListItem (props: {| index: int; style: obj |}) =
            let selectOption = fetchedMetadataValues.[props.index]

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
                       prop.onClick (fun _ -> dispatch (SelectItem(category, selectOption)))
                       prop.text selectOption.Name ]

        ReactBare?createElement (FixedSizeList,
                                 createObj [ "height" ==> 200
                                             "itemCount" ==> 10
                                             "itemSize" ==> 32
                                             "width" ==> 170 ],
                                 ListItem)

    let metadataSelect (category: Category) isOpen (metadataSelection: Shared.Metadata.Selection) dispatch =
        let categorySelection =
            metadataSelection.TryFind(category.Code)
            |> Option.defaultValue CategorySelection.Default

        Html.li [ Html.a [ prop.key category.Code
                           if isOpen then
                               prop.style [ style.lineHeight (length.em 1.7) ]
                           prop.onClick (fun _ -> dispatch (ToggleMetadataMenuOpen category))
                           prop.children [ Html.text category.Name
                                           if isOpen then
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
                                                                     prop.children [ Bulma.icon [ Html.i [ prop.className
                                                                                                               "fa fa-times" ] ] ] ] ] ]
                  if isOpen then
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
                                                           Html.span choice.Name ] ] ]

                      // The box containing already selected values
                      Html.div [ prop.className "metadata-menu-selection"
                                 prop.children choices ]

                      // The menu dropdown
                      Html.div [ prop.className "metadata-menu-list-container"
                                 prop.children [ selectDropdown category categorySelection dispatch ] ] ]

    let stringSelect (category: StringCategory) isOpen metadataSelection dispatch =
        metadataSelect category isOpen metadataSelection dispatch

    let numberSelect (category: NumberCategory) isOpen metadataSelection dispatch =
        metadataSelect category isOpen metadataSelection dispatch

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

                        stringSelect category isOpen props.MetadataSelection props.Dispatch
                    | NumberSelect category ->
                        let isOpen =
                            (Some category.Code = props.OpenCategoryCode)

                        numberSelect category isOpen props.MetadataSelection props.Dispatch
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
                170
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
                             Dispatch = dispatch |}
                  | StringSelect category ->
                      let isOpen =
                          (Some category.Code = model.OpenMetadataCategoryCode)

                      stringSelect category isOpen model.Search.MetadataSelection dispatch
                  | NumberSelect category ->
                      let isOpen =
                          (Some category.Code = model.OpenMetadataCategoryCode)

                      numberSelect category isOpen model.Search.MetadataSelection dispatch
                  | Interval category -> (interval category dispatch)
                  | FreeTextSearch category -> (freeTextSearch category dispatch) ]

        let showSelectionButton =
            Bulma.button.button [ button.isSmall
                                  button.isOutlined
                                  color.isInfo
                                  prop.title "Show selection"
                                  prop.style [ style.marginLeft 10 ]
                                  prop.onClick (fun _ -> dispatch ToggleShowSelectionOpen)
                                  prop.children [ Bulma.icon [ Html.i [ prop.className [ "fa fa-binoculars" ] ] ]
                                                  Html.span "Show" ] ]

        Html.span [ Html.div [ prop.style [ style.width sidebarWidth
                                            style.paddingLeft (length.em 0.75)
                                            style.marginBottom (length.rem 0.75) ]
                               prop.children [ Html.text "All 123 texts (123,456,789 tokens) selected"
                                               showSelectionButton ] ]
                    SelectionTable.SelectionTablePopup model dispatch
                    Bulma.menu [ prop.style [ style.width sidebarWidth
                                              style.overflowX.hidden ]
                                 prop.children [ Bulma.menuList menuItems ] ]

                     ]
