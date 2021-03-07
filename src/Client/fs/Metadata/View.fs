module Metadata.View

open Fable.Core.JsInterop
open Feliz
open Feliz.Bulma
open Shared.Metadata
open Model
open Metadata.Update

[<ReactComponent>]
let MetadataSelectionPopup model dispatch =
    let iconButton iconClass =
        Bulma.button.button [ button.isSmall
                              prop.children [ Bulma.icon [ Html.i [ prop.className [ "fa"; iconClass ] ] ] ] ]

    let header =
        Bulma.level [ prop.style [ style.padding 20
                                   //    style.marginTop 10
                                   style.marginBottom 0 ]
                      prop.children [ Bulma.levelLeft [ Bulma.levelItem [ Bulma.subtitle
                                                                              "All 123 texts (123,456,789 tokens) selected" ] ]
                                      Bulma.levelRight [ Bulma.levelItem [ Bulma.buttons [ iconButton
                                                                                               "fa-angle-double-left"
                                                                                           iconButton "fa-angle-left" ] ]
                                                         Bulma.levelItem [ Bulma.input.number [ input.isSmall
                                                                                                prop.style [ style.width
                                                                                                                 60
                                                                                                             style.textAlign.right ]
                                                                                                prop.value 1 ] ]
                                                         Bulma.levelItem [ Bulma.buttons [ iconButton "fa-angle-right"
                                                                                           iconButton
                                                                                               "fa-angle-double-right" ] ]
                                                         Bulma.levelItem [ Bulma.delete [ delete.isMedium
                                                                                          prop.title "Close"
                                                                                          prop.style [ style.marginLeft
                                                                                                           40 ]
                                                                                          prop.onClick
                                                                                              (fun _ ->
                                                                                                  dispatch
                                                                                                      ToggleShowSelectionOpen) ] ] ] ] ]

    let table =
        Bulma.tableContainer [ Bulma.table [ table.isStriped
                                             table.isFullWidth
                                             prop.children [ Html.thead [ Html.tr [ for category in model.Corpus.MetadataTable ->
                                                                                        Html.th category.Name ] ] ] ] ]

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

let shouldShowMetadata (model: LoadedCorpusModel) =
    if model.Corpus.MetadataMenu.IsEmpty then
        // Don't show metadata if the corpus doesn't have any (duh!)
        false
    else
        match model.ShouldShowMetadata with
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
            | CorpusStartPage -> true
            | ShowingResults -> not model.IsNarrowWindow

let FixedSizeList : obj = importMember "react-window"

let ListItem (props: {| index: int; style: obj |}) =
    Html.div [ prop.className "metadata-menu-list-item"
               prop.style [ style.height (int props.style?height)
                            style.left (int props.style?left)
                            style.position.absolute
                            style.top (int props.style?top)
                            style.width (length.percent 100)
                            style.padding 6 ]
               prop.text $"Metadataverdi nr. %d{props.index}" ]

// The components in react-window expect a `children` prop, which should be a
// component function or class. However, all React functions in Fable.React and
// Feliz (such as `ofImport`, `ofType`, and even `Fable.React.ReactBindings.React.createElement`)
// seem to automatically call createElement on (i.e. instantiate) whatever components we pass
// as their `children` arguments.
// Hence, the only solution I have found is to import React ourselves and and call
// React.createElement directly, since it allows us to provide function components as
// the `children` argument and leaves them uninstantiated.

let ReactBare : obj = importAll "react"

let selectDropdown : ReactElement =
    ReactBare?createElement (FixedSizeList,
                             createObj [ "height" ==> 200
                                         "itemCount" ==> 10
                                         "itemSize" ==> 32
                                         "width" ==> 170 ],
                             ListItem)

let selectMenu (category: Category) showDropdown dispatch =
    Html.li [ Html.a [ prop.key category.Code
                       prop.text category.Name
                       prop.onClick (fun _ -> dispatch (ToggleMetadataMenuOpen category)) ]
              if showDropdown then
                  Html.div [ prop.style [ style.border (1, borderStyle.solid, "#aaa")
                                          style.borderRadius 4 ]
                             prop.children selectDropdown ] ]

let stringSelect (category: StringCategory) showDropdown dispatch =
    selectMenu category showDropdown dispatch

let numberSelect (category: NumberCategory) showDropdown dispatch =
    selectMenu category showDropdown dispatch

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

[<ReactComponent>]
let Section
    (props: {| StartExpanded: bool
               Title: string
               Items: MenuItem list
               OpenCategoryCode: string option
               Dispatch: (Msg -> unit) |})
    =
    let (isExpanded, setIsExpanded) = React.useState (props.StartExpanded)

    let children =
        props.Items
        |> List.map
            (fun item ->
                match item with
                | StringSelect category ->
                    stringSelect category (Some category.Code = props.OpenCategoryCode) props.Dispatch
                | NumberSelect category ->
                    numberSelect category (Some category.Code = props.OpenCategoryCode) props.Dispatch
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

let menu (model: LoadedCorpusModel) (dispatch: Metadata.Update.Msg -> unit) =
    let sidebarWidth =
        if shouldShowMetadata model then
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
                         Dispatch = dispatch |}
              | StringSelect category ->
                  stringSelect category (Some category.Code = model.OpenMetadataCategoryCode) dispatch
              | NumberSelect category ->
                  numberSelect category (Some category.Code = model.OpenMetadataCategoryCode) dispatch
              | Interval category -> (interval category dispatch)
              | FreeTextSearch category -> (freeTextSearch category dispatch) ]

    Html.span [ Html.div [ prop.style [ style.width sidebarWidth
                                        style.paddingLeft (length.em 0.75)
                                        style.marginBottom (length.rem 0.75) ]
                           prop.children [ Html.text "All 123 texts (123,456,789 tokens) selected"
                                           Bulma.button.button [ button.isSmall
                                                                 button.isOutlined
                                                                 color.isInfo
                                                                 prop.title "Show selection"
                                                                 prop.style [ style.marginLeft 10 ]
                                                                 prop.onClick
                                                                     (fun _ -> dispatch ToggleShowSelectionOpen)
                                                                 prop.children [ Bulma.icon [ Html.i [ prop.className [ "fa fa-binoculars" ] ] ] ] ] ] ]
                MetadataSelectionPopup model dispatch
                Bulma.menu [ prop.style [ style.width sidebarWidth
                                          style.overflowX.hidden ]
                             prop.children [ Bulma.menuList menuItems ] ] ]
