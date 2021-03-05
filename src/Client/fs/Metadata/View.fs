module Metadata.View

open Fable.Core.JsInterop
open Feliz
open Feliz.Bulma
open Shared.Metadata
open Model
open Metadata.Update

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
    Html.div [ prop.style [ style.height (int props.style?height)
                            style.left (int props.style?left)
                            style.position.absolute
                            style.top (int props.style?top)
                            style.width (length.percent 100) ]
               prop.text props.index ]

// The components in react-window expect a `children` prop, which should be a
// component function or class. However, all React functions in Fable.React and
// Feliz (such as `ofImport`, `ofType`, and even `Fable.React.ReactBindings.React.createElement`)
// seem to automatically call createElement on (i.e. instantiate) whatever components we pass
// as their `children` arguments.
// Hence, the only solution I have found is to import React ourselves and and call
// React.createElement directly, since it allows us to provide function components as
// the `children` argument and leaves them uninstantiated.

let ReactBare : obj = importAll "react"

let fixedSizeList : ReactElement =
    ReactBare?createElement (FixedSizeList,
                             createObj [ "height" ==> 100
                                         "itemCount" ==> 10
                                         "itemSize" ==> 35
                                         "width" ==> 100 ],
                             ListItem)

let stringSelect (category: StringCategory) dispatch =
    Html.li (
        Html.a [ prop.key category.Code
                 prop.text category.Name
                 prop.onClick (fun _ -> dispatch (ToggleMetadataMenuOpen category)) ]
    )

let numberSelect (category: NumberCategory) dispatch =
    Html.li (
        Html.a [ prop.key category.Code
                 prop.text category.Name
                 prop.onClick (fun _ -> dispatch (ToggleMetadataMenuOpen category)) ]
    )

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
               Dispatch: (Msg -> unit) |})
    =
    let (isExpanded, setIsExpanded) = React.useState (props.StartExpanded)

    let children =
        props.Items
        |> List.map
            (fun item ->
                match item with
                | StringSelect category -> stringSelect category props.Dispatch
                | NumberSelect category -> numberSelect category props.Dispatch
                | Interval category -> interval category props.Dispatch
                | FreeTextSearch category -> freeTextSearch category props.Dispatch
                | Section _ -> failwith $"Sections are not allowed as children of other sections: {item}")

    Html.span [ if props.Title <> "" then
                    Bulma.menuLabel [ prop.style [ style.cursor "pointer"
                                                   style.marginTop 10
                                                   style.marginBottom 0 ]
                                      prop.onClick (fun _ -> setIsExpanded (not isExpanded))
                                      prop.children [ Html.text props.Title
                                                      Bulma.icon [ Html.i [ prop.className [ Icon.Fa
                                                                                             if isExpanded then
                                                                                                 Icon.FaAngleUp
                                                                                             else
                                                                                                 Icon.FaAngleDown ] ] ] ] ]
                if isExpanded then
                    Html.ul [ prop.className "menu-list"
                              prop.style [ style.borderLeft (1, borderStyle.solid, "#dbdbdb") ]
                              prop.children children ]

                    fixedSizeList ]

let menu (model: LoadedCorpusModel) dispatch =
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
                         Dispatch = dispatch |}
              | StringSelect category -> (stringSelect category dispatch)
              | NumberSelect category -> (numberSelect category dispatch)
              | Interval category -> (interval category dispatch)
              | FreeTextSearch category -> (freeTextSearch category dispatch) ]

    Html.span [ Bulma.block [ prop.style [ style.width sidebarWidth
                                           style.paddingLeft (length.em 0.75) ]
                              prop.text "All XXX texts (XXX tokens) selected" ]
                Bulma.menu [ prop.style [ style.width sidebarWidth
                                          style.overflowX.hidden ]
                             prop.children [ Bulma.menuList menuItems ] ] ]
