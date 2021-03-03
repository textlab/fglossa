module Metadata

open Fable.Core.JsInterop
open Feliz
open Feliz.Bulma
open Shared.Metadata
open Model

let FixedSizeList: obj = importMember "react-window"

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

let ReactBare: obj = importAll "react"

let fixedSizeList: ReactElement =
    ReactBare?createElement (FixedSizeList,
                             createObj [ "height" ==> 100
                                         "itemCount" ==> 10
                                         "itemSize" ==> 35
                                         "width" ==> 100 ],
                             ListItem)

let stringSelect (category: StringCategory) =
    Html.li (
        Html.a [ prop.key category.Code
                 prop.text category.Name ]
    )

let numberSelect (category: NumberCategory) =
    Html.li (
        Html.a [ prop.key category.Code
                 prop.text category.Name ]
    )

let interval (category: NumberCategory) =
    Html.li (
        Html.a [ prop.key category.Code
                 prop.text category.Name ]
    )

let freeTextSearch (category: LongTextCategory) =
    Html.li (
        Html.a [ prop.key category.Code
                 prop.text category.Name ]
    )

[<ReactComponent>]
let Section
    (props: {| StartExpanded: bool
               Title: string
               Items: MenuItem list |})
    =
    let (isExpanded, setIsExpanded) = React.useState (props.StartExpanded)

    let children =
        props.Items
        |> List.map
            (fun item ->
                match item with
                | StringSelect category -> stringSelect category
                | NumberSelect category -> numberSelect category
                | Interval category -> interval category
                | FreeTextSearch category -> freeTextSearch category
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
                         Items = items |}
              | StringSelect category -> (stringSelect category)
              | NumberSelect category -> (numberSelect category)
              | Interval category -> (interval category)
              | FreeTextSearch category -> (freeTextSearch category) ]

    Bulma.menu [ Bulma.menuList [ prop.style [ style.marginLeft 15 ]
                                  prop.children menuItems ] ]
