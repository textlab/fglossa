module Metadata

open Feliz
open Feliz.Bulma
open Shared.Metadata
open Model

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

module RW = Import.ReactWindow

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

                    RW.fixedSizeList ]

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
