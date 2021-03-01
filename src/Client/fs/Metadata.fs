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

let section (state: MenuState) (title: string) (items: MenuItem list) =
    let children =
        items
        |> List.map
            (fun item ->
                match item with
                | StringSelect category -> stringSelect category
                | NumberSelect category -> numberSelect category
                | Interval category -> interval category
                | FreeTextSearch category -> freeTextSearch category
                | Section _ -> failwith $"Sections are not allowed as children of other sections: {item}")

    [ if title <> "" then
          Bulma.menuLabel title
      Html.ul [ prop.className "menu-list"
                prop.style [ style.borderLeft (1, borderStyle.solid, "#dbdbdb") ]
                prop.children children ] ]

let menu (model: LoadedCorpusModel) dispatch =
    let menuItems =
        [ for item in model.Corpus.MetadataMenu do
              match item with
              | Section (state, title, items) -> yield! section state title items
              | StringSelect category -> yield (stringSelect category)
              | NumberSelect category -> yield (numberSelect category)
              | Interval category -> yield (interval category)
              | FreeTextSearch category -> yield (freeTextSearch category) ]

    Bulma.menu [ Bulma.menuList [ prop.style [ style.marginLeft 15 ]
                                  prop.children menuItems ] ]
