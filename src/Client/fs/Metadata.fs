module Metadata

open Feliz
open Feliz.Bulma
open Shared.Metadata

let section (state: MenuState) (title: string) (items: MenuItem list) =
    let children =
        items
        |> List.map
            (fun item ->
                match item with
                | StringSelect category -> Html.li (Html.a [ prop.text category.Name ])
                | Section _ -> failwith $"Sections are now allowed as children of other sections: {item}"
                | _ -> Html.li "annet")

    [ Bulma.menuLabel title
      Bulma.menuList children ]

let menu model dispatch =
    Bulma.menu [ prop.style [ style.marginLeft 15 ]
                 prop.children (
                     section Closed "Heidu" [ StringSelect(Corpora.Bokmal.MetadataCategories.Tid "Tekst-ID") ]
                 ) ]
