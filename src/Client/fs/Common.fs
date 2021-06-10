module Common

open Feliz
open Feliz.Bulma

let iconButton iconClass =
    Bulma.button.button [ button.isSmall
                          prop.children [ Bulma.icon [ Html.i [ prop.className [ "fa"; iconClass ] ] ] ] ]
