module LoadedCorpus.View

open Elmish
open Feliz
open Feliz.Bulma
open Model
open LoadedCorpus.Update

let topRowButtons =
    Bulma.buttons [ Bulma.button.button [ prop.text "Hide filters" ]
                    Bulma.button.button [ color.isInfo
                                          prop.text "Reset form" ] ]

let corpusStartPage (model: LoadedCorpusModel) (dispatch: Msg -> unit) = topRowButtons
