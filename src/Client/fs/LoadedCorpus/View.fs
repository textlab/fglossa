module LoadedCorpus.View

open Elmish
open Feliz
open Feliz.Bulma
open Model
open LoadedCorpus.Update

let topRowButtons =
    Bulma.field.div [ field.isGrouped
                      prop.children [ Bulma.control.div [ Bulma.button.button [ prop.text "Hide filters" ] ]
                                      Bulma.control.div [ Bulma.button.button [ color.isInfo
                                                                                prop.text "Reset form" ] ] ] ]

let corpusStartPage (model: LoadedCorpusModel) (dispatch: Msg -> unit) = topRowButtons
