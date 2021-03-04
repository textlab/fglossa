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

module CorpusStartPage =
    let corpusNameBox model =
        let logo =
            match model.Corpus.Config.Logo with
            | Some logo -> Html.img [ prop.src logo ]
            | None -> Html.none

        Bulma.box [ prop.style [ style.padding 20 ]
                    prop.children [ Bulma.title model.Corpus.Config.Name
                                    logo ] ]

    let view (model: LoadedCorpusModel) (dispatch: Msg -> unit) =
        Html.span [ topRowButtons
                    corpusNameBox model ]
