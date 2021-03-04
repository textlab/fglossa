module LoadedCorpus.View

open Elmish
open Feliz
open Feliz.Bulma
open Shared
open Model
open LoadedCorpus.Update

let topRowButtons =
    Bulma.buttons [ Bulma.button.button [ prop.text "Hide filters" ]
                    Bulma.button.button [ color.isInfo
                                          prop.text "Reset form" ] ]

module CorpusStartPage =
    let corpusNameBox config =
        let logo =
            match config.Logo with
            | Some logo -> Html.img [ prop.src $"corpora/%s{config.Code}/%s{logo}" ]
            | None -> Html.none

        Bulma.box [ prop.style [ style.padding 20 ]
                    prop.children [ Bulma.title config.Name
                                    logo ] ]

    let view (model: LoadedCorpusModel) (dispatch: Msg -> unit) =
        Html.span [ topRowButtons
                    corpusNameBox model.Corpus.Config ]
