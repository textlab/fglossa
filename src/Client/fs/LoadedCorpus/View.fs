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

let searchInterface (model: LoadedCorpusModel) dispatch =
    let simpleHeading = "Simple"
    let extendedHeading = "Extended"
    let cqpHeading = "CQP query"

    let link title (heading: string) ``interface`` =
        Html.a [ prop.href "#"
                 prop.title title
                 prop.onClick (fun _ -> dispatch (SetSearchInterface ``interface``))
                 prop.text heading ]

    let simpleLink = link "Simple search box" "Simple" Simple

    let extendedLink =
        link "Search for grammatical categories etc." "Extended" Extended

    let cqpLink = link "CQP expressions" "CQP query" Cqp

    let separator =
        Html.span [ prop.style [ style.whitespace.pre ]
                    prop.text " | " ]

    let links =
        match model.Search.Interface with
        | Simple ->
            [ Html.b simpleHeading
              separator
              extendedLink
              separator
              cqpLink ]
        | Extended ->
            [ simpleLink
              separator
              Html.b extendedHeading
              separator
              cqpLink ]
        | Cqp ->
            [ simpleLink
              separator
              extendedLink
              separator
              Html.b cqpHeading ]

    Html.div [ prop.style [ style.width 500 ]
               prop.children [ Bulma.level [ prop.style [ style.paddingTop 20 ]
                                             prop.children [ Bulma.levelLeft [ Bulma.levelItem links ]
                                                             Bulma.levelRight [ Bulma.button.button [ color.isSuccess
                                                                                                      prop.text "Search" ] ] ] ]
                               Bulma.field.div [ Bulma.control.div [ Bulma.input.search [] ] ]
                               Bulma.field.div [ Bulma.control.div [ Bulma.button.button "Or..." ] ] ] ]

module CorpusStartPage =
    let corpusNameBox config =
        let logo =
            match config.Logo with
            | Some logo -> Html.img [ prop.src $"corpora/{config.Code}/{logo}" ]
            | None -> Html.none

        Bulma.box [ prop.style [ style.padding 20 ]
                    prop.children [ Bulma.title config.Name
                                    logo ] ]

    let view (model: LoadedCorpusModel) (dispatch: LoadedCorpus.Update.Msg -> unit) =
        Html.span [ topRowButtons
                    corpusNameBox model.Corpus.Config
                    searchInterface model dispatch ]

let view (model: LoadedCorpusModel) (dispatch: LoadedCorpus.Update.Msg -> unit) =
    Html.span [ Bulma.section [ prop.style [ style.paddingTop (length.em 2.5) ]
                                prop.children [ Bulma.columns [ Bulma.column [ column.isNarrow
                                                                               prop.children [ Metadata.View.menu
                                                                                                   model
                                                                                                   (MetadataMsg
                                                                                                    >> dispatch) ] ]
                                                                Bulma.column [ CorpusStartPage.view model dispatch ] ] ] ] ]
