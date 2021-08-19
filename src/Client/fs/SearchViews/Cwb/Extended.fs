module View.SearchViews.Cwb.Extended

open Feliz
open Feliz.Bulma
open Shared
open Model
open Update.LoadedCorpus

let view (corpus: Corpus) (search: Search) (dispatch: Msg -> unit) =
    let query =
        CwbExtended.Query.OfCqp(search.Params.Queries.[0].Query)

    let checkbox label =
        Html.label [ prop.className "chekbox"
                     prop.style [ style.marginRight 15 ]
                     prop.children [ Bulma.input.checkbox []
                                     Bulma.text.span $" {label}" ] ]

    let hasLemma =
        match corpus.Config.LanguageConfig with
        | Monolingual (Some attributes) ->
            attributes
            |> List.exists (fun a -> a.Code = "lemma")
        | _ -> false

    Bulma.columns [ prop.style [ style.marginBottom 30 ]
                    prop.children [ Bulma.column [ Bulma.field.div [ field.hasAddons
                                                                     prop.children [ Bulma.control.div [ prop.children [ Bulma.button.button [ Bulma.icon [ Html.i [ prop.className
                                                                                                                                                                         "fas fa-list" ] ] ] ] ]
                                                                                     Bulma.control.div [ prop.children [ Bulma.button.button [ Bulma.icon [ Html.i [ prop.className
                                                                                                                                                                         "fas fa-chevron-down" ] ] ] ] ]
                                                                                     Bulma.control.div [ prop.children [ Bulma.input.text [ prop.style [ style.width
                                                                                                                                                             108 ] ] ] ] ] ]
                                                   Bulma.field.div [ field.isGrouped
                                                                     field.isGroupedMultiline
                                                                     prop.children [ if hasLemma then checkbox "Lemma"
                                                                                     checkbox "Start"
                                                                                     checkbox "End"
                                                                                     checkbox "Middle" ] ] ]
                                    Bulma.column [ Bulma.field.div [] ] ] ]
