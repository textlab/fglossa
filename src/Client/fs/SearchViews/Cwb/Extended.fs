module View.SearchViews.Cwb.Extended

open Feliz
open Feliz.Bulma
open Shared
open Model
open Update.LoadedCorpus

let view (corpus: Corpus) (search: Search) (dispatch: Msg -> unit) =
    let query =
        CwbExtended.Query.OfCqp(search.Params.Queries.[0].Query)

    Bulma.field.div [ field.hasAddons
                      prop.children [ Bulma.control.div [ prop.children [ Bulma.button.button [ Bulma.icon [ Html.i [ prop.className
                                                                                                                          "fas fa-list" ] ] ] ] ]
                                      Bulma.control.div [ prop.children [ Bulma.button.button [ Bulma.icon [ Html.i [ prop.className
                                                                                                                          "fas fa-chevron-down" ] ] ] ] ]
                                      Bulma.control.div [ prop.children [ Bulma.input.text [ prop.style [ style.width
                                                                                                              108 ] ] ] ] ] ]
