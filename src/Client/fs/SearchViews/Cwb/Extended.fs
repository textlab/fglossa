module View.SearchViews.Cwb.Extended

open Feliz
open Feliz.Bulma
open Shared
open Model
open Update.LoadedCorpus

let view (corpus: Corpus) (search: Search) (dispatch: Msg -> unit) =
    let query =
        if search.Params.Queries.Length > 0 then
            CwbExtended.Query.OfCqp(search.Params.Queries.[0].Query)
        else
            CwbExtended.Query.Default

    let checkbox label title =
        Html.label [ prop.title title
                     prop.style [ style.marginRight 15 ]
                     prop.children [ Bulma.input.checkbox []
                                     Bulma.text.span $" {label}" ] ]

    let (hasLemma, hasOrig) =
        match corpus.Config.LanguageConfig with
        | Monolingual (Some attributes) ->
            let hasAttr attrCode =
                attributes
                |> List.exists (fun a -> a.Code = attrCode)

            (hasAttr "lemma", hasAttr "orig")
        | _ -> (false, false)

    let termView index term =
        Bulma.column [ Bulma.field.div [ field.hasAddons
                                         prop.children [ Bulma.control.div [ prop.children [ Bulma.button.button [ Bulma.icon [ Html.i [ prop.className
                                                                                                                                             "fas fa-list" ] ] ] ] ]
                                                         Bulma.control.div [ prop.children [ Bulma.button.button [ Bulma.icon [ Html.i [ prop.className
                                                                                                                                             "fas fa-chevron-down" ] ] ] ] ]
                                                         Bulma.control.div [ prop.children [ Bulma.input.text [ prop.style [ style.width
                                                                                                                                 108 ] ] ] ] ] ]
                       Bulma.field.div [ field.isGrouped
                                         field.isGroupedMultiline
                                         prop.children [ if hasLemma then
                                                             checkbox "Lemma" "Lemma"
                                                         checkbox "Start" "Start of word"
                                                         checkbox "End" "End of word"
                                                         checkbox "Middle" "Middle of word"
                                                         if hasOrig then
                                                             checkbox "Original" "Original form" ] ] ]

    Bulma.columns [ prop.style [ style.marginBottom 30 ]
                    prop.children [ yield! query.Terms |> Array.mapi termView
                                    Bulma.column [ Bulma.field.div [] ] ] ]
