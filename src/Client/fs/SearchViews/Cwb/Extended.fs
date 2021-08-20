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

    let segmentType =
        match corpus.Config.Modality with
        | Spoken -> "Utterance"
        | Written -> "Sentence"

    let termView index term =
        [ if index > 0 then
              Bulma.column [ Bulma.field.div [ Bulma.input.text [] ] ]
          Bulma.column [ Bulma.field.div [ field.hasAddons
                                           if query.Terms.Length > 1 then
                                               field.hasAddonsRight
                                           prop.children [ Bulma.control.div [ prop.children [ Bulma.button.button [ Bulma.icon [ Html.i [ prop.className
                                                                                                                                               "fas fa-list" ] ] ] ] ]
                                                           Bulma.control.div [ prop.children [ Bulma.button.button [ Bulma.icon [ Html.i [ prop.className
                                                                                                                                               "fas fa-chevron-down" ] ] ] ] ]
                                                           Bulma.control.div [ prop.children [ Bulma.input.text [ prop.style [ style.width
                                                                                                                                   108 ] ] ] ]
                                                           if query.Terms.Length > 1 then
                                                               Bulma.control.div [ prop.children [ Bulma.button.button [ Bulma.icon [ Html.i [ prop.className
                                                                                                                                                   "fas fa-minus" ] ] ] ] ] ] ]
                         Bulma.field.div [ field.isGrouped
                                           field.isGroupedMultiline
                                           prop.children [ if hasLemma then
                                                               checkbox "Lemma" "Lemma"
                                                           checkbox "Start" "Start of word"
                                                           checkbox "End" "End of word"
                                                           checkbox "Middle" "Middle of word"
                                                           if hasOrig then
                                                               checkbox "Original" "Original form"
                                                           if index = 0 then
                                                               checkbox
                                                                   $"{segmentType} initial"
                                                                   $"{segmentType} initial"
                                                           elif index = query.Terms.Length - 1 then
                                                               // TODO: Add optional sentence final punctuation to query
                                                               // to make this work in written text as well
                                                               match corpus.Config.Modality with
                                                               | Spoken ->
                                                                   checkbox
                                                                       $"{segmentType} final"
                                                                       $"{segmentType} final"
                                                               | Written -> Html.none ] ] ]


          ]

    Bulma.columns [ prop.style [ style.marginBottom 30 ]
                    prop.children [ yield!
                                        [ for (index, term) in query.Terms |> Array.indexed do
                                              yield! termView index term ]
                                    Bulma.column [ Bulma.field.div [] ] ] ]
