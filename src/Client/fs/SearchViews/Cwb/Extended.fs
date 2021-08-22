module View.SearchViews.Cwb.Extended

open System
open Feliz
open Feliz.Bulma
open Shared
open Model
open CwbExtended
open Update.LoadedCorpus

let view (corpus: Corpus) (search: Search) (dispatch: Msg -> unit) =
    let query =
        if search.Params.Queries.Length > 0 then
            Query.OfCqp(corpus, search.Params.Queries.[0].QueryString)
        else
            Query.Default

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

    let termView termIndex (term: QueryTerm) =
        let minMaxInput (minMax: MinMax) =
            Bulma.control.div (
                Bulma.input.text [ prop.className "has-text-right"
                                   prop.style [ style.width 38 ]
                                   prop.value (
                                       match term.PrecedingInterval with
                                       | Some interval ->
                                           let valueStr value =
                                               value
                                               |> Option.map string
                                               |> Option.defaultValue ""

                                           match minMax with
                                           | Min -> valueStr interval.Min
                                           | Max -> valueStr interval.Max
                                       | None -> ""
                                   )
                                   prop.onChange
                                       (fun (s: string) ->
                                           if s = "" then
                                               dispatch (
                                                   CwbExtendedSetIntervalValue(query, 0, term, termIndex, minMax, None)
                                               )

                                           match Int32.TryParse(s) with
                                           | (true, v) ->
                                               dispatch (
                                                   CwbExtendedSetIntervalValue(
                                                       query,
                                                       0,
                                                       term,
                                                       termIndex,
                                                       minMax,
                                                       Some v
                                                   )
                                               )
                                           | (false, _) -> ignore None) ]
            )

        let mainStringInput =
            Bulma.input.text [ prop.value (term.MainStringValue |> Option.defaultValue "")
                               prop.onChange
                                   (fun (s: string) ->
                                       let v = if s = "" then None else Some s

                                       dispatch (CwbExtendedSetMainString(query, 0, term, termIndex, v)))
                               prop.style [ style.width 108 ] ]

        let checkbox label title isChecked (property: QueryProperty) =
            Html.label [ prop.title title
                         prop.style [ style.marginRight 15 ]
                         prop.children [ Bulma.input.checkbox [ prop.isChecked isChecked
                                                                prop.onCheckedChange
                                                                    (fun isChecked ->
                                                                        dispatch (
                                                                            CwbExtendedSetQueryProperty(
                                                                                query,
                                                                                0,
                                                                                term,
                                                                                termIndex,
                                                                                property,
                                                                                isChecked
                                                                            )
                                                                        )) ]
                                         Bulma.text.span $" {label}" ] ]

        let removeTermButton =
            Bulma.button.button [ prop.onClick (fun _ -> dispatch (CwbExtendedRemoveTerm(query, 0, termIndex)))
                                  prop.children [ Bulma.icon [ Html.i [ prop.className "fas fa-minus" ] ] ] ]

        [ if termIndex > 0 then
              let minMaxField (minMax: MinMax) =
                  Bulma.field.div [ field.isGrouped
                                    prop.className "is-align-items-center"
                                    prop.children [ minMaxInput (minMax)
                                                    Bulma.control.div (
                                                        Bulma.text.div (
                                                            match minMax with
                                                            | Min -> "min"
                                                            | Max -> "max"
                                                        )
                                                    ) ] ]

              Bulma.column [ minMaxField Min
                             minMaxField Max ]
          Bulma.column [ Bulma.field.div [ field.hasAddons
                                           if query.Terms.Length > 1 then
                                               field.hasAddonsRight
                                           prop.children [ Bulma.control.div (
                                                               Bulma.button.button [ Bulma.icon [ Html.i [ prop.className
                                                                                                               "fas fa-list" ] ] ]
                                                           )
                                                           Bulma.control.div (
                                                               Bulma.button.button [ Bulma.icon [ Html.i [ prop.className
                                                                                                               "fas fa-chevron-down" ] ] ]
                                                           )
                                                           Bulma.control.div (mainStringInput)
                                                           if query.Terms.Length > 1 then
                                                               Bulma.control.div (removeTermButton) ] ]
                         Bulma.field.div [ field.isGrouped
                                           field.isGroupedMultiline
                                           prop.children [ if hasLemma then
                                                               checkbox "Lemma" "Lemma" term.IsLemma IsLemma
                                                           checkbox "Start" "Start of word" term.IsStart IsStart
                                                           checkbox "End" "End of word" term.IsEnd IsEnd
                                                           checkbox "Middle" "Middle of word" term.IsMiddle IsMiddle
                                                           if hasOrig then
                                                               checkbox
                                                                   "Original"
                                                                   "Original form"
                                                                   term.IsOriginal
                                                                   IsOriginal
                                                           if termIndex = 0 then
                                                               checkbox
                                                                   $"{segmentType} initial"
                                                                   $"{segmentType} initial"
                                                                   term.IsInitial
                                                                   IsInitial
                                                           elif termIndex = query.Terms.Length - 1 then
                                                               // TODO: Add optional sentence final punctuation to query
                                                               // to make this work in written text as well
                                                               match corpus.Config.Modality with
                                                               | Spoken ->
                                                                   checkbox
                                                                       $"{segmentType} final"
                                                                       $"{segmentType} final"
                                                                       term.IsFinal
                                                                       IsFinal
                                                               | Written -> Html.none ] ] ]


          ]

    Bulma.columns [ prop.style [ style.marginBottom 30 ]
                    prop.children [ yield!
                                        [ for (index, term) in query.Terms |> Array.indexed do
                                              yield! termView index term ]
                                    Bulma.column [ Bulma.field.div (
                                                       Bulma.control.div (
                                                           Bulma.button.button [ color.isInfo
                                                                                 prop.onClick
                                                                                     (fun _ ->
                                                                                         dispatch (
                                                                                             CwbExtendedAddTerm(
                                                                                                 query,
                                                                                                 0
                                                                                             )
                                                                                         ))
                                                                                 prop.children [ Bulma.icon [ Html.i [ prop.className
                                                                                                                           "fas fa-plus" ] ] ] ]
                                                       )
                                                   ) ] ] ]
