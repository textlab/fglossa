module View.SearchViews.Cwb.Extended

open System
open Feliz
open Feliz.Bulma
open Shared
open Shared.StringUtils
open Model
open CwbExtended
open Update.LoadedCorpus

let includeExcludeTags (query: Query) queryIndex (term: QueryTerm) termIndex dispatch =
    [ for forms in term.ExtraForms do
          let attrStr =
              if [| "lemma"; "phon"; "orig" |]
                 |> Array.contains forms.Attr then
                  $"{forms.Attr}:"
              else
                  ""

          yield!
              [ for value in forms.Values ->
                    Bulma.tag [ if forms.Operator = NotEquals then
                                    color.isDanger
                                else
                                    color.isInfo
                                prop.children [ Html.span $"{attrStr}{value}"
                                                Html.button [ prop.onClick (fun _ ->
                                                                  dispatch (
                                                                      CwbExtendedRemoveExtraForms(
                                                                          query,
                                                                          queryIndex,
                                                                          term,
                                                                          termIndex,
                                                                          forms.Attr
                                                                      )
                                                                  ))
                                                              prop.className "delete is-small" ] ] ] ] ]

[<ReactComponent>]
let AttributeModal
    (
        corpus: Corpus,
        modalModel: AttributeModalModel,
        query: Query,
        queryIndex: int,
        term: QueryTerm,
        termIndex: int,
        dispatch: Msg -> unit
    ) : ReactElement =

    let mainCategoryButtons
        queryIndex
        sectionIndex
        (menuSectionCategories: CwbAttributeMenu.MainCategoryValue list)
        (termSectionSelection: Set<MainCategory>)
        =
        Bulma.buttons [ for (attr, attrValue, humanReadableName, _) in menuSectionCategories ->
                            let isSelected, isExcluded =
                                termSectionSelection
                                |> Set.toList
                                |> List.tryPick (fun selectedMainCat ->
                                    let isSel =
                                        selectedMainCat.Attr = attr.Code
                                        && selectedMainCat.Value = attrValue

                                    if isSel then
                                        Some(selectedMainCat.Operator = NotEquals)
                                    else
                                        None)
                                |> function
                                    | Some isExcl -> (true, isExcl)
                                    | None -> (false, false)

                            Bulma.button.button [ if isExcluded then color.isDanger
                                                  elif isSelected then color.isSuccess
                                                  prop.onClick (fun e ->
                                                      let category =
                                                          { Attr = attr.Code
                                                            Operator = if e.altKey then NotEquals else Equals
                                                            Value = attrValue
                                                            Subcategories = None }

                                                      dispatch (
                                                          CwbExtendedToggleAttributeCategory(
                                                              query,
                                                              queryIndex,
                                                              term,
                                                              termIndex,
                                                              sectionIndex,
                                                              category
                                                          )
                                                      ))
                                                  prop.text humanReadableName ] ]

    let subcategoryButtons
        sectionIndex
        (selectedMainCategory: MainCategory)
        (subcategories: CwbAttributeMenu.Subcategory list)
        =
        let buttonList (subcatValues: CwbAttributeMenu.SubcategoryValue list) =
            [ for ((attr: Cwb.PositionalAttribute), attrValue, humanReadableName) in subcatValues ->
                  let subCategory =
                      let attrWithoutValues =
                          { Attr = attr.Code
                            Operator = AttrOperator.OfString("=")
                            Values = Set.empty }

                      match selectedMainCategory.Subcategories with
                      | Some subcats ->
                          subcats
                          |> Set.toArray
                          |> Array.tryFind (fun subcat -> subcat.Attr = attr.Code)
                          |> function
                              | Some subcat -> subcat
                              | None -> attrWithoutValues
                      | None -> attrWithoutValues

                  let isSelected = subCategory.Values.Contains(attrValue)

                  Bulma.button.button [ if isSelected then color.isSuccess
                                        prop.onClick (fun _ ->
                                            let newSubcategory =
                                                if isSelected then
                                                    { subCategory with Values = subCategory.Values.Remove(attrValue) }
                                                else
                                                    { subCategory with Values = subCategory.Values.Add(attrValue) }

                                            dispatch (
                                                CwbExtendedToggleAttributeSubcategory(
                                                    query,
                                                    queryIndex,
                                                    term,
                                                    termIndex,
                                                    sectionIndex,
                                                    selectedMainCategory,
                                                    newSubcategory
                                                )
                                            ))
                                        prop.text humanReadableName ] ]

        Bulma.columns (
            Bulma.column [ for (heading, subcatValues) in subcategories ->
                               Bulma.level [ Bulma.levelLeft [ Bulma.levelItem (
                                                                   Bulma.title [ title.is6
                                                                                 prop.text (heading + ":") ]
                                                               )
                                                               Bulma.levelItem (Bulma.buttons (buttonList subcatValues)) ] ] ]
        )

    let includeExcludeOptions =
        let hasLemma = corpus.SharedInfo.HasAttribute("lemma")
        let hasPhon = corpus.SharedInfo.HasAttribute("phon")
        let hasOrig = corpus.SharedInfo.HasAttribute("orig")

        let wordName = if hasOrig then "corrected" else "word"

        [ Html.option [ prop.value "default"
                        prop.text "Specify/exclude" ]
          Html.option [ prop.value "specify_word"
                        prop.text $"Specify {wordName} form" ]
          if hasLemma then
              Html.option [ prop.value "specify_lemma"
                            prop.text $"Specify lemma" ]
          if hasPhon then
              Html.option [ prop.value "specify_phon"
                            prop.text $"Specify phonetic form" ]
          if hasOrig then
              Html.option [ prop.value "specify_orig"
                            prop.text $"Specify original form" ]
          Html.option [ prop.value "exclude_word"
                        prop.text $"Exclude {wordName} form" ]
          if hasLemma then
              Html.option [ prop.value "exclude_lemma"
                            prop.text $"Exclude lemma" ]
          if hasPhon then
              Html.option [ prop.value "exclude_phon"
                            prop.text $"Exclude phonetic form" ]
          if hasOrig then
              Html.option [ prop.value "exclude_orig"
                            prop.text $"Exclude original form" ] ]

    let attrMenu =
        match corpus.CwbAttributeMenu with
        | Some menuSections ->
            List.zip menuSections term.CategorySections
            |> List.mapi (fun sectionIndex (menuSection, termSectionSelection) ->
                let subcategoryPanels =
                    [ for (attr, attrValue, humanReadableName, subcategories) in menuSection.Values do
                          if subcategories.IsEmpty then
                              Html.none
                          else
                              termSectionSelection
                              |> Set.toArray
                              |> Array.tryFind (fun cat ->
                                  cat.Attr = attr.Code
                                  && cat.Value = attrValue
                                  && cat.Operator <> NotEquals)
                              |> function
                                  // If the main category has been selected, we show all its subcategories
                                  | Some cat ->
                                      let heading =
                                          menuSection.SubcategoryHeading.Replace("@category", humanReadableName)

                                      Bulma.message [ color.isInfo
                                                      prop.children [ Bulma.messageHeader [ Html.p heading ]
                                                                      Bulma.messageBody (
                                                                          subcategoryButtons
                                                                              sectionIndex
                                                                              cat
                                                                              subcategories
                                                                      ) ] ]
                                  | None -> Html.none ]

                Html.span [ Bulma.message [ color.isInfo
                                            prop.children [ Bulma.messageHeader [ Html.p menuSection.Heading ]
                                                            Bulma.messageBody [ mainCategoryButtons
                                                                                    queryIndex
                                                                                    sectionIndex
                                                                                    menuSection.Values
                                                                                    termSectionSelection ] ] ]
                            yield! subcategoryPanels
                            Bulma.level [ Bulma.levelLeft [ Bulma.levelItem [ Bulma.input.text [ prop.style [ style.width
                                                                                                                  200 ]
                                                                                                 prop.value
                                                                                                     modalModel.IncludeExcludeInput
                                                                                                 prop.onChange (
                                                                                                     CwbExtendedSetExtraForm
                                                                                                     >> dispatch
                                                                                                 ) ] ]
                                                            Bulma.levelItem [ Bulma.select [ prop.disabled (
                                                                                                 modalModel.IncludeExcludeInput =
                                                                                                     ""
                                                                                             )
                                                                                             prop.value "default"
                                                                                             prop.onChange

                                                                                                 (fun (command: string) ->
                                                                                                     dispatch (
                                                                                                         CwbExtendedIncludeOrExcludeExtraForm(
                                                                                                             query,
                                                                                                             queryIndex,
                                                                                                             term,
                                                                                                             termIndex,
                                                                                                             command
                                                                                                         )
                                                                                                     ))
                                                                                             prop.children
                                                                                                 includeExcludeOptions ] ]
                                                            Bulma.levelItem [ Bulma.tags (
                                                                                  includeExcludeTags
                                                                                      query
                                                                                      0
                                                                                      term
                                                                                      termIndex
                                                                                      dispatch
                                                                              ) ] ] ]
                            Bulma.level [ Bulma.levelLeft (
                                              Bulma.levelItem [ prop.text
                                                                    "Click to select; alt/option + click to exclude" ]
                                          )
                                          Bulma.levelRight (
                                              Bulma.buttons [ Bulma.button.button [ color.isDanger
                                                                                    prop.onClick (fun _ ->
                                                                                        dispatch (
                                                                                            CwbExtendedClearAttributeCategories(
                                                                                                query,
                                                                                                queryIndex,
                                                                                                term,
                                                                                                termIndex
                                                                                            )
                                                                                        ))
                                                                                    prop.text "Clear" ]
                                                              Bulma.button.button [ color.isSuccess
                                                                                    prop.onClick (fun _ ->
                                                                                        dispatch Search)
                                                                                    prop.text "Search" ]
                                                              Bulma.button.button [ color.isInfo
                                                                                    prop.onClick (fun _ ->
                                                                                        dispatch (
                                                                                            CwbExtendedToggleAttrModal
                                                                                                None
                                                                                        ))
                                                                                    prop.text "Close" ] ]
                                          ) ] ])
        | None -> []

    let elementRef = React.useElementRef ()

    let focusModal () =
        elementRef.current
        |> Option.iter (fun modalElement -> modalElement.focus ())

    // Focus the modal when mounted to enable it to receive keyboard events
    React.useEffectOnce (focusModal)

    Bulma.modal [ modal.isActive
                  // Set elementRef in order to apply the focusModal() function to this element
                  prop.ref elementRef
                  // Set tabIndex so that the lement receives keyboard events
                  prop.tabIndex 0
                  prop.onKeyUp (fun e ->
                      if e.key = "Escape" then
                          dispatch (CwbExtendedToggleAttrModal None))
                  prop.children [ Bulma.modalBackground [ prop.onClick (fun _ ->
                                                              dispatch (CwbExtendedToggleAttrModal None)) ]
                                  Bulma.modalContent [ prop.style [ style.width 900 ]
                                                       prop.children [ Bulma.box attrMenu ] ]

                                  Bulma.modalClose [ button.isLarge
                                                     prop.onClick (fun _ -> dispatch (CwbExtendedToggleAttrModal None)) ] ] ]


let view
    (corpus: Corpus)
    (search: Search)
    (underlyingQuery: Shared.Query)
    (queryIndex: int)
    (maybeAttrModalModel: AttributeModalModel option)
    (dispatch: Msg -> unit)
    =
    let query =
        if search.Params.Queries.Length > 0 then
            CwbExtended.Query.OfCqp(corpus, underlyingQuery.QueryString)
        else
            CwbExtended.Query.Default

    let segmentType =
        match corpus.SharedInfo.Modality with
        | Spoken -> "Segment"
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
                                   prop.onChange (fun (s: string) ->
                                       if s = "" then
                                           dispatch (
                                               CwbExtendedSetIntervalValue(
                                                   query,
                                                   queryIndex,
                                                   term,
                                                   termIndex,
                                                   minMax,
                                                   None
                                               )
                                           )

                                       match Int32.TryParse(s) with
                                       | (true, v) ->
                                           dispatch (
                                               CwbExtendedSetIntervalValue(
                                                   query,
                                                   queryIndex,
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
                               prop.onKeyUp (key.enter, (fun _ -> dispatch Search))
                               prop.onChange (fun (s: string) ->
                                   let v = if s = "" then None else Some s

                                   dispatch (CwbExtendedSetMainString(query, queryIndex, term, termIndex, v)))
                               prop.style [ style.width 108 ] ]

        let checkbox label title isChecked (property: QueryProperty) =
            Html.label [ prop.title title
                         prop.style [ style.marginRight 15 ]
                         prop.children [ Bulma.input.checkbox [ prop.isChecked isChecked
                                                                prop.onCheckedChange (fun isChecked ->
                                                                    dispatch (
                                                                        CwbExtendedSetQueryProperty(
                                                                            query,
                                                                            queryIndex,
                                                                            term,
                                                                            termIndex,
                                                                            property,
                                                                            isChecked
                                                                        )
                                                                    )) ]
                                         Bulma.text.span $" {label}" ] ]

        let removeTermButton =
            Bulma.button.button [ prop.title "Remove search word"
                                  prop.onClick (fun _ -> dispatch (CwbExtendedRemoveTerm(query, queryIndex, termIndex)))
                                  prop.children [ Bulma.icon [ Html.i [ prop.className "fas fa-minus" ] ] ] ]

        let attributeTags =
            // For each element in a list of subcategories given in the attribute menu in the corpus configuration,
            // check whether it is included in the set of selected subcategories for the given selected main category,
            // and if so, get its human-readable title to be shown in the tag.
            let checkForSubcatValues selectedMainCat (subcategories: CwbAttributeMenu.Subcategory list) =
                [ for (_, subcatValues) in subcategories ->
                      [ for (menuSubcatAttr, menuSubcatAttrValue, subcatHumanReadable) in subcatValues do
                            match selectedMainCat.Subcategories with
                            | Some selectedSubcats ->
                                if selectedSubcats
                                   |> Set.exists (fun selectedSubcat ->
                                       selectedSubcat.Attr = menuSubcatAttr.Code
                                       && selectedSubcat.Values.Contains(menuSubcatAttrValue)) then
                                    Some subcatHumanReadable
                            | None -> None ]
                      |> List.choose id
                      |> String.concat " or " ]
                |> String.concat " "

            match corpus.CwbAttributeMenu with
            | Some attributeMenu ->
                // Go through the attribute menu specified in the corpus configuration and construct a tag element for
                // each main category that has been selected for this query term along with its selected subcategories
                List.zip attributeMenu term.CategorySections
                |> List.indexed
                |> List.collect (fun (sectionIndex, (menuSection, termSectionSelection)) ->
                    [ for (mainAttr, mainAttrValue, mainHumanReadable, subcategories) in menuSection.Values do
                          termSectionSelection
                          |> Set.toList
                          |> List.tryFind (fun selectedCat ->
                              selectedCat.Attr = mainAttr.Code
                              && selectedCat.Value = mainAttrValue)
                          |> function
                              | Some selectedMainCat ->
                                  let subcatStr = checkForSubcatValues selectedMainCat subcategories

                                  Bulma.tag [ if selectedMainCat.Operator = NotEquals then
                                                  color.isDanger
                                              else
                                                  color.isInfo
                                              prop.children [ Html.span $"{mainHumanReadable} {subcatStr}"
                                                              Html.button [ prop.className "delete is-small"
                                                                            prop.onClick (fun _ ->
                                                                                dispatch (
                                                                                    CwbExtendedToggleAttributeCategory(
                                                                                        query,
                                                                                        queryIndex,
                                                                                        term,
                                                                                        termIndex,
                                                                                        sectionIndex,
                                                                                        selectedMainCat
                                                                                    )
                                                                                )) ] ] ]
                              | None -> Html.none ])
            | None -> []

        [ match maybeAttrModalModel with
          | Some am when am.TermIndex = termIndex ->
              AttributeModal(corpus, am, query, queryIndex, term, termIndex, dispatch)
          | _ -> ignore None

          if termIndex > 0 then
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
                                                               Bulma.button.button [ prop.onClick (fun _ ->
                                                                                         dispatch (
                                                                                             CwbExtendedToggleAttrModal(
                                                                                                 Some termIndex
                                                                                             )
                                                                                         ))
                                                                                     prop.children [ Bulma.icon [ Html.i [ prop.className
                                                                                                                               "fas fa-list" ] ] ] ]
                                                           )
                                                           // TODO: Check if people actually want this button. Since the trigger should be only
                                                           // a small button, not the full width of the list of categories, it probably needs to
                                                           // be implemented as a popup panel or something.
                                                           //    Bulma.control.div (
                                                           //        Bulma.button.button [ Bulma.icon [ Html.i [ prop.className
                                                           //                                                        "fas fa-chevron-down" ] ] ]
                                                           //    )
                                                           Bulma.control.div (mainStringInput)
                                                           if query.Terms.Length > 1 then
                                                               Bulma.control.div (removeTermButton) ] ]
                         Bulma.field.div [ field.isGrouped
                                           field.isGroupedMultiline
                                           // TODO: Use TryGetAttribute to get display names also for lemma, orig etc.?
                                           prop.children [ if corpus.SharedInfo.HasAttribute("lemma") then
                                                               checkbox "Lemma" "Lemma" term.IsLemma IsLemma

                                                           checkbox "Start" "Start of word" term.IsStart IsStart
                                                           checkbox "End" "End of word" term.IsEnd IsEnd
                                                           checkbox "Middle" "Middle of word" term.IsMiddle IsMiddle

                                                           match corpus.SharedInfo.TryGetAttribute("phon") with
                                                           | Some attr ->
                                                               checkbox attr.Name attr.Name term.IsPhonetic IsPhonetic
                                                           | None -> ignore ()

                                                           if corpus.SharedInfo.HasAttribute("orig") then
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

                                                           if termIndex = query.Terms.Length - 1 then
                                                               // TODO: Add optional sentence final punctuation to query
                                                               // to make this work in written text as well
                                                               match corpus.SharedInfo.Modality with
                                                               | Spoken ->
                                                                   checkbox
                                                                       $"{segmentType} final"
                                                                       $"{segmentType} final"
                                                                       term.IsFinal
                                                                       IsFinal
                                                               | Written -> Html.none ] ]
                         Bulma.field.div [ prop.style [ style.marginTop 10 ]
                                           prop.children [ Bulma.tags (
                                                               List.append
                                                                   attributeTags
                                                                   (includeExcludeTags query 0 term termIndex dispatch)
                                                           ) ] ] ]


          ]

    Bulma.columns [ prop.style [ style.marginBottom 15 ]
                    prop.children [ yield!
                                        [ for (index, term) in query.Terms |> Array.indexed do
                                              yield! termView index term ]
                                    Bulma.column (
                                        Bulma.buttons [ Bulma.button.button [ color.isInfo
                                                                              prop.title "Add search word"
                                                                              prop.onClick (fun _ ->
                                                                                  dispatch (
                                                                                      CwbExtendedAddTerm(
                                                                                          query,
                                                                                          queryIndex
                                                                                      )
                                                                                  ))
                                                                              prop.children [ Bulma.icon [ Html.i [ prop.className
                                                                                                                        "fas fa-plus" ] ] ] ]
                                                        if search.Params.Queries.Length > 1 then
                                                            Bulma.button.button [ color.isDanger
                                                                                  prop.title "Delete row"
                                                                                  prop.onClick (fun _ ->
                                                                                      dispatch (
                                                                                          RemoveQueryRow queryIndex
                                                                                      ))
                                                                                  prop.children [ Bulma.icon [ Html.i [ prop.className [ "fa fa-times" ] ] ] ] ]

                                                         ]
                                    ) ] ]
