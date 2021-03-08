module Metadata.Update

open Elmish
open Shared
open Model

type Msg =
    | SelectItem of Metadata.Category * Metadata.StringSelectOption
    | ToggleMetadataMenuOpen of category: Metadata.Category
    | ToggleShowSelectionOpen
    | DeselectItem of Metadata.Category * Metadata.StringSelectOption

let update (msg: Msg) (model: LoadedCorpusModel) : LoadedCorpusModel * Cmd<Msg> =
    match msg with
    | SelectItem (category, value) ->
        let newCategoryValues =
            // Find the already selected values for this category, if any, and append the new one
            match model.Search.MetadataSelection.TryFind category.Code with
            | Some values -> Array.append values [| value |] |> Array.distinct
            | None -> [| value |]

        let newSelection =
            model.Search.MetadataSelection
            |> Map.add category.Code newCategoryValues

        let newModel =
            { model with
                  Search =
                      { model.Search with
                            MetadataSelection = newSelection } }

        newModel, Cmd.none
    | ToggleMetadataMenuOpen category ->
        let newCode =
            if model.OpenMetadataCategoryCode = Some category.Code then
                None
            else
                Some category.Code

        { model with
              OpenMetadataCategoryCode = newCode },
        Cmd.none
    | ToggleShowSelectionOpen ->
        { model with
              IsShowSelectionOpen = not model.IsShowSelectionOpen },
        Cmd.none
    | DeselectItem (category, optionToRemove) ->
        let newSelection =
            // Find the selected values for this category and remove the given one
            model.Search.MetadataSelection
            |> Map.change
                category.Code
                (fun maybeSelectOptions ->
                    maybeSelectOptions
                    |> Option.map (fun selectOptions -> selectOptions |> Array.except [ optionToRemove ]))

        let newModel =
            { model with
                  Search =
                      { model.Search with
                            MetadataSelection = newSelection } }

        newModel, Cmd.none
