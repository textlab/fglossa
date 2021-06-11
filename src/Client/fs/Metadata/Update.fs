module Update.Metadata

open Elmish
open Shared
open Model

type Msg =
    | SelectItem of Metadata.Category * Metadata.StringSelectOption
    | ToggleExclude of category: Metadata.Category
    | ToggleMetadataMenuOpen of category: Metadata.Category
    | ToggleShowSelectionOpen
    | DeselectItem of Metadata.Category * Metadata.StringSelectOption

let update (msg: Msg) (model: LoadedCorpusModel) : LoadedCorpusModel * Cmd<Msg> =
    match msg with
    | SelectItem (category, selectedOption) ->
        let newCategorySelection =
            // Find the already selected values for this category, if any, and append the new one
            match model.Search.MetadataSelection.TryFind category.Code with
            | Some categorySelection ->
                let newChoices =
                    Array.append categorySelection.Choices [| selectedOption |]
                    |> Array.distinct

                { categorySelection with
                      Choices = newChoices }
            | None ->
                { Choices = [| selectedOption |]
                  ShouldExclude = false }

        let newSelection =
            model.Search.MetadataSelection
            |> Map.add category.Code newCategorySelection

        let newModel =
            { model with
                  Search =
                      { model.Search with
                            MetadataSelection = newSelection } }

        newModel, Cmd.none
    | ToggleExclude category ->
        let newMetadataSelection =
            model.Search.MetadataSelection
            |> Map.change
                category.Code
                (fun maybeCategorySelection ->
                    maybeCategorySelection
                    |> Option.map
                        (fun categorySelection ->
                            { categorySelection with
                                  ShouldExclude = not categorySelection.ShouldExclude }))

        { model with
              Search =
                  { model.Search with
                        MetadataSelection = newMetadataSelection } },
        Cmd.none
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
                (fun maybeCategorySelection ->
                    maybeCategorySelection
                    |> Option.map
                        (fun categorySelection ->
                            let newCategoryChoices =
                                categorySelection.Choices
                                |> Array.except [ optionToRemove ]

                            { categorySelection with
                                  Choices = newCategoryChoices }))

        let newModel =
            { model with
                  Search =
                      { model.Search with
                            MetadataSelection = newSelection } }

        newModel, Cmd.none
