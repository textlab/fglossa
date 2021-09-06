module Update.Metadata

open Elmish
open Shared
open Model

type Msg =
    | OpenMetadataMenu of category: Metadata.Category
    | ToggleMetadataMenuOpen of category: Metadata.Category
    | FetchMetadataValuesForCategory of category: Metadata.Category
    | FetchedMetadataValuesForCategory of results: string []
    | FetchTextAndTokenCounts
    | FetchedTextAndTokenCounts of TextAndTokenCounts
    | ToggleExclude of category: Metadata.Category
    | SelectItem of Metadata.Category * Metadata.StringSelectOption
    | DeselectItem of Metadata.Category * Metadata.StringSelectOption
    | DeselectAllItems of Metadata.Category
    | FetchMetadataForTexts
    | FetchedMetadataForTexts of results: string [] []
    | SetSelectionTablePage of pageNumber: int
    | CloseSelectionTable

let update (msg: Msg) (model: LoadedCorpusModel) : LoadedCorpusModel * Cmd<Msg> =
    match msg with
    | OpenMetadataMenu category ->
        if model.OpenMetadataCategoryCode
           <> Some category.Code then
            let newModel =
                { model with
                      OpenMetadataCategoryCode = Some category.Code
                      FetchedMetadataValues = [||] }

            let cmd =
                Cmd.ofMsg (FetchMetadataValuesForCategory category)

            newModel, cmd
        else
            model, Cmd.none
    | ToggleMetadataMenuOpen category ->
        let newCode, cmd =
            if model.OpenMetadataCategoryCode = Some category.Code then
                None, Cmd.none
            else
                Some category.Code, Cmd.ofMsg (FetchMetadataValuesForCategory category)

        { model with
              OpenMetadataCategoryCode = newCode
              FetchedMetadataValues = [||] },
        cmd
    | FetchMetadataValuesForCategory category ->
        let cmd =
            Cmd.OfAsync.perform
                serverApi.GetMetadataForCategory
                (model.Corpus.Config.Code, category.Code, model.Search.Params.MetadataSelection)
                FetchedMetadataValuesForCategory

        model, cmd
    | FetchTextAndTokenCounts ->
        let cmd =
            Cmd.OfAsync.perform
                serverApi.GetTextAndTokenCount
                (model.Corpus.Config.Code, model.Search.Params.MetadataSelection)
                FetchedTextAndTokenCounts

        model, cmd
    | FetchedTextAndTokenCounts counts ->
        { model with
              NumSelectedTexts = Some counts.NumTexts
              NumSelectedTokens = Some counts.NumTokens
              SelectionTablePageNumber = 1 },
        Cmd.none
    | FetchedMetadataValuesForCategory results ->
        { model with
              FetchedMetadataValues = results },
        Cmd.none
    | ToggleExclude category ->
        let newMetadataSelection =
            model.Search.Params.MetadataSelection
            |> Map.change
                category.Code
                (fun maybeCategorySelection ->
                    maybeCategorySelection
                    |> Option.map
                        (fun categorySelection ->
                            { categorySelection with
                                  ShouldExclude = not categorySelection.ShouldExclude }))

        let openMetadataCategoryCode, fetchedMetadataValues =
            match model.OpenMetadataCategoryCode with
            | Some openCategoryCode when openCategoryCode <> category.Code ->
                // Some other category was open when we toggled exclude for this one, so close
                // the other category and empty the array of fetched metadata values
                None, [||]
            | _ -> model.OpenMetadataCategoryCode, model.FetchedMetadataValues

        { model with
              FetchedMetadataValues = fetchedMetadataValues
              OpenMetadataCategoryCode = openMetadataCategoryCode
              Search =
                  { model.Search with
                        Params =
                            { model.Search.Params with
                                  MetadataSelection = newMetadataSelection } } },
        Cmd.ofMsg FetchTextAndTokenCounts
    | SelectItem (category, selectedOption) ->
        let newCategorySelection =
            // Find the already selected values for this category, if any, and append the new one
            match model.Search.Params.MetadataSelection.TryFind category.Code with
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
            model.Search.Params.MetadataSelection
            |> Map.add category.Code newCategorySelection

        let newModel =
            { model with
                  Search =
                      { model.Search with
                            Params =
                                { model.Search.Params with
                                      MetadataSelection = newSelection } } }

        let cmd = Cmd.ofMsg FetchTextAndTokenCounts

        newModel, cmd
    | DeselectItem (category, optionToRemove) ->
        let maybeNewCategorySelection =
            model.Search.Params.MetadataSelection.TryFind(category.Code)
            |> Option.map
                (fun categorySelection ->
                    let newCategoryChoices =
                        categorySelection.Choices
                        |> Array.except [ optionToRemove ]

                    { categorySelection with
                          Choices = newCategoryChoices })

        let newSelection =
            match maybeNewCategorySelection with
            | Some categorySelection ->
                // If removing the option left the selection for this category empty, remove the whole
                // category; otherwise set the new category selection
                if categorySelection.Choices.Length > 0 then
                    model.Search.Params.MetadataSelection.Add(category.Code, categorySelection)
                else
                    model.Search.Params.MetadataSelection.Remove(category.Code)
            | None -> model.Search.Params.MetadataSelection

        let newModel =
            { model with
                  Search =
                      { model.Search with
                            Params =
                                { model.Search.Params with
                                      MetadataSelection = newSelection } } }

        let cmd = Cmd.ofMsg FetchTextAndTokenCounts

        newModel, cmd
    | DeselectAllItems category ->
        let newSelection =
            // Remove the given category from the metadata selection
            model.Search.Params.MetadataSelection
            |> Map.remove category.Code

        let newModel =
            { model with
                  Search =
                      { model.Search with
                            Params =
                                { model.Search.Params with
                                      MetadataSelection = newSelection } } }

        let cmd = Cmd.ofMsg FetchTextAndTokenCounts

        newModel, cmd

    | FetchMetadataForTexts ->
        let columns =
            [ for category in model.Corpus.MetadataTable -> category.Code ]

        let cmd =
            Cmd.OfAsync.perform
                serverApi.GetMetadataForTexts
                (model.Corpus.Config.Code,
                 model.Search.Params.MetadataSelection,
                 columns,
                 model.SelectionTablePageNumber)
                FetchedMetadataForTexts

        model, cmd

    | FetchedMetadataForTexts results ->
        { model with
              FetchedTextMetadata = results
              IsSelectionTableOpen = true },
        Cmd.none

    | SetSelectionTablePage pageNumber ->
        { model with
              SelectionTablePageNumber = pageNumber },
        Cmd.ofMsg FetchMetadataForTexts
    | CloseSelectionTable ->
        { model with
              IsSelectionTableOpen = false },
        Cmd.none
