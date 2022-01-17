module Update.Metadata

open Elmish
open Shared
open Model

type Msg =
    | OpenMetadataMenu of category: Metadata.Category
    | CloseMetadataMenu
    | ToggleMetadataMenuOpen of category: Metadata.Category
    | SetIntervalCategoryMode of category: Metadata.NumberCategory * mode: ListOrIntervalMode
    | FetchMetadataValuesForCategory of category: Metadata.Category
    | FetchedMetadataValuesForCategory of results: string []
    | ToggleIntervalOpen of category: Metadata.NumberCategory
    | FetchMinAndMaxForCategory of category: Metadata.NumberCategory
    | FetchedMinAndMaxForCategory of (int64 * int64)
    | FetchTextAndTokenCounts
    | FetchedTextAndTokenCounts of TextAndTokenCounts
    | ToggleExclude of category: Metadata.Category
    | SelectItem of Metadata.Category * Metadata.CategoryMenuOption
    | DeselectItem of Metadata.Category * Metadata.CategoryMenuOption
    | DeselectAllItems of Metadata.Category
    | SetIntervalFrom of Metadata.NumberCategory * string
    | SetIntervalTo of Metadata.NumberCategory * string
    | FetchMetadataForTexts
    | FetchedMetadataForTexts of results: string [] []
    | SetSelectionTablePage of pageNumber: int
    | SetSelectionTableSort of Metadata.SortInfo
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

            let cmd = Cmd.ofMsg (FetchMetadataValuesForCategory category)

            newModel, cmd
        else
            model, Cmd.none
    | CloseMetadataMenu -> { model with OpenMetadataCategoryCode = None }, Cmd.none
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
    | SetIntervalCategoryMode (category, mode) ->
        { model with IntervalCategoryModes = model.IntervalCategoryModes.Add(category.Code, mode) }, Cmd.none
    | FetchMetadataValuesForCategory category ->
        let tableAndColumn =
            match category.TableName with
            | Some tableName -> $"{tableName}.{category.Code}"
            | None -> category.Code

        let cmd =
            Cmd.OfAsync.perform
                serverApi.GetMetadataForCategory
                (model.Corpus.SharedInfo.Code, tableAndColumn, model.Search.Params.MetadataSelection)
                FetchedMetadataValuesForCategory

        model, cmd
    | FetchedMetadataValuesForCategory results -> { model with FetchedMetadataValues = results }, Cmd.none
    | ToggleIntervalOpen category ->
        let newCode, cmd =
            if model.OpenMetadataCategoryCode = Some category.Code then
                None, Cmd.none
            else
                Some category.Code, Cmd.ofMsg (FetchMinAndMaxForCategory category)

        { model with
            OpenMetadataCategoryCode = newCode
            FetchedMetadataValues = [||]
            FetchedMinAndMax = None },
        cmd
    | FetchMinAndMaxForCategory category ->
        let table = category.TableName |> Option.defaultValue "texts"
        let catCode = $"{table}.{category.Code}"

        model,
        Cmd.OfAsync.perform
            serverApi.GetMinAndMaxForCategory
            (model.Corpus.SharedInfo.Code, catCode, model.Search.Params.MetadataSelection)
            FetchedMinAndMaxForCategory
    | FetchedMinAndMaxForCategory (min, max) -> { model with FetchedMinAndMax = Some(min, max) }, Cmd.none
    | FetchTextAndTokenCounts ->
        let countCmd =
            Cmd.OfAsync.perform
                serverApi.GetTextAndTokenCount
                (model.Corpus.SharedInfo.Code, model.Search.Params.MetadataSelection)
                FetchedTextAndTokenCounts

        let newModel, cmd =
            if model.IsSelectionTableOpen then
                // If the metadata selection table is open, fetch text metadata as well as
                // calculating text and token counts, so that the table will be updated immediately
                { model with SelectionTablePageNumber = 1 },
                Cmd.batch [ countCmd
                            Cmd.ofMsg FetchMetadataForTexts ]
            else
                model, countCmd

        newModel, cmd
    | FetchedTextAndTokenCounts counts ->
        { model with
            NumSelectedTexts = Some counts.NumTexts
            NumSelectedTokens = Some counts.NumTokens
            SelectionTablePageNumber = 1 },
        Cmd.none
    | ToggleExclude category ->
        let tableAndCode =
            match category.TableName with
            | Some tableName -> $"{tableName}.{category.Code}"
            | None -> category.Code

        let newMetadataSelection =
            model.Search.Params.MetadataSelection
            |> Map.change tableAndCode (fun maybeCategorySelection ->
                maybeCategorySelection
                |> Option.map (fun categorySelection ->
                    { categorySelection with ShouldExclude = not categorySelection.ShouldExclude }))

        let openMetadataCategoryCode, fetchedMetadataValues =
            match model.OpenMetadataCategoryCode with
            | Some openCategoryCode when openCategoryCode <> tableAndCode ->
                // Some other category was open when we toggled exclude for this one, so close
                // the other category and empty the array of fetched metadata values
                None, [||]
            | _ -> model.OpenMetadataCategoryCode, model.FetchedMetadataValues

        { model with
            FetchedMetadataValues = fetchedMetadataValues
            OpenMetadataCategoryCode = openMetadataCategoryCode
            Search =
                { model.Search with Params = { model.Search.Params with MetadataSelection = newMetadataSelection } } },
        Cmd.ofMsg FetchTextAndTokenCounts
    | SelectItem (category, selectedOption) ->
        let tableAndCode =
            match category.TableName with
            | Some tableName -> $"{tableName}.{category.Code}"
            | None -> category.Code

        let newCategorySelection =
            // Find the already selected values for this category, if any, and append the new one
            match model.Search.Params.MetadataSelection.TryFind tableAndCode with
            | Some categorySelection ->
                let newChoices =
                    Array.append categorySelection.Choices [| selectedOption |]
                    |> Array.distinct

                { categorySelection with Choices = newChoices }
            | None ->
                { Choices = [| selectedOption |]
                  ShouldExclude = false }

        let newSelection =
            model.Search.Params.MetadataSelection
            |> Map.add tableAndCode newCategorySelection

        let newModel =
            { model with
                Search = { model.Search with Params = { model.Search.Params with MetadataSelection = newSelection } } }

        let cmd = Cmd.ofMsg FetchTextAndTokenCounts

        newModel, cmd
    | DeselectItem (category, optionToRemove) ->
        let tableAndCode =
            match category.TableName with
            | Some tableName -> $"{tableName}.{category.Code}"
            | None -> category.Code

        let maybeNewCategorySelection =
            model.Search.Params.MetadataSelection.TryFind(tableAndCode)
            |> Option.map (fun categorySelection ->
                let newCategoryChoices =
                    categorySelection.Choices
                    |> Array.except [ optionToRemove ]

                { categorySelection with Choices = newCategoryChoices })

        let newSelection =
            match maybeNewCategorySelection with
            | Some categorySelection ->
                // If removing the option left the selection for this category empty, remove the whole
                // category; otherwise set the new category selection
                if categorySelection.Choices.Length > 0 then
                    model.Search.Params.MetadataSelection.Add(tableAndCode, categorySelection)
                else
                    model.Search.Params.MetadataSelection.Remove(tableAndCode)
            | None -> model.Search.Params.MetadataSelection

        let newModel =
            { model with
                Search = { model.Search with Params = { model.Search.Params with MetadataSelection = newSelection } } }

        let cmd = Cmd.ofMsg FetchTextAndTokenCounts

        newModel, cmd
    | DeselectAllItems category ->
        let tableAndCode =
            match category.TableName with
            | Some tableName -> $"{tableName}.{category.Code}"
            | None -> category.Code

        let newSelection =
            // Remove the given category from the metadata selection
            model.Search.Params.MetadataSelection
            |> Map.remove tableAndCode

        let newModel =
            { model with
                Search = { model.Search with Params = { model.Search.Params with MetadataSelection = newSelection } } }

        let cmd = Cmd.ofMsg FetchTextAndTokenCounts

        newModel, cmd

    | SetIntervalFrom (category, number) ->
        let table = category.TableName |> Option.defaultValue "texts"
        let catCode = $"{table}.{category.Code}"

        let (newCategoryValues: Metadata.CategorySelection) =
            let fromValue: Metadata.CategoryMenuOption =
                { Name = "glossa_interval_from"
                  Value = number }

            // Find the already selected values for this category, if any, and append the new one
            match model.Search.Params.MetadataSelection.TryFind catCode with
            | Some categoryValues ->
                let maybeToValue =
                    categoryValues.Choices
                    |> Array.tryFind (fun choice -> choice.Name = "glossa_interval_to")

                { Choices =
                    [| fromValue
                       match maybeToValue with
                       | Some toValue -> toValue
                       | None -> ignore None |]
                  ShouldExclude = false }
            | None ->
                { Choices = [| fromValue |]
                  ShouldExclude = false }

        let newSelection =
            model.Search.Params.MetadataSelection
            |> Map.add catCode newCategoryValues

        let newModel =
            { model with
                Search = { model.Search with Params = { model.Search.Params with MetadataSelection = newSelection } } }

        newModel, Cmd.none

    | SetIntervalTo (category, number) ->
        let table = category.TableName |> Option.defaultValue "texts"
        let catCode = $"{table}.{category.Code}"

        let (newCategoryValues: Metadata.CategorySelection) =
            let toValue: Metadata.CategoryMenuOption =
                { Name = "glossa_interval_to"
                  Value = number }

            // Find the already selected values for this category, if any, and append the new one
            match model.Search.Params.MetadataSelection.TryFind catCode with
            | Some categoryValues ->
                let maybeFromValue =
                    categoryValues.Choices
                    |> Array.tryFind (fun choice -> choice.Name = "glossa_interval_from")

                { Choices =
                    [| match maybeFromValue with
                       | Some fromValue -> fromValue
                       | None -> ignore None
                       toValue |]
                  ShouldExclude = false }
            | None ->
                { Choices = [| toValue |]
                  ShouldExclude = false }

        let newSelection =
            model.Search.Params.MetadataSelection
            |> Map.add catCode newCategoryValues

        let newModel =
            { model with
                Search = { model.Search with Params = { model.Search.Params with MetadataSelection = newSelection } } }

        newModel, Cmd.none


    | FetchMetadataForTexts ->
        let columns = [ for category in model.Corpus.MetadataTable ->
                            let table = category.TableName |> Option.defaultValue "texts"
                            $"{table}.{category.Code}"  ]

        let cmd =
            Cmd.OfAsync.perform
                serverApi.GetMetadataForTexts
                (model.Corpus.SharedInfo.Code,
                 model.Search.Params.MetadataSelection,
                 columns,
                 model.SelectionTablePageNumber,
                 model.SelectionTableSort)
                FetchedMetadataForTexts

        model, cmd

    | FetchedMetadataForTexts results ->
        { model with
            FetchedTextMetadata = results
            IsSelectionTableOpen = true },
        Cmd.none

    | SetSelectionTablePage pageNumber ->
        { model with SelectionTablePageNumber = pageNumber }, Cmd.ofMsg FetchMetadataForTexts
    | SetSelectionTableSort sortInfo ->
        { model with SelectionTableSort = Some sortInfo }, Cmd.ofMsg (SetSelectionTablePage 1)
    | CloseSelectionTable -> { model with IsSelectionTableOpen = false }, Cmd.none
