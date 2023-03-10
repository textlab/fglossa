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
    | FetchedTextAndTokenCounts of TextAndTokenCounts * TextSelectionInfo
    | ToggleExclude of category: Metadata.Category
    | SelectItem of Metadata.Category * Metadata.CategoryMenuOption
    | DeselectItem of Metadata.Category * Metadata.CategoryMenuOption
    | SetSelection of Metadata.Category * Metadata.CategoryMenuOption [] * bool
    | DeselectAllItems of Metadata.Category
    | SetIntervalFrom of Metadata.NumberCategory * string
    | SetIntervalTo of Metadata.NumberCategory * string
    | FetchMetadataForTexts
    | FetchedMetadataForTexts of results: string [] []
    | SetSelectionTablePage of pageNumber: int
    | SetSelectionTableSort of Metadata.SortInfo
    | CloseSelectionTable
    | FetchMetadataForGeoMap
    | OpenMetadataGeoMap of string [] []
    | CloseMetadataGeoMap
    | OpenInVoyant of string

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
            category.GetQualifiedColumnName()

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
        let catCode =
            category.GetQualifiedColumnName()

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
    | FetchedTextAndTokenCounts (counts, TextSelectionInfo textSelectionInfo) ->
        let newModel =
            { model with
                NumSelectedTexts = Some counts.NumTexts
                NumSelectedTokens = Some counts.NumTokens
                SelectionTablePageNumber = 1
                TextSelectionInfo = textSelectionInfo }

        newModel, Cmd.none
    | ToggleExclude category ->
        let tableAndCode =
            category.GetQualifiedColumnName()

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
            category.GetQualifiedColumnName()

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
            category.GetQualifiedColumnName()

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
    | SetSelection (category, choices, shouldFetchCounts) ->
        let tableAndCode =
            category.GetQualifiedColumnName()

        // Replace the entire current selection for this metadata category (if any)
        // with the given choices
        let newSelection =
            model.Search.Params.MetadataSelection
            |> Map.add
                tableAndCode
                { Choices = choices
                  ShouldExclude = false }

        let newModel =
            { model with
                Search = { model.Search with Params = { model.Search.Params with MetadataSelection = newSelection } } }

        let cmd =
            if shouldFetchCounts then
                Cmd.ofMsg FetchTextAndTokenCounts
            else
                Cmd.none

        newModel, cmd
    | DeselectAllItems category ->
        let tableAndCode =
            category.GetQualifiedColumnName()

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
        let catCode =
            category.GetQualifiedColumnName()

        let (newCategoryValues: Metadata.CategorySelection) =
            let fromValue: Metadata.CategoryMenuOption =
                { Name = "app_interval_from"
                  Value = number }

            // Find the already selected values for this category, if any, and append the new one
            match model.Search.Params.MetadataSelection.TryFind catCode with
            | Some categoryValues ->
                let maybeToValue =
                    categoryValues.Choices
                    |> Array.tryFind (fun choice -> choice.Name = "app_interval_to")

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
        let catCode =
            category.GetQualifiedColumnName()

        let (newCategoryValues: Metadata.CategorySelection) =
            let toValue: Metadata.CategoryMenuOption =
                { Name = "app_interval_to"
                  Value = number }

            // Find the already selected values for this category, if any, and append the new one
            match model.Search.Params.MetadataSelection.TryFind catCode with
            | Some categoryValues ->
                let maybeFromValue =
                    categoryValues.Choices
                    |> Array.tryFind (fun choice -> choice.Name = "app_interval_from")

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
        let columns =
            [ for category in model.Corpus.MetadataTable -> category.GetQualifiedColumnName() ]

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
    | FetchMetadataForGeoMap ->
        let categories =
            match model.Corpus.SharedInfo.GeoMapConfig with
            | Some config -> [ for cat in config.MetadataCategories -> cat.QualifiedColumnName ]
            | None -> failwith "No metadata categories defined for geographical metadata map!"

        let cmd =
            Cmd.OfAsync.perform
                serverApi.GetMetadataForTexts
                (model.Corpus.SharedInfo.Code, model.Search.Params.MetadataSelection, categories, 0, None)
                OpenMetadataGeoMap

        model, cmd
    | OpenMetadataGeoMap res ->
        { model with
            FetchedTextMetadata = res
            IsMetadataGeoMapOpen = true },
        Cmd.none
    | CloseMetadataGeoMap -> { model with IsMetadataGeoMapOpen = false }, Cmd.none
    | OpenInVoyant attributeName ->
        // First get a text representation of the metadata selection
        let serializedMetadataSelection =
            Thoth.Json.Encode.Auto.toString (0, model.Search.Params.MetadataSelection)

        // Since Voyant refuses to call URLs with characters such as braces in them, we Base64-encode
        // the text representation
        let base64Encoded =
            Browser.Dom.window.btoa serializedMetadataSelection

        // Finally, we need to URI-encode the Base64 representation, using encodeURIComponent instead
        // of just encodeURI so as to encode even characters such as equals signs
        let uriEncoded =
            Fable.Core.JS.encodeURIComponent base64Encoded

        let url =
            $"https://voyant.lincsproject.ca/?input=https://tekstlab.uio.no/glossa3/rest/text/{model.Corpus.SharedInfo.Code}/{attributeName}/{uriEncoded}"

        Browser.Dom.window.``open`` (url, "_blank")
        |> ignore

        model, Cmd.none
