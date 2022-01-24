module Update

open System
open System.Text.RegularExpressions
open Elmish
open Shared
open Model
open CwbExtended
open Shared.StringUtils

let cleanupResult (result: SearchResult) =
    let cleanLines =
        result.Text
        |> List.map (
            // Remove the beginning of the search result, which will be a position
            // number in the case of a monolingual result or the first language of a
            // multilingual result, or an arrow in the case of subsequent languages
            // in a multilingual result.
            replace "^\s*\d+:\s*" ""
            >> replace "^-->.+?:\s*" ""
            // When the match includes the first or last token of the s unit, the XML
            // tag surrounding the s unit is included inside the match braces (this
            // should probably be considered a bug in CQP). We need to fix that.
            >> replace "\{\{(<s_id\s+.+?>)" "$1{{"
            >> replace "(</s_id>)\}\}" "}}$1"
            >> replace "&nbsp;" "_"
        )

    { result with Text = cleanLines }

module LoadingCorpus =

    ////////////////////////////////
    // Update.LoadingCorpus
    ////////////////////////////////
    type Msg = FetchedCorpusConfig of SharedCorpusInfo

    let update (msg: Msg) (_model: Model) =
        match msg with
        | FetchedCorpusConfig corpusConfig ->
            let corpus = Corpora.Client.getCorpus corpusConfig

            let m =
                { Corpus = corpus
                  FetchedMetadataValues = [||]
                  FetchedMinAndMax = None
                  FetchedTextMetadata = [| [||] |]
                  IsNarrowWindow = false
                  IsSelectionTableOpen = false
                  IntervalCategoryModes = Map.empty
                  NumSelectedTexts = None
                  NumSelectedTokens = None
                  OpenMetadataCategoryCode = None
                  Search = Search.Init(corpus.SharedInfo)
                  ShouldShowMetadataMenu = None
                  ShouldShowQuickView = false
                  SelectionTablePageNumber = 1
                  SelectionTableSort = None
                  Substate = CorpusStart }

            LoadedCorpus m, Cmd.none


module LoadedCorpus =

    module ShowingResults =

        module Concordance =
            //////////////////////////////////////////////////
            // Update.LoadedCorpus.ShowingResults.Concordance
            //////////////////////////////////////////////////
            type Msg =
                | PerformSearchStep
                | SearchResultsReceived of SearchResultInfo
                | FetchResultWindow of centrePageNo: int * maybeSortKey: SortKey option
                | FetchedResultWindow of SearchResultPage []
                | SetPaginatorTextValue of string
                | SetPaginatorPage of pageNumber: int option * maybeSortKey: SortKey option
                | SetContextSizeTextValue of string
                | SetContextSize of int
                | FetchMetadataForText of corpus: Corpus * textId: string
                | FetchedMetadataForText of Metadata.CategoryNameAndValue list
                | CloseQuickView
                | FetchMediaObject of MediaPlayerType * rowIndex: int
                | FetchedMediaObject of MediaPlayerType * rowIndex: int * MediaObject
                | RemoveMediaObject
                | OpenDownloadWindow
                | CloseDownloadWindow
                | ToggleDownloadAttribute of Cwb.PositionalAttribute
                | ToggleHeadersInDownload
                | DownloadSearchResults of DownloadFormat
                | DownloadedSearchResults of string

            let update
                (msg: Msg)
                (loadedCorpusModel: LoadedCorpusModel)
                (concordanceModel: ConcordanceModel)
                : LoadedCorpusModel * ConcordanceModel * Cmd<Msg> =
                let registerResultPages results =
                    let fetchedPages =
                        results |> Array.map (fun page -> page.PageNumber)

                    // Remove the fetched pages from the set of pages currently being fetched...
                    let pagesBeingFetched =
                        concordanceModel.PagesBeingFetched
                        |> Array.except fetchedPages

                    // ...and add them to the map of fetched pages
                    let resultPages =
                        results
                        |> Array.fold
                            (fun (pageMap: Map<int, SearchResult []>) result ->
                                pageMap.Add(result.PageNumber, result.Results |> Array.map cleanupResult))
                            concordanceModel.ResultPages

                    { concordanceModel with
                          PagesBeingFetched = pagesBeingFetched
                          ResultPages = resultPages }

                match msg with
                | PerformSearchStep ->
                    let cmd =
                        Cmd.OfAsync.perform serverApi.SearchCorpus loadedCorpusModel.Search.Params SearchResultsReceived

                    loadedCorpusModel, concordanceModel, cmd

                | SearchResultsReceived results ->
                    let shouldRunMoreSteps =
                        concordanceModel.NumSteps > results.SearchStep

                    let newSearchParams =
                        loadedCorpusModel.Search.Params
                        |> fun p ->
                            let lastCount = results.Count

                            let step =
                                if shouldRunMoreSteps then
                                    p.Step + 1
                                else
                                    p.Step

                            { p with
                                  CpuCounts =
                                      match p.CpuCounts with
                                      | Some existingCounts ->
                                          Array.append existingCounts results.CpuCounts
                                          |> Some
                                      | None -> Some results.CpuCounts
                                  LastCount = Some lastCount
                                  SearchId = results.SearchId
                                  Step = step }

                    let cmd =
                        if shouldRunMoreSteps then
                            Cmd.ofMsg PerformSearchStep
                        else
                            Cmd.none

                    let fetchedPages =
                        results.ResultPages
                        |> Array.map (fun page -> page.PageNumber)

                    // Remove the fetched pages from the set of pages currently being fetched...
                    let pagesBeingFetched =
                        concordanceModel.PagesBeingFetched
                        |> Array.except fetchedPages

                    // ...and put them into the map of fetched pages. Since we may already have
                    // received a half-filled page from a previous search step, we need to gather
                    // all previous and current results together and chunk them into page-sized
                    // chunks to make sure we get the correct page sizes.
                    let existingResults =
                        concordanceModel.ResultPages
                        |> Map.toArray
                        |> Array.collect snd

                    let newResults =
                        results.ResultPages
                        |> Array.collect (fun page -> page.Results)

                    let resultPages =
                        Array.append existingResults newResults
                        |> Array.map cleanupResult
                        |> Array.chunkBySize loadedCorpusModel.Search.Params.PageSize
                        |> Array.mapi (fun index page -> (index + 1, page))
                        |> Map.ofArray

                    let newConcordanceModel =
                        { concordanceModel with
                              PagesBeingFetched = pagesBeingFetched
                              ResultPages = resultPages
                              IsSearching = shouldRunMoreSteps
                              NumResults = Some results.Count }

                    let newLoadedCorpusModel =
                        { loadedCorpusModel with
                              Search =
                                  { loadedCorpusModel.Search with
                                        Params = newSearchParams } }

                    newLoadedCorpusModel, newConcordanceModel, cmd

                // Fetch a window of search result pages centred on centrePageNo. Ignores pages that have
                // already been fetched or that are currently being fetched in another request (note that such
                // pages can only be located at the edges of the window, and not as 'holes' within the window,
                // since they must have been fetched as part of an earlier window).
                | FetchResultWindow (centrePageNo, maybeSortKey) ->
                    let sortKey =
                        maybeSortKey
                        |> Option.defaultValue loadedCorpusModel.Search.Params.SortKey

                    // Make sure the edges of the window are between 1 and the last page number
                    let startPage = max (centrePageNo - 1) 1

                    let endPage =
                        min
                            (centrePageNo + 1)
                            (concordanceModel.NumResultPages(loadedCorpusModel.Search.Params.PageSize))

                    let pageNumbers =
                        if sortKey <> loadedCorpusModel.Search.Params.SortKey then
                            // If we are changing the sort order, we need to fetch all specified pages
                            // regardless of whether those page numbers have already been fetched
                            [| startPage .. endPage |]
                        else
                            [| startPage .. endPage |]
                            // Ignore pages currently being fetched by another request
                            |> Array.filter
                                (fun page ->
                                    concordanceModel.PagesBeingFetched
                                    |> Array.contains page
                                    |> not)
                            // Ignore pages that have already been fetched
                            |> Array.filter
                                (fun page ->
                                    concordanceModel.ResultPages
                                    |> Map.containsKey page
                                    |> not)
                            // Create a new sequence to make sure we didn't create any "holes" in
                            // it (although that should not really happen in practice since we
                            // always fetch whole windows of pages)
                            |> fun pageNos ->
                                if Array.isEmpty pageNos then
                                    pageNos
                                else
                                    [| (Array.head pageNos) .. (Array.last pageNos) |]

                    if Array.isEmpty pageNumbers then
                        // All pages are either being fetched or already fetched, so there is nothing to do
                        loadedCorpusModel, concordanceModel, Cmd.none
                    else
                        // Calculate the first and last result index (zero-based) to request from the server
                        let firstResult =
                            (pageNumbers.[0] - 1)
                            * loadedCorpusModel.Search.Params.PageSize
                            |> uint64

                        let lastResult =
                            ((pageNumbers |> Array.last)
                             * loadedCorpusModel.Search.Params.PageSize)
                            - 1
                            |> uint64

                        let searchParams =
                            { loadedCorpusModel.Search.Params with
                                  Start = firstResult
                                  End = lastResult
                                  SortKey = sortKey }

                        let newConcordanceModel =
                            { concordanceModel with
                                  // Register the pages as being fetched
                                  PagesBeingFetched = Array.append concordanceModel.PagesBeingFetched pageNumbers }

                        let newLoadedCorpusModel =
                            { loadedCorpusModel with
                                  Search =
                                      { loadedCorpusModel.Search with
                                            Params = searchParams } }

                        let cmd =
                            Cmd.OfAsync.perform
                                serverApi.GetSearchResults
                                (searchParams, pageNumbers)
                                FetchedResultWindow

                        newLoadedCorpusModel, newConcordanceModel, cmd

                | FetchedResultWindow results ->
                    // TODO: Check if we get a 401 Unauthorized
                    let newConcordanceModel =
                        results
                        |> registerResultPages
                        // Now that result pages have been fetched, make sure we actually show the page that was
                        // selected in the paginator
                        |> fun m ->
                            { m with
                                  ResultPageNo = m.PaginatorPageNo }

                    loadedCorpusModel, newConcordanceModel, Cmd.none

                | SetPaginatorTextValue s ->
                    loadedCorpusModel,
                    { concordanceModel with
                          PaginatorTextValue = s },
                    Cmd.none

                | SetPaginatorPage (maybePageNo, maybeSortKey) ->
                    let pageNo =
                        match maybePageNo with
                        // If we got an explicit page number, use that
                        | Some p -> p
                        // If we didn't get an explicit page number, try to parse the current PaginatorTextValue
                        // as an int. If successful, use the result of that, otherwise keep the current page number
                        | None ->
                            match Int32.TryParse(concordanceModel.PaginatorTextValue) with
                            | true, v -> v
                            | false, _ -> concordanceModel.PaginatorPageNo

                    let newConcordanceModel =
                        { concordanceModel with
                              // Set the value of the page number shown in the paginator; it may
                              // differ from the page shown in the result table until we have
                              // actually fetched the data from the server
                              PaginatorPageNo = pageNo
                              PaginatorTextValue = string pageNo
                              ResultPageNo =
                                  if concordanceModel.ResultPages.ContainsKey(pageNo) then
                                      // If the newly selected result page has already been fetched from the
                                      // server, it can be shown in the result table immediately
                                      pageNo
                                  else
                                      // Otherwise, we need to wait until the results from the server
                                      // arrive before changing the page to be shown in the result
                                      // table
                                      concordanceModel.PaginatorPageNo
                              ResultPages =
                                  match maybeSortKey with
                                  | Some sortKey when sortKey <> loadedCorpusModel.Search.Params.SortKey ->
                                      // If we have received a sort key that is different from the one currently
                                      // set in the model, invalidate all previously fetched search result pages
                                      Map.empty
                                  | _ -> concordanceModel.ResultPages }


                    let cmd =
                        // If necessary, fetch any result pages in a window centred
                        // around the selected page in order to speed up pagination
                        // to nearby pages. No need to wait for it to finish though.
                        Cmd.ofMsg (FetchResultWindow(pageNo, maybeSortKey))

                    loadedCorpusModel, newConcordanceModel, cmd

                | SetContextSizeTextValue v ->
                    loadedCorpusModel,
                    { concordanceModel with
                          ContextSizeTextValue = v },
                    Cmd.none

                | SetContextSize size ->
                    let newSearchParams =
                        { loadedCorpusModel.Search.Params with
                              ContextSize = size }

                    let newLoadedCorpusModel =
                        { loadedCorpusModel with
                              Search =
                                  { loadedCorpusModel.Search with
                                        Params = newSearchParams } }

                    let newConcordanceModel =
                        { concordanceModel with
                              ResultPages = Map.empty }

                    newLoadedCorpusModel,
                    newConcordanceModel,
                    Cmd.ofMsg (FetchResultWindow(concordanceModel.ResultPageNo, None))

                | FetchMetadataForText (corpus, textId) ->
                    let (categories: Metadata.CategoryNameAndCode list) =
                        [ for category in corpus.MetadataQuickView ->
                              { Name = category.Name
                                Code = category.Code } ]

                    let getNew () =
                        loadedCorpusModel,
                        { concordanceModel with
                              TextIdInQuickView = Some textId },
                        Cmd.OfAsync.perform
                            serverApi.GetMetadataForSingleText
                            (corpus.SharedInfo.Code, categories, textId)
                            FetchedMetadataForText

                    match concordanceModel.TextIdInQuickView with
                    | Some textIdInQuickView when textIdInQuickView = textId ->
                        // We have already loaded this text into the quick view, so just make
                        // sure we show it
                        { loadedCorpusModel with
                              ShouldShowQuickView = true },
                        concordanceModel,
                        Cmd.none
                    | Some _
                    | None -> getNew ()

                | FetchedMetadataForText metadata ->
                    { loadedCorpusModel with
                          ShouldShowQuickView = true },
                    { concordanceModel with
                          QuickViewMetadata = metadata },
                    Cmd.none

                | CloseQuickView ->
                    { loadedCorpusModel with
                          ShouldShowQuickView = false },
                    { concordanceModel with
                          TextIdInQuickView = None },
                    Cmd.none

                | FetchMediaObject (mediaPlayerType, rowIndex) ->
                    let cmd =
                        Cmd.OfAsync.perform
                            serverApi.GetMediaObject
                            (loadedCorpusModel.Search.Params,
                             mediaPlayerType,
                             concordanceModel.ResultPageNo,
                             rowIndex,
                             concordanceModel.VideoContextSize,
                             concordanceModel.VideoContextUnit)
                            FetchedMediaObject

                    loadedCorpusModel, concordanceModel, cmd

                | FetchedMediaObject (mediaPlayerType, rowIndex, mediaObject) ->
                    loadedCorpusModel,
                    { concordanceModel with
                          MediaPlayer =
                              Some
                                  { Type = mediaPlayerType
                                    RowIndex = rowIndex
                                    MediaObject = mediaObject } },
                    Cmd.none

                | RemoveMediaObject ->
                    loadedCorpusModel,
                    { concordanceModel with
                          MediaPlayer = None },
                    Cmd.none
                | OpenDownloadWindow ->
                    loadedCorpusModel,
                    { concordanceModel with
                          ShouldShowDownloadWindow = true },
                    Cmd.none
                | CloseDownloadWindow ->
                    loadedCorpusModel,
                    { concordanceModel with
                          ShouldShowDownloadWindow = false },
                    Cmd.none
                | ToggleDownloadAttribute attribute ->
                    let alreadyExists =
                        concordanceModel.DownloadAttributes
                        |> List.contains attribute

                    let newAttributes =
                        if alreadyExists then
                            concordanceModel.DownloadAttributes
                            |> List.filter (fun attr -> attr <> attribute)
                        else
                            concordanceModel.DownloadAttributes
                            @ [ attribute ]

                    loadedCorpusModel,
                    { concordanceModel with
                          DownloadAttributes = newAttributes },
                    Cmd.none
                | ToggleHeadersInDownload ->
                    loadedCorpusModel,
                    { concordanceModel with
                          HeadersInDownload = not concordanceModel.HeadersInDownload },
                    Cmd.none
                | DownloadSearchResults format ->
                    let cmd =
                        Cmd.OfAsync.perform
                            serverApi.DownloadSearchResults
                            (loadedCorpusModel.Search.Params,
                             concordanceModel.DownloadAttributes,
                             format,
                             concordanceModel.HeadersInDownload)
                            DownloadedSearchResults

                    loadedCorpusModel,
                    { concordanceModel with
                          DownloadingFormat = Some format },
                    cmd
                | DownloadedSearchResults path ->
                    Browser.Dom.window.location.href <- path

                    loadedCorpusModel,
                    { concordanceModel with
                          DownloadingFormat = None },
                    Cmd.none


        module FrequencyLists =
            /////////////////////////////////////////////////////
            // Update.LoadedCorpus.ShowingResults.FrequencyLists
            /////////////////////////////////////////////////////
            type Msg =
                | ToggleAttribute of Cwb.PositionalAttribute
                | ToggleIsCaseSensitive
                | FetchFrequencyList
                | FetchedFrequencyList of string []
                | DownloadFrequencyList of DownloadFormat
                | DownloadedFrequencyList of string

            let update
                (msg: Msg)
                (loadedCorpusModel: LoadedCorpusModel)
                (frequencyListsModel: FrequencyListsModel)
                : LoadedCorpusModel * FrequencyListsModel * Cmd<Msg> =
                match msg with
                | ToggleAttribute attribute ->
                    let alreadyExists =
                        frequencyListsModel.Attributes
                        |> List.contains attribute

                    let newAttributes =
                        if alreadyExists then
                            frequencyListsModel.Attributes
                            |> List.filter (fun attr -> attr <> attribute)
                        else
                            frequencyListsModel.Attributes @ [ attribute ]

                    loadedCorpusModel,
                    { frequencyListsModel with
                          Attributes = newAttributes },
                    Cmd.none
                | ToggleIsCaseSensitive ->
                    loadedCorpusModel,
                    { frequencyListsModel with
                          IsCaseSensitive = not frequencyListsModel.IsCaseSensitive },
                    Cmd.none
                | FetchFrequencyList ->
                    loadedCorpusModel,
                    frequencyListsModel,
                    Cmd.OfAsync.perform
                        serverApi.GetFrequencyList
                        (loadedCorpusModel.Search.Params,
                         frequencyListsModel.Attributes,
                         frequencyListsModel.IsCaseSensitive)
                        FetchedFrequencyList
                | FetchedFrequencyList rows ->
                    let listItems =
                        [| for row in rows ->
                               let m = Regex.Match(row, "(\d+)\s+(.+)")
                               let freq = UInt64.Parse(m.Groups.[1].Value)

                               let attrValues = m.Groups.[2].Value.Split('\t')

                               { Frequency = freq
                                 AttributeValues = attrValues } |]

                    loadedCorpusModel,
                    { frequencyListsModel with
                          Frequencies = Some listItems },
                    Cmd.none
                | DownloadFrequencyList format ->
                    loadedCorpusModel,
                    { frequencyListsModel with
                          DownloadingFormat = Some format },
                    Cmd.OfAsync.perform
                        serverApi.DownloadFrequencyList
                        (loadedCorpusModel.Search.Params,
                         frequencyListsModel.Attributes,
                         frequencyListsModel.IsCaseSensitive,
                         format)
                        DownloadedFrequencyList
                | DownloadedFrequencyList path ->
                    Browser.Dom.window.location.href <- path

                    loadedCorpusModel,
                    { frequencyListsModel with
                          DownloadingFormat = None },
                    Cmd.none

        module MetadataDistribution =
            ///////////////////////////////////////////////////////////
            // Update.LoadedCorpus.ShowingResults.MetadataDistribution
            ///////////////////////////////////////////////////////////
            type Msg =
                | SelectAttribute of string
                | SelectCategory of Metadata.Category
                | SetKeepZero of bool
                | AddOrRemoveExcludedAttributeValue of string * bool
                | RemoveSelectedAttributeValues
                | FetchedMetadataDistribution of MetadataDistribution
                | DownloadMetadataDistribution of DownloadFormat
                | DownloadedMetadataDistribution of string

            let update
                (msg: Msg)
                (loadedCorpusModel: LoadedCorpusModel)
                (metadataDistributionModel: MetadataDistributionModel)
                : LoadedCorpusModel * MetadataDistributionModel * Cmd<Msg> =

                let buildCmd
                    { SelectedAttributeCode = maybeAttributeCode
                      SelectedCategory = maybeCategory
                      KeepZeroValues = keepZeroValues
                      ExcludedAttributeValues = excludedAttributeValues
                      DownloadingFormat = maybeDownloadingFormat }
                    =
                    match maybeAttributeCode, maybeCategory with
                    | Some attributeCode, Some category ->
                        // Since Newtonsoft.Json cannot deserialize abstract types such as Metadata.Category,
                        // we need to provide the category code and its type as concrete types
                        let categoryType =
                            match category with
                            | :? Metadata.StringCategory -> Metadata.StringCategoryType
                            | :? Metadata.LongTextCategory -> Metadata.StringCategoryType
                            | :? Metadata.NumberCategory -> Metadata.NumberCategoryType
                            | _ -> failwith $"Unknown metadata category type for {category}"

                        let table =
                            category.TableName |> Option.defaultValue "texts"

                        let catCode = $"{table}.{category.Code}"

                        match maybeDownloadingFormat with
                        | Some downloadingFormat ->
                            Cmd.OfAsync.perform
                                serverApi.DownloadMetadataDistribution
                                (loadedCorpusModel.Search.Params,
                                 attributeCode,
                                 catCode,
                                 categoryType,
                                 keepZeroValues,
                                 excludedAttributeValues.Accumulated,
                                 downloadingFormat)
                                DownloadedMetadataDistribution
                        | None ->
                            Cmd.OfAsync.perform
                                serverApi.GetMetadataDistribution
                                (loadedCorpusModel.Search.Params,
                                 attributeCode,
                                 catCode,
                                 categoryType,
                                 keepZeroValues,
                                 excludedAttributeValues.Accumulated)
                                FetchedMetadataDistribution
                    | _ -> Cmd.none

                match msg with
                | SelectAttribute attributeCode ->
                    let newMetadataDistributionModel =
                        { metadataDistributionModel with
                              SelectedAttributeCode = Some attributeCode }

                    let cmd = buildCmd newMetadataDistributionModel

                    loadedCorpusModel, newMetadataDistributionModel, cmd

                | SelectCategory category ->
                    let newMetadataDistributionModel =
                        { metadataDistributionModel with
                              SelectedCategory = Some category }

                    let cmd = buildCmd newMetadataDistributionModel

                    loadedCorpusModel, newMetadataDistributionModel, cmd

                | SetKeepZero shouldKeepZeroValues ->
                    let newMetadataDistributionModel =
                        { metadataDistributionModel with
                              KeepZeroValues = shouldKeepZeroValues }

                    let cmd = buildCmd newMetadataDistributionModel
                    loadedCorpusModel, newMetadataDistributionModel, cmd

                | AddOrRemoveExcludedAttributeValue (value, shouldAdd) ->
                    let newExclusions =
                        if shouldAdd then
                            metadataDistributionModel.ExcludedAttributeValues.New.Add(value)
                        else
                            metadataDistributionModel.ExcludedAttributeValues.New.Remove(value)

                    let newExcludedAttr =
                        { metadataDistributionModel.ExcludedAttributeValues with
                              New = newExclusions }

                    let newMetadataDistributionModel =
                        { metadataDistributionModel with
                              ExcludedAttributeValues = newExcludedAttr }

                    loadedCorpusModel, newMetadataDistributionModel, Cmd.none

                | RemoveSelectedAttributeValues ->
                    // Add the set of newly selected attribute values to the ones we have accumulated
                    // so far, and clear the set of new ones. We then build a command that will fetch
                    // a new metadata distribution table from the server, where the accumulated values
                    // will be removed from the results.
                    let newExclusions =
                        { Accumulated =
                              Set.union
                                  metadataDistributionModel.ExcludedAttributeValues.Accumulated
                                  metadataDistributionModel.ExcludedAttributeValues.New
                          New = Set.empty }

                    let newMetadataDistributionModel =
                        { metadataDistributionModel with
                              ExcludedAttributeValues = newExclusions }

                    let cmd = buildCmd newMetadataDistributionModel
                    loadedCorpusModel, newMetadataDistributionModel, cmd

                | FetchedMetadataDistribution metadataDistribution ->
                    loadedCorpusModel,
                    { metadataDistributionModel with
                          MetadataDistribution = metadataDistribution },
                    Cmd.none

                | DownloadMetadataDistribution format ->
                    let newMetadataDistributionModel =
                        { metadataDistributionModel with
                              DownloadingFormat = Some format }

                    let cmd = buildCmd newMetadataDistributionModel
                    loadedCorpusModel, newMetadataDistributionModel, cmd

                | DownloadedMetadataDistribution path ->
                    Browser.Dom.window.location.href <- path

                    loadedCorpusModel,
                    { metadataDistributionModel with
                          DownloadingFormat = None },
                    Cmd.none


        ///////////////////////////////////////////
        // Update.LoadedCorpus.ShowingResults
        ///////////////////////////////////////////
        type Msg =
            | ConcordanceMsg of Concordance.Msg
            | FrequencyListsMsg of FrequencyLists.Msg
            | MetadataDistributionMsg of MetadataDistribution.Msg

            | SelectResultTab of ResultTab

        let update
            (msg: Msg)
            (loadedCorpusModel: LoadedCorpusModel)
            (showingResultsModel: ShowingResultsModel)
            : LoadedCorpusModel * ShowingResultsModel * Cmd<Msg> =
            match msg, showingResultsModel.ActiveTab with
            | ConcordanceMsg msg', Concordance m ->
                let newLoadedCorpusModel, newConcordanceModel, cmd =
                    Concordance.update msg' loadedCorpusModel m

                newLoadedCorpusModel,
                { showingResultsModel with
                      ActiveTab = Concordance newConcordanceModel },
                Cmd.map ConcordanceMsg cmd
            | FrequencyListsMsg msg', FrequencyLists m ->
                let newLoadedCorpusModel, newFrequencyListsModel, cmd =
                    FrequencyLists.update msg' loadedCorpusModel m

                newLoadedCorpusModel,
                { showingResultsModel with
                      ActiveTab = FrequencyLists newFrequencyListsModel },
                Cmd.map FrequencyListsMsg cmd

            | MetadataDistributionMsg msg', MetadataDistribution m ->
                let newLoadedCorpusModel, newMetadataDistributionModel, cmd =
                    MetadataDistribution.update msg' loadedCorpusModel m

                newLoadedCorpusModel,
                { showingResultsModel with
                      ActiveTab = MetadataDistribution newMetadataDistributionModel },
                Cmd.map MetadataDistributionMsg cmd

            | SelectResultTab tab, _ ->
                loadedCorpusModel,
                { showingResultsModel with
                      ActiveTab = tab },
                Cmd.none

            | _ -> failwithf $"Incompatible message and model: {msg}; {showingResultsModel}"


    ////////////////////////////////
    // Update.LoadedCorpus
    ////////////////////////////////
    type Msg =
        | MetadataMsg of Update.Metadata.Msg
        | ShowingResultsMsg of ShowingResults.Msg

        | SetShouldShowMetadataMenu of bool
        | SetSearchInterface of SearchInterface
        | SetQueryText of query: string * queryIndex: int * hasFinalSpace: bool
        | AddQueryRow
        | RemoveQueryRow of queryIndex: int
        | CwbExtendedSetMainString of
            query: CwbExtended.Query *
            queryIndex: int *
            term: QueryTerm *
            termIndex: int *
            maybeValue: string option
        | CwbExtendedSetQueryProperty of
            query: CwbExtended.Query *
            queryIndex: int *
            term: QueryTerm *
            termIndex: int *
            property: QueryProperty *
            value: bool
        | CwbExtendedSetExtraForm of value: string
        | CwbExtendedIncludeOrExcludeExtraForm of
            query: CwbExtended.Query *
            queryIndex: int *
            term: QueryTerm *
            termIndex: int *
            command: string
        | CwbExtendedRemoveExtraForms of
            query: CwbExtended.Query *
            queryIndex: int *
            term: QueryTerm *
            termIndex: int *
            attrName: string
        | CwbExtendedToggleAttributeCategory of
            query: CwbExtended.Query *
            queryIndex: int *
            term: QueryTerm *
            termIndex: int *
            categorySectionIndex: int *
            category: MainCategory
        | CwbExtendedToggleAttributeSubcategory of
            query: CwbExtended.Query *
            queryIndex: int *
            term: QueryTerm *
            termIndex: int *
            categorySectionIndex: int *
            mainCategory: MainCategory *
            subcategory: Subcategory
        | CwbExtendedClearAttributeCategories of
            query: CwbExtended.Query *
            queryIndex: int *
            term: QueryTerm *
            termIndex: int
        | CwbExtendedSetIntervalValue of
            query: CwbExtended.Query *
            queryIndex: int *
            term: QueryTerm *
            termIndex: int *
            minMax: MinMax *
            maybeValue: int option
        | CwbExtendedAddTerm of query: CwbExtended.Query * queryIndex: int
        | CwbExtendedRemoveTerm of query: CwbExtended.Query * queryIndex: int * termIndex: int
        | CwbExtendedToggleAttrModal of maybeTermIndex: int option
        | SetNumRandomHits of uint64 option
        | SetRandomHitsSeed of int option
        | Search
        | ResetForm
        | ClosePopups

    let update (msg: Msg) (loadedCorpusModel: LoadedCorpusModel) : LoadedCorpusModel * Cmd<Msg> =
        match msg with
        | MetadataMsg msg' ->
            let newLoadedCorpusModel, cmd =
                Update.Metadata.update msg' loadedCorpusModel

            newLoadedCorpusModel, Cmd.map MetadataMsg cmd

        | ShowingResultsMsg msg' ->
            match loadedCorpusModel.Substate with
            | ShowingResults m ->
                let newLoadedCorpusModel, newShowingResultsModel, cmd =
                    ShowingResults.update msg' loadedCorpusModel m

                { newLoadedCorpusModel with
                      Substate = ShowingResults newShowingResultsModel },
                Cmd.map ShowingResultsMsg cmd
            | otherSubstate -> failwith $"Invalid substate for ShowingResultsMsg: {otherSubstate}"

        | SetShouldShowMetadataMenu shouldShow ->
            { loadedCorpusModel with
                  ShouldShowMetadataMenu = Some shouldShow },
            Cmd.none

        | SetSearchInterface ``interface`` ->
            { loadedCorpusModel with
                  Search =
                      { loadedCorpusModel.Search with
                            Interface = ``interface`` } },
            Cmd.none

        | SetQueryText (text, queryIndex, hasFinalSpace) ->
            let newQueries =
                loadedCorpusModel.Search.Params.Queries
                |> Array.mapi
                    (fun index query ->
                        if index = queryIndex then
                            { query with
                                  HasFinalSpace = hasFinalSpace
                                  QueryString = text }
                        else
                            query)

            let newSearchParams =
                { loadedCorpusModel.Search.Params with
                      Queries = newQueries }

            let newLoadedCorpusModel =
                { loadedCorpusModel with
                      Search =
                          { loadedCorpusModel.Search with
                                Params = newSearchParams } }

            newLoadedCorpusModel, Cmd.none

        | AddQueryRow ->
            let newQueries =
                Array.append loadedCorpusModel.Search.Params.Queries [| Query.Init(None) |]

            let newLoadedCorpusModel =
                { loadedCorpusModel with
                      Search =
                          { loadedCorpusModel.Search with
                                Params =
                                    { loadedCorpusModel.Search.Params with
                                          Queries = newQueries } } }

            newLoadedCorpusModel, Cmd.none

        | RemoveQueryRow queryIndex ->
            let newQueries =
                loadedCorpusModel.Search.Params.Queries
                |> Array.indexed
                |> Array.choose
                    (fun (index, query) ->
                        if index <> queryIndex then
                            Some query
                        else
                            None)

            let newLoadedCorpusModel =
                { loadedCorpusModel with
                      Search =
                          { loadedCorpusModel.Search with
                                Params =
                                    { loadedCorpusModel.Search.Params with
                                          Queries = newQueries } } }

            newLoadedCorpusModel, Cmd.none

        | CwbExtendedSetMainString (query, queryIndex, term, termIndex, maybeValue) ->
            let newTerm =
                { term with
                      MainStringValue = maybeValue }

            let newLoadedCorpusModel =
                updateQueryTerm loadedCorpusModel query queryIndex newTerm termIndex

            newLoadedCorpusModel, Cmd.none

        | CwbExtendedSetQueryProperty (query, queryIndex, term, termIndex, property, isChecked) ->
            let newTerm =
                match property with
                | IsLemma -> { term with IsLemma = isChecked }
                | IsPhonetic -> { term with IsPhonetic = isChecked }
                | IsOriginal -> { term with IsOriginal = isChecked }
                | IsStart ->
                    let isEnd = if isChecked then false else term.IsEnd

                    let isMiddle =
                        if isChecked then
                            false
                        else
                            term.IsMiddle

                    { term with
                          IsStart = isChecked
                          IsEnd = isEnd
                          IsMiddle = isMiddle }
                | IsEnd ->
                    let isStart =
                        if isChecked then
                            false
                        else
                            term.IsStart

                    let isMiddle =
                        if isChecked then
                            false
                        else
                            term.IsMiddle

                    { term with
                          IsStart = isStart
                          IsEnd = isChecked
                          IsMiddle = isMiddle }
                | IsMiddle ->
                    let isStart =
                        if isChecked then
                            false
                        else
                            term.IsStart

                    let isEnd = if isChecked then false else term.IsEnd

                    { term with
                          IsStart = isStart
                          IsEnd = isEnd
                          IsMiddle = isChecked }
                | IsInitial -> { term with IsInitial = isChecked }
                | IsFinal -> { term with IsFinal = isChecked }

            let newLoadedCorpusModel =
                updateQueryTerm loadedCorpusModel query queryIndex newTerm termIndex

            newLoadedCorpusModel, Cmd.none

        | CwbExtendedSetExtraForm value ->
            let newLoadedCorpusModel =
                match loadedCorpusModel.Search.Interface with
                | Extended (Some attrModalModel) ->
                    { loadedCorpusModel with
                          Search =
                              { loadedCorpusModel.Search with
                                    Interface =
                                        Extended(
                                            Some
                                                { attrModalModel with
                                                      IncludeExcludeInput = value }
                                        ) } }
                | _ -> loadedCorpusModel

            newLoadedCorpusModel, Cmd.none

        | CwbExtendedIncludeOrExcludeExtraForm (query, queryIndex, term, termIndex, command) ->
            match loadedCorpusModel.Search.Interface with
            | Extended (Some attrModalModel) ->
                let attrName, operator =
                    match command with
                    | "specify_word" -> "word", Equals
                    | "specify_lemma" -> "lemma", Equals
                    | "specify_phon" -> "phon", Equals
                    | "specify_orig" -> "phon", Equals
                    | "exclude_word" -> "word", NotEquals
                    | "exclude_lemma" -> "lemma", NotEquals
                    | "exclude_phon" -> "phon", NotEquals
                    | "exclude_orig" -> "phon", NotEquals
                    | _ -> failwith $"Unknown command: {command}"

                let newExtraForms =
                    addOrRemoveExtraForms term attrName operator attrModalModel.IncludeExcludeInput

                let newTerm = { term with ExtraForms = newExtraForms }

                let newLoadedCorpusModel =
                    updateQueryTerm loadedCorpusModel query queryIndex newTerm termIndex
                    |> fun m ->
                        { m with
                              Search =
                                  { m.Search with
                                        Interface =
                                            Extended(
                                                Some
                                                    { attrModalModel with
                                                          IncludeExcludeInput = "" }
                                            ) } }

                newLoadedCorpusModel, Cmd.none
            | _ -> loadedCorpusModel, Cmd.none

        | CwbExtendedRemoveExtraForms (query, queryIndex, term, termIndex, attrName) ->
            // Remove all extra forms for a given attribute name
            let newExtraForms =
                term.ExtraForms
                |> List.filter (fun forms -> forms.Attr <> attrName)

            let newTerm = { term with ExtraForms = newExtraForms }

            let newLoadedCorpusModel =
                updateQueryTerm loadedCorpusModel query queryIndex newTerm termIndex

            newLoadedCorpusModel, Cmd.none

        | CwbExtendedToggleAttributeCategory (query, queryIndex, term, termIndex, categorySectionIndex, category) ->
            let newCategorySections =
                term.CategorySections
                |> List.mapi
                    (fun i section ->
                        if i = categorySectionIndex then
                            section
                            |> Set.toArray
                            |> Array.tryFind (fun s -> s.Attr = category.Attr && s.Value = category.Value)
                            |> function
                                | Some existingCat -> section.Remove(existingCat)
                                | None ->
                                    // If the operator of the category we are currently selecting or excluding is different
                                    // from the existing ones, remove the existing ones, since it does not make any sense to
                                    // specify categories to include and to exclude at the same time (i.e., if you include specific
                                    // categories the rest are automatically excluded, and if you exclude specific categories
                                    // the rest are automatically included.)
                                    if section.Count > 0
                                       && section.MinimumElement.Operator
                                          <> category.Operator then
                                        Set.singleton category
                                    else
                                        section.Add(category)
                        else
                            section)

            let newTerm =
                { term with
                      CategorySections = newCategorySections }

            let newLoadedCorpusModel =
                updateQueryTerm loadedCorpusModel query queryIndex newTerm termIndex

            newLoadedCorpusModel, Cmd.none

        | CwbExtendedToggleAttributeSubcategory (query,
                                                 queryIndex,
                                                 term,
                                                 termIndex,
                                                 categorySectionIndex,
                                                 mainCategory,
                                                 subcategory) ->
            let newCategorySections =
                term.CategorySections
                |> List.mapi
                    (fun i sectionMainCategories ->
                        if i = categorySectionIndex then
                            // This is the section we want to change. Find the given main category
                            // and modify the given subcategory inside it.
                            sectionMainCategories
                            |> Set.map
                                (fun cat ->
                                    if cat = mainCategory then
                                        let subcategories =
                                            match cat.Subcategories with
                                            | Some subcats ->
                                                subcats
                                                |> Set.toArray
                                                |> Array.tryFind (fun sc -> sc.Attr = subcategory.Attr)
                                                |> function
                                                    | Some existingSubcat ->
                                                        let scs = subcats.Remove(existingSubcat)

                                                        if subcategory.Values.IsEmpty then
                                                            // All values in this subcategory have been deselected in the new version,
                                                            // so don't add it to the list of subcategories for this main category
                                                            scs
                                                        else
                                                            scs.Add(subcategory)
                                                    | None -> subcats.Add(subcategory)
                                            | None -> Set.singleton subcategory

                                        { cat with
                                              Subcategories = Some subcategories }
                                    else
                                        cat)
                        else
                            sectionMainCategories)

            let newTerm =
                { term with
                      CategorySections = newCategorySections }

            let newLoadedCorpusModel =
                updateQueryTerm loadedCorpusModel query queryIndex newTerm termIndex

            newLoadedCorpusModel, Cmd.none

        | CwbExtendedClearAttributeCategories (query, queryIndex, term, termIndex) ->
            let newTerm =
                { term with
                      CategorySections = []
                      ExtraForms = [] }

            let newLoadedCorpusModel =
                updateQueryTerm loadedCorpusModel query queryIndex newTerm termIndex

            newLoadedCorpusModel, Cmd.none
        | CwbExtendedSetIntervalValue (query, queryIndex, term, termIndex, minMax, maybeValue) ->
            let maybeNewInterval =
                let i =
                    term.PrecedingInterval
                    |> Option.defaultValue { Min = None; Max = None }

                match minMax with
                | Min -> { i with Min = maybeValue }
                | Max -> { i with Max = maybeValue }
                |> fun newI ->
                    if newI = { Min = None; Max = None } then
                        None
                    else
                        Some newI

            let newTerm =
                { term with
                      PrecedingInterval = maybeNewInterval }

            let newLoadedCorpusModel =
                updateQueryTerm loadedCorpusModel query queryIndex newTerm termIndex

            newLoadedCorpusModel, Cmd.none

        | CwbExtendedAddTerm (query, queryIndex) ->
            let newQueryTerms =
                Array.append query.Terms [| QueryTerm.Default |]

            let newLoadedCorpusModel =
                updateQuery loadedCorpusModel query queryIndex newQueryTerms

            newLoadedCorpusModel, Cmd.none

        | CwbExtendedRemoveTerm (query, queryIndex, termIndex) ->
            let newQueryTerms =
                query.Terms
                |> Array.indexed
                |> Array.choose
                    (fun (i, t) ->
                        if i = termIndex + 1 then
                            // When we remove a term, we also have to remove any interval that may have
                            // specified between it and the next term, if any. The interval is specified
                            // on the next term.
                            Some { t with PrecedingInterval = None }
                        elif i <> termIndex then
                            Some t
                        else
                            None)

            let newLoadedCorpusModel =
                updateQuery loadedCorpusModel query queryIndex newQueryTerms

            newLoadedCorpusModel, Cmd.none

        | CwbExtendedToggleAttrModal maybeTermIndex ->
            { loadedCorpusModel with
                  Search =
                      { loadedCorpusModel.Search with
                            Interface =
                                match maybeTermIndex with
                                | Some termIndex -> Extended(Some(AttributeModalModel.Init(termIndex)))
                                | None ->
                                    match loadedCorpusModel.Search.Interface with
                                    | Extended _ -> Extended None
                                    | someInterface -> someInterface } },
            Cmd.none

        | SetNumRandomHits maybeNumHits ->
            { loadedCorpusModel with
                  Search =
                      { loadedCorpusModel.Search with
                            Params =
                                { loadedCorpusModel.Search.Params with
                                      NumRandomHits = maybeNumHits } } },
            Cmd.none
        | SetRandomHitsSeed maybeSeed ->
            { loadedCorpusModel with
                  Search =
                      { loadedCorpusModel.Search with
                            Params =
                                { loadedCorpusModel.Search.Params with
                                      RandomHitsSeed = maybeSeed } } },
            Cmd.none
        | Search ->
            // Do three search steps only if multicpu_bounds is defined for this corpus
            let numSteps =
                if loadedCorpusModel.Corpus.SharedInfo.MultiCpuBounds.IsSome then
                    3
                else
                    1

            // If we have an empty query, don't do the search
            let shouldSearch =
                loadedCorpusModel.Search.Params.Queries
                |> Array.forall (fun query -> not query.IsEmpty)

            // TODO: Implement cancellation of searches. In the Clojure version, we simply cancel
            // any HTTP request which is already running when we start a new search, but what we
            // should do instead is to cancel the actual search that is running on the server.

            if shouldSearch then
                let queries =
                    loadedCorpusModel.Search.Params.Queries
                    |> Array.map
                        (fun query ->
                            { query with
                                  QueryString =
                                      query.QueryString
                                      |> replace "\"__QUOTE__\"" "'\"'" })

                let searchParams =
                    { loadedCorpusModel.Search.Params with
                          CpuCounts = None
                          End = 99UL
                          LastCount = None
                          Queries = queries
                          SearchId = 0
                          Start = 0UL
                          Step = 1 }

                let newLoadedCorpusModel =
                    { loadedCorpusModel with
                          Substate =
                              ShowingResults(
                                  ShowingResultsModel.Init(
                                      numSteps,
                                      string loadedCorpusModel.Search.Params.ContextSize,
                                      []
                                  )
                              )
                          Search =
                              { loadedCorpusModel.Search with
                                    Params = searchParams } }

                let cmds =
                    [ Cmd.ofMsg (CwbExtendedToggleAttrModal None)
                      Cmd.ofMsg (
                          ShowingResultsMsg(ShowingResults.ConcordanceMsg(ShowingResults.Concordance.PerformSearchStep))
                      ) ]
                    |> Cmd.batch

                newLoadedCorpusModel, cmds

            else
                printfn "Empty query!"
                loadedCorpusModel, Cmd.none

        | ResetForm ->
            { loadedCorpusModel with
                  Search = Search.Init(loadedCorpusModel.Corpus.SharedInfo)
                  Substate = CorpusStart
                  OpenMetadataCategoryCode = None },
            Cmd.ofMsg (MetadataMsg Update.Metadata.FetchTextAndTokenCounts)
        | ClosePopups ->
            { loadedCorpusModel with
                  OpenMetadataCategoryCode = None
                  ShouldShowQuickView = false },
            Cmd.none

////////////////////////////////
// Update
////////////////////////////////
type Msg =
    | LoadingCorpusMsg of LoadingCorpus.Msg
    | LoadedCorpusMsg of LoadedCorpus.Msg

let init () : Model * Cmd<Msg> =
    let model = LoadingCorpus

    let corpusCode =
        Browser.Dom.window.location.href.Split('/')
        |> Array.last

    let cmd =
        Cmd.OfAsync.perform serverApi.GetCorpusConfig corpusCode LoadingCorpus.FetchedCorpusConfig

    model, Cmd.map LoadingCorpusMsg cmd

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg, model with
    | LoadingCorpusMsg msg', LoadingCorpus ->
        let newModel, cmd = LoadingCorpus.update msg' model
        newModel, Cmd.map LoadingCorpusMsg cmd
    | LoadedCorpusMsg msg', LoadedCorpus model' ->
        let newModel, cmd = LoadedCorpus.update msg' model'
        LoadedCorpus newModel, Cmd.map LoadedCorpusMsg cmd
    | _ -> failwithf $"Incompatible message and model: {msg}; {model}"
