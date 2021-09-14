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
    type Msg = FetchedCorpusConfig of CorpusConfig

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
                  Search = Search.Init(corpus.Config)
                  ShouldShowMetadataMenu = None
                  SelectionTablePageNumber = 1
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

            let update (msg: Msg) (model: ConcordanceModel) : ConcordanceModel * Cmd<Msg> =
                let registerResultPages results =
                    let fetchedPages =
                        results |> Array.map (fun page -> page.PageNumber)

                    // Remove the fetched pages from the set of pages currently being fetched...
                    let pagesBeingFetched =
                        model.PagesBeingFetched
                        |> Array.except fetchedPages

                    // ...and add them to the map of fetched pages
                    let resultPages =
                        results
                        |> Array.fold
                            (fun (pageMap: Map<int, SearchResult []>) result ->
                                pageMap.Add(result.PageNumber, result.Results |> Array.map cleanupResult))
                            model.ResultPages

                    { model with
                          PagesBeingFetched = pagesBeingFetched
                          ResultPages = resultPages }

                match msg with
                | PerformSearchStep ->
                    let cmd =
                        Cmd.OfAsync.perform serverApi.SearchCorpus model.SearchParams SearchResultsReceived

                    model, cmd

                | SearchResultsReceived results ->
                    let shouldRunMoreSteps = model.NumSteps > results.SearchStep

                    let newSearchParams =
                        model.SearchParams
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
                        model.PagesBeingFetched
                        |> Array.except fetchedPages

                    // ...and put them into the map of fetched pages. Since we may already have
                    // recived a half-filled page from a previous search step, we need to gather
                    // all previsous and current results together and chunk them into page-sized
                    // chunks to make sure we get the correct page sizes.
                    let existingResults =
                        model.ResultPages
                        |> Map.toArray
                        |> Array.collect snd

                    let newResults =
                        results.ResultPages
                        |> Array.collect (fun page -> page.Results)

                    let resultPages =
                        Array.append existingResults newResults
                        |> Array.map cleanupResult
                        |> Array.chunkBySize model.SearchParams.PageSize
                        |> Array.mapi (fun index page -> (index + 1, page))
                        |> Map.ofArray

                    let newModel =
                        { model with
                              PagesBeingFetched = pagesBeingFetched
                              ResultPages = resultPages
                              IsSearching = shouldRunMoreSteps
                              NumResults = Some results.Count
                              SearchParams = newSearchParams }

                    newModel, cmd

                // Fetch a window of search result pages centred on centrePageNo. Ignores pages that have
                // already been fetched or that are currently being fetched in another request (note that such
                // pages can only be located at the edges of the window, and not as 'holes' within the window,
                // since they must have been fetched as part of an earlier window).
                | FetchResultWindow (centrePageNo, maybeSortKey) ->
                    let sortKey =
                        maybeSortKey
                        |> Option.defaultValue model.SearchParams.SortKey

                    // Make sure the edges of the window are between 1 and the last page number
                    let startPage = max (centrePageNo - 1) 1

                    let endPage =
                        min (centrePageNo + 1) (model.NumResultPages())

                    let pageNumbers =
                        if sortKey <> model.SearchParams.SortKey then
                            // If we are changing the sort order, we need to fetch all specified pages
                            // regardless of whether those page numbers have already been fetched
                            [| startPage .. endPage |]
                        else
                            [| startPage .. endPage |]
                            // Ignore pages currently being fetched by another request
                            |> Array.filter
                                (fun page ->
                                    model.PagesBeingFetched
                                    |> Array.contains page
                                    |> not)
                            // Ignore pages that have already been fetched
                            |> Array.filter (fun page -> model.ResultPages |> Map.containsKey page |> not)
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
                        model, Cmd.none
                    else
                        // Calculate the first and last result index (zero-based) to request from the server
                        let firstResult =
                            (pageNumbers.[0] - 1)
                            * model.SearchParams.PageSize
                            |> uint64

                        let lastResult =
                            ((pageNumbers |> Array.last)
                             * model.SearchParams.PageSize)
                            - 1
                            |> uint64

                        let searchParams =
                            { model.SearchParams with
                                  Start = firstResult
                                  End = lastResult
                                  SortKey = sortKey }

                        let newModel =
                            { model with
                                  SearchParams = searchParams
                                  // Register the pages as being fetched
                                  PagesBeingFetched = Array.append model.PagesBeingFetched pageNumbers }

                        let cmd =
                            Cmd.OfAsync.perform
                                serverApi.GetSearchResults
                                (searchParams, pageNumbers)
                                FetchedResultWindow

                        newModel, cmd

                | FetchedResultWindow results ->
                    // TODO: Check if we get a 401 Unauthorized
                    let newModel =
                        results
                        |> registerResultPages
                        // Now that result pages have been fetched, make sure we actually show the page that was
                        // selected in the paginator
                        |> fun m ->
                            { m with
                                  ResultPageNo = m.PaginatorPageNo }

                    newModel, Cmd.none

                | SetPaginatorTextValue s -> { model with PaginatorTextValue = s }, Cmd.none

                | SetPaginatorPage (maybePageNo, maybeSortKey) ->
                    let pageNo =
                        match maybePageNo with
                        // If we got an explicit page number, use that
                        | Some p -> p
                        // If we didn't get an explicit page number, try to parse the current PaginatorTextValue
                        // as an int. If successful, use the result of that, otherwise keep the current page number
                        | None ->
                            match Int32.TryParse(model.PaginatorTextValue) with
                            | (true, v) -> v
                            | (false, _) -> model.PaginatorPageNo

                    let newModel =
                        { model with
                              // Set the value of the page number shown in the paginator; it may
                              // differ from the page shown in the result table until we have
                              // actually fetched the data from the server
                              PaginatorPageNo = pageNo
                              PaginatorTextValue = string pageNo
                              ResultPageNo =
                                  if model.ResultPages.ContainsKey(pageNo) then
                                      // If the newly selected result page has already been fetched from the
                                      // server, it can be shown in the result table immediately
                                      pageNo
                                  else
                                      // Otherwise, we need to wait until the results from the server
                                      // arrive before changing the page to be shown in the result
                                      // table
                                      model.PaginatorPageNo
                              ResultPages =
                                  match maybeSortKey with
                                  | Some sortKey when sortKey <> model.SearchParams.SortKey ->
                                      // If we have received a sort key that is different from the one currently
                                      // set in the model, invalidate all previously fetched search result pages
                                      Map.empty
                                  | _ -> model.ResultPages }


                    let cmd =
                        // If necessary, fetch any result pages in a window centred
                        // around the selected page in order to speed up pagination
                        // to nearby pages. No need to wait for it to finish though.
                        Cmd.ofMsg (FetchResultWindow(pageNo, maybeSortKey))

                    newModel, cmd

                | SetContextSizeTextValue v -> { model with ContextSizeTextValue = v }, Cmd.none

                | SetContextSize _ -> failwith "The SetContextSize message should be handled by a parent!"

                | FetchMetadataForText (corpus, textId) ->
                    let (categories: Metadata.CategoryNameAndCode list) =
                        [ for category in corpus.MetadataQuickView ->
                              { Name = category.Name
                                Code = category.Code } ]

                    let newModel, cmd =
                        match model.TextIdInQuickView with
                        | Some textIdInQuickView when textIdInQuickView = textId ->
                            // We were already showing metadata for this text, so close the quickview instead
                            { model with TextIdInQuickView = None }, Cmd.ofMsg CloseQuickView
                        | _ ->
                            // We were NOT already showing metadata for this text, so mark it as being shown and fetch its metadata
                            { model with
                                  TextIdInQuickView = Some textId },
                            Cmd.OfAsync.perform
                                serverApi.GetMetadataForSingleText
                                (corpus.Config.Code, categories, textId)
                                FetchedMetadataForText

                    newModel, cmd

                | FetchedMetadataForText metadata ->
                    let newModel =
                        { model with
                              QuickViewMetadata = metadata
                              ShouldShowQuickView = true }

                    newModel, Cmd.none

                | CloseQuickView ->
                    { model with
                          ShouldShowQuickView = false },
                    Cmd.none

        ///////////////////////////////////////////
        // Update.LoadedCorpus.ShowingResults
        ///////////////////////////////////////////
        type Msg =
            | ConcordanceMsg of Concordance.Msg

            | SelectResultTab of ResultTab

        let update (msg: Msg) (model: ShowingResultsModel) : ShowingResultsModel * Cmd<Msg> =
            match msg with
            | ConcordanceMsg msg' ->
                match model.ActiveTab with
                | Concordance m ->
                    let newM, cmd = Concordance.update msg' m

                    { model with
                          ActiveTab = Concordance newM },
                    Cmd.map ConcordanceMsg cmd
                | otherSubstate -> failwith $"Invalid substate for ConcordanceMsg: {otherSubstate}"

            | SelectResultTab tab -> { model with ActiveTab = tab }, Cmd.none


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
        | Search
        | ResetForm

    let update (msg: Msg) (model: LoadedCorpusModel) : LoadedCorpusModel * Cmd<Msg> =
        match msg with
        | MetadataMsg msg' ->
            let model', cmd = Update.Metadata.update msg' model
            model', Cmd.map MetadataMsg cmd

        // The SetContextSize message is dispatched from the concordance view, but needs to be handled here
        // where the search params are available in the model and the Search message is also available
        | ShowingResultsMsg (ShowingResults.ConcordanceMsg (ShowingResults.Concordance.SetContextSize size)) ->
            let newSearchParams =
                { model.Search.Params with
                      ContextSize = size }

            let newModel =
                { model with
                      Search =
                          { model.Search with
                                Params = newSearchParams } }

            newModel, Cmd.ofMsg Search

        | ShowingResultsMsg msg' ->
            match model.Substate with
            | ShowingResults m ->
                let newM, cmd = ShowingResults.update msg' m

                { model with
                      Substate = ShowingResults newM },
                Cmd.map ShowingResultsMsg cmd
            | otherSubstate -> failwith $"Invalid substate for ShowingResultsMsg: {otherSubstate}"

        | SetShouldShowMetadataMenu shouldShow ->
            { model with
                  ShouldShowMetadataMenu = Some shouldShow },
            Cmd.none

        | SetSearchInterface ``interface`` ->
            { model with
                  Search =
                      { model.Search with
                            Interface = ``interface`` } },
            Cmd.none

        | SetQueryText (text, queryIndex, hasFinalSpace) ->
            let newQueries =
                model.Search.Params.Queries
                |> Array.mapi
                    (fun index query ->
                        if index = queryIndex then
                            { query with
                                  HasFinalSpace = hasFinalSpace
                                  QueryString = text }
                        else
                            query)

            let newSearchParams =
                { model.Search.Params with
                      Queries = newQueries }

            let newModel =
                { model with
                      Search =
                          { model.Search with
                                Params = newSearchParams } }

            newModel, Cmd.none

        | AddQueryRow ->
            let newQueries =
                Array.append model.Search.Params.Queries [| Query.Init(None) |]

            let newModel =
                { model with
                      Search =
                          { model.Search with
                                Params =
                                    { model.Search.Params with
                                          Queries = newQueries } } }

            newModel, Cmd.none

        | RemoveQueryRow queryIndex ->
            let newQueries =
                model.Search.Params.Queries
                |> Array.indexed
                |> Array.choose
                    (fun (index, query) ->
                        if index <> queryIndex then
                            Some query
                        else
                            None)

            let newModel =
                { model with
                      Search =
                          { model.Search with
                                Params =
                                    { model.Search.Params with
                                          Queries = newQueries } } }

            newModel, Cmd.none

        | CwbExtendedSetMainString (query, queryIndex, term, termIndex, maybeValue) ->
            let newTerm =
                { term with
                      MainStringValue = maybeValue }

            let newModel =
                updateQueryTerm model query queryIndex newTerm termIndex

            newModel, Cmd.none

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

            let newModel =
                updateQueryTerm model query queryIndex newTerm termIndex

            newModel, Cmd.none

        | CwbExtendedSetExtraForm value ->
            let newModel =
                match model.Search.Interface with
                | Extended (Some attrModalModel) ->
                    { model with
                          Search =
                              { model.Search with
                                    Interface =
                                        Extended(
                                            Some
                                                { attrModalModel with
                                                      IncludeExcludeInput = value }
                                        ) } }
                | _ -> model

            newModel, Cmd.none

        | CwbExtendedIncludeOrExcludeExtraForm (query, queryIndex, term, termIndex, command) ->
            match model.Search.Interface with
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

                let newModel =
                    updateQueryTerm model query queryIndex newTerm termIndex
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

                newModel, Cmd.none
            | _ -> model, Cmd.none

        | CwbExtendedRemoveExtraForms (query, queryIndex, term, termIndex, attrName) ->
            // Remove all extra forms for a given attribute name
            let newExtraForms =
                term.ExtraForms
                |> List.filter (fun forms -> forms.Attr <> attrName)

            let newTerm = { term with ExtraForms = newExtraForms }

            let newModel =
                updateQueryTerm model query queryIndex newTerm termIndex

            newModel, Cmd.none

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

            let newModel =
                updateQueryTerm model query queryIndex newTerm termIndex

            newModel, Cmd.none

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
                                            | None -> Set.singleton (subcategory)

                                        { cat with
                                              Subcategories = Some subcategories }
                                    else
                                        cat)
                        else
                            sectionMainCategories)

            let newTerm =
                { term with
                      CategorySections = newCategorySections }

            let newModel =
                updateQueryTerm model query queryIndex newTerm termIndex

            newModel, Cmd.none

        | CwbExtendedClearAttributeCategories (query, queryIndex, term, termIndex) ->
            let newTerm =
                { term with
                      CategorySections = []
                      ExtraForms = [] }

            let newModel =
                updateQueryTerm model query queryIndex newTerm termIndex

            newModel, Cmd.none
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

            let newModel =
                updateQueryTerm model query queryIndex newTerm termIndex

            newModel, Cmd.none

        | CwbExtendedAddTerm (query, queryIndex) ->
            let newQueryTerms =
                Array.append query.Terms [| QueryTerm.Default |]

            let newModel =
                updateQuery model query queryIndex newQueryTerms

            newModel, Cmd.none

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

            let newModel =
                updateQuery model query queryIndex newQueryTerms

            newModel, Cmd.none

        | CwbExtendedToggleAttrModal maybeTermIndex ->
            { model with
                  Search =
                      { model.Search with
                            Interface =
                                match maybeTermIndex with
                                | Some termIndex -> Extended(Some(AttributeModalModel.Init(termIndex)))
                                | None ->
                                    match model.Search.Interface with
                                    | Extended _ -> Extended None
                                    | someInterface -> someInterface } },
            Cmd.none

        | Search ->
            // Do three search steps only if multicpu_bounds is defined for this corpus
            let numSteps =
                if model.Corpus.Config.MultiCpuBounds.IsSome then
                    3
                else
                    1

            // If we have an empty query, don't do the search
            let shouldSearch =
                let firstQuery =
                    model.Search.Params.Queries |> Array.tryHead

                match firstQuery with
                | None -> false
                | Some query -> not query.IsEmpty

            // TODO: Implement cancellation of searches. In the Clojure version, we simply cancel
            // any HTTP request which is already running when we start a new search, but what we
            // should do instead is to cancel the actual search that is running on the server.

            if shouldSearch then
                let queries =
                    model.Search.Params.Queries
                    |> Array.map
                        (fun query ->
                            { query with
                                  QueryString =
                                      query.QueryString
                                      |> replace "\"__QUOTE__\"" "'\"'" })

                let searchParams =
                    { model.Search.Params with
                          Queries = queries }

                let newModel =
                    { model with
                          Substate =
                              ShowingResults(
                                  ShowingResultsModel.Init(
                                      searchParams,
                                      numSteps,
                                      string model.Search.Params.ContextSize,
                                      []
                                  )
                              ) }

                let cmds =
                    [ Cmd.ofMsg (CwbExtendedToggleAttrModal None)
                      Cmd.ofMsg (
                          ShowingResultsMsg(ShowingResults.ConcordanceMsg(ShowingResults.Concordance.PerformSearchStep))
                      ) ]
                    |> Cmd.batch

                newModel, cmds

            else
                printfn "Empty query!"
                model, Cmd.none

        | ResetForm ->
            { model with
                  Search = Search.Init(model.Corpus.Config)
                  Substate = CorpusStart
                  OpenMetadataCategoryCode = None },
            Cmd.ofMsg (MetadataMsg Update.Metadata.FetchTextAndTokenCounts)

////////////////////////////////
// Update
////////////////////////////////
type Msg =
    | LoadingCorpusMsg of LoadingCorpus.Msg
    | LoadedCorpusMsg of LoadedCorpus.Msg

let init () : Model * Cmd<Msg> =
    let model = LoadingCorpus

    let cmd =
        Cmd.OfAsync.perform serverApi.GetCorpusConfig "bokmal" LoadingCorpus.FetchedCorpusConfig

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
