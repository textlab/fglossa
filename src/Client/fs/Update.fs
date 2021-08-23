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
                  IsNarrowWindow = false
                  IsShowSelectionOpen = false
                  OpenMetadataCategoryCode = None
                  Search = Search.Init(corpus.Config)
                  ShouldShowMetadataMenu = None
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
                | FetchResultWindow of int
                | FetchedResultWindow of SearchResultPage []
                | SetPaginatorTextValue of string
                | SetPaginatorPage of int option
                | SetContextSizeTextValue of string
                | SetContextSize of int

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

                    let modelWithResultPages = registerResultPages results.ResultPages

                    let newModel =
                        { modelWithResultPages with
                              IsSearching = shouldRunMoreSteps
                              NumResults = Some results.Count
                              SearchParams = newSearchParams }

                    newModel, cmd

                | SetContextSizeTextValue v -> { model with ContextSizeTextValue = v }, Cmd.none

                | SetContextSize _ -> failwith "The SetContextSize message should be handled by a parent!"

                // Fetch a window of search result pages centred on centrePageNo. Ignores pages that have
                // already been fetched or that are currently being fetched in another request (note that such
                // pages can only be located at the edges of the window, and not as 'holes' within the window,
                // since they must have been fetched as part of an earlier window).
                | FetchResultWindow centrePageNo ->
                    // Make sure the edges of the window are between 1 and the last page number
                    let startPage = max (centrePageNo - 1) 1

                    let endPage =
                        min (centrePageNo + 1) (model.NumResultPages())

                    let pageNumbers =
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
                                  End = lastResult }

                        let newModel =
                            { model with
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

                | SetPaginatorPage maybePageNo ->
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
                                      model.PaginatorPageNo }

                    let cmd =
                        // If necessary, fetch any result pages in a window centred
                        // around the selected page in order to speed up pagination
                        // to nearby pages. No need to wait for it to finish though.
                        Cmd.ofMsg (FetchResultWindow pageNo)

                    newModel, cmd

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

        | SetSearchInterface of SearchInterface
        | SetQueryText of query: string * hasFinalSpace: bool
        | CwbExtendedSetMainString of
            query: Query *
            queryIndex: int *
            term: QueryTerm *
            termIndex: int *
            value: string option
        | CwbExtendedSetQueryProperty of
            query: Query *
            queryIndex: int *
            term: QueryTerm *
            termIndex: int *
            property: QueryProperty *
            value: bool
        | CwbExtendedToggleAttributeCategory of
            query: Query *
            queryIndex: int *
            term: QueryTerm *
            termIndex: int *
            categorySectionIndex: int *
            category: MainCategory
        | CwbExtendedSetIntervalValue of
            query: Query *
            queryIndex: int *
            term: QueryTerm *
            termIndex: int *
            minMax: MinMax *
            value: int option
        | CwbExtendedAddTerm of query: Query * queryIndex: int
        | CwbExtendedRemoveTerm of query: Query * queryIndex: int * termIndex: int
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

        | SetSearchInterface ``interface`` ->
            { model with
                  Search =
                      { model.Search with
                            Interface = ``interface`` } },
            Cmd.none

        | SetQueryText (text, hasFinalSpace) ->
            printfn $"query: {text}"

            let newSearchParams =
                { model.Search.Params with
                      Queries =
                          [| { HasFinalSpace = hasFinalSpace
                               LanguageCode = model.Search.Params.Queries.[0].LanguageCode
                               QueryString = text } |] }

            let newModel =
                { model with
                      Search =
                          { model.Search with
                                Params = newSearchParams } }

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

        | CwbExtendedToggleAttributeCategory (query, queryIndex, term, termIndex, categorySectionIndex, category) ->
            let newCategorySections =
                term.CategorySections
                |> List.mapi
                    (fun i section ->
                        if i = categorySectionIndex then
                            if section.Contains(category) then
                                section.Remove(category)
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
                |> Array.choose (fun (i, t) -> if i <> termIndex then Some t else None)

            let newModel =
                updateQuery model query queryIndex newQueryTerms

            newModel, Cmd.none

        | CwbExtendedToggleAttrModal maybeTermIndex ->
            { model with
                  Search =
                      { model.Search with
                            Interface = Extended maybeTermIndex } },
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
                                      string model.Search.Params.ContextSize
                                  )
                              ) }

                newModel,
                Cmd.ofMsg (
                    ShowingResultsMsg(ShowingResults.ConcordanceMsg(ShowingResults.Concordance.PerformSearchStep))
                )
            else
                printfn "Empty query!"
                model, Cmd.none

        | ResetForm ->
            { model with
                  Search = Search.Init(model.Corpus.Config)
                  Substate = CorpusStart },
            Cmd.none




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
