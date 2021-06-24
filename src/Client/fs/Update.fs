module Update

open System
open System.Text.RegularExpressions
open Elmish
open Shared
open Model
open Shared.StringUtils

let cleanupResult (result: SearchResult) =
    let cleanLines =
        result.Text
        |> List.map
            (fun line ->
                line
                // Remove the beginning of the search result, which will be a position
                // number in the case of a monolingual result or the first language of a
                // multilingual result, or an arrow in the case of subsequent languages
                // in a multilingual result.
                |> replace "^\s*\d+:\s*" ""
                |> replace "^-->.+?:\s*" ""
                // When the match includes the first or last token of the s unit, the XML
                // tag surrounding the s unit is included inside the match braces (this
                // should probably be considered a bug in CQP). We need to fix that.
                |> replace "\{\{(<s_id\s+.+?>)" "$1{{"
                |> replace "(</s_id>)\}\}" "}}$1"
                |> replace "&nbsp;" "_")

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
                  Search = Search.Init(corpus.Config.Code)
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
                | SetPaginatorPage of int

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
                        Cmd.OfAsync.perform serverApi.searchCorpus model.SearchParams SearchResultsReceived

                    model, cmd

                | SearchResultsReceived results ->
                    let modelWithResultPages = registerResultPages results.ResultPages

                    let newModel =
                        { modelWithResultPages with
                              IsSearching = false
                              NumResults = Some results.Count }

                    newModel, Cmd.none

                // Fetch a window of search result pages centred on centrePageNo. Ignores pages that have
                // already been fetched or that are currently being fetched in another request (note that such
                // pages can only be located at the edges of the window, and not as 'holes' within the window,
                // since they must have been fetched as part of an earlier window).
                | FetchResultWindow centrePageNo ->
                    // Make sure the edges of the window are between 1 and the last page number
                    let startPage = max (centrePageNo - 1) 1

                    let endPage =
                        min (centrePageNo + 1) model.ResultPages.Count

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
                            Cmd.OfAsync.perform serverApi.getSearchResults searchParams FetchedResultWindow

                        newModel, cmd

                | FetchedResultWindow results ->
                    // TODO: Check if we get a 401 Unauthorized
                    let newModel = registerResultPages results

                    newModel, Cmd.none

                | SetPaginatorPage pageNo ->
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
        | Search


    let update (msg: Msg) (model: LoadedCorpusModel) : LoadedCorpusModel * Cmd<Msg> =
        match msg with
        | MetadataMsg msg' ->
            let model', cmd = Update.Metadata.update msg' model
            model', Cmd.map MetadataMsg cmd

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
            let newSearchParams =
                { model.Search.Params with
                      Queries =
                          [| { HasFinalSpace = hasFinalSpace
                               LanguageCode = ""
                               Query = text } |] }

            let newModel =
                { model with
                      Search =
                          { model.Search with
                                Params = newSearchParams } }

            newModel, Cmd.none

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
                                  Query = query.Query |> replace "\"__QUOTE__\"" "'\"'" })

                let searchParams =
                    { model.Search.Params with
                          Queries = queries }

                let newModel =
                    { model with
                          Substate = ShowingResults(ShowingResultsModel.Init(searchParams, numSteps)) }

                newModel,
                Cmd.ofMsg (
                    ShowingResultsMsg(ShowingResults.ConcordanceMsg(ShowingResults.Concordance.PerformSearchStep))
                )
            else
                printfn "Empty query!"
                model, Cmd.none




////////////////////////////////
// Update
////////////////////////////////
type Msg =
    | LoadingCorpusMsg of LoadingCorpus.Msg
    | LoadedCorpusMsg of LoadedCorpus.Msg

let init () : Model * Cmd<Msg> =
    let model = LoadingCorpus

    let cmd =
        Cmd.OfAsync.perform serverApi.getCorpusConfig "bokmal" LoadingCorpus.FetchedCorpusConfig

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
