module Update

open Elmish
open Shared
open Model

module LoadingCorpus =

    ////////////////////////////////
    // Update.LoadingCorpus
    ////////////////////////////////
    type Msg =
        | FetchCorpusConfig of string
        | FetchedCorpusConfig of CorpusConfig

    let update (msg: Msg) (_model: Model) =
        match msg with
        | FetchCorpusConfig code ->
            let cmd =
                Cmd.OfAsync.perform serverApi.getCorpusConfig code FetchedCorpusConfig

            LoadingCorpus, cmd

        | FetchedCorpusConfig corpusConfig ->
            let corpus = Corpora.Client.getCorpus corpusConfig

            let m =
                { Corpus = corpus
                  IsNarrowWindow = false
                  IsShowSelectionOpen = false
                  OpenMetadataCategoryCode = None
                  Search = Search.Default
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
                | FetchResultWindow of int
                | SetPaginatorPage of int

            let update (msg: Msg) (model: ConcordanceModel) : ConcordanceModel * Cmd<Msg> =
                match msg with
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
                        let firstResult = (pageNumbers.[0] - 1) * model.PageSize

                        let lastResult =
                            ((pageNumbers |> Array.last) * model.PageSize) - 1

                        { model with
                              // Register the pages as being fetched
                              PagesBeingFetched = Array.append model.PagesBeingFetched pageNumbers },
                        Cmd.none

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
            | SearchResultsReceived of SearchResults

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

            | SearchResultsReceived results ->
                let newModel =
                    { model with
                          IsSearching = false
                          SearchResults = Some results }

                newModel, Cmd.none


    ////////////////////////////////
    // Update.LoadedCorpus
    ////////////////////////////////
    type Msg =
        | MetadataMsg of Update.Metadata.Msg
        | ShowingResultsMsg of ShowingResults.Msg

        | SetSearchInterface of SearchInterface
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

        | Search ->
            let searchParams =
                { ContextSize = 15
                  CorpusCode = "bokmal"
                  End = 99UL
                  LastCount = None
                  Metadata = None
                  NumRandomHits = None
                  PageSize = 50
                  Queries =
                      [| { LanguageCode = "no"
                           Query = "\"jeg\"" } |]
                  RandomHitsSeed = None
                  SearchId = 0
                  SortKey = Position
                  Start = 0UL
                  Step = 1 }

            let newModel =
                { model with
                      Substate = ShowingResults(ShowingResultsModel.Init(searchParams)) }

            let cmd =
                Cmd.OfAsync.perform
                    serverApi.searchCorpus
                    searchParams
                    (ShowingResultsMsg
                     << ShowingResults.SearchResultsReceived)

            newModel, cmd


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
