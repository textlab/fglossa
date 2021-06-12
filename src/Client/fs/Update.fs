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

        ///////////////////////////////////////////
        // Update.LoadedCorpus.ShowingResults
        ///////////////////////////////////////////
        type Msg =
            | SelectResultTab of ResultTab
            | SearchResultsReceived of SearchResults

        let update (msg: Msg) (model: ShowingResultsModel) : ShowingResultsModel * Cmd<Msg> =
            match msg with
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
