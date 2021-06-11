module LoadedCorpus.Update

open Elmish
open Shared
open Model

type ShowingResultsMsg = SelectResultTab of ResultTab

type Msg =
    | MetadataMsg of Metadata.Update.Msg
    | ShowingResultsMsg of ShowingResultsMsg
    | SetSearchInterface of SearchInterface
    | Search
    | SearchResultsReceived of SearchResults


let showingResultsUpdate (msg: ShowingResultsMsg) (model: ShowingResultsModel) =
    match msg with
    | SelectResultTab tab -> { model with ActiveTab = tab }, Cmd.none


let update (msg: Msg) (model: LoadedCorpusModel) : LoadedCorpusModel * Cmd<Msg> =
    match msg with
    | MetadataMsg msg' ->
        let model', cmd = Metadata.Update.update msg' model
        model', Cmd.map MetadataMsg cmd
    | ShowingResultsMsg msg' ->
        match model.Substate with
        | ShowingResults showingResultsModel ->
            let newShowingResultsModel, cmd =
                showingResultsUpdate msg' showingResultsModel

            { model with
                  Substate = ShowingResults newShowingResultsModel },
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
              PageSize = 25
              Queries =
                  [| { LanguageCode = "no"
                       Query = "\"jeg\"" } |]
              RandomHitsSeed = None
              SearchId = 0
              SortKey = Position
              Step = 1 }

        let newModel =
            { model with
                  Substate = ShowingResults ShowingResultsModel.Default }

        let cmd =
            Cmd.OfAsync.perform serverApi.searchCorpus searchParams SearchResultsReceived

        newModel, cmd
    | SearchResultsReceived results ->
        let newModel =
            { model with
                  Substate =
                      ShowingResults
                          { ShowingResultsModel.Default with
                                IsSearching = false
                                SearchResults = Some results } }

        newModel, Cmd.none
