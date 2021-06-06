module LoadedCorpus.Update

open Elmish
open Shared
open Model

type Msg =
    | MetadataMsg of Metadata.Update.Msg
    | SetSearchInterface of SearchInterface
    | Search
    | SearchResultsReceived of SearchResults

let update (msg: Msg) (model: LoadedCorpusModel) : LoadedCorpusModel * Cmd<Msg> =
    match msg with
    | MetadataMsg msg' ->
        let model', cmd = Metadata.Update.update msg' model
        model', Cmd.map MetadataMsg cmd
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
              Queries = [| { Language = "no"; Query = "\"jeg\"" } |]
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
        { model with
              Substate =
                  ShowingResults
                      { ShowingResultsModel.Default with
                            SearchResults = Some results } },
        Cmd.none
