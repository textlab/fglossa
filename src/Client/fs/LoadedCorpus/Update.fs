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
              LastCount = 0
              Metadata = None
              NumRandomHits = 0
              PageSize = 25
              Queries = [| "jeg" |]
              RandomHitsSeed = 0
              SearchId = None
              SortKey = "position"
              Step = 1 }

        let cmd =
            Cmd.OfAsync.perform serverApi.searchCorpus searchParams SearchResultsReceived

        model, cmd
    | SearchResultsReceived results -> model, Cmd.none
