module LoadedCorpus.Update

open Elmish
open Model

type Msg =
    | MetadataMsg of Metadata.Update.Msg
    | SetSearchInterface of SearchInterface
    | Search of Search

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
    | Search search -> model, Cmd.none
