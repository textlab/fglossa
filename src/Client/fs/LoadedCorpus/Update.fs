module LoadedCorpus.Update

open Elmish
open Model

type Msg =
    | SetSearchInterface of SearchInterface
    | Search of Search

let update (msg: Msg) (model: LoadedCorpusModel) : LoadedCorpusModel * Cmd<Msg> =
    match msg with
    | SetSearchInterface ``interface`` ->
        { model with
              Search =
                  { model.Search with
                        Interface = ``interface`` } },
        Cmd.none
    | Search search -> model, Cmd.none
