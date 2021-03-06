module Metadata.Update

open Elmish
open Shared
open Model

type Msg = ToggleMetadataMenuOpen of category: Metadata.Category

let update (msg: Msg) (model: LoadedCorpusModel) : LoadedCorpusModel * Cmd<Msg> =
    match msg with
    | ToggleMetadataMenuOpen category -> model, Cmd.none
