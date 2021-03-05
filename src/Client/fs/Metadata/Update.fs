module Metadata.Update

open Elmish
open Shared
open Model

type Msg = ToggleMetadataMenuOpen of category: Metadata.Category

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | ToggleMetadataMenuOpen category -> model, Cmd.none
