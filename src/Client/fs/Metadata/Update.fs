module Metadata.Update

open Elmish
open Shared
open Model

type Msg = ToggleMetadataMenuOpen of category: Metadata.Category

let update (msg: Msg) (model: LoadedCorpusModel) : LoadedCorpusModel * Cmd<Msg> =
    match msg with
    | ToggleMetadataMenuOpen category ->
        let newCode =
            if model.OpenMetadataCategoryCode = Some category.Code then
                None
            else
                Some category.Code

        { model with
              OpenMetadataCategoryCode = newCode },
        Cmd.none
