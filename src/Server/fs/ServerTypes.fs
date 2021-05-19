module ServerTypes

open Shared

type Corpus(config: CorpusConfig) =
    member _.Config = config

    member this.GetExtraInfo() =
        match this.Config.SearchEngine with
        | Cwb ->
            // let cwb-corpora =
            //     if corpus.Config.IsParallel then
            let output =
                Process.runCmdWithOutput "docker" "exec -i cwb cwb-describe-corpus"

            Some "hei"
        | Fcs -> None
