module ServerTypes

open Shared

type Corpus(config: CorpusConfig) =
    member _.Config = config
