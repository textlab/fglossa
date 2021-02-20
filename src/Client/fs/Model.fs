module Model

open Shared

type Corpus =
    { Config: CorpusConfig
      MetadataMenu: Metadata.Menu }

type Model =
    { IsNarrowWindow: bool
      IsShowingMetadata: bool
      CorpusConfig: CorpusConfig option }
    static member Default =
        { IsNarrowWindow = false
          IsShowingMetadata = true
          CorpusConfig = None }
