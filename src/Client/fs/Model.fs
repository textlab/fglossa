module Model

open Shared

type Corpus =
    { Config: CorpusConfig
      MetadataMenu: Metadata.Menu }

type LoadedCorpusModel =
    { IsNarrowWindow: bool
      IsShowingMetadata: bool
      Corpus: Corpus }

type Model =
    | LoadingCorpus
    | LoadedCorpus of LoadedCorpusModel
    static member Default = LoadingCorpus
