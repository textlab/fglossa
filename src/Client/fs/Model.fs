module Model

open Shared

type Corpus =
    { Config: CorpusConfig
      MetadataMenu: Metadata.Menu }

type LoadedCorpusSubstate =
    | StartPage
    | ShowingResults

type LoadedCorpusModel =
    { IsNarrowWindow: bool
      ShouldShowMetadata: bool option
      Corpus: Corpus
      Substate: LoadedCorpusSubstate }

type Model =
    | LoadingCorpus
    | LoadedCorpus of LoadedCorpusModel
    static member Default = LoadingCorpus
