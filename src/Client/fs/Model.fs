module Model

open Shared

type Corpus =
    { Config: CorpusConfig
      MetadataMenu: Metadata.MenuItem list }

type LoadedCorpusSubstate =
    | StartPage
    | ShowingResults

type LoadedCorpusModel =
    { Corpus: Corpus
      IsNarrowWindow: bool
      ShouldShowMetadata: bool option
      Substate: LoadedCorpusSubstate }

type Model =
    | LoadingCorpus
    | LoadedCorpus of LoadedCorpusModel
    static member Default = LoadingCorpus
