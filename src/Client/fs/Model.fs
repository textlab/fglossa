module Model

open Shared

type SearchInterface =
    | Simple
    | Extended
    | Cqp

type MetadataSelection = (Metadata.Category * (string list)) list

type Search =
    { Interface: SearchInterface
      MetadataSelection: MetadataSelection
      Query: string }
    static member Default =
        { Interface = Simple
          MetadataSelection = []
          Query = "" }

type Corpus =
    { Config: CorpusConfig
      MetadataMenu: Metadata.MenuItem list
      MetadataTable: Metadata.Category list }

type LoadedCorpusSubstate =
    | CorpusStartPage
    | ShowingResults

type LoadedCorpusModel =
    { Corpus: Corpus
      IsNarrowWindow: bool
      IsShowSelectionOpen: bool
      OpenMetadataCategoryCode: string option
      Search: Search
      ShouldShowMetadata: bool option
      Substate: LoadedCorpusSubstate }

type Model =
    | LoadingCorpus
    | LoadedCorpus of LoadedCorpusModel
    static member Default = LoadingCorpus
