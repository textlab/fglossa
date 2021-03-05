module Model

open Zanaptak.TypedCssClasses
open Shared

type Icon = CssClasses<"../../node_modules/@fortawesome/fontawesome-free/css/all.min.css", Naming.PascalCase>

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
      MetadataMenu: Metadata.MenuItem list }

type LoadedCorpusSubstate =
    | CorpusStartPage
    | ShowingResults

type LoadedCorpusModel =
    { Corpus: Corpus
      IsNarrowWindow: bool
      Search: Search
      ShouldShowMetadata: bool option
      Substate: LoadedCorpusSubstate }

type Model =
    | LoadingCorpus
    | LoadedCorpus of LoadedCorpusModel
    static member Default = LoadingCorpus
