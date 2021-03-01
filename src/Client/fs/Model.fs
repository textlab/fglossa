module Model

open Zanaptak.TypedCssClasses
open Shared

type Icon = CssClasses<"../../node_modules/@fortawesome/fontawesome-free/css/all.min.css", Naming.PascalCase>

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
