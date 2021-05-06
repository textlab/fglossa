module Model

open Fable.Remoting.Client
open Shared

let serverApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IServerApi>

type SearchInterface =
    | Simple
    | Extended
    | Cqp

type Search =
    { Interface: SearchInterface
      MetadataSelection: Metadata.Selection
      Query: string }
    static member Default =
        { Interface = Simple
          MetadataSelection = Map.empty
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
      ShouldShowMetadataMenu: bool option
      Substate: LoadedCorpusSubstate }

type Model =
    | LoadingCorpus
    | LoadedCorpus of LoadedCorpusModel
    static member Default = LoadingCorpus
