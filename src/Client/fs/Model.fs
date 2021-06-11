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
      Params: SearchParams option }
    static member Default =
        { Interface = Simple
          MetadataSelection = Map.empty
          Params = None }

type Corpus =
    { Config: CorpusConfig
      MetadataMenu: Metadata.MenuItem list
      MetadataTable: Metadata.Category list }

type ResultTab =
    | Concordance
    | Statistics

type ShowingResultsModel =
    { ActiveTab: ResultTab
      IsSearching: bool
      SearchParams: SearchParams
      SearchResults: SearchResults option }
    static member Init(searchParams) =
        { ActiveTab = Concordance
          IsSearching = true
          SearchParams = searchParams
          SearchResults = None }

type LoadedCorpusSubstate =
    | CorpusStartPage
    | ShowingResults of ShowingResultsModel

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
