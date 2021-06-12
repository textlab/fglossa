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

type ConcordanceModel =
    { PaginatorPageNo: int
      ResultPages: Map<int, SearchResult []> }
    static member Default =
        { PaginatorPageNo = 1
          ResultPages = Map.empty }

type ResultTab =
    | Concordance of ConcordanceModel
    | Statistics

type ShowingResultsModel =
    { ActiveTab: ResultTab
      IsSearching: bool
      FetchingPages: int [] option
      SearchParams: SearchParams
      SearchResults: SearchResults option }
    static member Init(searchParams) =
        { ActiveTab = Concordance ConcordanceModel.Default
          FetchingPages = None
          IsSearching = true
          SearchParams = searchParams
          SearchResults = None }

type LoadedCorpusSubstate =
    | CorpusStart
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
