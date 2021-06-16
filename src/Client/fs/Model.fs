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
      Params: SearchParams }
    static member Init(corpusCode) =
        { Interface = Simple
          MetadataSelection = Map.empty
          Params = SearchParams.Init(corpusCode) }

type Corpus =
    { Config: CorpusConfig
      MetadataMenu: Metadata.MenuItem list
      MetadataTable: Metadata.Category list }

type ConcordanceModel =
    { // The page numbers of the result pages currently being fetched from the server
      PagesBeingFetched: int []
      // The page number shown in the paginator, which may differ from the actual
      // result page being shown until a the page has been fetched from the server
      PaginatorPageNo: int
      // The text in the paginator input box, which may differ from the paginator page number
      // while the user is editing the text
      PaginatorTextValue: string
      // The nmber of the result page actually being shown at the moment
      ResultPageNo: int
      ResultPages: Map<int, SearchResult []>
      SearchParams: SearchParams }
    static member Init(searchParams) =
        { PagesBeingFetched = [||]
          PaginatorPageNo = 1
          PaginatorTextValue = ""
          ResultPageNo = 1
          ResultPages = Map.empty
          SearchParams = searchParams }

type ResultTab =
    | Concordance of ConcordanceModel
    | Statistics

type ShowingResultsModel =
    { ActiveTab: ResultTab
      IsSearching: bool
      SearchParams: SearchParams
      SearchResults: SearchResultInfo option }
    static member Init((searchParams: SearchParams)) =
        { ActiveTab = Concordance(ConcordanceModel.Init(searchParams))
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
