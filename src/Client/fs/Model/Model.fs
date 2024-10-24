module Model

open Fable.Remoting.Client
open Feliz
open Shared

let serverApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IServerApi>

type LoadingCorpusModel =
    { MaybeCorpusList: (CorpusCode * CorpusName) [] option }

module CwbAttributeMenu =
    open Shared.Cwb

    //////////////////////////////////////////////////////////////////////////////////////////////////
    /// A set of types used to represent the list of tree structures (e.g. grammatical information
    /// and corpus-specific attributes) shown in the attribute popup
    //////////////////////////////////////////////////////////////////////////////////////////////////

    type Heading = string

    type AttributeValue = string
    type AttributeValueHumanReadable = string

    // e.g. (number, "sg", "singular")
    type SubcategoryValue = PositionalAttribute * AttributeValue * AttributeValueHumanReadable

    // e.g. ("Number", [ (number, "sg", "singular"); (number, "pl", "plural")])
    type Subcategory = Heading * SubcategoryValue list

    // e.g. (pos, "adj", "adjective",
    //          [ ("Number", [ (number, "sg", "singular"); (number, "pl", "plural") ])
    //            ("Degree", [ (degr, "pos", "positive"); (degr, "comp", "comparative"); (degr, "sup", "superlative") ])])
    type MainCategoryValue = PositionalAttribute * AttributeValue * AttributeValueHumanReadable * Subcategory list

    type AttributeSection =
        { Heading: string
          SubcategoryHeading: string
          Values: MainCategoryValue list }


type AttributeModalModel =
    { TermIndex: int
      IncludeExcludeInput: string }
    static member Init(termIndex: int) =
        { TermIndex = termIndex
          IncludeExcludeInput = "" }

type ListOrIntervalMode =
    | ListMode
    | IntervalMode

type SearchInterface =
    | Simple
    | Extended of AttributeModalModel option
    | Cqp

type Search =
    { Interface: SearchInterface
      Params: SearchParams }
    static member Init(corpusConfig: SharedCorpusInfo) =
        { Interface = Simple
          Params = SearchParams.Init(corpusConfig) }

type CwbAttributeMenu = CwbAttributeMenu.AttributeSection list

// Implement the Corpus type as a class instead of a record so as to allow individual corpora to
// override methods as they are instantiated using object expressions.
type Corpus
    (
        sharedInfo: SharedCorpusInfo,
        ?cwbAttributeMenu: CwbAttributeMenu,
        ?metadataMenu: Metadata.MenuItem list,
        ?metadataTable: Metadata.Category list,
        ?metadataQuickView: Metadata.Category list,
        ?metadataDistributionCategories: Metadata.Category list
    ) =
    member val SharedInfo = sharedInfo
    member val CwbAttributeMenu = cwbAttributeMenu
    member val MetadataMenu = defaultArg metadataMenu []
    member val MetadataTable = defaultArg metadataTable []
    member val MetadataQuickView = defaultArg metadataQuickView []
    member val MetadataDistributionCategories = defaultArg metadataDistributionCategories []

    abstract member ResultLinks:
        pageNumber: int * rowIndex: int * textId: string * messageId: string * corpusInfo: SharedCorpusInfo -> ReactElement

    default _.ResultLinks(_, _, _, _, _) = Html.none

    // The first column in the selection table (showing metadata for the currently selected texts), which
    // may e.g. show links to PDFs or other info about each text
    abstract member SelectionTableFirstColumn: textId: string * corpusInfo: SharedCorpusInfo -> ReactElement

    default _.SelectionTableFirstColumn(_, _) = Html.none

type MediaPlayerInfo =
    { Type: MediaPlayerType
      RowIndex: int
      MediaObject: MediaObject }

type SyntaxNode =
    { index: int
      ort: string
      pos: string
      dep: int
      ``fun``: string
      ``match``: bool }

type ConcordanceModel =
    { ContextSizeTextValue: string
      IsSearching: bool
      ShouldShowDownloadWindow: bool
      DownloadingFormat: DownloadFormat option
      HeadersInDownload: bool
      DownloadAttributes: Cwb.PositionalAttribute list
      DownLoadCategories: Metadata.Category list
      // The page numbers of the result pages currently being fetched from the server
      NumResults: int64 option
      NumSteps: int
      PagesBeingFetched: int []
      // The page number shown in the paginator, which may differ from the actual
      // result page being shown until a the page has been fetched from the server
      PaginatorPageNo: int
      // The text in the paginator input box, which may differ from the paginator page number
      // while the user is editing the text
      PaginatorTextValue: string
      // The number of the result page actually being shown at the moment
      ResultPageNo: int
      ResultPages: Map<int, SearchResult []>
      MediaPlayer: MediaPlayerInfo option
      VideoContextSize: int
      VideoContextUnit: string
      Translations: Map<string, string>
      VisibleSyntaxTreeKeys: Set<string> }
    static member Init(numSteps, contextSizeTextValue) =
        { ContextSizeTextValue = contextSizeTextValue
          IsSearching = true
          ShouldShowDownloadWindow = false
          DownloadingFormat = None
          HeadersInDownload = true
          DownloadAttributes = []
          DownLoadCategories = []
          NumResults = None
          NumSteps = numSteps
          PagesBeingFetched = [||]
          PaginatorPageNo = 1
          PaginatorTextValue = "1"
          ResultPageNo = 1
          ResultPages = Map.empty
          MediaPlayer = None
          VideoContextSize = 25
          VideoContextUnit = "who_start"
          Translations = Map.empty
          VisibleSyntaxTreeKeys = Set.empty }

    member this.NumResultPages(pageSize) =
        match this.NumResults with
        | Some numResults -> float numResults / float pageSize |> ceil |> int
        | None -> 0

type FrequencyListItem =
    { Frequency: int64
      AttributeValues: string [] }

type FrequencyListsModel =
    { Attributes: Cwb.PositionalAttribute list
      Frequencies: FrequencyListItem [] option
      IsCaseSensitive: bool
      IsFetchingFrequencyList: bool
      TokenBoundaries: FreqListTokenBoundaries
      DownloadingFormat: DownloadFormat option }
    static member Default =
        { Attributes = []
          Frequencies = None
          IsCaseSensitive = false
          IsFetchingFrequencyList = false
          TokenBoundaries = { From = None; To = None }
          DownloadingFormat = None }

/// Attribute values that are excluded from the metadata distribution table.
/// Each time we check a box in front of a row, it is added to the New set,
/// and when we click the 'Remove selected' button, that set is added to the
/// Accumulated set (and the New set is emptied).
type ExcludedAttributeValues =
    { Accumulated: Set<string>
      New: Set<string> }

type MetadataDistributionModel =
    { SelectedAttributeCode: string option
      SelectedCategory: Metadata.Category option
      MetadataDistribution: MetadataDistribution
      KeepZeroValues: bool
      ExcludedAttributeValues: ExcludedAttributeValues
      IsFetching: bool
      DownloadingFormat: DownloadFormat option }
    static member Init() =
        { SelectedAttributeCode = None
          SelectedCategory = None
          MetadataDistribution =
            { Distribution = [||]
              CategoryValueStats = []
              TotalTokenCount = 0
              TotalDp = 0. }
          KeepZeroValues = true
          ExcludedAttributeValues =
            { Accumulated = Set.empty
              New = Set.empty }
          IsFetching = false
          DownloadingFormat = None }

type ResultTab =
    | Concordance
    | GeoDistributionMap of GeoMapConfig * GeoDistribution
    | FrequencyLists of FrequencyListsModel
    | MetadataDistribution of MetadataDistributionModel

type ShowingResultsModel =
    { ActiveTab: ResultTab
      SearchResults: SearchResultInfo option
      NumSteps: int
      ConcordanceModel: ConcordanceModel }
    static member Init
        (
            numSteps: int,
            contextSizeTextValue: string
        ) =
        { ActiveTab = Concordance
          SearchResults = None
          NumSteps = numSteps
          ConcordanceModel = ConcordanceModel.Init(numSteps, contextSizeTextValue) }

type LoadedCorpusSubstate =
    | CorpusStart
    | ShowingResults of ShowingResultsModel

type LoadedCorpusModel =
    { Corpus: Corpus
      FetchedMetadataValues: string []
      FetchedMinAndMax: (int64 * int64) option
      FetchedTextMetadata: string [] []
      GeoDistribution: GeoDistribution
      IsMetadataGeoMapOpen: bool
      IsNarrowWindow: bool
      IsSelectionTableOpen: bool
      // This determines whether a certain category will displayed as a select
      // list or an interval. When a category is not yet registered in the map, it will
      // be shown as a list, but may be individually switched to an interval by the user.
      IntervalCategoryModes: Map<Metadata.CategoryCode, ListOrIntervalMode>
      NumSelectedTexts: int64 option
      NumSelectedTokens: int64 option
      OpenMetadataCategoryCode: string option
      Search: Search
      ShouldShowMetadataMenu: bool option
      ShouldShowQuickView: bool
      SelectionTablePageNumber: int
      SelectionTableSort: Metadata.SortInfo option
      Substate: LoadedCorpusSubstate
      TextSelectionInfo: string
      TextIdInQuickView: string option
      QuickViewMetadata: Metadata.CategoryNameAndValue list }

type Model =
    | LoadingCorpus of LoadingCorpusModel
    | LoadedCorpus of LoadedCorpusModel
