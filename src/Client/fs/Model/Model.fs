module Model

open Fable.Remoting.Client
open Shared
open Shared.StringUtils

let serverApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IServerApi>

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

type SearchInterface =
    | Simple
    | Extended of AttributeModalModel option
    | Cqp

type Search =
    { Interface: SearchInterface
      MetadataSelection: Metadata.Selection
      Params: SearchParams }
    static member Init(corpusConfig: CorpusConfig) =
        { Interface = Simple
          MetadataSelection = Map.empty
          Params = SearchParams.Init(corpusConfig) }

type CwbAttributeMenu = CwbAttributeMenu.AttributeSection list

type Corpus =
    { Config: CorpusConfig
      CwbAttributeMenu: CwbAttributeMenu option
      MetadataMenu: Metadata.MenuItem list
      MetadataTable: Metadata.Category list }
    static member Init(config) =
        { Config = config
          CwbAttributeMenu = None
          MetadataMenu = []
          MetadataTable = [] }

type ConcordanceModel =
    { ContextSizeTextValue: string
      IsSearching: bool
      // The page numbers of the result pages currently being fetched from the server
      NumResults: uint64 option
      NumSteps: int
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
    static member Init(searchParams, numSteps, contextSizeTextValue) =
        { ContextSizeTextValue = contextSizeTextValue
          IsSearching = true
          NumResults = None
          NumSteps = numSteps
          PagesBeingFetched = [||]
          PaginatorPageNo = 1
          PaginatorTextValue = "1"
          ResultPageNo = 1
          ResultPages = Map.empty
          SearchParams = searchParams }

    member this.NumResultPages() =
        match this.NumResults with
        | Some numResults ->
            float numResults
            / float this.SearchParams.PageSize
            |> ceil
            |> int
        | None -> 0

type ResultTab =
    | Concordance of ConcordanceModel
    | Statistics

type ShowingResultsModel =
    { ActiveTab: ResultTab
      SearchParams: SearchParams
      SearchResults: SearchResultInfo option
      NumSteps: int }
    static member Init((searchParams: SearchParams, numSteps: int, contextSizeTextValue: string)) =
        { ActiveTab = Concordance(ConcordanceModel.Init(searchParams, numSteps, contextSizeTextValue))
          SearchParams = searchParams
          SearchResults = None
          NumSteps = numSteps }

type LoadedCorpusSubstate =
    | CorpusStart
    | ShowingResults of ShowingResultsModel

type LoadedCorpusModel =
    { Corpus: Corpus
      FetchedMetadataValues: string []
      IsNarrowWindow: bool
      IsShowSelectionOpen: bool
      OpenMetadataCategoryCode: string option
      Search: Search
      ShouldShowMetadataMenu: bool option
      Substate: LoadedCorpusSubstate }

type Model =
    | LoadingCorpus
    | LoadedCorpus of LoadedCorpusModel
