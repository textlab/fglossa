namespace Shared

open System
open System.Text.RegularExpressions

module StringUtils =
    let replace (pattern: string) (replacement: string) input =
        Regex.Replace(input, pattern, replacement)

    let truncate (maxLength: int) (s: string) =
        if s.Length > maxLength - 3 then
            s.[0 .. maxLength - 3] + "..."
        else
            s

module Cwb =
    /// Positional CWB attribute
    type PositionalAttribute =
        {
          // Short name, which is also the name of the attribute in CWB
          Code: string
          // Human-readable name
          Name: string }

module Metadata =
    type GeoMapControlType =
        | DiscreteControl
        | IntervalControl
        override this.ToString() =
            match this with
            | DiscreteControl -> "discrete"
            | IntervalControl -> "interval"

    type GeoMapCategoryConfig =
        { QualifiedColumnName: string
          Name: string
          ControlType: GeoMapControlType }

    /// Base class for all metadata categories
    [<AbstractClass>]
    type Category(aName: string) =
        /// Human-readable name
        member _.Name = aName
        /// Short name, which is also the name of the database column for this category
        abstract member Code: string

        /// The name of the database table where the values for this category are stored.
        /// This is relevant for many-to-many relations such as author name for texts in
        /// written corpora or birth place of informants in multi-informant recordings in
        /// spoken corpora. For instance, for Author, there will be a table Author and a
        /// join table AuthorText in addition to the standard Text table, and in that case
        /// TableName should be 'Author'. A Value of None means that the category is stored
        /// in a column in the Text table.
        abstract member TableName: string option

        default _.TableName = None

        member this.GetQualifiedColumnName() =
            let tableName =
                this.TableName |> Option.defaultValue "texts"

            $"{tableName}.{this.Code}"

        /// This indicates what kind of selection control should be used for the category
        /// in the geographical map used for selecting informants in a spoken corpus. It is
        /// overridden for each subtype of Category, and can also be overriden by individual
        /// categories if needed.
        abstract member GeoMapControlType: GeoMapControlType

        member this.GetGeoMapConfig() =
            { QualifiedColumnName = this.GetQualifiedColumnName()
              Name = this.Name
              ControlType = this.GeoMapControlType }


    /// Metadata category that can be presented as a metadata value list
    [<AbstractClass>]
    type StringCategory(aName) =
        inherit Category(aName)
        override _.GeoMapControlType = DiscreteControl

    /// Metadata category that can be presented as a metadata value list (like StringCategory),
    /// but that will be sorted numerically.
    /// Can also be used with an interval selector.
    [<AbstractClass>]
    type NumberCategory(aName) =
        inherit Category(aName)
        override _.GeoMapControlType = IntervalControl

    /// Metadata category that can be used with a free text search input.
    [<AbstractClass>]
    type LongTextCategory(aName) =
        inherit Category(aName)
        override _.GeoMapControlType = DiscreteControl

    type MenuState =
        | Open
        | Closed

    type MenuItem =
        | CategoryMenu of Category
        // Since a section may have no title, the proper F# way would be to define it
        // as a "string option", but using a normal string (and simply omitting the title
        // bar if the title is empty) makes it easier for non-programmers to define menus.
        | Section of MenuState * title: string * items: MenuItem list

    type CategoryCode = string
    type CategoryMenuOption = { Name: string; Value: string }

    type CategoryType =
        | StringCategoryType
        | NumberCategoryType

    type CategoryInfo =
        { Name: string
          Code: string
          Type: CategoryType
          Table: string }

    type CategoryNameAndCode = { Name: string; Code: string }

    type CategoryNameAndValue = CategoryMenuOption

    type CategorySelection =
        { Choices: CategoryMenuOption []
          ShouldExclude: bool }
        static member Default =
            { Choices = [||]
              ShouldExclude = false }

    type Selection = Map<CategoryCode, CategorySelection>

    type Value = string * string

    type SortDirection =
        | Asc
        | Desc

    type SortInfo =
        { CategoryCode: CategoryCode
          Direction: SortDirection }

type SearchEngine =
    | Cwb
    | Fcs

type CorpusModality =
    | Spoken
    | Written

type Language =
    { Code: string
      Name: string
      TokenAttributes: Cwb.PositionalAttribute list option }

type LanguageConfig =
    | Monolingual of Cwb.PositionalAttribute list option
    | Multilingual of Language []

type ExternalTool =
    | GoogleTranslate
    | Voyant

type GeoMapConfig =
    { CenterLat: float
      CenterLng: float
      ZoomLevel: float
      MetadataCategories: Metadata.GeoMapCategoryConfig list
      LocationMetadataCategory: Metadata.GeoMapCategoryConfig }

/// Corpus info that is shared between server and client.
type SharedCorpusInfo =
    { Code: string
      ExternalTools: ExternalTool list
      FontFamily: string option
      GeoCoordinates: (string * float * float) [] option
      GeoMapConfig: GeoMapConfig option
      GoogleMapsApiKey: string option
      GoogleTranslateApiKey: string option
      Info: string option
      LanguageConfig: LanguageConfig
      Logo: string option
      Modality: CorpusModality
      MultiCpuBounds: int64 [] [] option
      Name: string
      SearchEngine: SearchEngine
      TextsReferral: string
      TotalTexts: int64
      TotalTokens: int64 }
    static member Init
        (
            code,
            name,
            ?modality,
            ?languageConfig,
            ?logo,
            ?multiCpuBounds,
            ?searchEngine,
            ?externalTools,
            ?geoMapConfig,
            ?textsReferral
        ) =
        let derivedTextsReferral =
            match textsReferral with
            | Some ref -> ref
            | None ->
                match modality with
                | Some modality ->
                    match modality with
                    | Spoken -> "speakers"
                    | Written -> "texts"
                | None -> "texts"

        { Code = code
          ExternalTools = defaultArg externalTools []
          FontFamily = None
          GeoCoordinates = None
          GeoMapConfig = defaultArg geoMapConfig None
          GoogleMapsApiKey = None
          GoogleTranslateApiKey = None
          Info = None
          LanguageConfig = defaultArg languageConfig (Monolingual None)
          Logo = logo
          Modality = defaultArg modality Written
          MultiCpuBounds = defaultArg multiCpuBounds None
          Name = name
          SearchEngine = defaultArg searchEngine Cwb
          TextsReferral = derivedTextsReferral
          TotalTexts = 0L
          TotalTokens = 0L }

    member this.TryGetAttribute(attrCode: string) =
        match this.LanguageConfig with
        | Monolingual (Some attributes) ->
            attributes
            |> List.tryFind (fun a -> a.Code = attrCode)
        | _ -> failwith "NOT IMPLEMENTED!"

    member this.HasAttribute(attrCode: string) =
        match this.LanguageConfig with
        | Monolingual (Some attributes) ->
            attributes
            |> List.exists (fun a -> a.Code = attrCode)
        | Monolingual None -> false
        | _ -> failwith "NOT IMPLEMENTED!"

    member this.GetDefaultAttribute() : Cwb.PositionalAttribute =
        let name =
            if this.HasAttribute("orig") then
                "Corrected form"
            else
                "Word form"

        { Code = "word"; Name = name }

type CorpusCode = string
type CorpusName = string

type Query =
    { HasFinalSpace: bool
      LanguageCode: string option
      QueryString: string }
    static member Init(languageCode: string option) =
        { HasFinalSpace = false
          LanguageCode = languageCode
          QueryString = "[]" }

    member this.IsEmpty =
        let query = this.QueryString.Trim()

        String.IsNullOrWhiteSpace(query)
        || query = "\"\""
        // Check for one or more empty terms possibly separated by intervals
        || Regex.IsMatch(query, "^\[\](\s*\[\](\{\d*,\d*\})?)*$")

type SortKey =
    | Position
    | Match
    | Left
    | Right
    static member OfString(s) =
        match s with
        | "Position" -> Position
        | "Match" -> Match
        | "Left" -> Left
        | "Right" -> Right
        | _ -> failwith $"Unrecognized sort key: {s}"

type SearchParams =
    { ContextSize: int
      CorpusCode: string
      CpuCounts: int64 [] option
      End: int64
      LastCount: int64 option
      MetadataSelection: Metadata.Selection
      NumRandomHits: int64 option
      PageSize: int
      Queries: Query []
      RandomHitsSeed: int option
      SearchId: int
      SortKey: SortKey
      Start: int64
      Step: int }
    static member Init(corpusConfig: SharedCorpusInfo) =
        let languageCode =
            match corpusConfig.LanguageConfig with
            | Monolingual _ -> None
            | Multilingual languages ->
                match languages |> Array.tryHead with
                | Some language -> Some language.Code
                | None -> None

        { ContextSize = 15
          CorpusCode = corpusConfig.Code
          CpuCounts = None
          End = 99L
          LastCount = None
          MetadataSelection = Map.empty
          NumRandomHits = None
          PageSize = 50
          Queries = [| Query.Init(languageCode) |]
          RandomHitsSeed = None
          SearchId = 0
          SortKey = Position
          Start = 0L
          Step = 1 }

type SpeechSegment =
    { Speaker: string
      Line: (int * Map<string, string>) []
      From: string
      To: string
      IsMatch: bool }

type MovieInfo =
    { Supplied: string
      Path: string
      MovieLoc: string
      Start: string
      Stop: string }

type MediaObject =
    { Title: string
      LastLine: int
      DisplayAttribute: string
      CorpusCode: string
      Mov: MovieInfo
      Divs: Map<int, SpeechSegment>
      StartAt: int
      EndAt: int
      MinStart: int
      MaxEnd: int }

type AudioType =
    | Sound
    | Nosound

type MediaPlayerType =
    | AudioPlayer
    | VideoPlayer
    | WaveformPlayer

type SearchResult =
    { AudioType: AudioType option
      HasVideo: bool
      Text: string list }

type SearchResultPage =
    { PageNumber: int
      Results: SearchResult [] }

type SearchResultInfo =
    { Count: int64
      CpuCounts: int64 []
      SearchId: int
      SearchStep: int
      ResultPages: SearchResultPage [] }

type TextAndTokenCounts = { NumTexts: int64; NumTokens: int64 }

type ResultPageNumbers = int seq

type DownloadFormat =
    | Multiline
    | Excel
    | Csv
    | Tsv

type AttributeValueDistribution =
    { AttributeValue: string
      MetadataValueFrequencies: int64 []
      AttributeValueTotal: int64
      // Deviation of proportions (Gries 2008: "Dispersions and adjusted frequencies in corpora")
      Dp: float }

type CategoryValueStat =
    { Value: string
      CategoryValueTotal: int64
      TokenCount: int64 }

type MetadataDistribution =
    { Distribution: AttributeValueDistribution []
      CategoryValueStats: CategoryValueStat list
      TotalTokenCount: int64
      TotalDp: float }

// Used when we want to compute frequencies for only a subsequence of each search result
// (e.g. only the first token in each result)
type FreqListTokenBoundaries = { From: int option; To: int option }

// Map from attribute value to geographical location to frequency
type GeoDistribution = Map<string, Map<string, int64>>

// Define type aliases that help clarify the parameters of the IServerApi functions.
// If we could have used an actual interface instead, we could have used methods
// with named attributes, but unfortunately Fable.Remote requires us to use a record
// with functions.

type DatabaseColumn = string

type PageNumber = int

type TextId = string

type IsCaseSensitive = bool

type ShouldCreateHeader = bool

type AttributeCode = string

type KeepZeroValues = bool

type AccExcludedAttrValues = Set<string>

type TextSelectionInfo = TextSelectionInfo of string

module Route =
    let builder typeName methodName =
        $"/glossa3/api/%s{typeName}/%s{methodName}"

type IServerApi =
    { GetCorpusConfig: CorpusCode -> Async<SharedCorpusInfo>
      GetCorpusList: unit -> Async<(CorpusCode * CorpusName) list>
      GetMetadataForCategory: CorpusCode * Metadata.CategoryCode * Metadata.Selection -> Async<string []>
      GetMinAndMaxForCategory: CorpusCode * Metadata.CategoryCode * Metadata.Selection -> Async<int64 * int64>
      GetMetadataForTexts: CorpusCode * Metadata.Selection * DatabaseColumn list * PageNumber * Metadata.SortInfo option -> Async<string [] []>
      GetMetadataForSingleText: CorpusCode * Metadata.CategoryNameAndCode list * TextId -> Async<Metadata.CategoryNameAndValue list>
      GetTextAndTokenCount: CorpusCode * Metadata.Selection -> Async<TextAndTokenCounts * TextSelectionInfo>
      SearchCorpus: SearchParams -> Async<SearchResultInfo>
      GetSearchResults: SearchParams * ResultPageNumbers -> Async<SearchResultPage []>
      DownloadSearchResults: SearchParams * Cwb.PositionalAttribute list * Metadata.CategoryInfo list * DownloadFormat * ShouldCreateHeader -> Async<byte []>
      GetMediaObject: SearchParams * MediaPlayerType * int * int * int * string -> Async<MediaPlayerType * int * MediaObject>
      GetGeoDistribution: SearchParams -> Async<GeoDistribution>
      GetFrequencyList: SearchParams * Cwb.PositionalAttribute list * IsCaseSensitive * FreqListTokenBoundaries -> Async<string []>
      DownloadFrequencyList: SearchParams * Cwb.PositionalAttribute list * IsCaseSensitive * FreqListTokenBoundaries * DownloadFormat -> Async<byte []>
      GetMetadataDistribution: SearchParams * AttributeCode * Metadata.CategoryCode * Metadata.CategoryType * KeepZeroValues * AccExcludedAttrValues -> Async<MetadataDistribution>
      DownloadMetadataDistribution: SearchParams * AttributeCode * Metadata.CategoryCode * Metadata.CategoryType * KeepZeroValues * AccExcludedAttrValues * DownloadFormat -> Async<byte []> }
