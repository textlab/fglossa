namespace Shared

open System
open System.Text.RegularExpressions

module StringUtils =
    let replace (pattern: string) (replacement: string) input =
        Regex.Replace(input, pattern, replacement)

module Metadata =
    /// Base class for all metadata categories
    [<AbstractClass>]
    type Category(aName: string) =
        /// Human-readable name
        member _.Name = aName
        /// Short name, which is also the name of the database column for this category
        abstract member Code : string
        /// The name of the database table where the values for this category are stored.
        /// This is relevant for many-to-many relations such as author name for texts in
        /// written corpora or birth place of informants in multi-informant recordings in
        /// spoken corpora. For instance, for Author, there will be a table Author and a
        /// join table AuthorText in addition to the standard Text table, and in that case
        /// TableName should be 'Author'. A Value of None means that the category is stored
        /// in a column in the Text table.
        abstract member TableName : string option
        default _.TableName = None

    /// Metadata category that can be presented as a metadata value list
    [<AbstractClass>]
    type StringCategory(aName) =
        inherit Category(aName)

    /// Metadata category that can be presented as a metadata value list (like StringCategory),
    /// but that will be sorted numerically.
    /// Can also be used with an interval selector.
    [<AbstractClass>]
    type NumberCategory(aName) =
        inherit Category(aName)

    /// Metadata category that can be used with a free text search input.
    [<AbstractClass>]
    type LongTextCategory(aName) =
        inherit Category(aName)

    type MenuState =
        | Open
        | Closed

    type MenuItem =
        | StringSelect of StringCategory
        | NumberSelect of NumberCategory
        | Interval of NumberCategory
        | FreeTextSearch of LongTextCategory
        // Since a section may have no title, the proper F# way would be to define it
        // as a "string option", but using a normal string (and simply omitting the title
        // bar if the title is empty) makes it easier for non-programmers to define menus.
        | Section of MenuState * title: string * items: MenuItem list

    type CategoryCode = string
    type StringSelectOption = { Name: string; Value: string }

    type CategorySelection =
        { Choices: StringSelectOption []
          ShouldExclude: bool }
        static member Default =
            { Choices = [||]
              ShouldExclude = false }

    type Selection = Map<CategoryCode, CategorySelection>

    type Value = string * string

type SearchEngine =
    | Cwb
    | Fcs

type CorpusModality =
    | Spoken
    | Written

type AttributeCode = string
type AttributeName = string

type TokenAttribute = AttributeCode * AttributeName

type Language =
    { Code: string
      Name: string
      TokenAttributes: TokenAttribute [] }

type LanguageConfig =
    | Monolingual of TokenAttribute []
    | Multilingual of Language []

type CorpusConfig =
    { Code: string
      FontFamily: string option
      LanguageConfig: LanguageConfig
      Modality: CorpusModality
      Logo: string option
      MultiCpuBounds: uint64 [] [] option
      Name: string
      SearchEngine: SearchEngine
      Sizes: Map<string, uint64> }
    static member Init(code, name, ?encoding, ?modality, ?languageConfig, ?logo, ?multiCpuBounds, ?searchEngine) =
        { Code = code
          FontFamily = None
          LanguageConfig = defaultArg languageConfig (Monolingual [||])
          Modality = defaultArg modality Written
          Logo = logo
          MultiCpuBounds = defaultArg multiCpuBounds None
          Name = name
          SearchEngine = defaultArg searchEngine Cwb
          Sizes = Map.empty }

type CorpusCode = string
type CorpusName = string

type Query =
    { HasFinalSpace: bool
      LanguageCode: string
      Query: string }
    member this.IsEmpty =
        let query = this.Query.Trim()

        String.IsNullOrWhiteSpace(query)
        || query = "\"\""
        // Check for one or more empty terms possibly separated by intervals
        || Regex.IsMatch(query, "\[\](\s*\[\](\{\d*,\d*\})?)*")

type SortKey =
    | Position
    | Match
    | Left
    | Right

type SearchParams =
    { ContextSize: int
      CorpusCode: string
      End: uint64
      LastCount: uint64 option
      Metadata: string option
      NumRandomHits: uint64 option
      PageSize: int
      Queries: Query []
      RandomHitsSeed: int option
      SearchId: int
      SortKey: SortKey
      Start: uint64
      Step: int }
    static member Init(corpusCode) =
        { ContextSize = 15
          CorpusCode = corpusCode
          End = 99UL
          LastCount = None
          Metadata = None
          NumRandomHits = None
          PageSize = 50
          Queries = [||]
          RandomHitsSeed = None
          SearchId = 0
          SortKey = Position
          Start = 0UL
          Step = 1 }

type SearchResult =
    { HasAudio: bool
      HasVideo: bool
      Text: string list }

type SearchResultPage =
    { PageNumber: int
      Results: SearchResult [] }

type SearchResultInfo =
    { Count: uint64
      CpuCounts: uint64 []
      SearchId: int
      SearchStep: int
      ResultPages: SearchResultPage [] }

module Route =
    let builder typeName methodName =
        sprintf "/glossa3/api/%s/%s" typeName methodName

type IServerApi =
    { getCorpusConfig: string -> Async<CorpusConfig>
      getCorpusList: unit -> Async<(CorpusCode * CorpusName) list>
      getMetadataForCategory: string * Metadata.Selection -> Async<string * string []>
      getSearchResults: SearchParams -> Async<SearchResultPage []>
      searchCorpus: SearchParams -> Async<SearchResultInfo> }
