namespace Shared

open System

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

type TextEncoding =
    | UTF8
    | Latin1

type CorpusModality =
    | Spoken
    | Written

type TokenAttributes = string []

type Language =
    { Code: string
      Name: string
      TokenAttributes: TokenAttributes }

type LanguageConfig =
    | Monolingual of TokenAttributes
    | Multilingual of Language []

type CorpusConfig =
    { Code: string
      Encoding: TextEncoding
      LanguageConfig: LanguageConfig
      Modality: CorpusModality
      Logo: string option
      MultiCpuBounds: uint32 array option
      Name: string
      SearchEngine: SearchEngine }
    static member Init(code, name, ?encoding, ?modality, ?languageConfig, ?logo, ?searchEngine) =
        { Code = code
          Encoding = defaultArg encoding UTF8
          LanguageConfig = defaultArg languageConfig (Monolingual [||])
          Modality = defaultArg modality Written
          Logo = logo
          MultiCpuBounds = None
          Name = name
          SearchEngine = defaultArg searchEngine Cwb }

type CorpusCode = string
type CorpusName = string

type Query = { Language: string; Query: string }

type SearchParams =
    { ContextSize: int
      CorpusCode: string
      LastCount: int
      Metadata: string option
      NumRandomHits: int
      PageSize: int
      Queries: Query []
      RandomHitsSeed: int
      SearchId: int option
      SortKey: string
      Step: int }

type SearchResult =
    { HasAudio: bool
      HasVideo: bool
      Text: string }

type SearchResults =
    { Count: uint32
      CpuCounts: uint32 []
      SearchId: int
      Results: SearchResult [] }

module Route =
    let builder typeName methodName =
        sprintf "/glossa3/api/%s/%s" typeName methodName

type IServerApi =
    { getCorpusConfig: string -> Async<CorpusConfig>
      getCorpusList: unit -> Async<(CorpusCode * CorpusName) list>
      getMetadataForCategory: string * Metadata.Selection -> Async<string * string []>
      searchCorpus: SearchParams -> Async<SearchResults> }
