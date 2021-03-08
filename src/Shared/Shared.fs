namespace Shared

open System

type SearchEngine =
    | Cwb
    | Fcs

type TextEncoding =
    | UTF8
    | Latin1

module Metadata =
    /// Base class for all metadata categories
    [<AbstractClass>]
    type Category(aName: string) =
        member _.Name = aName
        abstract member Code : string

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

    type Selection = Map<CategoryCode, StringSelectOption []>

    type Value = string * string

type CorpusConfig =
    { Code: string
      Encoding: TextEncoding
      IsParallel: bool
      IsSpoken: bool
      Logo: string option
      Name: string
      SearchEngine: SearchEngine }
    static member Init(code, name, ?encoding, ?isSpoken, ?isParallel, ?logo, ?searchEngine) =
        { Code = code
          Encoding = defaultArg encoding UTF8
          IsParallel = defaultArg isParallel false
          IsSpoken = defaultArg isSpoken false
          Logo = logo
          Name = name
          SearchEngine = defaultArg searchEngine Cwb }

module Route =
    let builder typeName methodName =
        sprintf "/glossa3/api/%s/%s" typeName methodName

type CorpusCode = string
type CorpusName = string

type IServerApi =
    { GetCorpusList: unit -> Async<(CorpusCode * CorpusName) list>
      GetCorpusConfig: string -> Async<CorpusConfig>
      GetMetadataForCategory: string * Metadata.Selection -> Async<string * string []> }
