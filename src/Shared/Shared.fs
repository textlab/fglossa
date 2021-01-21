namespace Shared

open System

type SearchEngine =
    | Cwb
    | Fcs

type TextEncoding =
    | UTF8
    | Latin1

module Metadata =
    type Category =
        | Text of code: string * name: string
        | Inteval of code: string * name: string * from: int * ``to``: int

    type CategoryNode =
        | NonLeafNode of CategoryNode list
        | LeafNode of Category

    type MetadataCategories =
        | CategoryList of Category list
        | CategoryTree of CategoryNode

    type Selection = Category []

type Corpus =
    { Code: string
      Encoding: TextEncoding
      Logo: string option
      MetadataCategories: Metadata.MetadataCategories option
      Name: string
      SearchEngine: SearchEngine }

module Route =
    let builder typeName methodName =
        sprintf "/glossa3/api/%s/%s" typeName methodName

type IServerApi =
    { getCorpora: unit -> Async<Corpus []>
      getCorpus: string -> Async<Corpus>
      getMetadataForCategory: string * Metadata.Selection -> Async<string * string []> }
