namespace Shared

open System

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
      Name: string
      MetadataCategories: Metadata.MetadataCategories }

module Route =
    let builder typeName methodName =
        sprintf "/glossa3/api/%s/%s" typeName methodName

type IServerApi =
    { getCorpora: unit -> Async<Corpus list>
      getCorpus: string -> Async<Corpus>
      getMetadataForCategory: string * Metadata.Selection -> Async<string * string []> }
