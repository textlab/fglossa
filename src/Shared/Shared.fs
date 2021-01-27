namespace Shared

open System

type SearchEngine =
    | Cwb
    | Fcs

type TextEncoding =
    | UTF8
    | Latin1

module Metadata =
    type Value = string * string

    type Category =
        | Strings of code: string * name: string * values: Value []
        | Interval of code: string * name: string * from: int * ``to``: int
        | FreeText of code: string * name: string * value: string
        | Section of title: string option * isOpen: bool * children: Category list

    type Selection = Category []

type Corpus =
    { Code: string
      Encoding: TextEncoding
      Logo: string option
      Name: string
      SearchEngine: SearchEngine }

module Route =
    let builder typeName methodName =
        sprintf "/glossa3/api/%s/%s" typeName methodName

type IServerApi =
    { getCorpora: unit -> Async<Corpus list>
      getCorpus: string -> Async<Corpus>
      getMetadataForCategory: string * Metadata.Selection -> Async<string * string []> }
