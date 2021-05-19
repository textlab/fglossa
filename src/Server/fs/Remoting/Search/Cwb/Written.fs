module Remoting.Search.Cwb.Written

open ServerTypes
open Shared
open Remoting.Search.Cwb.Common

let runQueries (corpus: Corpus) (searchId: int) (searchParams: SearchParams) =
    let namedQuery = cwbQueryName corpus searchId
    let startpos = 0
    ()
