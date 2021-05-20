module Remoting.Search.Cwb.Spoken

open ServerTypes
open Shared
open Remoting.Search.Cwb.Common

let runQueries (corpus: Corpus) (searchParams: SearchParams) =
    let namedQuery =
        cwbQueryName corpus searchParams.SearchId

    let startpos = 0
    ()
