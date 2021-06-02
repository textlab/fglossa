module Remoting.Search.Cwb.Spoken

open Serilog
open ServerTypes
open Shared
open Remoting.Search.Cwb.Common

let runQueries (logger: ILogger) (corpus: Corpus) (searchParams: SearchParams) (command: string option) =
    async {
        let namedQuery =
            cwbQueryName corpus searchParams.SearchId

        let startpos = 0

        return
            {| Hits = [||]
               Count = 0
               Counts = [||] |}
    }
