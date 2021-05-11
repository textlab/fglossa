module Remoting.Search.Core

open Serilog
open Shared
open ServerTypes

let searchCorpus (connStr: string) (logger: ILogger) (searchParams: SearchParams) =
    let corpus =
        Corpora.Server.getCorpus searchParams.CorpusCode

    match corpus.Config.SearchEngine with
    | Cwb -> Remoting.Search.Cwb.Common.searchCorpus connStr logger searchParams corpus
    | Fcs -> failwith "Not implemented"
