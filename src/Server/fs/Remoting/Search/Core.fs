module Remoting.Search.Core

open Serilog
open Shared
open ServerTypes

let searchCorpus (connStr: string) (logger: ILogger) (searchParams: SearchParams) =
    let corpus = Corpora.Server.getCorpus searchParams.CorpusCode

    match corpus.Config.SearchEngine with
    | Cwb -> Cwb.Core.searchCorpus connStr logger searchParams corpus
    | Fcs -> failwith "Not implemented"


let getSearchResults (logger: ILogger) (searchParams: SearchParams) maybeAttributes (pageNumbers: ResultPageNumbers) =
    async {
        let corpus = Corpora.Server.getCorpus searchParams.CorpusCode

        return!
            match corpus.Config.SearchEngine with
            | Cwb -> Cwb.Core.getSearchResults logger searchParams corpus maybeAttributes pageNumbers
            | Fcs -> failwith "Not implemented"
    }


let downloadSearchResults (logger: ILogger) (searchParams: SearchParams) (format: DownloadFormat) =
    async {
        let corpus = Corpora.Server.getCorpus searchParams.CorpusCode

        let! results =
            let pageNumbers = Seq.initInfinite (fun index -> index)

            match corpus.Config.SearchEngine with
            | Cwb -> Cwb.Core.getSearchResults logger searchParams corpus None pageNumbers
            | Fcs -> failwith "Not implemented"

        return "hei"
    }
