module Remoting.Search.Core

open Serilog
open Shared
open ServerTypes

let getSearchResults (connStr: string) (logger: ILogger) (searchParams: SearchParams) (pageNumbers: ResultPageNumbers) =
    async {
        let corpus =
            Corpora.Server.getCorpus searchParams.CorpusCode

        let! hits =
            match corpus.Config.SearchEngine with
            | Cwb -> Cwb.Core.getSearchResults logger searchParams corpus
            | Fcs -> failwith "Not implemented"

        let hitPages =
            hits |> Array.chunkBySize searchParams.PageSize

        return
            (hitPages, pageNumbers)
            ||> Array.map2
                    (fun pageHits pageNumber ->
                        { PageNumber = pageNumber
                          Results =
                              [| for hitLines in pageHits ->
                                     { AudioType = None
                                       HasVideo = false
                                       Text = hitLines } |] })
    }


let searchCorpus (connStr: string) (logger: ILogger) (searchParams: SearchParams) =
    let corpus =
        Corpora.Server.getCorpus searchParams.CorpusCode

    match corpus.Config.SearchEngine with
    | Cwb -> Cwb.Core.searchCorpus connStr logger searchParams corpus
    | Fcs -> failwith "Not implemented"
