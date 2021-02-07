module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared

let serverApi =
    { getCorpusList = fun () -> Remoting.Corpus.getCorpusList ()
      getCorpus = fun code -> Remoting.Corpus.getCorpus code
      getMetadataForCategory = fun (code, selection) -> async { return "", [||] } }

let webApp =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue serverApi
    |> Remoting.buildHttpHandler

let app =
    application {
        url "http://0.0.0.0:8085"
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
    }

run app
