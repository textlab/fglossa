module Server

open System
open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Giraffe.SerilogExtensions
open Giraffe
open Serilog
open Saturn

open Config
open Shared

let createServerApi ctx =
    let cnf = Controller.getConfig ctx
    let logger = ctx.Logger()
    let connStr = cnf.connectionString

    { GetCorpusConfig = fun code -> Remoting.Corpus.getCorpusConfig code
      GetCorpusList = fun () -> Remoting.Corpus.getCorpusList ()
      GetMetadataForCategory =
          fun (corpusCode, categoryCode, selection) ->
              Remoting.Metadata.getMetadataForCategory logger corpusCode categoryCode selection
              |> Async.AwaitTask
      GetSearchResults =
          fun (searchParams, pageNumbers) ->
              Remoting.Search.Core.getSearchResults connStr logger searchParams pageNumbers
      SearchCorpus = fun searchParams -> Remoting.Search.Core.searchCorpus connStr logger searchParams }

let errorHandler (ex: Exception) (routeInfo: RouteInfo<Microsoft.AspNetCore.Http.HttpContext>) =
    // do some logging
    printfn $"Error at {routeInfo.path} on method {routeInfo.methodName}"
    printfn $"{ex}"
    Ignore
// decide whether or not you want to propagate the error to the client
// match ex with
// | :? System.IO.IOException as x ->
//     let customError =
//         { errorMsg = "Something terrible happened" }

//     Propagate customError
// | :? Exception as x ->
//     // ignore error
//     Ignore

let remotingRouter =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.withErrorHandler errorHandler
    |> Remoting.fromContext createServerApi
    |> Remoting.buildHttpHandler

let browserPipeline =
    pipeline {
        plug acceptHtml
        plug putSecureBrowserHeaders
        plug fetchSession
        set_header "x-pipeline-type" "Browser"
        set_header "Cache-Control" "no-cache, no-store, must-revalidate"
        set_header "Pragma" "no-cache"
        set_header "Expires" "0"
    }

let htmlRouter =
    router { get "/" (htmlView Index.layout) }

let browserRouter =
    router {
        //Use the default 404 webpage
        not_found_handler (htmlView NotFound.layout)
        //Use the default browser pipeline
        pipe_through browserPipeline
        //Use routes for full HTML pages
        forward "" htmlRouter
    }

// The `forward` operation in the `router` computation expression does not
// currently work with .NET 5, so user Giraffe's routing functions instead
let webApp =
    choose [ routeStartsWith "/glossa3/api" >=> remotingRouter
             route "" >=> browserRouter ]
// router {
//     // Remoting requests.
//     forward "/api/IServerApi" remotingRouter
//     // Requests for full HTML pages (i.e. non-remoting/non-ajax).
//     forward "" browserRouter
// }

let webAppWithLogging: HttpHandler = SerilogAdapter.Enable(webApp)

// The default template does not include date, but we want that
let outputTemplate =
    "[{Timestamp:dd/MM/yy HH:mm:ss} {Level:u3}] {Message:lj}{NewLine}{Exception}"

// Giraffe.SerilogExtensions has native destructuring mechanism.
// This helps Serilog deserialize the fsharp types like unions/records
let typeConf =
    LoggerConfiguration().Destructure.FSharpTypes()

Log.Logger <-
    typeConf
        .WriteTo
        .Console(outputTemplate = outputTemplate)
        .CreateLogger()

let app =
    application {
        url "http://0.0.0.0:8085"
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
        use_config (fun _ -> { connectionString = "DataSource=db/fglossa.sqlite" })
    }

run app
