module Server

open System
// open Microsoft.AspNetCore.Builder
// open Microsoft.AspNetCore.Hosting
// open Microsoft.Extensions.DependencyInjection
// open Microsoft.Extensions.Hosting
open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Giraffe.SerilogExtensions
open Giraffe
open Serilog
open Saturn

open Config
open Shared
open ServerTypes

let createServerApi ctx =
    let cnf = Controller.getConfig ctx
    let logger = ctx.Logger()
    let connStr = cnf.connectionString

    { GetCorpusList = fun () -> Remoting.Corpus.getCorpusList ()
      GetCorpusConfig =
        fun code ->
            Remoting.Corpus.getCorpusConfig logger code
            |> Async.AwaitTask
      GetMetadataForCategory =
        fun (corpusCode, categoryCode, selection) ->
            Remoting.Metadata.getMetadataForCategory logger corpusCode categoryCode selection
            |> Async.AwaitTask
      GetMinAndMaxForCategory =
        fun (corpusCode, categoryCode, selection) ->
            Remoting.Metadata.getMinAndMaxForCategory logger corpusCode categoryCode selection
            |> Async.AwaitTask
      GetMetadataForTexts =
        fun (corpusCode, selection, columns, pageNumber, maybeSortInfo) ->
            Remoting.Metadata.getMetadataForTexts logger corpusCode selection columns pageNumber maybeSortInfo
            |> Async.AwaitTask
      GetMetadataForSingleText =
        fun (corpusCode, categories, textId) ->
            Remoting.Metadata.getMetadataForSingleText logger corpusCode categories textId
            |> Async.AwaitTask
      GetTextAndTokenCount =
        fun (corpusCode, selection) ->
            Remoting.Corpus.getTextAndTokenCount logger corpusCode selection
            |> Async.AwaitTask
      SearchCorpus = fun searchParams -> Remoting.Search.Core.searchCorpus connStr logger searchParams
      GetSearchResults =
        fun (searchParams, pageNumbers) -> Remoting.Search.Core.getSearchResults logger searchParams None pageNumbers
      DownloadSearchResults =
        fun (searchParams, attributes, format, shouldCreateHeader) ->
            Remoting.Search.Core.downloadSearchResults logger searchParams attributes format shouldCreateHeader
      GetMediaObject =
        fun (searchParams, mediaPlayerType, pageNumber, resultIndex, contextSize, contextUnit) ->
            Remoting.Search.Cwb.Spoken.getMediaObject
                logger
                searchParams
                mediaPlayerType
                pageNumber
                resultIndex
                contextSize
                contextUnit
      GetGeoDistribution =
        fun searchParams ->
            Remoting.Search.Cwb.Core.getGeoDistribution logger searchParams
            |> Async.AwaitTask
      GetFrequencyList =
        fun (searchParams, attributes, isCaseSensitive, freqListTokenBoundaries) ->
            Remoting.Search.Cwb.Core.getFrequencyList
                logger
                searchParams
                attributes
                isCaseSensitive
                freqListTokenBoundaries
      DownloadFrequencyList =
        fun (searchParams, attributes, isCaseSensitive, freqListTokenBoundaries, format) ->
            Remoting.Search.Cwb.Core.downloadFrequencyList
                logger
                searchParams
                attributes
                isCaseSensitive
                freqListTokenBoundaries
                format
      GetMetadataDistribution =
        fun (searchParams, attributeCode, categoryCode, categoryType, keepZeroValues, accExcludedAttrValues) ->
            Remoting.Search.Cwb.Core.getMetadataDistribution
                logger
                searchParams
                attributeCode
                categoryCode
                categoryType
                keepZeroValues
                accExcludedAttrValues
            |> Async.AwaitTask
      DownloadMetadataDistribution =
        fun (searchParams, attributeCode, categoryCode, categoryType, keepZeroValues, accExcludedAttrValues, format) ->
            Remoting.Search.Cwb.Core.downloadMetadataDistribution
                logger
                searchParams
                attributeCode
                categoryCode
                categoryType
                keepZeroValues
                accExcludedAttrValues
                format
            |> Async.AwaitTask }

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
    router {
        // Ignore the last part of the path, which is the corpus code, since it
        // it will be handled by the client code instead.b
        getf "/%s" (fun _ _ ctx -> (Controller.file ctx "public/index.html"))
    }

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
// currently work with .NET 5, so use Giraffe's routing functions instead
let webApp =
    choose [ routeStartsWith "/glossa3/api" >=> remotingRouter
             // Ignore the corpus code that is provided as path and just return
             // index.html. The corpus code will be handled by the client code.
             routef "/%s" (fun _ -> browserRouter) ]
// router {
//     // Remoting requests.
//     forward "/api/IServerApi" remotingRouter
//     // Requests for full HTML pages (i.e. non-remoting/non-ajax).
//     forward "" browserRouter
// }

let webAppWithLogging: HttpHandler =
    SerilogAdapter.Enable(webApp)

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

// Since the Saturn application builder does not allow us to set a path base, configure
// Giraffe directly instead.

// let configureApp (app: IApplicationBuilder) =
//     // Add Giraffe to the ASP.NET Core pipeline
//     app.UseGiraffe webAppWithLogging
//     app.UsePathBase("/glossa3") |> ignore

// let configureServices (services: IServiceCollection) =
//     // Add Giraffe dependencies
//     services.AddGiraffe() |> ignore

// [<EntryPoint>]
// let main _ =
//     Host
//         .CreateDefaultBuilder()
//         .ConfigureWebHostDefaults(fun webHostBuilder ->
//             webHostBuilder
//                 .Configure(configureApp)
//                 .ConfigureServices(configureServices)
//                 .UseUrls("http://localhost:8085/")
//             |> ignore)
//         .Build()
//         .Run()

//     0

let app =
    application {
        url "http://0.0.0.0:8085"
        use_router webAppWithLogging
        memory_cache
        use_static "public"
        use_gzip
        use_config (fun _ -> { connectionString = $"DataSource={corpusRoot}/fglossa.sqlite" })
    }

run app
