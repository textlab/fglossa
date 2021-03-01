module Update

open Elmish
open Fable.Remoting.Client
open Shared
open Model

let serverApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IServerApi>

type Msg =
    | FetchCorpusConfig of string
    | FetchedCorpusConfig of CorpusConfig

let init (): Model * Cmd<Msg> =
    let model = Model.Default

    let cmd =
        Cmd.OfAsync.perform serverApi.getCorpusConfig "bokmal" FetchedCorpusConfig

    model, cmd

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | FetchCorpusConfig code ->
        let cmd =
            Cmd.OfAsync.perform serverApi.getCorpusConfig code FetchedCorpusConfig

        LoadingCorpus, cmd
    | FetchedCorpusConfig corpusConfig ->
        let corpus = Corpora.Client.getCorpus corpusConfig

        let m =
            { Corpus = corpus
              IsNarrowWindow = false
              ShouldShowMetadata = None
              Substate = StartPage }

        LoadedCorpus m, Cmd.none
