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
    | FetchCorpus of string
    | FetchedCorpus of CorpusConfig

let init (): Model * Cmd<Msg> =
    let model = Model.Default

    let cmd =
        Cmd.OfAsync.perform serverApi.getCorpus "bokmal" FetchedCorpus

    model, cmd

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | FetchCorpus code -> LoadingCorpus, Cmd.OfAsync.perform serverApi.getCorpus code FetchedCorpus
    | FetchedCorpus corpusConfig ->
        let corpus = Corpora.Client.getCorpus corpusConfig

        let m =
            { Corpus = corpus
              IsNarrowWindow = false
              ShouldShowMetadata = None
              Substate = StartPage }

        LoadedCorpus m, Cmd.none
