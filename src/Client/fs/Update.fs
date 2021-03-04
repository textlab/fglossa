module Update

open Elmish
open Fable.Remoting.Client
open Shared
open Model
open Metadata.Update

let serverApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IServerApi>

type Msg =
    | MetadataMsg of Metadata.Update.Msg
    | LoadedCorpusMsg of LoadedCorpus.Update.Msg
    | FetchCorpusConfig of string
    | FetchedCorpusConfig of CorpusConfig

let init () : Model * Cmd<Msg> =
    let model = Model.Default

    let cmd =
        Cmd.OfAsync.perform serverApi.GetCorpusConfig "bokmal" FetchedCorpusConfig

    model, cmd

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | MetadataMsg msg' -> model, Cmd.none
    | LoadedCorpusMsg msg' -> model, Cmd.none
    | FetchCorpusConfig code ->
        let cmd =
            Cmd.OfAsync.perform serverApi.GetCorpusConfig code FetchedCorpusConfig

        LoadingCorpus, cmd
    | FetchedCorpusConfig corpusConfig ->
        let corpus = Corpora.Client.getCorpus corpusConfig

        let m =
            { Corpus = corpus
              IsNarrowWindow = false
              ShouldShowMetadata = None
              Substate = CorpusStartPage }

        LoadedCorpus m, Cmd.none
