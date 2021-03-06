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
    | LoadedCorpusMsg msg' ->
        match model with
        | LoadedCorpus loadedCorpusModel ->
            let newModel, cmd =
                LoadedCorpus.Update.update msg' loadedCorpusModel

            LoadedCorpus newModel, Cmd.map LoadedCorpusMsg cmd
        | _ -> failwith "Wrong model for LoadedCorpusMsg"

    | FetchCorpusConfig code ->
        let cmd =
            Cmd.OfAsync.perform serverApi.GetCorpusConfig code FetchedCorpusConfig

        LoadingCorpus, cmd
    | FetchedCorpusConfig corpusConfig ->
        let corpus = Corpora.Client.getCorpus corpusConfig

        let m =
            { Corpus = corpus
              IsNarrowWindow = false
              OpenMetadataMenu = None
              Search = Search.Default
              ShouldShowMetadata = None
              Substate = CorpusStartPage }

        LoadedCorpus m, Cmd.none
