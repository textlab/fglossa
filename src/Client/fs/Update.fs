module Update

open Elmish
open Fable.Remoting.Client
open Shared
open Model

let serverApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IServerApi>

type MainMsg =
    | FetchCorpusConfig of string
    | FetchedCorpusConfig of CorpusConfig

type Msg =
    | MainMsg of MainMsg
    | LoadedCorpusMsg of LoadedCorpus.Update.Msg

let init () : Model * Cmd<Msg> =
    let model = Model.Default

    let cmd =
        Cmd.OfAsync.perform serverApi.GetCorpusConfig "bokmal" FetchedCorpusConfig

    model, Cmd.map MainMsg cmd

let mainMsgUpdate (msg: MainMsg) (_model: Model) =
    match msg with
    | FetchCorpusConfig code ->
        let cmd =
            Cmd.OfAsync.perform serverApi.GetCorpusConfig code FetchedCorpusConfig

        LoadingCorpus, cmd
    | FetchedCorpusConfig corpusConfig ->
        let corpus = Corpora.Client.getCorpus corpusConfig

        let m =
            { Corpus = corpus
              IsNarrowWindow = false
              IsShowSelectionOpen = false
              OpenMetadataCategoryCode = None
              Search = Search.Default
              ShouldShowMetadataMenu = None
              Substate = CorpusStartPage }

        LoadedCorpus m, Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg, model with
    | MainMsg msg', LoadingCorpus ->
        let newModel, cmd = mainMsgUpdate msg' model
        newModel, Cmd.map MainMsg cmd
    | LoadedCorpusMsg msg', LoadedCorpus model' ->
        let newModel, cmd = LoadedCorpus.Update.update msg' model'
        LoadedCorpus newModel, Cmd.map LoadedCorpusMsg cmd
    | _ -> failwithf $"Incompatible message and model: {msg}; {model}"
