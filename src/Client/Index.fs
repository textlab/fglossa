module Index

open Elmish
open Fable.Remoting.Client
open Shared

type Model =
    { CorpusList: (CorpusCode * CorpusName) list }

type Msg =
    | FetchCorpora
    | FetchedCorpora of (CorpusCode * CorpusName) list

let serverApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IServerApi>

let init (): Model * Cmd<Msg> =
    let model = { CorpusList = [] }

    let cmd =
        Cmd.OfAsync.perform serverApi.getCorpusList () FetchedCorpora

    model, cmd

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | FetchCorpora -> model, Cmd.OfAsync.perform serverApi.getCorpusList () FetchedCorpora
    | FetchedCorpora corpora -> { model with CorpusList = corpora }, Cmd.none

open Fable.React
open Fable.React.Props
open Feliz
open Feliz.Bulma
open Zanaptak.TypedCssClasses

type Icon = CssClasses<"../../node_modules/@fortawesome/fontawesome-free/css/all.min.css", Naming.PascalCase>

module Navbar =
    let view model dispatch =
        Bulma.navbar [ navbar.isFixedTop
                       navbar.hasShadow
                       prop.style [ style.padding (length.rem 0.4) ]
                       prop.children [ Bulma.navbarBrand.div (
                                           Bulma.navbarItem.div (
                                               Html.span [ prop.style [ style.fontSize 20 ]
                                                           prop.text "Glossa" ]
                                           )
                                       )
                                       Bulma.navbarMenu (
                                           Bulma.navbarEnd.div [ Bulma.navbarItem.div (
                                                                     Html.img [ prop.style [ style.width 80
                                                                                             style.maxHeight 100 ]
                                                                                prop.src "clarino-green-sml.png" ]
                                                                 )
                                                                 Bulma.navbarItem.div (
                                                                     Html.img [ prop.src "logo.png"
                                                                                prop.style [ style.marginBottom 5 ] ]
                                                                 ) ]
                                       ) ] ]

let view (model: Model) (dispatch: Msg -> unit) =
    span [] [
        Navbar.view model dispatch
        Html.i [ prop.className [ Icon.Fas; Icon.FaPen ] ]
    ]
