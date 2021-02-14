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

// let navBrand =
//     Navbar.Brand.div [] [
//         Navbar.Item.a [ Navbar.Item.Props [ Href "https://safe-stack.github.io/" ]
//                         Navbar.Item.IsActive true ] [
//             img [ Src "/favicon.png"; Alt "Logo" ]
//         ]
//     ]

// let containerBox (model: Model) (dispatch: Msg -> unit) =
//     Box.box' [] [
//         Content.content [] [
//             Content.Ol.ol [] [
//                 for corpus in model.CorpusList do
//                     li [] [ str (fst corpus) ]
//             ]
//         ]
//     ]

let view (model: Model) (dispatch: Msg -> unit) =
    span [] [
        Html.i [ prop.className [ Icon.Fas; Icon.FaPen ] ]
    ]
// Hero.hero [ Hero.Color IsPrimary
//             Hero.IsFullHeight
//             Hero.Props [ Style [ Background
//                                      """linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url("https://unsplash.it/1200/900?random") no-repeat center center fixed"""
//                                  BackgroundSize "cover" ] ] ] [
//     Hero.head [] [
//         Navbar.navbar [] [
//             Container.container [] [ navBrand ]
//         ]
//     ]

//     Hero.body [] [
//         Container.container [] [
//             Column.column [ Column.Width(Screen.All, Column.Is6)
//                             Column.Offset(Screen.All, Column.Is3) ] [
//                 Heading.p [ Heading.Modifiers [ Modifier.TextAlignment(Screen.All, TextAlignment.Centered) ] ] [
//                     str "fglossa"
//                 ]
//                 containerBox model dispatch
//             ]
//         ]
//     ]
// ]
