module Index

open Elmish
open Fable.Remoting.Client
open Shared
open Model

type Msg =
    | FetchCorpus of string
    | FetchedCorpus of CorpusConfig

let serverApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IServerApi>

let init (): Model * Cmd<Msg> =
    let model = Model.Default

    let cmd =
        Cmd.OfAsync.perform serverApi.getCorpus "bokmal" FetchedCorpus

    model, cmd

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | FetchCorpus code -> model, Cmd.OfAsync.perform serverApi.getCorpus code FetchedCorpus
    | FetchedCorpus corpusConfig ->
        { model with
              CorpusConfig = Some corpusConfig },
        Cmd.none

open Fable.React
open Fable.React.Props
open Feliz
open Feliz.Bulma
open Zanaptak.TypedCssClasses

type Icon = CssClasses<"../../node_modules/@fortawesome/fontawesome-free/css/all.min.css", Naming.PascalCase>

let tableRow (children: ReactElement list) =
    Html.div [ prop.style [ style.display.tableRow ]
               prop.children children ]

let tableCell (children: ReactElement list) =
    Html.div [ prop.style [ style.display.tableCell ]
               prop.children children ]

let tableCellWithWidth (width: int) (children: ReactElement list) =
    Html.div [ prop.style [ style.display.tableCell
                            style.maxWidth width
                            style.width width ]
               prop.children children ]

let navbar model dispatch =
    Bulma.navbar [ navbar.isFixedTop
                   navbar.hasShadow
                   prop.children [ Bulma.navbarBrand.div [ Bulma.navbarItem.a [ prop.src
                                                                                    "https://www.hf.uio.no/iln/english/about/organization/text-laboratory/services/glossa/index.html"
                                                                                prop.target "_blank"
                                                                                prop.style [ style.fontSize 20 ]
                                                                                prop.text "Glossa" ] ]
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

let metadataSidebar model dispatch = Html.div "HEI"

let view (model: Model) (dispatch: Msg -> unit) =
    let metadataSidebarWidth =
        if model.IsShowingMetadata then
            170
        else
            0

    Html.span [ navbar model dispatch
                Html.div [ prop.style [ style.display.table
                                        style.marginTop 53 ]
                           prop.children [ tableRow [ tableCellWithWidth
                                                          metadataSidebarWidth
                                                          [ metadataSidebar model dispatch ]
                                                      tableCell [ Html.span "DU" ] ] ] ] ]
