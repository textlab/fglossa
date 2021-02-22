module Index

open Elmish
open Shared
open Model
open Update

open Fable.React
open Fable.React.Props
open Feliz
open Feliz.Bulma
open Zanaptak.TypedCssClasses

type Icon = CssClasses<"../../node_modules/@fortawesome/fontawesome-free/css/all.min.css", Naming.PascalCase>

let shouldShowMetadata (model: LoadedCorpusModel) =
    // Don't show metadata if the corpus doesn't have any (duh!)
    if model.Corpus.MetadataMenu.IsEmpty then
        false
    // If ShouldShowMetadata is a Some, the user has explicitly chosen whether to see metadata,
    // so we respect that unconditionally
    else
        match model.ShouldShowMetadata with
        | Some shouldShow -> shouldShow
        // Now we know that we have metadata, and that the user has not explicitly chosen
        // whether to see them. If we are showing search results, we hide the metadata if the
        // window is narrow; if instead we are showing the start page, we show the metadata
        // regardless of window size.
        | None ->
            match model.Substate with
            | StartPage -> true
            | ShowingResults -> not model.IsNarrowWindow

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

let view (model: Model) (dispatch: Msg -> unit) =
    match model with
    | LoadingCorpus -> Html.none
    | LoadedCorpus loadedCorpusModel ->
        let metadataSidebarWidth =
            if shouldShowMetadata loadedCorpusModel then
                170
            else
                0

        Html.span [ navbar model dispatch
                    Html.div [ prop.style [ style.display.table
                                            style.marginTop 65 ]
                               prop.children [ tableRow [ tableCellWithWidth
                                                              metadataSidebarWidth
                                                              [ Metadata.menu model dispatch ]
                                                          tableCell [ Html.span "DU" ] ] ] ] ]
