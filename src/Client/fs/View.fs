module View.Index

open Model
open Update

open Feliz
open Feliz.Bulma
open Feliz.Recoil

let navbar _model _dispatch =
    Bulma.navbar [ navbar.isFixedTop
                   navbar.hasShadow
                   prop.children [ Bulma.navbarBrand.div [ Bulma.navbarItem.a [ prop.href
                                                                                    "https://www.hf.uio.no/iln/english/about/organization/text-laboratory/services/glossa/index.html"
                                                                                prop.target "_blank"
                                                                                prop.style [ style.fontSize 20 ]
                                                                                prop.text "Glossa" ]
                                                           Bulma.navbarItem.a [ prop.href "#"
                                                                                prop.text "Corpus list" ] ]
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

// We need to define the view as a component in order to use Recoil.root
[<ReactComponent>]
let MainView (model: Model) (dispatch: Msg -> unit) =
    Recoil.root [ match model with
                  | LoadingCorpus -> Html.none
                  | LoadedCorpus loadedCorpusModel ->
                      Html.span [ navbar model dispatch
                                  LoadedCorpus.view loadedCorpusModel (LoadedCorpusMsg >> dispatch) ] ]
