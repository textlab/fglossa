module View.Index

open Model
open Update

open Feliz
open Feliz.Bulma

let navbar model _dispatch =
    Bulma.navbar [ navbar.isFixedTop
                   navbar.hasShadow
                   prop.children [ Bulma.navbarBrand.div [ Bulma.navbarItem.a [ prop.href
                                                                                    "https://www.hf.uio.no/iln/english/about/organization/text-laboratory/services/glossa/index.html"
                                                                                prop.target "_blank"
                                                                                prop.style [ style.fontSize 20 ]
                                                                                prop.text "Glossa" ]
                                                           match model with
                                                           | LoadingCorpus _ -> Html.none
                                                           | LoadedCorpus _ ->
                                                               Bulma.navbarItem.a [ prop.href "/glossa3"
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

[<ReactComponent>]
let MainView (model: Model) (dispatch: Msg -> unit) =
    match model with
    | LoadingCorpus loadingCorpusModel ->
        match loadingCorpusModel.MaybeCorpusList with
        | Some corpusList ->
            Html.span [ navbar model dispatch
                        Bulma.section [ prop.style [ style.marginLeft 100 ]
                                        prop.children [ Html.h1 [ prop.style [ style.fontSize 36
                                                                               style.fontWeight.bold ]
                                                                  prop.text "Glossa" ]
                                                        Html.h2 [ prop.style [ style.fontSize 22
                                                                               style.marginBottom 10 ]
                                                                  prop.text "Available corpora" ]
                                                        Html.ul [ for corpusCode, corpusName in corpusList ->
                                                                      Html.li [ Html.a [ prop.href
                                                                                             $"/glossa3/{corpusCode}"
                                                                                         prop.text corpusName ] ] ] ] ]
                        Bulma.section [ prop.style [ style.paddingTop 0
                                                     style.marginLeft 100 ]
                                        prop.children [ Html.h1 [ prop.style [ style.fontSize 22 ]
                                                                  prop.text "About Glossa" ]
                                                        Html.ul [ prop.style [ style.listStyleType.circle ]
                                                                  prop.children [ Html.li [ Html.span
                                                                                                "Developed at the "
                                                                                            Html.a [ prop.href
                                                                                                         "http://www.hf.uio.no/iln/english/about/organization/text-laboratory/"
                                                                                                     prop.target.blank
                                                                                                     prop.text
                                                                                                         "Text Laboratory, University of Oslo" ]
                                                                                            Html.span
                                                                                                ", with support from the Norwegian contribution to the "
                                                                                            Html.a [ prop.href
                                                                                                         "http://clarin.eu/"
                                                                                                     prop.target.blank
                                                                                                     prop.text "CLARIN" ]
                                                                                            Html.span
                                                                                                " infrastructure, "
                                                                                            Html.a [ prop.href
                                                                                                         "http://clarin.b.uib.no/"
                                                                                                     prop.target.blank
                                                                                                     prop.text "CLARINO" ]
                                                                                            Html.span "." ]
                                                                                  Html.li [ Html.span
                                                                                                "Freely available for download from "
                                                                                            Html.a [ prop.href
                                                                                                         "https://github.com/textlab/fglossa"
                                                                                                     prop.target.blank
                                                                                                     prop.text "GitHub" ]
                                                                                            Html.span
                                                                                                ". Easy to install on your own server." ]
                                                                                  Html.li [ Html.span
                                                                                                "Search engine agnostic. Comes with support for the "
                                                                                            Html.a [ prop.href
                                                                                                         "http://cwb.sourceforge.net/"
                                                                                                     prop.target.blank
                                                                                                     prop.text
                                                                                                         "IMS Open Corpus Workbench" ]
                                                                                            Html.span " and "
                                                                                            Html.a [ prop.href
                                                                                                         "http://www.clarin.eu/node/3449"
                                                                                                     prop.target.blank
                                                                                                     prop.text
                                                                                                         "CLARIN federated content search " ]
                                                                                            Html.span " out of the box." ]
                                                                                  Html.li [ Html.span "Click "
                                                                                            Html.a [ prop.href
                                                                                                         "http://www.tekstlab.uio.no/clarino/privacy-policy.html"
                                                                                                     prop.target.blank
                                                                                                     prop.text "here" ]
                                                                                            Html.span
                                                                                                " to see our privacy policy." ] ] ] ] ] ]
        | None -> Html.none
    | LoadedCorpus loadedCorpusModel ->
        Html.span [ navbar model dispatch
                    LoadedCorpus.view loadedCorpusModel (LoadedCorpusMsg >> dispatch) ]
