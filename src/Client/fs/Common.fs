module Common

open Feliz
open Feliz.Bulma

let iconButton iconClass isDisabled onClick =
    Bulma.button.button [ button.isSmall
                          prop.disabled isDisabled
                          prop.onClick onClick
                          prop.children [ Bulma.icon [ Html.i [ prop.className [ "fa"; iconClass ] ] ] ] ]


// Container component that covers its child components with a semi-transparent overlay
// that also shows a spinner.

// Supported options are spin, which shows the overlay and spinner when true, and CSS
// styles that will override the default styles for the spinner.
let spinnerOverlay (spin: bool) maybeStyles children =
    // Wrap everything in a relatively positioned div (whose width and height will be
    // determined by the child components of c) and insert an absolutely positioned div
    // with a high z-index that will fill the same space, thus overlaying the child
    // components.
    let spinnerChildren =
        if spin then
            let spinner =
                Html.div [ prop.style [ style.position.absolute
                                        style.top 0
                                        style.right 0
                                        style.bottom 0
                                        style.left 0
                                        style.backgroundColor "white"
                                        style.opacity 0.7
                                        style.zIndex 1000 ]
                           prop.children [ Html.div [ prop.className "spinner"
                                                      match maybeStyles with
                                                      | Some styles -> prop.style styles
                                                      | None -> ignore None
                                                      prop.text "Loading..." ] ] ]

            spinner :: children
        else
            children

    Html.div [ prop.style [ style.position.relative ]
               prop.children spinnerChildren ]
