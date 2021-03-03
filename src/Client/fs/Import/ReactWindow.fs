module Import.ReactWindow

open Fable.Core
open Fable.React
open Fable.Core.JsInterop
open Feliz

type Props =
    | Height of int
    | ItemCount of int
    | ItemSize of int
    | Width of int

let FixedSizeList: obj = importMember "react-window"

let ListItem (props: {| index: int; style: obj |}) =
    Html.div [ prop.style [ style.height (int props.style?height)
                            style.left (int props.style?left)
                            style.position.absolute
                            style.top (int props.style?top)
                            style.width (length.percent 100) ]
               prop.text props.index ]

// The components in react-window expect a `children` prop, which should be a
// component function or class. However, all React functions in Fable.React and
// Feliz (such as `ofImport`, `ofType`, and even `Fable.React.ReactBindings.React.createElement`)
// seem to automatically call createElement on (i.e. instantiate) whatever components we pass
// as their `children` arguments.
// Hence, the only solution I have found is to import React ourselves and and call
// React.createElement directly, since it allows us to provide function components as
// the `children` argument and leaves them uninstantiated.

let React: obj = importAll "react"

let fixedSizeList: ReactElement =
    React?createElement (FixedSizeList,
                         createObj [ "height" ==> 100
                                     "itemCount" ==> 10
                                     "itemSize" ==> 35
                                     "width" ==> 100 ],
                         ListItem)
