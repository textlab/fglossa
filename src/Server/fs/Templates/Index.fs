module Index

open Giraffe.GiraffeViewEngine

let index =
    [ div [ _id "elmish-app" ] []
      script [ _src "js/vendors.js" ] []
      script [ _src "js/app.js" ] [] ]

let layout = App.layout index
