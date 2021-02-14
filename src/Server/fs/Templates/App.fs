module App

open Giraffe.GiraffeViewEngine

let layout (content: XmlNode list) =
    let urlBaseValue =
        System.Environment.GetEnvironmentVariable("URL_BASE")

    let headContent =
        [ if not (isNull urlBaseValue) then
              yield (Giraffe.GiraffeViewEngine.``base`` [ _href urlBaseValue ])
          yield meta [ _charset "utf-8" ]
          yield
              meta [ _name "viewport"
                     _content "width=device-width, initial-scale=1" ]
          yield title [] [ encodedText "Glossa" ]
          yield
              link [ _rel "shortcut icon"
                     _type "image/png"
                     _href "Images/safe_favicon.png" ]
          yield
              link [ _rel "stylesheet"
                     _href "css/vendors.css" ]
          yield
              link [ _rel "stylesheet"
                     _href "css/app.css" ] ]

    html [ _class "has-navbar-fixed-top" ] [
        head [] headContent
        body [] content
    ]
