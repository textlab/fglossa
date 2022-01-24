module View.SearchViews.Cwb

open System.Text.RegularExpressions
open Feliz
open Feliz.Bulma
open Shared
open Shared.StringUtils
open Model
open Update.LoadedCorpus

let view (corpus: Corpus) (search: Search) (dispatch: Msg -> unit) =
    let simpleHeading = "Simple"
    let extendedHeading = "Extended"
    let cqpHeading = "CQP query"

    let link title (heading: string) ``interface`` =
        Html.a [ prop.title title
                 prop.onClick (fun _ -> dispatch (SetSearchInterface ``interface``))
                 prop.text heading ]

    let simpleLink = link "Simple search box" "Simple" Simple

    let extendedLink =
        link "Search for grammatical categories etc." "Extended" (Extended None)

    let cqpLink = link "CQP expressions" "CQP query" Cqp

    let separator =
        Html.span [ prop.style [ style.whitespace.pre ]
                    prop.text " | " ]

    let links =
        match search.Interface with
        | Simple ->
            [ Html.b simpleHeading
              separator
              extendedLink
              separator
              cqpLink ]
        | Extended _ ->
            [ simpleLink
              separator
              Html.b extendedHeading
              separator
              cqpLink ]
        | Cqp ->
            [ simpleLink
              separator
              extendedLink
              separator
              Html.b cqpHeading ]

    let queryText (query: Query) =
        match search.Interface with
        | Simple ->
            query.QueryString
            |> replace "</?(?:s|who)?>" ""
            // Unescape any escaped chars, since we don't want the backslashes
            // to show in the text input
            |> replace "\\\(.)" "$1"
            |> replace "\[\(?\w+=\"(.*?)\"(?:\s+%c)?\)?\]" "$1"
            |> replace "\"([^\s=]+)\"" "$1"
            |> replace "\s*\[\]\s*" " .* "
            |> replace "^!" ""
            |> replace "__QUOTE__" "\""
            // Replace .* or .+ by a single asterisk, used for truncation in the simple view
            |> replace "\.[\*\+]" "*"
            |> fun text ->
                if text = " * " then ""
                elif query.HasFinalSpace then text + " "
                else text
        | Extended _ -> ""
        | Cqp ->
            query.QueryString
            |> replace "__QUOTE__" "\""
            |> fun text ->
                if query.HasFinalSpace then
                    text + " "
                else
                    text

    let textInputToQuery (inputValue: string) : string * bool =
        match search.Interface with
        | Simple ->
            let query =
                Regex.Split(inputValue.Trim(), "\s+")
                |> Array.map (
                    // Replace literal quotes with __QUOTE__ to prevent them from confusing
                    // our regexes later on
                    replace "\"" "__QUOTE__"
                    // Escape other special characters using a regex from
                    // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions
                    >> replace "[\.\+\?\^\$\{\}\(\)\|\[\]\\\]" "\$&"
                    // Convert truncation to its regex equivalent
                    >> replace "\*" ".*"
                    >> fun token ->
                        if token = "" then
                            "[]"
                        else
                            // We need to use sprintf instead of string interpolation here
                            // because the latter actually outputs the extra percentage sign
                            // that is needed to escape the one in '%c'.
                            sprintf "[word=\"%s\" %%c]" token
                )
                |> String.concat " "

            let hasFinalSpace = Regex.IsMatch(inputValue, "\s+$")
            (query, hasFinalSpace)
        | Extended _ -> failwith "Unused!"
        | Cqp ->
            let query =
                // Replace literal quotes with __QUOTE__ to prevent
                // them from confusing our regexes later on
                inputValue.Trim()
                |> (replace "word=\"\\\"\"" "word=\"__QUOTE__\""
                    >> replace "^\\\"$" "[word=\"__QUOTE__\"]")

            let hasFinalSpace = Regex.IsMatch(inputValue, "\s+$")
            (query, hasFinalSpace)

    let simpleView (query: Query) queryIndex =
        Bulma.input.search [ prop.value (queryText query)
                             prop.onKeyUp (key.enter, (fun _ -> dispatch Search))
                             prop.onChange
                                 (fun (s: string) ->
                                     let query, hasFinalSpace = textInputToQuery s
                                     dispatch (SetQueryText(query, queryIndex, hasFinalSpace))) ]

    let cqpView (query: Query) queryIndex =
        Bulma.input.search [ prop.value (queryText query)
                             prop.onKeyUp (key.enter, (fun _ -> dispatch Search))
                             prop.onChange
                                 (fun (s: string) ->
                                     let query, hasFinalSpace = textInputToQuery s
                                     dispatch (SetQueryText(query, queryIndex, hasFinalSpace))) ]

    let searchInterface (query: Query) queryIndex =
        match search.Interface with
        | Simple -> simpleView query queryIndex
        | Extended maybeAttrModalModel -> Cwb.Extended.view corpus search query queryIndex maybeAttrModalModel dispatch
        | Cqp -> cqpView query queryIndex

    let queryRows =
        let shouldShowDeleteRowButton =
            match search.Interface with
            | Simple
            | Cqp -> search.Params.Queries.Length > 1
            | Extended _ -> false

        search.Params.Queries
        |> Array.mapi
            (fun queryIndex query ->
                Bulma.field.div [ if shouldShowDeleteRowButton then
                                      field.hasAddons
                                      field.hasAddonsRight
                                  prop.children [ Bulma.control.div [ control.isExpanded
                                                                      prop.children [ searchInterface query queryIndex ] ]
                                                  if shouldShowDeleteRowButton then
                                                      Bulma.control.div (
                                                          Bulma.button.button [ color.isDanger
                                                                                prop.title "Delete row"
                                                                                prop.onClick
                                                                                    (fun _ ->
                                                                                        dispatch (
                                                                                            RemoveQueryRow queryIndex
                                                                                        ))
                                                                                prop.children [ Bulma.icon [ Html.i [ prop.className [ "fa fa-times" ] ] ] ] ]
                                                      ) ] ])

    let randomResultsControls =
        [ Bulma.levelItem [ prop.style [ style.marginRight (length.rem 0.5) ]
                            prop.children [ Bulma.field.div (
                                                Bulma.control.div (
                                                    Bulma.input.text [ prop.style [ style.width 55 ]
                                                                       prop.value (
                                                                           search.Params.NumRandomHits
                                                                           |> Option.map string
                                                                           |> Option.defaultValue ""
                                                                       )
                                                                       prop.onChange
                                                                           (fun (s: string) ->
                                                                               let value =
                                                                                   match System.UInt64.TryParse(s) with
                                                                                   | true, v -> Some v
                                                                                   | false, _ -> None

                                                                               dispatch (SetNumRandomHits value))
                                                                       prop.onKeyUp (
                                                                           key.enter,
                                                                           (fun _ -> dispatch Search)
                                                                       ) ]
                                                )
                                            ) ] ]
          Bulma.levelItem [ prop.style [ style.marginRight (length.rem 0.35) ]
                            prop.children [ Bulma.text.span "random results (with seed:" ] ]
          Bulma.levelItem [ prop.style [ style.marginRight (length.rem 0.5) ]
                            prop.children [ Bulma.input.text [ prop.style [ style.width 50 ]
                                                               prop.value (
                                                                   search.Params.RandomHitsSeed
                                                                   |> Option.map string
                                                                   |> Option.defaultValue ""
                                                               )
                                                               prop.onChange
                                                                   (fun (s: string) ->
                                                                       let value =
                                                                           match System.Int32.TryParse(s) with
                                                                           | true, v -> Some v
                                                                           | false, _ -> None

                                                                       dispatch (SetRandomHitsSeed value))
                                                               prop.onKeyUp (key.enter, (fun _ -> dispatch Search)) ]

                                             ] ]
          Bulma.levelItem (Bulma.text.span ")") ]

    let searchButton =
        Bulma.button.button [ color.isSuccess
                              prop.text "Search"
                              prop.onClick (fun _ -> dispatch Search) ]

    let orButton =
        Bulma.button.button [ prop.onClick (fun _ -> dispatch AddQueryRow)
                              prop.text "Or..." ]

    Html.div [ prop.style [ style.width 500 ]
               prop.children [ Bulma.level [ prop.style [ style.paddingTop 20 ]
                                             prop.children [ Bulma.levelLeft [ Bulma.levelItem links ]
                                                             Bulma.levelRight [ searchButton ] ] ]
                               yield! queryRows
                               Bulma.level [ Bulma.levelLeft [ Bulma.levelItem [ prop.style [ style.marginRight (
                                                                                                  length.rem 4.5
                                                                                              ) ]
                                                                                 prop.children [ Bulma.field.div (
                                                                                                     Bulma.control.div
                                                                                                         orButton
                                                                                                 ) ] ]
                                                               if search.Interface <> Simple then
                                                                   yield! randomResultsControls ] ] ] ]
