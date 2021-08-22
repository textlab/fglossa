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
        Html.a [ prop.href "#"
                 prop.title title
                 prop.onClick (fun _ -> dispatch (SetSearchInterface ``interface``))
                 prop.text heading ]

    let simpleLink = link "Simple search box" "Simple" Simple

    let extendedLink =
        link "Search for grammatical categories etc." "Extended" Extended

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
        | Extended ->
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

    let queryText =
        match search.Interface with
        | Simple ->
            search.Params.Queries
            |> Array.tryHead
            |> Option.map
                (fun query ->
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
                        else text)
            |> Option.defaultValue ""
        | Extended -> ""
        | Cqp ->
            search.Params.Queries
            |> Array.tryHead
            |> Option.map
                (fun query ->
                    query.QueryString
                    |> replace "__QUOTE__" "\""
                    |> fun text ->
                        if query.HasFinalSpace then
                            text + " "
                        else
                            text)
            |> Option.defaultValue ""

    let textInputToQuery (inputValue: string) : (string * bool) =
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
        | Extended ->
            let isPhonetic = inputValue.Contains("phon=")
            let isOriginal = inputValue.Contains("orig=")

            let mainAttr =
                if isPhonetic then "phon"
                elif isOriginal then "orig"
                else "word"

            (sprintf "[word=\"%s\" %%c]" inputValue, false)
        | Cqp ->
            let query =
                // Replace literal quotes with __QUOTE__ to prevent
                // them from confusing our regexes later on
                inputValue.Trim()
                |> (replace "word=\"\\\"\"" "word=\"__QUOTE__\""
                    >> replace "^\\\"$" "[word=\"__QUOTE__\"]")

            let hasFinalSpace = Regex.IsMatch(inputValue, "\s+$")
            (query, hasFinalSpace)

    let simpleView =
        Bulma.input.search [ prop.value queryText
                             prop.onChange (textInputToQuery >> SetQueryText >> dispatch) ]

    let cqpView =
        Bulma.input.search [ prop.value queryText
                             prop.onChange (textInputToQuery >> SetQueryText >> dispatch) ]

    let searchInterface =
        match search.Interface with
        | Simple -> simpleView
        | Extended -> Cwb.Extended.view corpus search dispatch
        | Cqp -> cqpView

    Html.div [ prop.style [ style.width 500 ]
               prop.children [ Bulma.level [ prop.style [ style.paddingTop 20 ]
                                             prop.children [ Bulma.levelLeft [ Bulma.levelItem links ]
                                                             Bulma.levelRight [ Bulma.button.button [ color.isSuccess
                                                                                                      prop.text "Search"
                                                                                                      prop.onClick
                                                                                                          (fun _ ->
                                                                                                              dispatch
                                                                                                                  Search) ] ] ] ]
                               Bulma.field.div [ Bulma.control.div [ searchInterface ] ]
                               Bulma.field.div [ Bulma.control.div [ Bulma.button.button "Or..." ] ] ] ]
