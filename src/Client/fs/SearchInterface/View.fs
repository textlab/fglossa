module View.SearchInterface

open Feliz
open Feliz.Bulma
open Model
open Update.LoadedCorpus

let view (search: Search) (dispatch: Msg -> unit) =
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

    let inputValue =
        match search.Interface with
        | Simple ->
            search.Params.Queries
            |> Array.tryHead
            |> Option.map (fun query -> query.Query)
            |> Option.defaultValue ""
        | Extended -> ""
        | Cqp -> ""

    let convertQuery value =
        match search.Interface with
        | Simple -> $"[word=\"{value}\" %%c]"
        | Extended -> $"[word=\"{value}\" %%c]"
        | Cqp -> value

    Html.div [ prop.style [ style.width 500 ]
               prop.children [ Bulma.level [ prop.style [ style.paddingTop 20 ]
                                             prop.children [ Bulma.levelLeft [ Bulma.levelItem links ]
                                                             Bulma.levelRight [ Bulma.button.button [ color.isSuccess
                                                                                                      prop.text "Search"
                                                                                                      prop.onClick
                                                                                                          (fun _ ->
                                                                                                              dispatch
                                                                                                                  Search) ] ] ] ]
                               Bulma.field.div [ Bulma.control.div [ Bulma.input.search [ prop.value inputValue
                                                                                          prop.onChange
                                                                                              (fun (v: string) ->
                                                                                                  dispatch (
                                                                                                      SetQueryText(
                                                                                                          convertQuery v
                                                                                                      )
                                                                                                  )) ] ] ]
                               Bulma.field.div [ Bulma.control.div [ Bulma.button.button "Or..." ] ] ] ]
