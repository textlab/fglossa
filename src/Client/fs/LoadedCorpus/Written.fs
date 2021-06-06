module LoadedCorpus.Written

open Feliz
open Feliz.Bulma
open Shared

let concordanceTable (searchResults: SearchResults) =
    Bulma.table [ table.isStriped
                  table.isFullWidth
                  prop.children [ Html.tbody [ for result in searchResults.Results -> Html.tr [ Html.td result.Text ] ] ] ]
