module View.SearchViews.Cwb.Extended

open Feliz
open Feliz.Bulma
open Shared
open Model
open Update.LoadedCorpus

let view (corpus: Corpus) (search: Search) (dispatch: Msg -> unit) =
    let query =
        CwbExtended.Query.OfCqp(search.Params.Queries.[0].Query)

    Html.div "hei"
