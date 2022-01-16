module View.LoadedCorpus.ResultViews.Cwb.Common

open System.Text.RegularExpressions
open Feliz
open Model
open Update.LoadedCorpus.ShowingResults.Concordance

type ResultLineFields =
    { SId: string
      PreMatch: ReactElement []
      SearchWord: ReactElement []
      PostMatch: ReactElement [] }

type ResultInfo =
    { Word: ResultLineFields
      MaybeOrig: ResultLineFields option }

let idColumn (corpus: Corpus) sId (pageNumber: int) rowIndex (dispatch: Msg -> unit) =

    // If the 'match' property is defined, we know that we have a result from a monolingual
    // search or the first language of a multilingual one. If that is the case, and s-id is
    // defined, we print it in the first column (if we have a non-first language result, we
    // will include it in the next column instead).

    // Remove any "suffixes" from the s-id, since they typically denote individual sentences,
    // which are irrelevant when fetching metadata.
    let textId = Regex.Replace(sId, "\..+", "")

    Html.div [ Html.a [ prop.href ""
                        prop.onClick (fun e ->
                            e.preventDefault ()
                            e.stopPropagation ()
                            dispatch (FetchMetadataForText(corpus, textId)))
                        prop.children [ Html.span sId ] ]
               corpus.ResultLinks(pageNumber, rowIndex) ]


let textColumns (resultLineFields: ResultLineFields) =
    if not (resultLineFields.SearchWord |> Array.isEmpty) then
        // If the 'match' value is defined, we know that we have a result from a monolingual
        // search or the first language of a multilingual one, and then we want pre-match, match
        // and post-match in separate columns.
        [ Html.td [ prop.key 0
                    prop.className "left-context"
                    prop.children resultLineFields.PreMatch ]
          Html.td [ prop.key 1
                    prop.className "match"
                    prop.children resultLineFields.SearchWord ]
          Html.td [ prop.key 2
                    prop.className "right-context"
                    prop.children resultLineFields.PostMatch ] ]
    else
        // Otherwise, we have a result from a non-first language of a multilingual search. In that
        // case, CQP doesn't mark the match, so we leave the first column blank and put all of the
        // text in a single following column.
        [ Html.td [ prop.colSpan 3
                    prop.children resultLineFields.PreMatch ] ]


let separatorRow rowIndex =
    // When we show several rows for each search results, this separator row can be used to
    // more clearly separate individual search results.
    Html.tr [ prop.key $"sep{rowIndex}"
              prop.style [ style.backgroundColor "#f1f1f1" ]
              prop.children [ Html.td [ prop.colSpan 4
                                        prop.style [ style.padding 2 ] ] ] ]
