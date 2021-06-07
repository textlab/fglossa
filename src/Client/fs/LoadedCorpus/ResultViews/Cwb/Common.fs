module LoadedCorpus.ResultViews.Cwb.Common

open System.Text.RegularExpressions
open Feliz

type ResultAttributeInfo =
    { SId: string
      PreMatch: string
      SearchWord: string
      PostMatch: string }

type ResultInfo =
    { Word: ResultAttributeInfo
      MaybeOrig: ResultAttributeInfo option }

let idColumn resultAttributeInfo rowIndex =
    // If the 'match' property is defined, we know that we have a result from a monolingual
    // search or the first language of a multilingual one. If that is the case, and s-id is
    // defined, we print it in the first column (if we have a non-first language result, we
    // will include it in the next column instead).

    // Remove any "suffixes" from the s-id, since they typcially denote individual sentences,
    // which are irrelevant when fetching metadata.
    let textId =
        Regex.Replace(resultAttributeInfo.SId, "\..+", "")

    Html.div [ Html.a [ prop.href ""
                        prop.onClick (fun _ -> ())
                        prop.children [ Html.span resultAttributeInfo.SId ] ] ]
