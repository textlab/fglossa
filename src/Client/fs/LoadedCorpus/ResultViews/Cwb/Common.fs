module View.LoadedCorpus.ResultViews.Cwb.Common

open System.Text.RegularExpressions
open Fetch
open Feliz
open Feliz.Bulma
open Model
open Shared
open StringUtils
open Update.LoadedCorpus.ShowingResults.Concordance

[<ReactComponent>]
let TranslationButton pageNumber rowIndex fullText (translations: Map<string, string>) googleTransKey dispatch =
    let translationKey =
        $"{pageNumber}_{rowIndex}"

    let onClick _ =
        let target = "en"

        let searchText =
            Fable.Core.JS.encodeURI fullText

        let url =
            $"https://www.googleapis.com/language/translate/v2?q={searchText}&key={googleTransKey}&target={target}"

        promise {
            let! response = fetch url []
            let! text = response.text ()

            // Quick and hackish way to extract the translation from the string representation of the returned JSON...
            let m =
                Regex.Match(text, "translatedText\":\s*\"(.+?)\",")

            if m.Success then
                let translation = m.Groups[1].Value
                dispatch (SetTranslation(translationKey, translation))
        }
        |> Promise.start

    Html.div [ prop.style [ style.display.inlineBlock
                            style.marginLeft 7
                            style.marginRight 1
                            style.marginBottom 2 ]
               prop.children [ Bulma.button.button [ button.isSmall
                                                     prop.style [ style.fontSize 10 ]
                                                     prop.disabled (translations.ContainsKey translationKey)
                                                     prop.onClick onClick
                                                     prop.title "Translate with Google Translate"
                                                     prop.text "Trans" ] ] ]

let TranslationButtonSaami pageNumber rowIndex fullText (translations: Map<string, string>) googleTransKey dispatch =
    let translationKey =
        $"{pageNumber}_{rowIndex}"

    let onClick _ =
        let target = "en"

        let searchText =
            Fable.Core.JS.encodeURI fullText

        let url =
            $"https://gtweb.uit.no/apy/translate?langpair=sme|nob&q={searchText}"

        promise {
            let! response = fetch url []
            let! text = response.text ()

            // Quick and hackish way to extract the translation from the string representation of the returned JSON...
            let m =
                Regex.Match(text, "translatedText\":\s*\"(.+?)\"")

            if m.Success then
                let translation = m.Groups[1].Value
                dispatch (SetTranslation(translationKey, translation))
        }
        |> Promise.start

    Html.div [ prop.style [ style.display.inlineBlock
                            style.marginLeft 7
                            style.marginRight 1
                            style.marginBottom 2 ]
               prop.children [ Bulma.button.button [ button.isSmall
                                                     prop.style [ style.fontSize 10 ]
                                                     prop.disabled (translations.ContainsKey translationKey)
                                                     prop.onClick onClick
                                                     prop.title "Translate with Giellatekno Apertium"
                                                     prop.text "Trans" ] ] ]

type ResultLineFields =
    { SId: string
      PreMatch: ReactElement []
      SearchWord: ReactElement []
      PostMatch: ReactElement [] }

type ResultInfo =
    { Word: ResultLineFields
      MaybeOrig: ResultLineFields option }

let idColumn
    (corpus: Corpus)
    (model: ConcordanceModel)
    sId
    messageId
    (maybeFullText: string option)
    rowIndex
    (loadedCorpusDispatch: Update.LoadedCorpus.Msg -> unit)
    (dispatch: Msg -> unit)
    =

    // If the 'match' property is defined, we know that we have a result from a monolingual
    // search or the first language of a multilingual one. If that is the case, and s-id is
    // defined, we print it in the first column (if we have a non-first language result, we
    // will include it in the next column instead).

    // Remove any "suffixes" from the s-id, since they typically denote individual sentences,
    // which are irrelevant when fetching metadata.
    let textId = Regex.Replace(sId, "\..+", "")

    Html.div [ Html.div [ Html.div [ Html.a [ prop.href ""
                                              prop.onClick (fun e ->
                                                  e.preventDefault ()
                                                  e.stopPropagation ()
                                                  loadedCorpusDispatch (Update.LoadedCorpus.FetchMetadataForText(corpus, textId)))
                                              prop.children [ Html.span sId ] ] ]
                          Html.div [ match corpus.SharedInfo.GoogleTranslateApiKey, maybeFullText with
                                     | Some key, Some fullText ->
                                         if corpus.SharedInfo.ExternalTools
                                            |> List.contains ExternalTool.GoogleTranslate then
                                             TranslationButton
                                                 model.ResultPageNo
                                                 rowIndex
                                                 fullText
                                                 model.Translations
                                                 key
                                                 dispatch
                                         elif corpus.SharedInfo.ExternalTools
                                              |> List.contains ExternalTool.SaamiTranslate then
                                             TranslationButtonSaami
                                                 model.ResultPageNo
                                                 rowIndex
                                                 fullText
                                                 model.Translations
                                                 key
                                                 dispatch
                                         else
                                             Html.none
                                     | _ -> Html.none
                                     if corpus.SharedInfo.IsTreebank then
                                         Html.div [ prop.style [ style.display.inlineBlock
                                                                 style.marginLeft 1
                                                                 style.marginRight 1
                                                                 style.marginBottom 2 ]
                                                    prop.children [ Bulma.button.button [ button.isSmall
                                                                                          prop.style [ style.fontSize 10 ]
                                                                                          prop.title
                                                                                              "Show syntactic tree"
                                                                                          prop.onClick (fun _ ->
                                                                                              SetSyntaxTree
                                                                                                  $"{model.ResultPageNo}_{rowIndex}"
                                                                                              |> dispatch)
                                                                                          prop.children [ Bulma.icon [ Html.i [ prop.className [ "fa fa-sitemap" ] ] ] ] ] ] ]
                                     else
                                         Html.none ] ]
               corpus.ResultLinks(model.ResultPageNo, rowIndex, textId, messageId, corpus.SharedInfo) ]


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

[<ReactComponent(import = "default", from = "../../../../syntax_tree.jsx")>]
let SyntaxTree (cnl: SyntaxNode []) = React.imported ()

