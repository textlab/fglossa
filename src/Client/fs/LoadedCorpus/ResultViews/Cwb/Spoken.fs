module View.LoadedCorpus.ResultViews.Cwb.Spoken

open Fable.Core
open Fable.Core.JsInterop
open System
open System.Text.RegularExpressions
open Feliz
open Feliz.Bulma
open Shared
open Shared.StringUtils
open Model
open Update.LoadedCorpus.ShowingResults.Concordance
open View.LoadedCorpus.ResultViews.Cwb.Common

// Ugly, but it seems we need to abuse JSON.parse to correctly convert Norwegian characters
// in the translation from Saami...
[<Emit("JSON.parse('[\"' + $0 + '\"]')[0]")>]
let jsonParse (s: string) : string = failwith "JS only"

////////// CHANGE THIS! /////////
type SearchResultInfo =
    { AudioType: AudioType option
      HasVideo: bool
      SId: string
      PreMatch: ReactElement []
      Match: ReactElement []
      PostMatch: ReactElement []
      FullText: string option }

[<ReactComponent(import = "WFplayer", from = "../../../../wfplayer.js")>]
let WFplayer (mediaObj: MediaObject, divs: obj) = React.imported ()

[<ReactComponent(import = "Jplayer", from = "../../../../jplayer.js")>]
let Jplayer (mediaObj: MediaObject, divs: obj, mediaType: string, hasLocalMedia: bool, hasAudio: bool) =
    React.imported ()

[<ReactComponent>]
let MediaPlayerPopup (mediaPlayerInfo: MediaPlayerInfo) (dispatch: Msg -> unit) =
    let header =
        Bulma.level [ prop.style [ style.padding 20
                                   style.marginBottom 0 ]
                      prop.children [ Bulma.levelLeft [ Bulma.levelItem [] ]
                                      Bulma.levelRight [ Bulma.levelItem (
                                                             Bulma.button.button [ prop.title "Close"
                                                                                   prop.text "Close"
                                                                                   color.isInfo
                                                                                   prop.style [ style.marginLeft 40 ]
                                                                                   prop.onClick (fun _ ->
                                                                                       dispatch RemoveMediaObject) ]
                                                         ) ] ] ]

    let mediaDisplay =
        // Since JS does not understand F# maps, we need to convert the Divs
        // map and the attribute map for each div to a JS objects
        let divs =
            mediaPlayerInfo.MediaObject.Divs
            |> Map.toArray
            |> Array.map (fun (key, value) ->
                let tokens =
                    value.Line
                    |> Array.map (fun (tokenIndex, attributeMap) ->
                        let attrObj =
                            attributeMap
                            |> Map.toList
                            |> List.map (fun (attrName, attrValue) -> (attrName, box attrValue))
                            |> createObj

                        (string tokenIndex, attrObj))
                    |> createObj

                let valueObj =
                    [ "Speaker" ==> value.Speaker
                      "Line" ==> tokens
                      "From" ==> value.From
                      "To" ==> value.To
                      "IsMatch" ==> value.IsMatch ]
                    |> createObj

                string key, valueObj)
            |> createObj

        Bulma.section [ match mediaPlayerInfo.Type with
                        | WaveformPlayer -> WFplayer(mediaPlayerInfo.MediaObject, divs)
                        | AudioPlayer -> Jplayer(mediaPlayerInfo.MediaObject, divs, "audio", false, true)
                        | VideoPlayer -> Jplayer(mediaPlayerInfo.MediaObject, divs, "video", false, true) ]

    let footer =
        Bulma.level [ prop.style [ style.padding 20
                                   style.marginBottom 0 ]
                      prop.children [ Bulma.levelLeft []
                                      Bulma.levelRight [ Bulma.levelItem (
                                                             Bulma.button.button [ prop.title "Close"
                                                                                   prop.text "Close"
                                                                                   color.isInfo
                                                                                   prop.style [ style.marginLeft 40 ]
                                                                                   prop.onClick (fun _ ->
                                                                                       dispatch RemoveMediaObject) ]
                                                         ) ] ] ]

    let elementRef = React.useElementRef ()

    let focusPopup () =
        elementRef.current
        |> Option.iter (fun popupElement -> popupElement.focus ())

    // Focus the popup when mounted to enable it to receive keyboard events
    React.useEffectOnce focusPopup

    let popup =
        Html.div [ prop.className "overscroll-contain"
                   prop.style [ style.height (length.percent 100)
                                style.top 0
                                style.left 0
                                style.width (length.percent 100)
                                style.position.absolute
                                style.zIndex 40
                                style.backgroundColor "white"
                                style.overflowX.hidden
                                style.border (1, borderStyle.solid, "black")
                                style.transitionProperty transitionProperty.height
                                style.transitionDuration (TimeSpan(3500000L))
                                style.transitionTimingFunction.easeOut ]
                   // Set elementRef in order to apply the focusPopup() function to this element
                   prop.ref elementRef
                   // Set tabIndex so that the element receives keyboard events
                   prop.tabIndex 0
                   prop.onKeyUp (fun e ->
                       if e.key = "Escape" then
                           dispatch RemoveMediaObject)
                   // Stop propagation to prevent trouble caused by re-rendering
                   prop.onClick (fun e -> e.stopPropagation ())
                   prop.children [ header
                                   mediaDisplay
                                   footer ] ]


    let root =
        Browser.Dom.document.getElementById "media-player-popup-root"

    ReactDOM.createPortal (popup, root)

let concordanceTable
    (model: ConcordanceModel)
    (corpus: Corpus)
    (pageResults: SearchResult [] option)
    (loadedCorpusDispatch: Update.LoadedCorpus.Msg -> unit)
    (dispatch: Msg -> unit)
    =
    let mediaPlayer =
        match model.MediaPlayer with
        | Some mediaPlayerInfo -> MediaPlayerPopup mediaPlayerInfo dispatch
        | None -> Html.none

    let extractFields result =
        let m =
            Regex.Match(result, "^<who_name\s+(\S*?)>:\s+(.*)\{\{(.+?)\}\}(.*?)$")

        let groupValues =
            m.Groups
            |> Seq.map (fun group -> group.Value)
            |> Seq.toList

        let sId = groupValues[1]

        let pre =
            groupValues[2]
            // If the result begins with a who_name tag with the same ID as the one for the
            // actual match, it feels redundant (since that speaker ID is listed just
            // to the left of it), so just remove it.
            |> replace $"^<who_name\s+{sId}>" ""

        let searchWord =
            groupValues[3]
            |> fun m ->
                // Do the same with the match if there is no left context
                if String.IsNullOrWhiteSpace(pre) then
                    Regex.Replace(m, $"^<who_name\s+{sId}>", "")
                else
                    m

        let post = groupValues[4]

        (sId, pre, searchWord, post)

    let audioVideoLinks (resultInfo: SearchResultInfo) rowIndex : ReactElement =
        let audioButton title icon =
            Bulma.button.button [ button.isSmall
                                  prop.title title
                                  prop.style [ style.marginLeft 2
                                               style.marginTop 2
                                               style.marginBottom 2 ]
                                  prop.onClick (fun _ -> dispatch (FetchMediaObject(AudioPlayer, rowIndex)))
                                  prop.children [ Bulma.icon [ Html.i [ prop.className [ "fa"; icon ] ] ] ] ]

        Html.span [ prop.style [ style.whitespace.nowrap ]
                    prop.children [ if resultInfo.HasVideo then
                                        Bulma.button.button [ button.isSmall
                                                              prop.title "Show video"
                                                              prop.style [ style.marginTop 2
                                                                           style.marginBottom 2 ]
                                                              prop.onClick (fun _ ->
                                                                  dispatch (FetchMediaObject(VideoPlayer, rowIndex)))
                                                              prop.children [ Bulma.icon [ Html.i [ prop.className [ "fa fa-film" ] ] ] ] ]
                                    match resultInfo.AudioType with
                                    | Some Sound -> audioButton "Play audio" "fa-volume-up"
                                    | Some Nosound -> audioButton "Show more context" "fa-search"
                                    | None -> Html.none
                                    if resultInfo.AudioType = Some Sound then
                                        Bulma.button.button [ button.isSmall
                                                              prop.title "Show waveform"
                                                              prop.style [ style.marginLeft 2
                                                                           style.marginTop 2
                                                                           style.marginBottom 2
                                                                           style.paddingLeft 6
                                                                           style.paddingRight 6 ]
                                                              prop.onClick (fun _ ->
                                                                  dispatch (FetchMediaObject(WaveformPlayer, rowIndex)))
                                                              prop.children [ Html.img [ prop.src "speech/waveform.png"
                                                                                         prop.style [ style.width 12 ] ] ] ] ] ]

    let orthographicRow (corpus: Corpus) (resultInfo: SearchResultInfo) rowIndex =
        let hasPhon =
            corpus.SharedInfo.HasAttribute("phon")

        let (resultLineFields: ResultLineFields) =
            { SId = resultInfo.SId
              PreMatch = resultInfo.PreMatch
              SearchWord = resultInfo.Match
              PostMatch = resultInfo.PostMatch }

        Html.tr [ prop.key $"ort{rowIndex}"
                  prop.children [ Html.td [ prop.style [ style.textAlign.center
                                                         style.verticalAlign.middle ]
                                            prop.children [ idColumn
                                                                corpus
                                                                model
                                                                resultInfo.SId
                                                                resultInfo.FullText
                                                                rowIndex
                                                                loadedCorpusDispatch
                                                                dispatch
                                                            if not hasPhon then
                                                                // If we don't have a phonetic transcription, we need to show the audio and video
                                                                // links in the orthographic row instead
                                                                Html.div [ prop.style [ style.marginTop 5 ]
                                                                           prop.children (
                                                                               audioVideoLinks resultInfo rowIndex
                                                                           ) ] ] ]
                                  yield! textColumns resultLineFields ] ]

    let translatedRow rowIndex =
        let translationKey =
            $"{model.ResultPageNo}_{rowIndex}"

        match model.Translations.TryFind(translationKey) with
        | Some translation ->
            if corpus.SharedInfo.ExternalTools
               |> List.contains GoogleTranslate then
                Html.tr [ Html.td [ Html.a [ prop.href "http://translate.google.com/"
                                             prop.target.blank
                                             prop.children[Html.img [ prop.src "attr1-2.png" ]] ] ]
                          Html.td [ prop.colSpan 3
                                    prop.style [ style.color "#737373" ]
                                    prop.dangerouslySetInnerHTML translation ] ]
            else
                Html.tr [ Html.td [ Html.span "Translated by "
                                    Html.a [ prop.href "http://gtweb.uit.no/jorgal/index.nob.html?dir=sme-nob#"
                                             prop.target.blank
                                             prop.text "Giellatekno Apertium" ] ]
                          Html.td [ prop.colSpan 3
                                    prop.style [ style.color "#737373" ]
                                    prop.text (jsonParse translation) ] ]
        | None -> Html.none

    let phoneticRow _corpus (resultInfo: SearchResultInfo) rowIndex =
        let (resultLineFields: ResultLineFields) =
            { SId = resultInfo.SId
              PreMatch = resultInfo.PreMatch
              SearchWord = resultInfo.Match
              PostMatch = resultInfo.PostMatch }

        Html.tr [ prop.key $"phon{rowIndex}"
                  prop.children [ Html.td [ prop.style [ style.textAlign.center
                                                         style.verticalAlign.middle ]
                                            prop.children [ audioVideoLinks resultInfo rowIndex ] ]
                                  yield! textColumns resultLineFields ] ]

    let processToken token index displayedFieldIndex (_maybeOrtPhonIndex: int option) maybeLemmaIndex tipFieldIndexes =
        if String.IsNullOrWhiteSpace(token) then
            (Html.none, "")

        else
            let attrs =
                token.Split("/")
                |> Array.mapi (fun index attr ->
                    attr
                    // TODO: Fix the use of italics. Does not work with Feliz.Bulma.Tooltip
                    // |> fun a ->
                    //     // Show the orthographic or phonetic form in italics
                    //     // if present
                    //     match maybeOrtPhonIndex with
                    //     | Some ortPhonIndex when ortPhonIndex = index -> $"<i>{a}</i>"
                    //     | _ -> a
                    |> fun a ->
                        // Show the lemma in quotes, if present
                        match maybeLemmaIndex with
                        | Some lemmaIndex when lemmaIndex = index -> $"\"{a}\""
                        | _ -> a)

            let maybeUrlsIndex =
                attrs
                |> Array.tryFindIndex (fun attr -> attr.Contains("\$https?:"))

            let tipAttrs =
                attrs
                |> Array.indexed
                |> Array.filter (fun (index, _) -> tipFieldIndexes |> List.contains index)
                // Remove attributes that contain URLs for linking to dictionaries etc.
                |> Array.filter (fun (index, _) ->
                    match maybeUrlsIndex with
                    | Some urlsIndex -> index <> urlsIndex
                    | None -> true)
                |> Array.filter (fun (_, attr) ->
                    [ "__UNDEF__"
                      "\"__UNDEF__\""
                      "<i>__UNDEF__</i>"
                      "-"
                      "_"
                      "\"_\""
                      "<i>_</i>" ]
                    |> List.contains attr
                    |> not)
                |> fun a ->
                    if corpus.SharedInfo.IsTreebank then
                        Array.sub a 0 (a.Length - 2)
                    else
                        a
                |> Array.map snd

            let tipText = tipAttrs |> String.concat " "

            let maybeUrls =
                match maybeUrlsIndex with
                | Some urlsIndex ->
                    attrs[urlsIndex]
                    |> replace "!!" "/"
                    |> fun s -> s.Split('$')
                    |> Array.filter (not << String.IsNullOrWhiteSpace)
                    |> Some
                | None -> None

            let dt =
                attrs[displayedFieldIndex]
                |> replace "__UNDEF__" ""

            let displayedText =
                match maybeUrls with
                | Some urls ->
                    let links =
                        (urls, [| "[1]"; "[2]" |])
                        ||> Array.map2 (fun url symbol ->
                            Html.a [ prop.key url
                                     prop.href url
                                     prop.target "_blank"
                                     prop.dangerouslySetInnerHTML symbol ])

                    Html.span [ prop.style [ style.whitespace.nowrap ]
                                prop.children [ Html.span dt
                                                Html.sup links ] ]
                | None -> Html.text dt

            (Html.span [ prop.key $"{index}{tipText}"
                         tooltip.text tipText
                         prop.className "has-tooltip-arrow"
                         match corpus.SharedInfo.FontFamily with
                         | Some fontFamily -> prop.style [ style.fontFamily fontFamily ]
                         | None -> ignore None
                         prop.children [ displayedText
                                         Html.text " " ] ],
             dt)

    // Processes a pre-match, match, or post-match field
    let processField
        displayedFieldIndex
        maybeOrtPhonIndex
        maybeLemmaIndex
        tipFieldIndexes
        field
        : (ReactElement * string) [] =
        let tokens =
            field
            |> replace "<who_name\s+(.+?)>\s*" "<who_name_$1> "
            |> replace "\s*</who_name>" " $&"
            |> fun s -> s.Split()

        tokens
        |> Array.indexed
        |> Array.choose (fun (index, token) ->
            let m =
                Regex.Match(token, "<who_name_(.+?)>")

            if m.Success then
                // Extract the speaker ID and put it in front of its segment
                let speakerId = m.Groups[1].Value

                Some(
                    (Html.span [ prop.key index
                                 prop.className "speaker-id"
                                 prop.text $"<{speakerId}> " ],
                     "")
                )
            // Ignore end-of-segment tags; process all other tokens
            elif not (token.Contains("</who_name>")) then
                processToken token index displayedFieldIndex maybeOrtPhonIndex maybeLemmaIndex tipFieldIndexes
                |> Some
            else
                None)

    let syntaxRow (model: ConcordanceModel) rowIndex =
        let key = $"{model.ResultPageNo}_{rowIndex}"

        if model.VisibleSyntaxTreeKeys.Contains(key) then
            let searchResult =
                model.ResultPages[model.ResultPageNo][rowIndex]

            let tokens =
                searchResult.Text[0]
                |> replace "<who_name\s+(.+?)>\s*" "<who_name_$1> "
                |> replace "\s*</who_name>" " $&"
                |> fun s -> s.Split()
                |> Array.skip 3

            let nodes =
                [| for token in tokens ->
                       let isMatch =
                           Regex.IsMatch(token, "^\{\{.+\}\}$")

                       let attrs =
                           token
                           |> replace "^\{\{" ""
                           |> replace "\}\}$" ""
                           |> fun t -> t.Split('/')

                       let orthographicForm = attrs[0]
                       let partOfSpeech = attrs[3]
                       let synFunc = attrs[attrs.Length - 3]
                       let index = attrs[attrs.Length - 2] |> int

                       let dependency =
                           attrs[attrs.Length - 1] |> int

                       { index = index
                         ort = orthographicForm
                         pos = partOfSpeech
                         dep = dependency
                         ``fun`` = synFunc
                         ``match`` = isMatch } |]

            Html.tr [ prop.key $"syntax_{key}"
                      prop.children [ Html.td [ prop.colSpan 4
                                                prop.style [ style.maxWidth 1100
                                                             style.overflowX.auto ]
                                                prop.children (SyntaxTree(nodes)) ] ] ]
        else
            Html.none

    // Returns one or more rows representing a single search result
    let singleResultRows
        ortIndex
        maybePhonIndex
        maybeLemmaIndex
        ortTipIndexes
        phonTipIndexes
        (searchResult: SearchResult)
        rowIndex
        =

        let line = searchResult.Text.Head

        let sId, pre, searchWord, post =
            extractFields line

        let ortPre =
            processField ortIndex maybePhonIndex maybeLemmaIndex ortTipIndexes pre

        let ortMatch =
            processField ortIndex maybePhonIndex maybeLemmaIndex ortTipIndexes searchWord

        let ortPost =
            processField ortIndex maybePhonIndex maybeLemmaIndex ortTipIndexes post

        let ortText =
            [| ortPre |> Array.map snd
               ortMatch |> Array.map snd
               ortPost |> Array.map snd |]
            |> Array.concat
            |> String.concat " "
            |> replace "\s*#" ""

        let ortResInfo =
            { AudioType = searchResult.AudioType
              HasVideo = searchResult.HasVideo
              SId = sId
              PreMatch = ortPre |> Array.map fst
              Match = ortMatch |> Array.map fst
              PostMatch = ortPost |> Array.map fst
              FullText = Some ortText }

        let orthographic =
            orthographicRow corpus ortResInfo rowIndex

        let phonetic =
            match maybePhonIndex with
            | Some phonIndex ->
                let phonPre =
                    processField phonIndex (Some ortIndex) maybeLemmaIndex phonTipIndexes pre

                let phonMatch =
                    processField phonIndex (Some ortIndex) maybeLemmaIndex phonTipIndexes searchWord

                let phonPost =
                    processField phonIndex (Some ortIndex) maybeLemmaIndex phonTipIndexes post

                let phonText =
                    [| phonPre |> Array.map snd
                       phonMatch |> Array.map snd
                       phonPost |> Array.map snd |]
                    |> Array.concat
                    |> String.concat " "
                    |> replace "\s*#" ""

                let phonResInfo =
                    { AudioType = searchResult.AudioType
                      HasVideo = searchResult.HasVideo
                      SId = sId
                      PreMatch = phonPre |> Array.map fst
                      Match = phonMatch |> Array.map fst
                      PostMatch = phonPost |> Array.map fst
                      FullText = Some phonText }


                Some(phoneticRow corpus phonResInfo rowIndex)
            | None -> None

        let separator = separatorRow rowIndex

        // let phonPre, phonMatch, phonPost =
        //     match maybePhonIndex with
        //     | Some phonIndex ->
        //         processField phonIndex (Some ortIndex) maybeLemmaIndex ortTipIndexes [ pre; searchWord; post ]
        //     | None -> None, None, None

        [ orthographic
          translatedRow rowIndex
          if phonetic.IsSome then phonetic.Value
          if corpus.SharedInfo.IsTreebank then
              syntaxRow model rowIndex
          separator ]

    let attributes =
        match corpus.SharedInfo.LanguageConfig with
        | Monolingual (Some attrs) -> attrs
        | Monolingual None -> []
        | Multilingual _languages -> failwith "NOT IMPLEMENTED!"

    let ortIndex = 0

    // We need to increment phonIndex and lemmaIndex, as well as the ranges of indexes below,
    // since the first attribute ('word') is not in the list because it is shown by
    // default by CQP
    let maybePhonIndex =
        attributes
        |> List.tryFindIndex (fun attr -> attr.Code = "phon")
        |> Option.map (fun i -> i + 1)

    let maybeLemmaIndex =
        attributes
        |> List.tryFindIndex (fun attr -> attr.Code = "lemma")
        |> Option.map (fun i -> i + 1)

    let ortTipIndexes =
        [ 0 .. attributes.Length ]
        |> List.except [ ortIndex ]

    let phonTipIndexes =
        match maybePhonIndex with
        | Some phonIndex ->
            [ 0 .. attributes.Length ]
            |> List.except [ phonIndex ]
        | None -> [ 0 .. attributes.Length ]

    let rows =
        match pageResults with
        | Some results ->
            results
            |> Array.toList
            |> List.indexed
            |> List.collect (fun (index, result) ->
                singleResultRows ortIndex maybePhonIndex maybeLemmaIndex ortTipIndexes phonTipIndexes result index)
        | None -> []


    Html.span [ mediaPlayer
                Bulma.table [ table.isFullWidth
                              table.isBordered
                              table.isNarrow
                              prop.className "concordance-table"
                              prop.children [ Html.tbody rows ] ] ]
