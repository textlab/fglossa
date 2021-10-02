module ServerTypes

open System.IO
open System.Text.RegularExpressions
open Shared
open Shared.StringUtils
open Database

let getConnectionString corpusCode =
    let code = sanitizeString corpusCode

    $"DataSource=../Corpora/corpora/{code}/{code}.sqlite"

type Corpus(config: CorpusConfig) =
    let corpusInfo =
        try
            Some(File.ReadAllText($"../Corpora/corpora/{config.Code}/{config.Code}.html"))
        with
        | :? FileNotFoundException -> None

    member _.Config = { config with Info = corpusInfo }

    member val Encoding = System.Text.Encoding.UTF8 with get, set

    /// Only implemented for Corpus Workbench corpora. A map from CWB corpus name (lowercase) to number of tokens
    member _.CorpusSizes() =
        match config.SearchEngine with
        | Cwb ->
            // Run cwb-describe-corpus and extract the corpus size(s) from its output
            let cwbCorpora =
                match config.LanguageConfig with
                | Monolingual _ -> [| config.Code |]
                | Multilingual languages ->
                    languages
                    |> Array.map (fun lang -> $"{config.Code}_{lang.Code}")

            cwbCorpora
            |> Array.fold
                (fun (sizeMap: Map<string, uint64>) cwbCorpus ->
                    let corpusDescription =
                        Process.runCmdWithOutput "docker" $"exec -i cwb cwb-describe-corpus {cwbCorpus}"

                    let sizeLine =
                        corpusDescription.Split('\n')
                        |> Array.filter (fun line -> Regex.IsMatch(line, "^size\s+\(tokens\)"))
                        |> Array.head

                    let size =
                        Regex
                            .Match(
                                sizeLine,
                                "^size\s+\(tokens\):\s+(\d+)"
                            )
                            .Groups.[1]
                            .Value
                        |> uint64

                    sizeMap.Add(cwbCorpus, size))
                Map.empty
        | Fcs -> failwith "Not implemented"

    /// A set containing the paths of audio files belonging to this corpus, with the
    /// '.mp3' extension removed
    member _.AudioFiles() =
        try
            Directory.GetFiles($"../Corpora/corpora/{config.Code}/audio")
            |> Array.filter (fun filename -> filename.EndsWith(".mp3"))
            |> Array.map (replace ".+/" "" >> replace "\.mp3$" "")
            |> Set.ofArray
        with
        | :? DirectoryNotFoundException -> Set.empty

    /// A set containing the paths of video files belonging to this corpus, with the
    /// '.mp4' extension removed
    member _.VideoFiles() =
        try
            Directory.GetFiles($"../Corpora/corpora/{config.Code}/video")
            |> Array.filter (fun filename -> filename.EndsWith(".mp4"))
            |> Array.map (replace ".+/" "" >> replace "\.mp4$" "")
            |> Set.ofArray
        with
        | :? DirectoryNotFoundException -> Set.empty
