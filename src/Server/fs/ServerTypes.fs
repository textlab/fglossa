module ServerTypes

open System.IO
open System.Text.RegularExpressions
open System.Threading.Tasks
open Serilog
open Shared
open Shared.StringUtils
open Database

let corpusRoot =
    let envVar =
        System.Environment.GetEnvironmentVariable("GLOSSA_CORPUS_ROOT")

    if isNull envVar then
        "../Corpora/corpora"
    else
        envVar

let downloadRoot =
    let envVar =
        System.Environment.GetEnvironmentVariable("GLOSSA_DOWNLOAD_ROOT")

    if isNull envVar then
        "../Client/public"
    else
        envVar

let getConnectionString corpusCode =
    let code = sanitizeString corpusCode

    $"DataSource={corpusRoot}/{code}/{code}.sqlite"

type Corpus(config: SharedCorpusInfo) =
    let corpusInfo =
        try
            Some(File.ReadAllText($"{corpusRoot}/{config.Code}/{config.Code}.html"))
        with
        | :? FileNotFoundException -> None

    let geoCoord =
        try
            let coordFile =
                $"{corpusRoot}/{config.Code}/{config.Code}_coords.tsv"

            [| for line in File.ReadAllLines(coordFile) ->
                   try
                       let fields = line.Split('\t')
                       let locationName = fields[0]
                       let lat = float fields[1]
                       let lng = float fields[2]
                       (locationName, lat, lng)
                   with
                   | e -> failwith $"Error in geo coordinate line: {line}" |]
            |> Some
        with
        | :? FileNotFoundException -> None

    member _.Config =
        let googleMapsApiKey =
            System.Environment.GetEnvironmentVariable("GOOGLE_MAPS_API_KEY")
            |> Option.ofObj

        let googleTranslateApiKey =
            System.Environment.GetEnvironmentVariable("GOOGLE_TRANSLATE_API_KEY")
            |> Option.ofObj

        { config with
            Info = corpusInfo
            GeoCoordinates = geoCoord
            GoogleMapsApiKey = googleMapsApiKey
            GoogleTranslateApiKey = googleTranslateApiKey }

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
                (fun (sizeMap: Map<string, int64>) cwbCorpus ->
                    let corpusDescription =
                        if System.Environment.GetEnvironmentVariable("CWB_IN_DOCKER") = "1" then
                            Process.runCmdWithOutput "docker" $"exec -i cwb cwb-describe-corpus {cwbCorpus}"
                        else
                            Process.runCmdWithOutput "cwb-describe-corpus" cwbCorpus

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
                        |> int64

                    sizeMap.Add(cwbCorpus, size))
                Map.empty
        | Fcs -> failwith "Not implemented"

    /// A set containing the paths of audio files belonging to this corpus, with the
    /// '.mp3' extension removed
    member _.AudioFiles() =
        try
            Directory.GetFiles($"{corpusRoot}/{config.Code}/audio")
            |> Array.filter (fun filename -> filename.EndsWith(".mp3"))
            |> Array.map (replace ".+/" "" >> replace "\.mp3$" "")
            |> Set.ofArray
        with
        | :? DirectoryNotFoundException -> Set.empty

    /// A set containing the paths of video files belonging to this corpus, with the
    /// '.mp4' extension removed
    member _.VideoFiles() =
        try
            Directory.GetFiles($"{corpusRoot}/{config.Code}/video")
            |> Array.filter (fun filename -> filename.EndsWith(".mp4"))
            |> Array.map (replace ".+/" "" >> replace "\.mp4$" "")
            |> Set.ofArray
        with
        | :? DirectoryNotFoundException -> Set.empty

    /// Returns additional text to be added after "All xxx texts selected" etc.
    abstract member GetTextSelectionInfo: ILogger * Metadata.Selection -> Task<string>
    default _.GetTextSelectionInfo(_, _) = Task.FromResult ""
