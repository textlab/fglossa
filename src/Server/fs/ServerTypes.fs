module ServerTypes

open System.Text.RegularExpressions
open Shared

type Corpus(config: CorpusConfig) =
    // Determine the size of the corpus (or the corpora, in the case of parallel corpora)
    // in the class constructor and store it in a private field, which can then be provided
    // as part of the Config property (see below) without having to recompute it each time
    // the proeperty is accessed.
    let corpusSizes =
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

    member _.Config = { config with Sizes = corpusSizes }
    member val Encoding = System.Text.Encoding.UTF8 with get, set
