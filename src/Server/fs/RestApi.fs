module RestApi

open System
open Serilog
open Shared
open Remoting.Search.Cwb.Common

let getText (logger: ILogger) (corpusCode: string) (attributeName: string) (metadataSelection: Metadata.Selection) =
    let corpus =
        Corpora.Server.getCorpus corpusCode

    let cwbCorpus = corpus.Config.Code.ToUpper()

    // We need to create a new SearchParams record containing the metadata selection since
    // printPositionsMatchingMetadata expects that.
    let searchParams =
        { SearchParams.Init(corpus.Config) with MetadataSelection = metadataSelection }

    let startpos = 0L

    let endpos =
        corpus.CorpusSizes()[corpus.Config.Code]

    let guid = Guid.NewGuid()

    let positionsFilename =
        $"/tmp/glossa/positions_get_text_{guid}"

    let res =
        printPositionsMatchingMetadata logger corpus searchParams startpos endpos positionsFilename

    res.Wait()

    let text =
        if Environment.GetEnvironmentVariable("CWB_IN_DOCKER") = "1" then
            Process.runCmdWithOutput
                "docker"
                $"exec -i cwb cwb-decode -H -f {positionsFilename} {cwbCorpus} -P {attributeName}"
        else
            Process.runCmdWithOutput "cwb-decode" $"-H -f {positionsFilename} {cwbCorpus} -P {attributeName}"

    text
