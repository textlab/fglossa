module Remoting.Search

open System.Threading.Tasks
open FSharp.Control.Tasks
open Shared

let runCmd (cmd: string) (args: string) =
    let proc =
        System.Diagnostics.Process.Start(cmd, args)

    proc.WaitForExit()

let runCmdWithOutput (cmd: string) (args: string) =
    let startInfo =
        System.Diagnostics.ProcessStartInfo(cmd, args)

    startInfo.UseShellExecute <- false
    startInfo.RedirectStandardOutput <- true

    let proc =
        System.Diagnostics.Process.Start(startInfo)

    let output = proc.StandardOutput.ReadToEnd()
    proc.WaitForExit()
    output

// If the number of running CQP processes exceeds this number, we do not allow a new
// search in a corpus that does parallel search using all cpus to be started.
let maxCqpProcesses = 8

let searchCorpus (searchParams: SearchParams) =
    task {
        let cqpProcs = runCmdWithOutput "pgrep" "-f cqp"
        let nCqpProcs = cqpProcs.Split('\n').Length

        let corpus =
            Corpora.Server.getCorpus searchParams.CorpusCode

        // If we are searching a corpus that does parallel search with multiple cpus and
        // this is the first search step, we check that we don't already exceed the max number
        // of CQP processes before starting the search. If we are at step 2 or 3, we should finish
        // what we started. Corpora that don't use multiple cpus are assumed to be small and
        // should not cause any problems even with a lot of CQP processes.
        if corpus.Config.MultiCpuBounds.IsNone
           || nCqpProcs < maxCqpProcesses
           || searchParams.Step > 1 then

            return
                { Count = 1u
                  CpuCounts = [| 1u |]
                  SearchId = 1
                  Results =
                      [| { HasAudio = false
                           HasVideo = false
                           Text = "heidu" } |] }
        else
            return failwith $"TOO MANY CQP PROCESSES: {nCqpProcs}; aborting search at {System.DateTime.Now}"
    }
