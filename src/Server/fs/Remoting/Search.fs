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

let searchCorpus (searchParams: SearchParams) =
    task {
        let cqpProcs = runCmdWithOutput "pgrep" "cqp"
        printfn $"{cqpProcs}"

        return
            { Count = 1u
              CpuCounts = [| 1u |]
              SearchId = 1
              Results =
                  [| { HasAudio = false
                       HasVideo = false
                       Text = "heidu" } |] }
    }
