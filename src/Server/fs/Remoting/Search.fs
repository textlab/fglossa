module Remoting.Search

open System.Threading.Tasks
open FSharp.Control.Tasks
open Shared

let searchCorpus (searchParams: SearchParams) =
    task {
        return
            { Count = 1u
              CpuCounts = [| 1u |]
              SearchId = 1
              Results =
                  [| { HasAudio = false
                       HasVideo = false
                       Text = "heidu" } |] }
    }
