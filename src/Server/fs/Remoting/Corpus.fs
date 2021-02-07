module Remoting.Corpus

open System.Data.SQLite
open System.Threading.Tasks
open FSharp.Control.Tasks.ContextInsensitive
open Serilog
open ServerTypes
open Database
open Shared

let getCorpusList () =
    async { return Corpora.Server.getCorpusList () }

let getCorpus code =
    async {
        let corpus = Corpora.Server.getCorpus code
        return corpus.Config
    }
