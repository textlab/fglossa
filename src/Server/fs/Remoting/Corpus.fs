module Remoting.Corpus

open System.Data.SQLite
open System.Threading.Tasks
open FSharp.Control.Tasks.ContextInsensitive
open Serilog
open ServerTypes
open Database
open Shared

let getCorpora () = task { return [] }

let getCorpus code =
    task {
        let corpus = Corpora.Server.getCorpus code
        return corpus.Config
    }
