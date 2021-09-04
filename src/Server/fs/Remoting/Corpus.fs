module Remoting.Corpus

open System.Data.SQLite
open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Text.RegularExpressions
open Serilog
open ServerTypes
open Database
open Shared

let getCorpusConfig (corpusCode: string) =
    async {
        let corpus = Corpora.Server.getCorpus corpusCode
        return corpus.Config
    }

let getCorpusList () =
    async { return Corpora.Server.getCorpusList () }
