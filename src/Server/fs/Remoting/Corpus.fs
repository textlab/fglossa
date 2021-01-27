module Remoting.Corpus

open System.Data.SQLite
open System.Threading.Tasks
open FSharp.Control.Tasks.ContextInsensitive
open Serilog
open Database
open Shared

open Shared

let getCorpora () = task { return [] }

let getCorpus code = task
