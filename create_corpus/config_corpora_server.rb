#!/usr/bin/env ruby

require "pathname"

corpus_dirs = Pathname.new("../src/Corpora/corpora/").children.select(&:directory?).map(&:basename).map(&:to_s).sort

module_names = corpus_dirs.map { |dir| dir.split('_').collect(&:capitalize).join }

corpus_list = module_names.map { | module_name | "          #{module_name}.Server.ZCore.corpusConfig" }.join("\n")
corpus_matches = corpus_dirs.zip(module_names).map { | dir, mod | "        | \"#{dir}\" -> #{mod}.Server.ZCore.getCorpus ()" }.join("\n")

puts "module Corpora.Server

/////////////////////////////////////////////////////////////////////
//// !!! Auto-generated from corpus directory structure. Do not edit!
/////////////////////////////////////////////////////////////////////

open ServerTypes
open Shared

let getCorpusList () =
    [
#{corpus_list}
    ]
    |> List.map (fun config -> (config.Code, config.Name))

let getCorpus (corpusCode: string) : Corpus =
    match corpusCode with
#{corpus_matches}
        | _ -> failwithf $\"Unknown corpus: %s{corpusCode}\"
"
