#!/usr/bin/env ruby

require "pathname"

corpus_dirs = Pathname.new("../src/Corpora/corpora/").children.select(&:directory?).map(&:basename).map(&:to_s).sort

module_names = corpus_dirs.map { |dir| dir.split('_').collect(&:capitalize).join }

corpus_matches = corpus_dirs.zip(module_names).map { | dir, mod | "    | \"#{dir}\" -> #{mod}.Client.ZCore.getCorpus config" }.join("\n")

puts "module Corpora.Client

/////////////////////////////////////////////////////////////////////
//// !!! Auto-generated from corpus directory structure. Do not edit!
/////////////////////////////////////////////////////////////////////

open Shared
open Model

let getCorpus (config: SharedCorpusInfo) : Corpus =
    match config.Code with
#{corpus_matches}
    | _ -> failwithf $\"Unknown corpus: %s{config.Code}\"
"
