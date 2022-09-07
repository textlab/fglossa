#!/usr/bin/env ruby

if ARGV.length < 3
    STDERR.puts "Usage: create_server_code.rb CORPUS_DIR CORPUS_NAME MODULE"
    exit
end

corpus_dir = ARGV[0]
corpus_name = ARGV[1]
mod = ARGV[2]

puts "module Corpora.#{mod}.Server.ZCore

open Shared
open ServerTypes

let corpusConfig =
    SharedCorpusInfo.Init(code = \"#{corpus_dir}\", name = \"#{corpus_name}\")

let getCorpus () = Corpus(corpusConfig)
"
