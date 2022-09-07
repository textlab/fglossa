#!/usr/bin/env ruby

if ARGV.length < 1
    STDERR.puts "Usage: create_client_code.rb MODULE"
    exit
end

mod = ARGV[0]

puts "module Corpora.#{mod}.Client.ZCore

open Corpora.#{mod}
open Shared.Metadata
open MetadataCategories
open Model

let getCorpus config =
    Corpus(config)
"
