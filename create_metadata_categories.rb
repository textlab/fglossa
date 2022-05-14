#!/usr/bin/env ruby

if ARGV.length < 2
    STDERR.puts "Usage: create_metadata_categories.rb [METADATA-TSV-FILE] [MODULE]"
    exit
end

cats = File.readlines(ARGV[0])[0].split

puts "module Corpora.#{ARGV[1]}.MetadataCategories

///////////////////////////////////////////////////
//// !!! Auto-generated from TSV file. Do not edit!
///////////////////////////////////////////////////

open Shared.Metadata
"

cats.each do |cat|
    cat_code, cat_type = cat.split('$')
    catClass = cat_code.split('_').collect(&:capitalize).join
    superclass = if cat_type && cat_type.start_with?('n') then "NumberCategory" else "StringCategory" end


    if cat_code != "bounds"
        puts "
type #{catClass}(aName) =
    inherit #{superclass}(aName)
    override _.Code = \"#{cat_code}\"
"
    end
end