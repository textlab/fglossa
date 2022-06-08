#!/usr/bin/env ruby

if ARGV.length < 1
    STDERR.puts "Usage: create_corpus.rb CORPUS_DIR [METADATA-TSV-FILE]"
    exit
end

corpus_dir = ARGV[0].split("/").last
full_corpus_path = "../src/Corpora/corpora/#{corpus_dir}"

metadata_file =
    if ARGV.length == 2 then ARGV[1].split("/").last else "#{corpus_dir}_texts.tsv" end

the_module = corpus_dir.split('_').collect(&:capitalize).join

system("./create_metadata_categories.rb #{full_corpus_path}/#{metadata_file} #{the_module} > " +
       "#{full_corpus_path}/Shared/MetadataCategories.fs")

system("./create_db_definition.rb #{full_corpus_path}/#{metadata_file} > " +
       "#{full_corpus_path}/create_db.sql")

system("./config_corpora.sh")

system("rm -f #{full_corpus_path}/#{corpus_dir}.sqlite")
system("sqlite3 #{full_corpus_path}/#{corpus_dir}.sqlite --init #{full_corpus_path}/create_db.sql")