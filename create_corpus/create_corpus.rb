#!/usr/bin/env ruby

if ARGV.length < 2
    STDERR.puts "Usage: create_corpus.rb CORPUS_DIR CORPUS_NAME [METADATA-TSV-FILE]"
    exit
end

corpus_dir = ARGV[0].split("/").last
full_corpus_path = "../src/Corpora/corpora/#{corpus_dir}"

corpus_name = ARGV[1]

metadata_file =
    if ARGV.length == 3 then ARGV[2].split("/").last else "#{corpus_dir}_texts.tsv" end

the_module = corpus_dir.split('_').collect(&:capitalize).join

system("mkdir -p #{full_corpus_path}/Shared")
system("mkdir -p #{full_corpus_path}/Client")
system("mkdir -p #{full_corpus_path}/Server")

system("./create_metadata_categories.rb #{full_corpus_path}/#{metadata_file} #{the_module} > " +
       "#{full_corpus_path}/Shared/MetadataCategories.fs")

if !File.exists?("#{full_corpus_path}/Client/ZCore.fs")
        system("./create_client_code.rb #{the_module} > " +
               "#{full_corpus_path}/Client/ZCore.fs")
end

if !File.exists?("#{full_corpus_path}/Server/ZCore.fs")
        system("./create_server_code.rb #{corpus_dir} \"#{corpus_name}\" #{the_module} > " +
               "#{full_corpus_path}/Server/ZCore.fs")
end

system("./create_db_definition.rb #{full_corpus_path}/#{metadata_file} > " +
       "#{full_corpus_path}/create_db.sql")

system("./config_corpora.sh")

system("rm -f #{full_corpus_path}/#{corpus_dir}.sqlite")
system("sqlite3 #{full_corpus_path}/#{corpus_dir}.sqlite --init #{full_corpus_path}/create_db.sql")
