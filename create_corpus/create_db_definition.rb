#!/usr/bin/env ruby

if ARGV.length < 1
    STDERR.puts "Usage: create_db_definition.rb [METADATA-TSV-FILE]"
    exit
end

# Read the categories from the first line, but drop 'tid' (which should be the first one),
# since we will treat it differently from the other categories
cats = File.readlines(ARGV[0])[0].split.drop(1)

puts "CREATE TABLE texts (
    tid VARCHAR NOT NULL,
"

non_position_cats = cats.reject { |cat| cat.match?(/^(?:startpos|endpos|bounds)\$?/) }

non_position_cats.each do |cat|
    cat_code, cat_type = cat.split('$')
    datatype = if cat_type && cat_type.start_with?('n') then "INT" else "VARCHAR" end
    puts "    #{cat_code} #{datatype},"
end

if cats.include?("bounds")
    puts "    bounds VARCHAR NOT NULL"
else
    puts "    startpos UNSIGNED INT NOT NULL,\n    endpos UNSIGNED INT NOT NULL"
end

puts ");"
puts
puts "CREATE INDEX idx_texts_tid ON texts(tid);"

non_position_cats.each do |cat|
    cat_code = cat.split('$')[0]
    puts "CREATE INDEX idx_texts_#{cat_code} ON texts(#{cat_code});"
end

puts

non_position_cats.each do |cat|
    cat_code = cat.split('$')[0]
    puts "CREATE INDEX idx_texts_#{cat_code}_tid ON texts(#{cat_code}, tid);"
end
