#!/usr/bin/env ruby

# To convert metadata from the cglossa version of Glossa to the fglossa version,
# first export the tables from the cglossa_xxx database for the corpus to files:
#
# select id, startpos, endpos from text into outfile '/tmp/texts.txt';
# select id, metadata_category_id, text_value from metadata_value into outfile '/tmp/values.txt';
# select metadata_value_id, text_id from metadata_value_text into outfile '/tmp/tmp.txt';
#
# $ sort -u /tmp/tmp.txt  > /tmp/values_texts.txt
#
# Get num_categories by running "select count(*) from metadata_category"
#
# Use csvkit to create the database (with the correct data types based of analysis of the
# data file) and import the data:
# csvsql -t --tables texts --db sqlite:///mycorpus.sqlite --insert mycorpus_texts.tsv
#
# To determine the column types:
# csvstat -t --type mycorpus_texts.tsv

def cleanup_column(column)
    if column.nil?
        ""
    else
        column.strip
    end
end

num_categories = 18

text_lines = File.readlines("/tmp/texts.txt")
metadata_value_lines = File.readlines("/tmp/values.txt")
values_texts_lines = File.readlines("/tmp/values_texts.txt")

# Create a row for each text with the number of metadata columns plus two for startpos and endpos
texts = Hash.new { |hash, key| hash[key] = Array.new(num_categories + 2) }
metadata_values = {}

text_lines.each do |line|
    text_id, startpos, endpos = line.split("\t").map { |column| cleanup_column column }
    texts[text_id][num_categories] = startpos
    texts[text_id][num_categories + 1] = endpos
end

metadata_value_lines.each do |line|
    metadata_value_id, metadata_category_id, text_value = line.split("\t").map { |column| cleanup_column column }
    metadata_values[metadata_value_id] =
        { :column_index => metadata_category_id.to_i - 1, :text_value => text_value }
end

values_texts_lines.each do |line|
    metadata_value_id, text_id = line.split("\t").map { |column| cleanup_column column }

    column_index = metadata_values[metadata_value_id][:column_index]
    # if !texts[text_id][column_index].nil?
    #     puts "Metadata already exists in column, when processing line: #{line}"
    #     exit
    # end
    texts[text_id][column_index] = metadata_values[metadata_value_id][:text_value]
end

texts.each_pair do |key, value|
    columns = value.map { |column| cleanup_column column }
    puts columns.join("\t")
end