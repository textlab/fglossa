#!/usr/bin/env ruby

require 'edn'

File.open("/Users/noklesta_adm/prosjekter/lein/cglossa_cljs/resources/attributes/scandiasyn.edn") do |f|
  data = EDN.read(f)
  pos_menu = data.drop(1)

  pos_menu.each_with_index do |pos, pos_index|
    attr_value = pos[0]
    attr_name = pos[1]
    cats = pos[3].each_slice(2).to_a

    if pos_index == 0 then
        puts "        [ (pos,"
    else
        puts "          (pos,"
    end

    puts "           \"#{attr_value}\","
    puts "           \"#{attr_name}\","

    cats.each_with_index do |cat, cat_index|
        cat_name = cat[0]
        if cat_index == 0 then
            puts "           [ (\"#{cat_name}\","
        else
            puts "             (\"#{cat_name}\","
        end
        cat_values = cat[1]
        cat_values.each_with_index do |cat_value, cat_value_index|
            cat_value_name, cat_value_value, cat_value_text = cat_value
            if cat_value_index == 0 then
                puts "              [ (#{cat_value_name}, \"#{cat_value_value}\", \"#{cat_value_text}\")"
            elsif cat_value_index == cat_values.size - 1 then
                puts "                (#{cat_value_name}, \"#{cat_value_value}\", \"#{cat_value_text}\") ])"
            else
                puts "                (#{cat_value_name}, \"#{cat_value_value}\", \"#{cat_value_text}\")"
            end
        end
    end
  end
end
