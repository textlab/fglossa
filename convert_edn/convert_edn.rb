#!/usr/bin/env ruby

require 'edn'

File.open("/Users/noklesta_adm/prosjekter/lein/cglossa_cljs/resources/attributes/scandiasyn.edn") do |f|
  data = EDN.read(f)
  pos_menu = data.drop(1)

  pos_menu.each_with_index do |pos, pos_index|
    attr_value = pos[0]
    attr_name = pos[1]
    cats = pos[3].each_slice(2).to_a

    if cats.size > 0 then
        if pos_index == 0 then
            puts "        [ (pos,"
        else
            puts "          (pos,"
        end

        puts "           \"#{attr_value}\","
        puts "           \"#{attr_name}\","

        cats.each_with_index do |cat, cat_index|
            cat_name = cat[0]
            cat_values = cat[1]
            if cat_values.size == 1 then
                cva, cat_value_value, cat_value_text = cat_values[0]
                cat_value_attr_name = if cva == :type then "``type``" else cva end
                if cat_index == 0 then
                    puts "           [ (\"#{cat_name}\", [ (#{cat_value_attr_name}, \"#{cat_value_value}\", \"#{cat_value_text}\") ]) ])"
                else
                    puts "             (\"#{cat_name}\", [ (#{cat_value_attr_name}, \"#{cat_value_value}\", \"#{cat_value_text}\") ]) ])"
                end
            else
                if cat_index == 0 then
                    puts "           [ (\"#{cat_name}\","
                else
                    puts "             (\"#{cat_name}\","
                end
                cat_values.each_with_index do |cat_value, cat_value_index|
                    cva, cat_value_value, cat_value_text = cat_value
                    cat_value_attr_name = if cva == :type then "``type``" else cva end

                    if cat_value_index == 0 then
                        puts "              [ (#{cat_value_attr_name}, \"#{cat_value_value}\", \"#{cat_value_text}\")"
                    elsif cat_value_index == cat_values.size - 1 then
                        if cat_index == cats.size - 1 then
                            puts "                (#{cat_value_attr_name}, \"#{cat_value_value}\", \"#{cat_value_text}\") ]) ])"
                        else
                            puts "                (#{cat_value_attr_name}, \"#{cat_value_value}\", \"#{cat_value_text}\") ])"
                        end
                    else
                        puts "                (#{cat_value_attr_name}, \"#{cat_value_value}\", \"#{cat_value_text}\")"
                    end
                end
            end
        end
    else
        if pos_index == pos_menu.size - 1 then
            puts "          (pos, \"#{attr_value}\", \"#{attr_name}\", []) ]"
        else
            puts "          (pos, \"#{attr_value}\", \"#{attr_name}\", [])"
        end
    end
  end
end
