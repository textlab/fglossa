#!/bin/bash

# Usage: util/multisort.sh file [sort args]

num_cores=8

set -e
file="$1"
shift
lines=`wc -l "$file" |awk '{print $1}'`
if [ "$lines" -lt 128000 ]; then
  exec sort -t $'\t' "$@" "$file"
  exit
fi
split_lines=$[lines/num_cores+1]
id="${file}_${RANDOM}"
split -l $split_lines "$file" "${id}_split_"
for f in "${id}_split_"*; do
  sort -t $'\t' "$@" "$f" >"${f//_split_/_sorted_}" &
done
wait `jobs -p`
rm -f "${id}_split_"*
sort -t $'\t' "$@" -m "${id}_sorted_"*
rm -f "${id}_sorted_"*
