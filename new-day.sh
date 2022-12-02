#!/bin/sh

set -e

# Identify the next day.
currentMax=$(find . -type d -name "day*" | sort -r | egrep -o "[0-9]+" | head -n 1)
nextDayNo=$(echo "$currentMax" + 1 | bc)
echo "Starting day $nextDayNo"
cp -r _tpl/ "day$nextDayNo"

# Format the Makefile.
sed -i "" "s/__day__/$nextDayNo/g" "day$nextDayNo/Makefile"
