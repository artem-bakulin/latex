#!/bin/bash

make4ht -u -c simple -e simple $1
 
filename=$(basename -- "$1")
extension="${filename##*.}"
filename="${filename%.*}"

cat "${filename}.html" | tr -s '\n' ' ' > "${filename}_clean.html"
sed -i -e "s/<\\/p> <p>/<\\/p>\n\n<p>/g" "${filename}_clean.html"
sed -i -e "s/<p> /<p>/g" "${filename}_clean.html"
sed -i -e "s/ <\\/p>/<\\/p>/g" "${filename}_clean.html"
sed -i -e "s/ )/)/g" "${filename}_clean.html"
sed -i -e "s/thinspace/\\&#8239;/g" "${filename}_clean.html"
