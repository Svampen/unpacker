#!/bin/ksh

# Unpack files
function unpack {
   [[ "$1" == "" || "$2" == "" ]] && return 1
   find $1 -type f -name *.rar ! -name *part[0-9][0,2-9].rar ! -name *part[1-9][0-9].rar | while read rarfile; do
       echo $rarfile
       unrar lb $rarfile | while read file; do
           echo "File: $file"
           if [[ -f $2/$file ]]; then
              echo "$file already exists at $2"
           fi
       done
       mkdir -p $2
       unrar e -o- $rarfile $2
   done
}
