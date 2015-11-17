#!/bin/bash

set -e

name="$1"
Name=$(echo "$name" | sed -e 's/^./\U&/g')
year=$(date +%Y)
for f in *
do
    if [[ -f "$f" ]]; then
        sed -i -e "s/eliom-lang/$name/; s/Eliom-lang/$Name/; s/2015/$year/" "$f"
    fi
done
mv "src/eliom-lang.ml" "src/$name.ml"
oasis setup
rm init.sh
