#!/bin/bash

set -e

name="$1"
Name=$(echo "$name" | sed -e 's/^./\U&/g')
year=$(date +%Y)
for f in *
do
    if [[ -f "$f" ]]; then
        sed -i -e "s/<name>/$name/; s/<Name>/$Name/; s/<year>/$year/" "$f"
    fi
done
mv "src/<name>.ml" "src/$name.ml"
oasis setup
rm init.sh
