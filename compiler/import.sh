#!/bin/sh

if [ -z $1 ]
then
    echo "Usage: sh import.sh path/to/ocaml-eliom/sources."
    exit 1
else
    OCAMLSRC=$1
fi

mkdir -p typing
cp $OCAMLSRC/typing/*.ml* typing
