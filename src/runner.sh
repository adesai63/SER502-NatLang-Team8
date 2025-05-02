#!/bin/bash

set -e

if [ "$#" -ne 1 ]; then
  echo "Usage: ./runner.sh <path-to-source-file>"
  exit 1
fi


python3 tokenizer.py "$1"

filename=$(basename -- "$1")
filename_noext="${filename%.*}"
token_file="tokens/${filename_noext}.txt"

swipl -q -s evaluator.pl -g "consult(parser), open('$token_file', read, Stream), read(Stream, Tokens), close(Stream), run_program(Tokens), halt."