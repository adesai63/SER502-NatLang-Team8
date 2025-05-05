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

set +e
output=$(swipl -q -s evaluator.pl \
  -g "consult(parser),
      open('$token_file', read, Stream),
      read(Stream, Tokens),
      close(Stream),
      run_program(Tokens),
      halt." 2>&1)
gRC=$?
set -e

if [ "$gRC" -ne 0 ]; then
  echo "Syntax error, try again"
  exit 1
fi

if [ -z "$output" ]; then
  echo "Syntax error, try again"
else
  echo "$output"
fi