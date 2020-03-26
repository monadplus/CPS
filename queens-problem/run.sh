#!/usr/bin/env bash

if [ -z "$1" ]
  then
    echo "No argument supplied"
else
  for n in $(seq 4 200); do
      echo "n:$n"
      time ./queens-$1 $n # ^ < $n if n is read from stdin
  done
fi
