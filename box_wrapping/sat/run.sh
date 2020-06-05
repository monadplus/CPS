#!/usr/bin/env bash

P="cabal v1-run"

for input in ../instances/*.inp; do
    output=$(basename $input .inp).out
    if timeout 60 $P < $input > ../out/$output; then
      echo "ok!"
      ./checker < ../out/$output
    else
      echo "time out!"
      rm -f ../out/$output
    fi
done
