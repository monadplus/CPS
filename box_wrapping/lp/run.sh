#!/usr/bin/env bash

for input in ../instances/*.inp; do
    output=$(basename $input .inp).out
    if timeout 60 ./p < $input > ../out/$output; then
      echo "ok!"
      ./checker < ../out/$output
    else
      echo "time out!"
      rm -f ../out/$output
    fi
done

