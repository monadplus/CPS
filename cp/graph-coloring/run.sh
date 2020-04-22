#!/usr/bin/env bash

TIME_LIMIT=15 # In secs.
BENCH_DIR=instances
EXE=graph-coloring

######## DO NOT CHANGE BELOW THIS LINE ########

TIMED_OUT=124

n_solved=0
n_all_pb=0
total_time=0
for ifile in $BENCH_DIR/*.pb; do

    /usr/bin/time --format "%e\n%x" --output=out.txt  \
		  timeout $TIME_LIMIT                  \
		  $EXE $ifile &> /dev/null
    time=$(head -n 1 out.txt)
    code=$(tail -n 1 out.txt)
    if [ "$code" = $TIMED_OUT ]; then
	echo "Timed out: $ifile"
    else
	echo "OK       : $ifile"
	n_solved=$(( n_solved+1 ))
	total_time=$(echo "$total_time + $time" | bc)
    fi
    n_all_pb=$(( n_all_pb+1 ))
done
echo "Time solved problems: $total_time"
echo "Num. solved problems: $n_solved"
echo "Num.  all   problems: $n_all_pb"
perc=$(echo "scale=2; (100*$n_solved)/$n_all_pb" | bc )
echo "Percentage: $perc %"
rm --force out.txt
