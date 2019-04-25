#!/bin/bash

source bash_env.sh

declare -a results

for g in EEE; do
    for len in 200 400; do
        echo
        echo "# ------------------------------------------------------------"
        echo "# Grammar $g, length $len"
        echo "# ------------------------------------------------------------"
        for parser in simple unstaged; do
            ch=1;
            time $run_main $parser $g :${ch}x${len}; 
        done
    done
done


for g in aho_s aho_sml; do
    for len in 500; do
        echo
        echo "# ------------------------------------------------------------"
        echo "# Grammar $g, length $len"
        echo "# ------------------------------------------------------------"
        for parser in simple unstaged; do
            ch=x;
            time $run_main $parser $g :${ch}x${len}; 
        done
    done
done


time $run_main actions :1x400
