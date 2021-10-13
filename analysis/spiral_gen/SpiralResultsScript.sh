#!/bin/bash

F=./results.csv
n=100

echo "" > $F

for i in {1..100}; do
    #./synth -ac1 20min.chains -r $n $i -b 12 | tr -s ' ' | sed 's/^ //' | cut -d' ' -f1,3,5 >> $F;
    ./synth -iac1 20min.chains -r $n $i -b 12 | tr -s ' ' | sed 's/^ //' | cut -d' ' -f1,3,5 >> $F;
    #./synth -bhm -r $n $i -b 12 | tr -s ' ' | sed 's/^ //' | cut -d' ' -f1,3,5 >> $F;
done

