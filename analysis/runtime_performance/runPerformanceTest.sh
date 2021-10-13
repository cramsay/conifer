#!/bin/bash
for b in {12..27}
do
   for n in 1 5 10 20 50 100 
   do
      /usr/bin/time -f "%C, %E, %M" ./TestAdderCount $n $b > /dev/null
   done
done
