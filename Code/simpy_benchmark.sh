#!/bin/bash

for j in $(seq 2 5); do
	for i in `seq 1 100`; do
	    python3 python/simpy_script.py $j
	done
done

j=$((6))

for i in `seq 1 10`; do
	python3 python/simpy_script.py $j
done


