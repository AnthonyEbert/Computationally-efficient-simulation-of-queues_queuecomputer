#!/bin/bash

rm -f *.csv */*.csv *.pdf */*.pdf

## Run this bash script rather than executing the R and Python scripts directly. 

# Create departure times for 10^4 customer queue in simpy. See Python script for specifics of queue. 
python3 python/simpy_script.py 4 -v 

# Create benchmark times for simpy. 

./simpy_benchmark.sh >> output/raw_simpy_times.csv

# Validate and Benchmark simmer and queuecomputer. Create benchmark plot. 

Rscript R/code.R

pdftk Rplots.pdf cat 1 output theoretical.pdf
pdftk Rplots.pdf cat 2 output benchmark_083.pdf
pdftk Rplots.pdf cat 3 output hist_083.pdf
pdftk Rplots.pdf cat 4 output qlength_083.pdf
pdftk Rplots.pdf cat 5 output customers_083.pdf
pdftk Rplots.pdf cat 6 output ecdf_083.pdf
pdftk Rplots.pdf cat 7 output queuelength_dplyr.pdf
pdftk Rplots.pdf cat 8 output boxplot_dplyr.pdf

rm Rplots.pdf
cp *.pdf ../Figures
rm *.pdf



