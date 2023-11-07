#!/bin/bash

#$ -N synth_detrend
#$ -j y
#$ -tc 32

Rscript simulation.R

# run:
# qsub -t 1-4 ./submit.sh
