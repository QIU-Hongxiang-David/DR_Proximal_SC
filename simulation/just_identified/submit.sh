#!/bin/bash

#$ -N synth
#$ -j y

Rscript simulation.R

# run:
# qsub -t 1-16 ./submit.sh
