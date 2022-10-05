#!/bin/bash

#$ -N synth
#$ -j y

Rscript simulation.R

# run:
# qsub -t 1-5 ./submit.sh
