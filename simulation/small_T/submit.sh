#!/bin/bash

#$ -N synth_smallT
#$ -j y
#$ -tc 32

Rscript simulation.R

# run:
# qsub -t 1-2 ./submit.sh
