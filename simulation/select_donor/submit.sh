#!/bin/bash

#$ -N synth_selectdonor
#$ -j y

Rscript simulation.R

# run:
# qsub -t 1-4 ./submit.sh
