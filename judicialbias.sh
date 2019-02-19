#!/bin/bash

#SBATCH -p general
#SBATCH -N 10
#SBATCH --mem=8g
#SBATCH -n 24
#SBATCH -t 1-
#SBATCH --mail-user=andre.assumpcao@gmail.com
#SBATCH --mail-type=ALL

R CMD BATCH --no-save judicialbias.R
