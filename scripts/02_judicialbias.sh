#!/bin/bash
#SBATCG -p general
#SBATCH -N 1
#SBATCH --mem=0
#SBATCH -n 16
#SBATCH -t 1-
#SBATCH --mail-user=andre.assumpcao@gmail.com
#SBATCH --mail-type=ALL

R CMD BATCH --no-save scripts/06_tjsp_analysis1.R
