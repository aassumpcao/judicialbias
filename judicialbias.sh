#!/bin/bash

#SBATCH -p general
#SBATCH -N 11
#SBATCH -t 1-
#SBATCH --mail-user=andre.assumpcao@gmail.com
#SBATCH --mail-type=ALL

python3.7 00_tjsp_scraper.py
