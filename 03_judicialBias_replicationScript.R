### judicial favoritism of politicians: analysis script
# to be updated
# by andre.assumpcao

# import statements
library(here)
library(tidyverse)
library(magrittr)
library(readxl)
library(AER)
library(stargazer)
library(lfe)
library(extrafont)

# load rproj (comment out if using another R IDE)
rstudioapi::openProject('2019 Judicial Bias.Rproj')

# wrangle candidate data
source('00_judicialBias_candidates.R')

