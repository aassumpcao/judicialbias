### judicial favoritism of politicians
# section wrangling
#   this script wrangles electoral results data at the lowest-level possible
#   (the electoral section). the work here is necessary to recover vote counts
#   for politicians.
# author: andre assumpcao
# by andre.assumpcao@gmail.com

### import statements
# import packages
library(tidyverse)
library(magrittr)

# load data
load('data/candidatesSP.Rda')
load('data/results.Rda')
load('data/sections2004.Rda')
load('data/sections2008.Rda')
load('data/sections2012.Rda')
load('data/sections2016.Rda')

### wrangle candidate datasets to match results
# filter candidates by election unit, year and candidate number
cities <- candidatesSP %$% unique(election.ID)
people <- candidatesSP %$% unique(candidate.number)
joinkey <- c('election.ID' = 'SIGLA_UE', 'candidate.ID',
             'election.stage' = 'NUM_TURNO', 'election.year' = 'ANO_ELEICAO')

# prepare valid results dataset
results %<>%
  mutate(candidate.ID = SQ_CANDIDATO) %>%
  group_by(SIGLA_UE, NUM_TURNO, ANO_ELEICAO, candidate.ID) %>%
  summarize(votes = sum(TOTAL_VOTOS)) %>%
  ungroup() %>%
  mutate_all(as.character)

# prepare results-by-section dataset by filtering the relevant cities in whic
# candidates ran for office
sections2004 %<>% filter(SIGLA_UE %in% cities)
sections2008 %<>% filter(SIGLA_UE %in% cities)
sections2012 %<>% filter(SIGLA_UE %in% cities)
sections2016 %<>% filter(SIGLA_UE %in% cities)

# bind datasets, and create votes variable
section <- bind_rows(sections2004, sections2008, sections2012, sections2016) %>%
  group_by(SIGLA_UE, NUM_TURNO, CODIGO_CARGO, ANO_ELEICAO, NUM_VOTAVEL) %>%
  summarize(votes = sum(QTDE_VOTOS)) %>%
  ungroup() %>%
  mutate_all(as.character)

# join candidates and valid results
tseCandidates <- candidatesSP %>%
  mutate_all(as.character) %>%
  left_join(results, by = joinkey) %>%
  mutate(votes = ifelse(is.na(votes) | votes == 0, NA_character_, votes)) %>%
  select(-candidate.plaintiff, -trial.outcome, -exp)

#
save(tseCandidates, file = 'data/tseCandidates.Rda')

# remove everything for serial sourcing
rm(list = ls())
