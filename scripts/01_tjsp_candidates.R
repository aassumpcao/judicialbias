### judicial favoritism of politicians
# section wrangling
#   this script wrangles electoral results data at the lowest-level possible
#   (the electoral section). the work here is necessary to recover vote counts
#   for politicians.
# author: andre assumpcao
# by andre.assumpcao@gmail.com

# testing
rm(list = ls())

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
load('data/vacancies.Rda')

### wrangle candidate datasets to match results
# filter candidates by election unit, year and candidate number
cities   <- candidatesSP %$% unique(election.ID)
people   <- candidatesSP %$% unique(candidate.number)
joinkey1 <- c('election.ID' = 'SIGLA_UE', 'candidate.ID',
              'election.stage' = 'NUM_TURNO', 'election.year' = 'ANO_ELEICAO')
joinkey2 <- c('election.ID' = 'SIGLA_UE', 'candidate.number' = 'NUM_VOTAVEL',
              'election.stage' = 'NUM_TURNO', 'election.year' = 'ANO_ELEICAO')

# prepare valid results dataset
results %<>%
  mutate(candidate.ID = SQ_CANDIDATO) %>%
  group_by(SIGLA_UE, NUM_TURNO, ANO_ELEICAO, candidate.ID) %>%
  summarize(votes = sum(TOTAL_VOTOS)) %>%
  ungroup() %>%
  mutate_all(as.character)

# prepare results-by-section dataset by filtering the relevant cities in which
# candidates ran for office
sections2004 %<>% filter(SIGLA_UE %in% cities)
sections2008 %<>% filter(SIGLA_UE %in% cities)
sections2012 %<>% filter(SIGLA_UE %in% cities)
sections2016 %<>% filter(SIGLA_UE %in% cities)

# bind datasets, and create votes variable
section <- bind_rows(sections2004, sections2008, sections2012, sections2016) %>%
  group_by(SIGLA_UE, NUM_TURNO, CODIGO_CARGO, ANO_ELEICAO) %>%
  mutate(election.votes = sum(QTDE_VOTOS)) %>%
  group_by(NUM_VOTAVEL, add = TRUE) %>%
  summarize(votes = sum(QTDE_VOTOS), election.votes = first(election.votes)) %>%
  ungroup() %>%
  mutate_all(as.character) %>%
  filter(!(NUM_VOTAVEL %in% c(95, 96, 97))) %>%
  mutate(candidate.votes = as.numeric(votes)) %>%
  arrange(SIGLA_UE, ANO_ELEICAO, CODIGO_CARGO, NUM_TURNO, desc(votes)) %>%
  group_by(SIGLA_UE, ANO_ELEICAO, CODIGO_CARGO, NUM_TURNO) %>%
  mutate(rank = row_number())

# join candidates and valid results
tseCandidates <- candidatesSP %>%
  mutate_all(as.character) %>%
  left_join(results, by = joinkey1) %>%
  mutate(votes = ifelse(is.na(votes) | votes == 0, NA_character_, votes)) %>%
  select(-candidate.plaintiff, -trial.outcome, -exp) %>%
  left_join(section, by = joinkey2) %>%
  filter(!is.na(votes.x) | !is.na(votes.y)) %>%
  mutate(votes = ifelse(is.na(votes.x), votes.y, votes.x)) %>%
  select(-votes.x, -CODIGO_CARGO, -votes.y)

# edit vacancies dataset before joining onto sections
vacancies %<>%
  mutate(election.ID = ifelse(ANO_ELEICAO == 2016, str_pad(SG_UE, 5, pad = '0'),
    SIGLA_UE), office.ID = ifelse(ANO_ELEICAO == 2016, CD_CARGO, CODIGO_CARGO),
    office.vacancies = ifelse(ANO_ELEICAO == 2016, QT_VAGAS, QTDE_VAGAS),
    election.year = ANO_ELEICAO
  ) %>%
  mutate_all(as.character)

# create joinkey3
joinkey3 <- c('election.ID', 'election.year', 'office.ID')

# create final dataset with SP candidates and their results
tseCandidates %<>%
  left_join(vacancies, by = joinkey3) %>%
  select(1:33, 58) %>%
  group_by(election.year, candidate.ssn) %>%
  filter(rank == max(rank)) %>%
  filter(n() == 1) %>%
  mutate(candidate.elected = ifelse(rank >= office.vacancies, 1, 0)) %>%
  select(-rank, -votes) %>%
  select(matches('^elect'), matches('^off'), matches('^candidate'),
    matches('^candidacy'), everything()
  )

# save dataset to file
save(tseCandidates, file = 'data/tseCandidates.Rda')

# remove everything for serial sourcing
rm(list = ls())
