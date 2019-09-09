### judicial favoritism of politicians
# this script wrangles candidate data
# author: andre assumpcao
# by andre.assumpcao@gmail.com

### import statements
# import packages
library(tidyverse)
library(magrittr)

# load data
load('data/candidatesSP.Rda')
load('data/results.Rda')
load('data/sections.Rda')
load('data/vacancies.Rda')

### wrangle candidate datasets to match results
# filter candidates by election unit, year and candidate number
joinkey1 <- c('election.ID' = 'SIGLA_UE', 'candidate.number' = 'NUMERO_CAND',
              'election.year' = 'ANO_ELEICAO')
joinkey2 <- c('election.ID' = 'SIGLA_UE', 'candidate.number' = 'NUM_VOTAVEL',
              'election.year' = 'ANO_ELEICAO')

# edit results to match information on candidatesSP set
results %<>%
  filter(NUM_TURNO == 1) %>%
  mutate_all(as.character)
sections %<>%
  filter(NUM_TURNO == 1 & !(NUM_VOTAVEL %in% c(95, 96, 97))) %>%
  mutate_all(as.character) %>%
  group_by(ANO_ELEICAO, SIGLA_UE, CODIGO_CARGO) %>%
  arrange(ANO_ELEICAO, SIGLA_UE, CODIGO_CARGO, desc(as.integer(voto.secao))) %>%
  mutate(rank = row_number()) %>%
  ungroup()

# join candidates and valid results
tseCandidates <- candidatesSP %>%
  mutate_all(as.character) %>%
  left_join(results, by = joinkey1) %>%
  select(-candidate.plaintiff, -trial.outcome, -exp) %>%
  left_join(sections, by = joinkey2) %>%
  filter(!is.na(voto.municipio) | !is.na(voto.secao)) %>%
  mutate(votes = ifelse(is.na(voto.municipio), voto.secao, voto.municipio)) %>%
  select(-matches('\\.[xy]$'))

# edit vacancies dataset before joining onto sections
vacancies %<>%
  mutate(
    election.ID = ifelse(ANO_ELEICAO == 2016, str_pad(SG_UE, 5, pad = '0'),
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
  ungroup() %>%
  mutate_at(vars(votes, office.vacancies), as.integer) %>%
  mutate(
    total.votes = votes, election.votes = case_when(
      office.ID == 11 ~ floor(votes / 2),
      office.ID == 13 ~ floor(votes / office.vacancies)
    )
  ) %>%
  mutate(candidate.elected = ifelse(votes >= election.votes, 1, 0)) %>%
  select(
    matches('^elect|^tot'), matches('^off'), matches('^candidate'),
    matches('^candidacy'), everything(), -rank
  ) %>%
  mutate_all(as.character)

# save dataset to file
write_csv(tseCandidates, 'data/tseCandidates.csv')
save(tseCandidates, file = 'data/tseCandidates.Rda')

# remove everything for serial sourcing
rm(list = ls())
