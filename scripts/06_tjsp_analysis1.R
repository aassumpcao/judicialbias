### judicial favoritism of politicians
#  this script produces consistent datasets for analysis
# andre assumpcao and julio trecenti
# email: andre.assumpcao@gmail.com
# email: julio.trecenti@gmail.com

### data and library calls
# import libraries
library(tidyverse)
library(magrittr)

# load data
tjspLitigantsRandom <- read_csv('data/tjspLitigantsRandom.csv')
load('data/tjspSentences.Rda')
load('data/tjspAnalysis.Rda')
load('data/tjspAnalysisRandom.Rda')

# only claimant == individual and defendant == company
clean_name <- function(x) {
  x %>%
    str_remove_all('[[:punct:]]') %>%
    str_squish() %>%
    return()
}

# check for individual vs. business
is_person <- function(x) {
  regex <- paste0(
    '^(prefei|fazen|banc|telefon|santa|companhia|munic|sindicato|camara|',
    'strategic|sucral|bv|instituto|vivo|claro|cia|elektro|facebook|tim|caixa|',
    'unimed|sky|magazine|omni|lojas|panamericano|mrv|associacao|spprev|itau|',
    'detran|gol|itauto|qualicorp|volkswagen|azul|servico|telecomunicacoes|',
    'centro|departmento|mastercard|comercial|imobiliaria|nextel|samsung|',
    'electrolux|ativos|construtora|jornal|mcm|atlantico|b2w|carreira|casas|',
    'fundo|iamspe|tam|brookfield|cardif|casa|cetro|cifra|estado|emais|evendas|',
    'fundacao|porto|cooperativa|facebookcom|engenharia|industria|losango|',
    'carrefour|embratel|hsbc|luizacred|mercado|via|ympactus)'
  )
  re_comp <- regex(regex, ignore_case = TRUE)
  !str_detect(x, re_comp) %>% return()
}

# compute type of litigant for each dataset
politician_pj <- tjspSentences %>%
  mutate_at(vars(claimant, defendant), clean_name) %>%
  mutate_at(vars(claimant, defendant), list(pe = is_person)) %>%
  select(case.ID = caseID, claimant_pe, defendant_pe) %>%
  right_join(tjspAnalysis, by = 'case.ID') %>%
  mutate(
    case.claim = str_remove_all(case.claim, '\\.|R\\$') %>%
                 str_replace_all(',', '.') %>%
                 as.numeric()
  )
random_pj <- tjspLitigantsRandom %>%
  mutate_at(vars(claimant, defendant), clean_name) %>%
  mutate_at(vars(claimant, defendant), list(pe = is_person)) %>%
  select(case.ID = id, claimant_pe, defendant_pe) %>%
  right_join(tjspAnalysisRandom, by = 'case.ID') %>%
  group_by(case.ID) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(
    case.claim = str_remove_all(case.claim, '\\.|R\\$') %>%
                 str_replace_all(',', '.') %>%
                 as.numeric()
  )

# check frequency for defendant companies
table1 <- politician_pj %>%
  filter(!is.na(case.claim) & !is.na(defendant_pe)) %>%
  mutate(defendant_pe = as.integer(defendant_pe)) %>%
  {table(.$defendant_pe) / nrow(.)}
table2 <- random_pj %>%
  filter(!is.na(case.claim) & !is.na(defendant_pe)) %>%
  mutate(defendant_pe = as.integer(defendant_pe)) %>%
  {table(.$defendant_pe) / nrow(.)}

# examine correlation case claim and defendant type
bind_rows(
  politician_pj %>%
    filter(!is.na(case.claim) & !is.na(defendant_pe)) %>%
    mutate(defendant_pe = as.integer(defendant_pe)),
  random_pj %>%
    filter(!is.na(case.claim) & !is.na(defendant_pe)) %>%
    mutate(defendant_pe = as.integer(defendant_pe))
) %$%
cor(log(case.claim), defendant_pe) -> correlation

# create different datasets for individual (pf) vs businesses
pf_vs_pj_random <- tjspLitigantsRandom %>%
  mutate_at(vars(claimant, defendant), clean_name) %>%
  mutate_at(vars(claimant, defendant), list(pe = is_person)) %>%
  filter(claimant_pe, !defendant_pe) %>%
  select(case.ID = id, claimant)

pf_vs_pj_politician <- tjspSentences %>%
  mutate_at(vars(claimant, defendant), clean_name) %>%
  mutate_at(vars(claimant, defendant), list(pe = is_person)) %>%
  filter(claimant_pe, !defendant_pe) %>%
  select(case.ID = caseID, claimant)

tjspRandom_pf_vs_pj <- tjspAnalysisRandom %>%
  mutate(origin = 'random') %>%
  mutate_if(is.factor, as.character) %>%
  semi_join(pf_vs_pj_random, 'case.ID')

tjspPolitician_pf_vs_pj <- tjspAnalysis %>%
  filter(candidate.litigant.type == 'Claimant') %>%
  mutate(origin = 'politician') %>%
  mutate_if(is.factor, as.character) %>%
  semi_join(pf_vs_pj_politician, 'case.ID')

# save datasets
save(tjspRandom_pf_vs_pj, file = 'data/tjspRandom_pf_vs_pj.Rda')
save(tjspPolitician_pf_vs_pj, file = 'data/tjspPolitician_pf_vs_pj.Rda')

# remove everything for serial sourcing
rm(list = ls())


