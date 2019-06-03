### judicial favoritism of politicians
# this script prepares data for analysis and matches candidates across datasets
# author: andre assumpcao
# by andre.assumpcao@gmail.com

# testing
rm(list = ls())

### import statements
# import packages
library(tidyverse)
library(magrittr)

# load data
load('data/tseCandidates.Rda')
load('data/sctDetails.Rda')
load('data/sctSummary.Rda')

### find litigant type for each politician in the dataset
# isolate and reshape litigants data
sctSummary %<>% mutate(id = as.character(row_number()))
levels <- c('Claimant', 'Defendant', 'Claimant Lawyer', 'Defendant Lawyer')

# create search object
search.object <- sctSummary %>%
  select(id, candidateID, 9:12) %>%
  mutate(claimant  = str_split(claimant,  ';')) %>% unnest(claimant)  %>%
  mutate(defendant = str_split(defendant, ';')) %>% unnest(defendant) %>%
  mutate(clawyers  = str_split(clawyers,  ';')) %>% unnest(clawyers)  %>%
  mutate(dlawyers  = str_split(dlawyers,  ';')) %>% unnest(dlawyers)  %>%
  distinct(.keep_all = TRUE) %>%
  mutate_all(str_to_lower)

# use search object to find what type of litigants politicians were
match.object <- tseCandidates %>%
  ungroup() %>%
  select(scraper.id, candidate.name) %>%
  mutate_all(str_to_lower) %>%
  {left_join(search.object, ., by = c('candidateID' = 'scraper.id'))}

# create vectors of names to be used in stringdist match
claimants  <- unlist(match.object[, 3]) %>% unname()
defendants <- unlist(match.object[, 4]) %>% unname()
clawyers   <- unlist(match.object[, 5]) %>% unname()
dlawyers   <- unlist(match.object[, 6]) %>% unname()
politician <- unlist(match.object[, 7]) %>% unname()

# match anyone whose levenshtein distance < 6
claimant  <- which(stringdist::stringdist(politician, claimants,  'lv') <= 5)
defendant <- which(stringdist::stringdist(politician, defendants, 'lv') <= 5)
clawyer   <- which(stringdist::stringdist(politician, clawyers,   'lv') <= 5)
dlawyer   <- which(stringdist::stringdist(politician, dlawyers,   'lv') <= 5)

# bind results onto match.object
match.object[claimant, 'candidate.claimant'] <- 1
match.object[defendant, 'candidate.defendant'] <- 1
match.object[clawyer, 'candidate.clawyer'] <- 1
match.object[dlawyer, 'candidate.dlawyer'] <- 1

# create variable capturing litigant type
match.object %<>%
  select(-c(3:7)) %>%
  filter_at(vars(3:6), any_vars(. == 1)) %>%
  mutate(litigant.type = case_when(candidate.claimant == 1 ~ 1,
    candidate.defendant == 1 ~ 2, candidate.clawyer == 1 ~ 3,
    candidate.dlawyer == 1 ~ 4)
  )

# joint litigant type back to sct dataset
tjspSummary <- sctSummary %>%
  left_join(match.object, 'id') %>%
  distinct(caseID, .keep_all = TRUE) %>%
  filter(!is.na(litigant.type)) %>%
  select(-c(14:18)) %>%
  mutate(litigant.type = factor(litigant.type, labels = levels))

# remove useless objects
rm(list = objects(pattern = 'object|cl|dl|politician|def|levels'))

### identify case rulings for each politician in dataset
# define row numbers to rows to keep in details dataset
keep.rows  <- sctDetails$caseID %in% sctSummary$caseID
sctDetails %<>% {.[keep.rows,]}

# extract only judicial sentences in each case
sentences  <- which(str_detect(sctDetails$values, '^Concluso'))
sentences  <- c(sentences - 1, sentences) %>% sort()
sctDetails %<>% {.[sentences,]}

# add space before capital letters
sctDetails %>%
  mutate(values = str_trim(str_replace_all(values,'(\\w)([A-Z])','\\1 \\2')))%>%
  mutate(values = str_squish(str_replace_all(values, '(\\.)', '\\1 ')))
