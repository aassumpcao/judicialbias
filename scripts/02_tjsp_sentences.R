### judicial favoritism of politicians
# this script wrangles sentences data
# author: andre assumpcao
# by andre.assumpcao@gmail.com

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
sctDetails <- sctDetails[keep.rows,]

# # extract only judicial sentences in each case
# sentences <- which(str_detect(sctDetails$values, '^Concluso'))
# sentences  <- c(sentences - 1) %>% sort()
# sctDetails <- sctDetails[sentences - 1,]

# define keywords determining whether judge ruled in favor of claimant or
# defendant
convict <- 'provi(são|men)+|proced[êe]n|def[ei]r|conced[oi](d)?|conden|f[áa]vor'
acquit  <- '(negar provimento)|(improcede)|(não conced)|(indef[ei]r)'
dismiss <- 'extint[oa]'
agree   <- 'ac[óo]rd(o|ão)?.*(partes|firmado)?|conciliação'

# create variables based on keyword search
sctDetails$claimant.win  <- str_detect(sctDetails$values, regex(convict, TRUE))
sctDetails$claimant.loss <- str_detect(sctDetails$values, regex(acquit, TRUE))
# sctDetails$case.dismiss <- str_detect(sctDetails$values, regex(dismiss, TRUE))
# sctDetails$agreement    <- str_detect(sctDetails$values, regex(agree, TRUE))

# reformat court outcome
sctDetails %<>%
  mutate(claimant.win = ifelse(claimant.win, 1, NA_character_)) %>%
  mutate(claimant.loss = ifelse(claimant.loss, 1, NA_character_)) %>%
  group_by(caseID) %>%
  filter(!is.na(claimant.win) | !is.na(claimant.loss)) %>%
  mutate(claimant.win = ifelse(!is.na(claimant.loss), 0, 1)) %>%
  ungroup()

# join sentence summary with outcomes
tjspSentences <- tjspSummary %>%
  left_join(sctDetails, 'caseID') %>%
  select(-claimant.loss) %>%
  group_by(caseID) %>%
  filter(row_number() == 1) %>%
  ungroup()

# save to file
save(tjspSentences, file = 'data/tjspSentences.Rda')

# remove everything for serial sourcing
rm(list = ls())
