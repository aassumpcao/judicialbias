### judicial favoritism of politicians
# this script wrangles sentences data
# author: andre assumpcao
# by andre.assumpcao@gmail.com

### import statements
# import packages
library(tidyverse)
library(magrittr)

# load data
sctDetails <- read_csv('data/sctDetails_random.csv') %>% select(-1)
sctSummary <- read_csv('data/sctSummary_random.csv')
sctLitigants <- read_csv('data/sctLitigants_random.csv')

# merge litigants and
### identify case rulings for each politician in dataset
# define row numbers to rows to keep in details dataset
keep.rows  <- sctDetails$Processo %in% sctSummary$Processo
sctDetails <- sctDetails[keep.rows,]

# define keywords determining whether judge ruled in favor of claimant or
# defendant
convict <- 'provi(são|men)+|proced[êe]n|def[ei]r|conced[oi](d)?|conden|f[áa]vor'
acquit  <- '(negar provimento)|(improcede)|(não conced)|(indef[ei]r)'
dismiss <- 'extint[oa]'
agree   <- 'ac[óo]rd(o|ão)?.*(partes|firmado)?|conciliação'

# create variables based on keyword search
sctDetails$claimant.win  <- str_detect(sctDetails$values, regex(convict, TRUE))
sctDetails$claimant.loss <- str_detect(sctDetails$values, regex(acquit, TRUE))

# reformat court outcome
sctDetails %<>%
  mutate(claimant.win = ifelse(claimant.win, 1, NA_character_)) %>%
  mutate(claimant.loss = ifelse(claimant.loss, 1, NA_character_)) %>%
  group_by(Processo) %>%
  filter(!is.na(claimant.win) | !is.na(claimant.loss)) %>%
  mutate(claimant.win = ifelse(!is.na(claimant.loss), 0, 1)) %>%
  ungroup()

# join sentence summary with outcomes
tjspSentences <- sctSummary %>%
  left_join(sctDetails, 'Processo') %>%
  select(-claimant.loss) %>%
  group_by(Processo) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  filter(!is.na(claimant.win))

# work to turn litigants table to tidy format (one litigant per line)
sctLitigants %<>%
  select(id = Processo, 1:4) %>%
  mutate(claimant  = str_split(claimant,  ';')) %>% unnest(claimant)  %>%
  mutate(defendant = str_split(defendant, ';')) %>% unnest(defendant) %>%
  mutate(clawyers  = str_split(clawyers,  ';')) %>% unnest(clawyers)  %>%
  mutate(dlawyers  = str_split(dlawyers,  ';')) %>% unnest(dlawyers)  %>%
  distinct(.keep_all = TRUE) %>%
  mutate_all(str_to_lower) %>%
  mutate_all(str_squish) %>%
  mutate_at(vars(2:5), ~stringi::stri_trans_general(., 'Latin-ASCII')) %>%
  mutate_at(vars(2:5), ~str_replace(., ' .*', ''))

# create final dataset
tjspLitigants <- sctLitigants

# create variables
tjspLitigants$claimant.sex  <- genderBR::get_gender(tjspLitigants$claimant)
tjspLitigants$defendant.sex <- genderBR::get_gender(tjspLitigants$defendant)
tjspLitigants$clawyers.sex  <- genderBR::get_gender(tjspLitigants$clawyers)
tjspLitigants$dlawyers.sex  <- genderBR::get_gender(tjspLitigants$dlawyers)

# fill in information for companies or groups
tjspLitigants %<>% mutate_at(vars(6:7), ~ifelse(is.na(.), 'Other', .))

# save to file
write_csv(tjspSentences, path = 'data/tjspSentencesRandom.csv')
write_csv(tjspLitigants, path = 'data/tjspLitigantsRandom.csv')

# remove everything for serial sourcing
rm(list = ls())
