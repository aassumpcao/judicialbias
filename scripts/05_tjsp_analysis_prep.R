### judicial favoritism of politicians
# this script prepares all data for analysis
# author: andre assumpcao
# by andre.assumpcao@gmail.com

### import statements
# import packages
library(tidyverse)
library(magrittr)

# load data
load('data/campaign.Rda')
load('data/tjspJudges.Rda')
load('data/tjspMun.Rda')
load('data/tjspSentences.Rda')
load('data/tseCandidates.Rda')
load('data/sections.Rda')
tjspSentencesRandom <- read_csv('data/tjspSentencesRandom.csv')
tjspLitigantsRandom <- read_csv('data/tjspLitigantsRandom.csv')

### wrangle datasets
# organize sections dataset so that we know who's been elected
sections %<>%
  group_by(ANO_ELEICAO, SIGLA_UE, NUM_TURNO, CODIGO_CARGO) %>%
  mutate(voto.total = sum(voto.secao)) %>%
  arrange(SIGLA_UE, ANO_ELEICAO, CODIGO_CARGO, desc(voto.secao)) %>%
  mutate(
    candidate.elect = ifelse(CODIGO_CARGO == 11 & row_number() == 1, 1, 0),
    candidate.rank = ifelse(CODIGO_CARGO == 13, row_number(), 1)
  ) %>%
  ungroup() %>%
  mutate_all(as.character)

# create key to join tse data
joinkey1 <- c(
  'election.year' = 'ANO_ELEICAO', 'election.ID' = 'SIGLA_UE',
  'election.stage' = 'NUM_TURNO', 'office.ID' = 'CODIGO_CARGO',
  'candidate.number' = 'NUM_VOTAVEL'
)

# join tse data on itself
tseCandidates %<>%
  select(-matches('votes$|^voto')) %>%
  left_join(sections, joinkey1) %>%
  mutate(candidate.elect = ifelse(
    office.ID == 13 & candidate.rank >= office.vacancies, 1, candidate.elect
  ))

# join onto tjsp
tjspAnalysis <- tjspSentences %>%
  left_join(tseCandidates, c('candidateID.x' = 'scraper.id')) %>%
  filter(!is.na(claimant.win)) %>%
  filter(class == 'Procedimento do Juizado Especial Cível')

# narrrow random litigants dataset to main litigant only
tjspFinalRandom <- tjspLitigantsRandom %>%
  group_by(id) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  right_join(tjspSentencesRandom, c('id' = 'Processo'))

# reorder columns to match candidates dataset
tjspAnalysisRandom <- select(
  tjspFinalRandom, Classe, Área, Assunto, Distribuição, Juiz, 16, id,
    claimant.sex, defendant.sex, clawyers.sex, dlawyers.sex, claimant.win,
    updates
)

# rename random dataset
names(tjspAnalysisRandom)[1:7] <- names(tjspSentences)[1:7]

# wrangle election information
electionMatch <- tseCandidates %>%
  select(
    candidate.ssn, election.year, election.ID, election.stage, office.ID,
    candidate.number
  )

# wrangle election results
joinkey1 <- c(
  'ANO_ELEICAO', 'SIGLA_UE', 'NUM_TURNO', 'CODIGO_CARGO',
  'NUM_VOTAVEL' = 'NUMERO_CAND'
)

# define joinkey for finding campaign expenditure
joinkey4 <- c('election.year', 'election.ID', 'office.ID', 'candidate.number')

# work on getting the right campaign expenditure variable
campaign %<>%
  filter(SG_UE_SUPERIOR == 'SP') %>%
  filter(ANO_ELEICAO %in% seq(2008, 2016, 4) & DS_CARGO != 'Vice-prefeito') %>%
  mutate(office.ID = as.character(ifelse(DS_CARGO == 'Vereador', 13, 11))) %>%
  rename(election.year = ANO_ELEICAO, election.ID = SG_UE) %>%
  rename(candidate.number = NR_CANDIDATO) %>%
  inner_join(electionMatch, joinkey4) %>%
  group_by(election.year, candidate.ssn) %>%
  summarize(x = sum(TOTAL_DESPESA))

tjspAnalysis %<>%
  left_join(campaign, c('election.year', 'candidate.ssn')) %>%
  group_by(election.ID) %>%
  mutate(x = ifelse(is.na(x), mean(x, na.rm = TRUE), x)) %>%
  group_by(office.ID) %>%
  mutate(x = ifelse(is.na(x), mean(x, na.rm = TRUE), x)) %>%
  ungroup() %>%
  rename(candidacy.expenditures.actual = x)

# define joinkey for finding judges responsible for each case
joinkey <- c('judge' = 'judge.name')

# reformat judge spelling in tjsp database
tjspAnalysis %<>%
  mutate_at(vars(judge),  str_to_lower) %>%
  mutate_at(vars(judge), ~iconv(., 'UTF-8', 'ASCII//TRANSLIT')) %>%
  mutate_at(vars(judge), ~str_remove_all(., '\\^|~|\'|\\"')) %>%
  mutate_at(vars(judge), str_squish)

tjspAnalysisRandom %<>%
  mutate_at(vars(judge),  str_to_lower) %>%
  mutate_at(vars(judge), ~iconv(., 'UTF-8', 'ASCII//TRANSLIT')) %>%
  mutate_at(vars(judge), ~str_remove_all(., '\\^|~|\'|\\"')) %>%
  mutate_at(vars(judge), str_squish)

# reformat judge spelling in tjsp pay database
tjspJudges$judge.name %<>% str_to_lower()

# match judge individual information onto sentences and electoral data
tjspAnalysis %<>%
  fuzzyjoin::fuzzy_left_join(tjspJudges, joinkey, match_fun = str_detect) %>%
  mutate(judge.pay = judge.pay %>% {ifelse(is.na(.), '35023.25', .)}) %>%
  mutate_at(
    vars(assignment, updates, judge.tenure.start), ~as.Date(., '%d/%m/%Y')
  ) %>%
  mutate(t = as.numeric(updates - judge.tenure.start)) %>%
  mutate(c = as.numeric(updates - assignment)) %>%
  mutate(judge.tenure  = t %>% {ifelse(. < 1, median(., na.rm = TRUE), .)}) %>%
  mutate(case.duration = c %>% {ifelse(. < 1, median(., na.rm = TRUE), .)}) %>%
  select(-t, -c)

tjspAnalysisRandom %<>%
  fuzzyjoin::fuzzy_left_join(tjspJudges, joinkey, match_fun = str_detect) %>%
  mutate(judge.pay = judge.pay %>% {ifelse(is.na(.), '35023.25', .)}) %>%
  mutate_at(
    vars(assignment, updates, judge.tenure.start), ~as.Date(., '%d/%m/%Y')
  ) %>%
  mutate(t = as.numeric(updates - judge.tenure.start)) %>%
  mutate(c = as.numeric(updates - assignment)) %>%
  mutate(judge.tenure  = t %>% {ifelse(. < 1, median(., na.rm = TRUE), .)}) %>%
  mutate(case.duration = c %>% {ifelse(. < 1, median(., na.rm = TRUE), .)}) %>%
  select(-t, -c)

# fill in missing gender of judges in politicians dataset
names <- tjspAnalysis[is.na(tjspAnalysis$judge.gender), 'judge'] %>% unlist()
names %<>% {ifelse(str_detect(., 'carlos'), 'Male', 'Female')}
names[is.na(names)] <- 'Male'
tjspAnalysis[is.na(tjspAnalysis$judge.gender), 'judge.gender'] <- names

# fill in missing gender of judges in random dataset
names <- tjspAnalysisRandom[is.na(tjspAnalysisRandom$judge.gender), 'judge'] %>%
         unlist()
names %<>% {ifelse(str_detect(.,'marcus|murillo') | is.na(.), 'Male', 'Female')}
tjspAnalysisRandom[
  is.na(tjspAnalysisRandom$judge.gender),'judge.gender'
] <- names

# anonymize judge names
tjspAnalysis %<>% {mutate(., judge = group_indices(., judge))}

# create variables to match cases in random and politician datasets
one_up <- tjspAnalysis$caseID %>% str_sub(1, 7) %>% as.integer() + 1
one_dw <- tjspAnalysis$caseID %>% str_sub(1, 7) %>% as.integer() - 1
match <- str_pad(c(one_up, one_dw), 7, 'left', '0')

# create matching dataset
match.cases <- tibble(caseID = rep(tjspAnalysis$caseID, 2), match = match) %>%
               mutate(key = paste0(match, str_sub(caseID, 10, 20)))

# match on random cases
tjspAnalysisRandom %<>%
  mutate(key = paste0(str_sub(caseID, 1, 7), str_sub(caseID, 10, 20))) %>%
  left_join(match.cases, 'key') %>%
  group_by(caseID.x) %>%
  filter(row_number() == 1) %>%
  rename(caseID = caseID.x, politician.case = caseID.y) %>%
  inner_join(tjspAnalysis, c('politician.case' = 'caseID')) %>%
  ungroup() %>%
  select(1:12, politician.case, case.duration.x, matches('^judge(.)*y$'))

# match circuit and municipal information
rows1 <- match(tjspAnalysis$caseID, str_remove_all(tjspMun$caseID, '\\.|-'))
rows2 <- match(
  tjspAnalysisRandom$politician.case, str_remove_all(tjspMun$caseID, '\\.|-')
)

# create new variables containing circuit, electoral district, and municipal
# information
tjspAnalysis$tjsp.ID <- unlist(tjspMun[rows1, 'tj'])
tjspAnalysis$ibge.ID <- unlist(tjspMun[rows1, 'ibge'])
tjspAnalysisRandom$tjsp.ID <- unlist(tjspMun[rows2, 'tj'])
tjspAnalysisRandom$ibge.ID <- unlist(tjspMun[rows2, 'ibge'])

# create list of useless variables for candidates cases
vars <- c(
  'candidateID.x', 'claimant', 'defendant', 'clawyers', 'dlawyers', 'id',
  'values', 'candidateID', 'candidate.ID', 'candidate.number', 'candidate.name',
  'candidate.ethnicity.ID', 'candidate.gender.ID', 'candidate.occupation.ID',
  'candidate.maritalstatus.ID', 'candidacy.situation.ID', 'coalition.ID',
  'judge.tenure.start', 'candidacy.expenditures'
)

# drop useless vars
tjspAnalysis %<>% select(-vars)
varNames <- names(tjspAnalysis)

# rename remaining vars
varNames[1:7]  <- c(paste0('case.', varNames[1:6]), 'case.ID')
varNames[8]    <- 'candidate.litigant.type'
varNames[9:10] <- c('case.lastupdate', 'case.claimant.win')
varNames[35]   <- 'candidate.expenditure'
names(tjspAnalysis) <- varNames

# reorder variables
tjspAnalysis %<>%
  select(
    matches('^case'), matches('^judge'), tjsp.ID, ibge.ID, election.ID,
    matches('^elect|^tot|^voto'), matches('^off'), matches('^candida'),
    matches('^par'), -judge.name
  )

# do the same for random cases
varNames <- names(tjspAnalysisRandom)
varNames[1:7] <- c(paste0('case.', varNames[1:6]), 'case.ID')
varNames %<>% str_remove_all('\\.(x|y)$')
names(tjspAnalysisRandom) <- varNames

# anonymize dataset
tjspAnalysisRandom %<>% select(-case.judge) %>% rename(case.judge = judge)

# save to file
save(tjspAnalysis, file = 'data/tjspAnalysis.Rda')
save(tjspAnalysisRandom, file = 'data/tjspAnalysisRandom.Rda')

# remove everything for serial sourcing
rm(list = ls())
