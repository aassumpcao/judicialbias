################################################################################
# electoral crime paper
# candidates wrangling

# this script narrows down the database of candidates who had their candidacies
# appealed before the elections but have not heard back before election date.
# after it filters down candidates, it prepares the data for the tse case
# scraper, which is a program that visits each candidate's website at tse and
# downloads the case and protocol number for all their candidacies.

# by andre.assumpcao@gmail.com

# import statements
library(here)
library(tidyverse)
library(magrittr)

# load statements
load('../2019 Electoral Crime/candidates.2012.Rda')
load('../2019 Electoral Crime/candidates.2016.Rda')

# define function to calculate age from dob
calc_age <- function(birthDate, refDate = Sys.Date()) {
  # Args:
  #   birthDate: argument taking up date of birth (YMD format)
  #   refDate:   reference date to calculate age (also YMD format)

  # Returns:
  #   individual's age in years

  # Body:
  #   make one call to lubridate functions
  time <- lubridate::as.period(lubridate::interval(birthDate, refDate), 'year')

  #   return year element of period object
  return(time$year)
}

################################################################################
# narrow down searchable candidates
# join all elected candidates from 2012 and 2016 elections
candidates <- bind_rows(candidates.2012, candidates.2016) %>%
              filter(COD_SIT_TOT_TURNO %in% 1:3) %>%
              filter(SIGLA_UF == 'SP') %>%
              filter(CODIGO_CARGO != 12)

# rename variables
candidates %<>%
  transmute(election.year              = ANO_ELEICAO,
            election.stage             = NUM_TURNO,
            election.state             = SIGLA_UF,
            election.ID                = SIGLA_UE,
            office.ID                  = CODIGO_CARGO,
            candidate.ID               = SEQUENCIAL_CANDIDATO,
            candidate.number           = NUMERO_CANDIDATO,
            candidate.name             = NOME_CANDIDATO,
            candidate.ssn              = CPF_CANDIDATO,
            candidate.dob              = DATA_NASCIMENTO,
            candidate.age              = IDADE_DATA_ELEICAO,
            candidate.ethnicity        = DESCRICAO_COR_RACA,
            candidate.ethnicity.ID     = CODIGO_COR_RACA,
            candidate.gender           = DESCRICAO_SEXO,
            candidate.gender.ID        = CODIGO_SEXO,
            candidate.occupation       = DESCRICAO_OCUPACAO,
            candidate.occupation.ID    = CODIGO_OCUPACAO,
            candidate.education        = DESCRICAO_GRAU_INSTRUCAO,
            candidate.education.ID     = COD_GRAU_INSTRUCAO,
            candidate.maritalstatus    = DESCRICAO_ESTADO_CIVIL,
            candidate.maritalstatus.ID = CODIGO_ESTADO_CIVIL,
            candidacy.situation        = DES_SITUACAO_CANDIDATURA,
            candidacy.situation.ID     = COD_SITUACAO_CANDIDATURA,
            candidacy.expenditures     = DESPESA_MAX_CAMPANHA,
            party.number               = NUMERO_PARTIDO,
            party.coalition            = COMPOSICAO_LEGENDA,
            coalition.ID               = CODIGO_LEGENDA)

# prepare covariates for summary statistics
#   1. age
#   2. gender
#   3. education
#   4. marital status
#   5. ethnicity             - not available before 2016
#   6. campaign expenditures - requires matching to campaign funding dataset
#   7. candidate's political experience

# wrangle age
candidates %<>%
  mutate(dob = lubridate::dmy(candidate.dob)) %>%
  mutate(age = case_when(election.year == 2012 ~ calc_age(dob, '2012-10-07'),
                         election.year == 2016 ~ calc_age(dob, '2016-10-02'))
  ) %>%
  mutate(age = ifelse(candidate.age == -1, age, candidate.age)) %>%
  mutate(age = ifelse(age > 100, 2012 - age, age), candidate.age = age) %>%
  select(-age, -dob)

# wrangle gender
candidates %<>% mutate(candidate.male = ifelse(candidate.gender.ID != 4, 1, 0))

# wrangle education
candidates %<>% select(-candidate.education.ID)

# wrangle candidacy expenditures
candidates %<>%
  mutate(exp = candidacy.expenditures) %>%
  mutate(exp = ifelse(is.na(exp) | exp == -1, mean(exp, na.rm = TRUE), exp)) %>%
  mutate(candidacy.expenditures = exp)

# define vector for finding political occupations
politicians <- 'VEREADOR|PREFEITO|DEPUTADO|GOVERNADOR|SENADOR|PRESIDENTE'

# wrangle political experience
candidates %<>%
  mutate(candidate.occupation = iconv(candidate.occupation,'Latin1','ASCII'))%>%
  mutate(candidate.experience = case_when(
    str_detect(candidate.occupation, politicians) == TRUE  ~ 1,
    str_detect(candidate.occupation, politicians) == FALSE ~ 0,
    is.na(str_detect(candidate.occupation, politicians))   ~ 0))

# transform variable type to factor
candidates %<>% mutate_at(vars(matches('education|maritalstatus')), factor)