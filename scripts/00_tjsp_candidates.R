### judicial favoritism of politicians
# candidates wrangling
#   this script wrangles the candidates in all local elections in São Paulo
#   between 2004 and 2016.
# author: andre assumpcao
# email:  andre.assumpcao@gmail.com

# for testing
rm(list = ls())

### import statements
# import packages
library(magrittr)
library(readr)
library(tidyverse)

# load data
load('data/candidates.upto.2010.Rda')
load('data/candidates.2012.Rda')
load('data/candidates.2016.Rda')

### function definitions
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

### body
# bind all data
candidates <- bind_rows(candidates.upto.2010, candidates.2012, candidates.2016)

# extract people in são paulo
candidates %<>% filter(SIGLA_UF == 'SP')

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
            party.number               = NUMERO_PARTIDO,
            party.coalition            = COMPOSICAO_LEGENDA)

# prepare covariates
#   1. age
#   2. gender
#   3. education
#   4. marital status
#   5. ethnicity             - not available before 2016
#   6. campaign expenditures
#   7. candidate's political experience

candidates[complete.cases(candidates),]

[nchar(candidates$candidate.dob) == 7,] %>% View()



%>%
  mutate(age = case_when(election.year == 2004 ~ calc_age(dob, '2004-10-03'),
                         election.year == 2008 ~ calc_age(dob, '2008-10-05'),
                         election.year == 2012 ~ calc_age(dob, '2012-10-07'),
                         election.year == 2016 ~ calc_age(dob, '2016-10-02'))
  ) %>%
  mutate(age = ifelse(is.na(age), as.integer(mean(age, na.rm = TRUE)), age)) %>%
  mutate(age = ifelse(age > 86, 2008 - age, age), candidate.age = age) %>%
  select(-age, -dob)

# wrangle gender
candidates %<>%
  mutate(candidate.male = ifelse(candidate.gender.ID != 4, 1, 0)) %>%
  select(1:21, candidate.male, 24:38)

# wrangle education
candidates %<>%
  select(-candidate.education.ID) %>%
  mutate(candidate.education = str_remove(candidate.education, 'ENSINO')) %>%
  mutate(candidate.education = str_trim(candidate.education)) %>%
  mutate(candidate.education = ifelse(candidate.education == 'NÃO INFORMADO',
                                      'SUPERIOR COMPLETO', candidate.education))
# wrangle marital status
candidates %<>%
  mutate(candidate.maritalstatus = ifelse(
    candidate.maritalstatus == 'NÃO INFORMADO', 'SOLTEIRO(A)',
    candidate.maritalstatus)
  ) %>%
  select(-candidate.maritalstatus.ID)

# define vector for finding political occupations
politicians <- 'VEREADOR|PREFEITO|DEPUTADO|GOVERNADOR|SENADOR|PRESIDENTE'

# wrangle political experience
candidates %<>%
  mutate(
    candidate.occupation = iconv(candidate.occupation, 'Latin1', 'ASCII'),
    candidate.experience = case_when(
      str_detect(candidate.occupation, politicians) == TRUE  ~ 1,
      str_detect(candidate.occupation, politicians) == FALSE ~ 0,
      is.na(str_detect(candidate.occupation, politicians))   ~ 0
    )
  )
