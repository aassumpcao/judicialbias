candidates.2010 %>% names()
candidates.2012 %>% names()
candidates.2016 %>% names()

candidates %>% names()

candidates %$% table(is.na(candidate.dob))


candidates %$% table(candidate.gender)

# join all elected candidates from 2012 and 2016 elections
candidates <- bind_rows(candidates.2012, candidates.2016) %>%
              filter(COD_SIT_TOT_TURNO %in% 1:3) %>%
              filter(SIGLA_UF == 'SP')

candidates %$% table(candidate.occupation)

candidatesSP2008 <- filter(candidates, election.year == 2008)
candidatesSP2012 <- filter(candidates, election.year == 2012)
candidatesSP2016 <- filter(candidates, election.year == 2016)

left_join(candidatesSP2012, candidatesSP2008, by = 'candidate.ssn')

# find repeated candidates
candidates.reelected2012 <- candidatesSP2012$candidate.ssn %>%
                            match(candidatesSP2008$candidate.ssn) %>%
                            subset(!is.na(.))

candidates.reelected2016 <- candidatesSP2016$candidate.ssn %>%
                            match(candidatesSP2012$candidate.ssn) %>%
                            subset(!is.na(.))

candidates2004 %>% names()
candidates2004 %$% table(DESC_SIT_TOT_TURNO, COD_SIT_TOT_TURNO)

# identify the reelected candidates in 2008.
candidates2004 <- filter(candidates.2010, ANO_ELEICAO == 2004) %>%
                  filter(COD_SIT_TOT_TURNO %in% c(1, 5))
candidates2008 <- filter(candidates.2010, ANO_ELEICAO == 2008) %>%
                  filter(COD_SIT_TOT_TURNO %in% c(1, 5))

candidates2012 <- filter(candidates.2012, ANO_ELEICAO == 2012)
candidates2016 <- filter(candidates.2016, ANO_ELEICAO == 2016)

candidates2008 %$% table(DESCRICAO_SEXO, CODIGO_SEXO)

candidates %$% table(candidacy.expenditures, election.year)

library(tidyverse)
library(magrittr)
library(feather)

# load to file
load('candidatesSP.Rda')
load('candidatesUnique.Rda')

nrow(candidatesSP)
nrow(candidatesUnique)

candidateCPF <- candidatesSP %>% select(candidateCPF = candidate.ssn,
                                        scraperID = scraper.id,
                                        munID = election.ID)

write_feather(candidateCPF, 'candidateCPF.feather')

install.packages('peticoesTJSP-master', repos = NULL, type = 'source')

peticoesTJSP::download_documents('00116417520158260481', '.', only_petitions = TRUE)

vacancies2016 %>%
  filter(SG_UF == 'SP') %>%
  filter(SG_UE == '61018') %>% .[1,] %>% unlist()
