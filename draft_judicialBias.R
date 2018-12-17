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

