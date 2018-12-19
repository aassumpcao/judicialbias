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
