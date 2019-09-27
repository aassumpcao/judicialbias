rm(list = ls())

load('data/campaign.Rda')
load('data/results.Rda')
load('data/sections.Rda')
load('data/tjspSentences.Rda')
load('data/tseCandidates.Rda')


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

join <- c(
  'election.year' = 'ANO_ELEICAO', 'election.ID' = 'SIGLA_UE',
  'election.stage' = 'NUM_TURNO', 'office.ID' = 'CODIGO_CARGO',
  'candidate.number' = 'NUM_VOTAVEL'
)


tjspSentences %>%
  left_join(tseCandidates, c('candidateID.x' = 'scraper.id')) %$%
  table(candidate.elect, office.ID)
