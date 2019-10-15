library(tidyverse)

# load data
sctLitigants_random <- read_csv('data/sctLitigants_random.csv')
load('data/sctSummary.Rda')
load('data/tjspAnalysis.Rda')
load('data/tjspAnalysisRandom.Rda')

# only claimant = person and defendant = company
clean_name <- function(x) {
  x %>% 
    tolower() %>% 
    abjutils::rm_accent() %>% 
    str_remove_all("[^a-z ]") %>% 
    str_squish()
}
is_person <- function(x) {
  re_comp <- regex("EPP$|(?<!DE )SA$|ltda| ME$|condomi|lojas|fazenda|prefeit|municip", ignore_case = TRUE)
  !str_detect(x, re_comp)
}
pf_vs_pj_random <- sctLitigants_random %>%
  mutate_at(vars(claimant, defendant), clean_name) %>% 
  mutate_at(vars(claimant, defendant), list(pe = is_person)) %>% 
  filter(claimant_pe, !defendant_pe) %>% 
  select(case.ID = Processo, claimant)

pf_vs_pj_politician <- sctSummary %>%
  mutate_at(vars(claimant, defendant), clean_name) %>% 
  mutate_at(vars(claimant, defendant), list(pe = is_person)) %>% 
  filter(claimant_pe, !defendant_pe) %>% 
  select(case.ID = caseID, claimant)

tjspAnalysisRandom <- tjspAnalysisRandom %>% 
  mutate(origin = "random") %>% 
  mutate_if(is.factor, as.character) %>% 
  semi_join(pf_vs_pj_random, c("case.ID"))

d_final <- tjspAnalysis %>% 
  semi_join(pf_vs_pj_politician, "case.ID") %>% 
  filter(candidate.litigant.type == "Claimant") %>% 
  mutate(origin = "politician") %>% 
  rename(claimant.win = case.claimant.win) %>% 
  bind_rows(mutate_if(tjspAnalysisRandom, is.factor, as.character)) %>% 
  select(origin, claimant.win, 
         case.class, case.subclass, case.subject, case.assignment, 
         case.judge, case.claim, case.ID, judge.name, judge.pay, judge.gender, 
         judge.tenure, tjsp.ID, ibge.ID) %>% 
  filter(!is.na(case.subject)) %>% 
  mutate(
    case.claim = parse_number(case.claim, locale = locale(decimal_mark = ",", grouping_mark = ".")),
    case.subject = clean_name(case.subject),
    case.subject = fct_lump(case.subject, 10)
  )

# no interaction terms
model_simple <- glm(
  claimant.win ~ origin + case.subject + judge.tenure + case.claim,
  family = binomial(), 
  data = d_final
)

# with interaction terms
model_interact <- glm(
  claimant.win ~ origin * case.subject + judge.tenure + case.claim,
  family = binomial(), 
  data = d_final
)

broom::tidy(model_simple) %>% 
  filter(p.value < 0.05)

broom::tidy(model_interact) %>% 
  filter(p.value < 0.05)

