### judicial favoritism of politicians
# this script prepares judge information for analysis
# author: andre assumpcao
# by andre.assumpcao@gmail.com

### import statements
# import packages
library(magrittr)
library(tidyverse)
library(pdftools)

# find data files
data <- list.files('data', pattern = 'ResCNJ|ListaAn') %>% {paste0('data/', .)}

### function definitions
# function to find judges in the state court pdf list
judge_find <- function(page) {
  # Args:     page = pdf page
  # Returns:  list of judge names and tenure start

  # Body:
  # split pdf text into rows, and find first and last rows with judge info
  x <- str_split(page, '\n') %>% unlist()
  i <- str_which(x, 'QUINTO')
  j <- str_which(x, 'SPDB')

  # extract header and cotent info
  head <- x[i]
  body <- x[(i + 2):(j - 1)]

  # drop useless rows
  body <- body[str_which(body, '^\\d')]

  # find names
  name.start <- str_locate(body, '\\.')[,1] + 1
  name.end <- rep(str_locate(head, 'ENTR')[,1] - 1, length(name.start))
  names <- str_sub(body, name.start, name.end) %>% trimws()
  names <- str_remove_all(names, 'Entra(n)?(c)?(i)?(a)?$') %>% trimws()

  # find tenure
  tenures <- stringi::stri_locate_last_regex(body, '(.){2}/(.){2}/[0-9]{4}')
  tenure.start <- str_sub(body, tenures[,1], tenures[,2])

  # build result list
  result <- list(names = names, tenure.start = tenure.start)

  # return results
  return(result)
}

### find judges in main pdf
# load and wrangle first data file
trial.judges <- pdf_text(data[1])

# extract list of judges
list <- lapply(trial.judges[1:150], judge_find)

# create empty dataset and bind all elements from judge list
tjspJudges <- tibble()

# loop binding rows together
for (i in 1:length(list)) {tjspJudges <- bind_rows(tjspJudges, list[i])}

# convert all names to lower
tjspJudges$names %<>%
  str_to_lower() %>%
  iconv('UTF-8', 'ASCII//TRANSLIT') %>%
  str_remove_all('\\^|~|\'|\\"') %>%
  str_squish()

### find judges pay
# create tibble of judge pay
tjspPay <- tibble()

# loop csv files
for (i in 2:length(data)){
  # create each df from file
  df <- read_delim(data[i], ';', locale = locale(encoding = 'latin1'),
    trim_ws = TRUE, col_types = cols(.default = 'c'))
  # bind onto empty df
  tjspPay <- bind_rows(tjspPay, df)
}

# edit dataset (names and monetary values)
tjspPay %<>%
  mutate_at(vars(1:2),  str_to_lower) %>%
  mutate_at(vars(1:2), ~iconv(., 'UTF-8', 'ASCII//TRANSLIT')) %>%
  mutate_at(vars(1:2), ~str_remove_all(., '\\^|~|\'|\\"')) %>%
  mutate_at(vars(1:2), str_squish) %>%
  mutate_at(vars(3:18), ~str_replace_all(., ',', '.'))

# join dataset
tjspJudges %<>%
  {fuzzyjoin::regex_left_join(tjspPay, ., by = c('Nome' = 'names'))} %>%
  slice(-6, -1323, -2283, -2285) %>%
  distinct(Nome, .keep_all = TRUE) %>%
  mutate(tenure.start = ifelse(is.na(tenure.start), '16/07/1993', tenure.start)
  ) %>%
  transmute(judge.name = str_to_title(Nome), judge.pay = `Rendimento Liquido`,
    judge.tenure.start = tenure.start
  )

# guess judge gender
tjspJudges$judge.gender <- genderBR::get_gender(tjspJudges$judge.name)

# manually fill in judges searching their names on google
gender <- c('Female', 'Male', 'Female', rep('Male', 2), 'Female', rep('Male',6),
            'Female', rep('Male', 9), 'Female', rep('Male', 12), 'Female',
            rep('Male', 3))

# replace missing with genders from google
tjspJudges[is.na(tjspJudges$judge.gender), 'judge.gender'] <- gender

# save to file
save(tjspJudges, file = 'data/tjspJudges.Rda')

# remove everything for serial sourcing
rm(list = ls())


