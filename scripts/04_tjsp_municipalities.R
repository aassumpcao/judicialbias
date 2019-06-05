### judicial favoritism of politicians
# this script prepares circuit and municipal data for analysis
# author: andre assumpcao
# by andre.assumpcao@gmail.com

### import statements
# import packages
library(magrittr)
library(tidyverse)

# load data
tjspMun <- read_csv('data/sct.csv', locale = locale(encoding = 'latin1'),
  trim_ws = TRUE, col_types = cols(.default = 'c'))

# tjspMun
