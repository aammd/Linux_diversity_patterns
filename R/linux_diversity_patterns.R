## "range"

library(dplyr)
library(readxl)
library(tidyr)
library(stringr)

traits <- read_excel("~/Dropbox/Linux_project/3_traits/traits_sample.xlsx", na = "NA")

distscop <- traits %>% 
  filter(!is.na(Packages)) %>% 
  group_by(Distribution) %>% 
  do(data_frame(scope = str_split(.$Scope, ","))) %>% 
  ungroup %>% 
  unnest(scope)

distscop %>% select(scope) %>% distinct %>% View

distscop %>% 
  group_by(Distribution) %>% 
  tally %>% 
  arrange(desc(n)) %>% 
  .[["n"]] %>% 
  plot(type = "p")

distscop %>% 
  group_by(Distribution) %>% 
  tally %>% 
  rename(abd = n) %>% 
  group_by(abd) %>% 
  tally %>% 
  arrange(n)


