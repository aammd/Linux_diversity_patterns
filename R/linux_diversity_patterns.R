## "range"

library(dplyr)
library(googlesheets)
library(tidyr)
library(stringr)

traits_sheet <- gs_title("traits_sample2")

traits <- gs_read_csv(traits_sheet)

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


