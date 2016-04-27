## parsing dates
## how has the number of distributions changed over time


# read data, packages ---------------------------------

library(lubridate)
library(dplyr)
library(readr)
library(tidyr)

gldt <- read_csv("data/gldt/gldt.csv", skip = 22)

firstline <- min(which(rowSums(is.na(gldt)) == ncol(gldt)))

fixdate <- function(olddate){
  olddate2 <- ifelse(nchar(olddate) < 5, paste0(olddate, ".01.01"), olddate)
  ymd(ifelse(nchar(olddate2) < 8, paste0(olddate2, ".01"), olddate2))
}

gldt_time <- gldt %>% 
  slice(1:(firstline - 1)) %>%
  .[,2:6] %>% 
  mutate(Start = fixdate(Start),
         Stop  = fixdate(Stop)) %>% 
  replace_na(replace = list(Stop = ymd("2016.01.01"))) %>% 
  select(-Color)

write_csv(gldt_time, "data/gldt_time_debian.csv")

