## obtaining package data for all debian distros
### WORK IN PROGRESS and possibly 

# packages and functions ------------------------------

library(rvest)
library(stringr)
source("R/scraping_functions.R")


# get the operating systems ---------------------------

## first step is to obtain some OS data

distros <- read.csv("data/gldt_time_debian.csv")
distronames <- sapply(tolower(distros$Name), URLencode)

## when was it last updated? (and does it exist at all in our website)

library(purrr)
library(dplyr)

webpages <- distronames %>% 
  map(~paste0("http://distrowatch.com/table.php?distribution=", .x)) %>% 
  data_frame(distrourl = .) %>% 
  unnest(distrourl) %>% 
  group_by(distrourl) %>% 
  do(read_html(.$distrourl))
