## download tree data
# 
# download.file(url = "http://futurist.se/gldt/wp-content/uploads/12.10/gldt1210.tar.bz2",
#               destfile = "data/gldt.tar.bz2")


# loading packages and data ---------------------------


library(dplyr)
library(rvest)
library(readr)

gldt <- read_csv("data/gldt/gldt.csv", skip = 22)

## identify the completely blank line

min(which(rowSums(is.na(gldt)) == ncol(gldt)))

gldt %>% 
  slice(210:220)