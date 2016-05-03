## parsing dates
## how has the number of distributions changed over time
## download tree data
# 
# download.file(url = "http://futurist.se/gldt/wp-content/uploads/12.10/gldt1210.tar.bz2",
#               destfile = "data/gldt.tar.bz2")


# read data, packages ---------------------------------

library(lubridate)
library(dplyr)
library(readr)
library(tidyr)

gldt <- read_csv("data/gldt/gldt.csv", skip = 22)


## blank rows indicate separations between chunks of data
firstlines <- which(rowSums(is.na(gldt)) == ncol(gldt))

gldt %>% 
  slice(firstlines + 1) %>% 
  View
## SLS is 8th
## RedHat is 17

end_deb <- min(which(rowSums(is.na(gldt)) == ncol(gldt)))

fixdate <- function(olddate){
  olddate2 <- ifelse(nchar(olddate) < 5, paste0(olddate, ".01.01"), olddate)
  ymd(ifelse(nchar(olddate2) < 8, paste0(olddate2, ".01"), olddate2))
}

fix_dates_and_sort <- . %>% 
  .[,2:6] %>% 
  mutate(Start = fixdate(Start),
         Stop  = fixdate(Stop)) %>% 
  replace_na(replace = list(Stop = ymd("2013.01.01"))) %>% 
  select(-Color)

gldt_deb <- gldt %>% 
  slice(1:(end_deb - 1)) %>%
  fix_dates_and_sort

write_csv(gldt_deb, "data/gldt_time_debian.csv")

## SLS -----

sls_vec <- (firstlines[8] + 1):(firstlines[9] - 1)

gldt_sls <- gldt %>% 
  slice(sls_vec) %>% 
  fix_dates_and_sort


# redhat ----------------------------------------------

redhat_vec <- (firstlines[17] + 1):(firstlines[18] - 1)

gldt_red <- gldt %>% 
  slice(redhat_vec) %>% 
  fix_dates_and_sort

## total ---

bind_rows(Debian = gldt_deb,
           SLS    = gldt_sls,
           Redhat = gldt_red,
           .id = "clade") %>% 
  write_csv("data/distro_time.csv")
