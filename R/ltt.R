
# read data, packages ---------------------------------

library(lubridate)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

gldt_time <- read_csv("data/gldt_time_debian.csv")

## what was the start time:
gldt_time$Start %>% min
## ok so from 1993-01-01

test <- ymd("2000-01-01")

gldt_int <- gldt_time %>% 
  mutate(int = interval(Start, Stop))

monthly_21c <- test + months(0:(12*16))

is_distro <- function(dataset){
  function(s) sum(s %within% dataset$int)
} 

n_dist_time <- sapply(monthly_21c, is_distro(gldt_int))


data_frame(time = monthly_21c,
           ndist = n_dist_time) %>% 
  ggplot(aes(x = time, y = ndist)) +
  geom_point() + geom_line() 

## what is the distribution of long dead things?

gldt_extinct <- gldt_int %>% 
  filter(Stop < ymd("2015-01-01")) %>% 
  mutate(int = interval(Start, Stop))

monthly_21c <- test + months(0:(12*16))

n_dist_time <- sapply(monthly_21c, is_distro(gldt_extinct))


data_frame(time = monthly_21c,
           ndist = n_dist_time)  %>% 
  ggplot(aes(x = time, y = ndist)) +
  geom_point() + geom_line() 


# how long does a distro live? ------------------------



# can we calculate diversification rate with o --------
## extant species
gldt_extant <- gldt_int %>% 
  filter(Stop > ymd("2015-01-01")) %>% 
  mutate(int = interval(Start, Stop))

n_dist_time_extant <- sapply(monthly_21c, is_distro(gldt_extant))

data_frame(time = monthly_21c,
           all = n_dist_time,
           extant = n_dist_time_extant) %>% 
  gather(living, number, -time) %>% 
  ggplot(aes(x = time, y = number, color = living)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~living)

# measure extinction ----------------------------------


