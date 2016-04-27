
# read data, packages ---------------------------------

library(lubridate)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

gldt_time <- read_csv("data/distro_time.csv",
                      col_types = "ccccc") %>% 
  mutate(Start = ymd_hms(Start),
         Stop = ymd_hms(Stop))

## what was the start time:
gldt_time$Start %>% min
## ok so from 1993-01-01

test <- ymd("2000-01-01")

gldt_int <- gldt_time %>% 
  mutate(int = interval(Start, Stop))

monthly_21c <- test + months(0:(12*13))

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
  filter(Stop < ymd("2013-01-01")) %>% 
  mutate(int = interval(Start, Stop))


n_dist_time_ext <- sapply(monthly_21c, is_distro(gldt_extinct))


data_frame(time = monthly_21c,
           ndist = n_dist_time_ext)  %>% 
  ggplot(aes(x = time, y = ndist)) +
  geom_point() + geom_line() 


# can we calculate diversification rate with o --------

## assuming all species last forever
gldt_eternal <- gldt_int %>% 
  mutate(Stop = max(Stop)) %>% 
  mutate(int = interval(Start, Stop))

n_dist_time_eternal <- sapply(monthly_21c, is_distro(gldt_eternal))

## counting extinctions
gldt_extinctions <- gldt_int %>% 
  filter(Stop < ymd("2013-01-01")) %>% 
  mutate(int = interval(Stop, ymd("2013-01-01")))


n_dist_time_extinctions <- sapply(monthly_21c, is_distro(gldt_extinctions))

data_frame(time = monthly_21c,
           # all = n_dist_time,
           speciation = n_dist_time_eternal,
           extinction = n_dist_time_extinctions) %>% 
  gather(living, number, -time) %>% 
  group_by(living) %>% 
  mutate(number = number - lag(number,12)) %>% 
  ggplot(aes(x = time, y = number, color = living)) +
  geom_point() + 
  geom_line() +
  theme_minimal() + 
  scale_color_discrete(guide_legend(title = "")) +
  ylab("Number of GNU/Linux distribution") + 
  xlab("Time") 
  # facet_wrap(~living)

ggsave("figures/distro_ltt.pdf")

# measure lifespan ----------------------------------

gldt_int %>% 
  mutate(lifespan = as.numeric(Stop - Start)) %>% 
  .[["lifespan"]] %>% 
  # log() %>% 
  hist()

# negative interactions -------------------------------





