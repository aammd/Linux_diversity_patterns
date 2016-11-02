#' # Author: A Andrew M MacDonald
# email: a.a.m.macdonald@gmail.com

# read data, packages ---------------------------------

library(lubridate)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

## read in time data as character, then convert to dates via
## lubridate
gldt_time <- read_csv("../data/distro_time.csv",
                      col_types = "ccccc") %>% 
  mutate(Start = ymd(Start),
         Stop = ymd(Stop))


# calculate start time --------------------------------

## convert dates to an "interval" format
gldt_int <- gldt_time %>% 
  mutate(int = interval(Start, Stop))

# create a vector of months from 2000 to 2013
monthly_21c <- ymd("2000-01-01") + months(0:(12*13))

# function which counts how many distros are "alive" (that
# is, between their Start and Stop dates) at any given date.
is_distro <- function(dataset){
  function(s) sum(s %within% dataset$int)
} 

## a function which applies is_distro over the whole time
## vector, then spits out the answer in a data frame
make_extant_df <- function(dataset,
                           timevec = monthly_21c){
  # create the vector of "extant" distros for every month
  time_ext <- sapply(timevec, is_distro(dataset))
  # place these values into a data frame. 
  
  data_frame(time = timevec,
             ndist = time_ext) 
}


make_extant_df(gldt_int) %>% 
  ggplot(aes(x = time, y = ndist)) +
  geom_point() + geom_line() 


## what is the distribution of long dead things?

gldt_extinct <- gldt_int %>% 
  filter(Stop < ymd("2013-01-01")) %>% 
  mutate(int = interval(Start, Stop))



make_extant_df(gldt_extinct) %>% 
  ggplot(aes(x = time, y = ndist)) +
  geom_point() + geom_line() 


# can we calculate diversification rate with o --------

# CUMULATIVE DIVERSIFICATION AND EXTINCTION THROUGHT TIME PLOTS

## assuming all species last forever
make_timeline <- function(.gldt_time){
  gldt_eternal <- .gldt_time %>% 
    mutate(Stop = max(Stop)) %>% 
    mutate(int = interval(Start, Stop))

  
  n_dist_time_eternal <- sapply(monthly_21c, is_distro(gldt_eternal))
  
  ## counting extinctions
  gldt_extinctions <- .gldt_time %>% 
    ## don't include ones still alive
    filter(Stop < ymd("2013-01-01")) %>% 
    mutate(int = interval(Stop, ymd("2013-01-01")))
  
  
  n_dist_time_extinctions <- sapply(monthly_21c, is_distro(gldt_extinctions))
  
  df <- data_frame(time = monthly_21c,
                   # all = n_dist_time,
                   speciation = n_dist_time_eternal,
                   extinction = n_dist_time_extinctions)
  return(df)
}

make_timeline(gldt_time)

nrow(gldt_time)

## create a master timeline dataset
distro_timeline <- gldt_time %>% 
  split(gldt_time$clade) %>% 
  lapply(make_timeline) %>% 
  bind_rows(.id = "clade")
  
## speciation and extinction over time
distro_timeline %>% 
  gather(living, number, -time, -clade) %>% 
  ggplot(aes(x = time, y = number, color = living)) +
  # geom_point() +
  geom_line() +
  facet_wrap(~clade) + #, scales = "free_y") +
  theme_bw() + 
  theme(legend.position="top", plot.title=element_text(hjust=-0.05)) +
  scale_color_discrete(guide_legend(title = "")) +
  ggtitle('B') +
  ylab("Cumulative # of distros") + 
  xlab("Time") 
ggsave("figures/Figure_3B.pdf", width = 10, height = 4)

## change in distribution number over time (instantaneous growth)
distro_timeline %>% 
  group_by(clade) %>%  
  mutate(speciation = (speciation - lag(speciation,12))/n_dist_time,
         extinction = (extinction - lag(extinction,12))/n_dist_time) %>% 
  filter(time >= ymd("2001-01-01")) %>% 
  gather(living, number, -time, -clade) %>% 
  ungroup %>% 
  ggplot(aes(x = time, y = number, color = living, group = living)) +
  # geom_point() + 
  geom_line() +
  facet_grid(~clade) +
  theme_bw() + 
  scale_color_discrete(guide=FALSE) +
  theme(legend.position="top", plot.title=element_text(hjust=-0.05)) +
  ggtitle('C') + 
  ylab("Instantaneous per-distro rate") + 
  xlab("Time") 
ggsave("figures/Figure_3C.pdf", width = 10, height = 4)

# measure lifespan ----------------------------------

par(mfrow = c(1,1))
gldt_int %>% 
  mutate(lifespan = as.numeric(Stop - Start)) %>% 
  .[["lifespan"]] %>% 
  # log() %>% 
  hist()

# negative interactions -------------------------------

# merge the two .pdf files using Linux's pdfnup utility
system("
    cd figures
    pdfcrop Figure_3B.pdf Figure_3B.pdf 
    pdfcrop Figure_3C.pdf Figure_3C.pdf 
    pdfnup --nup 1x3 Figure_3A.pdf Figure_3B.pdf Figure_3C.pdf --outfile Figure_3.pdf
    pdfcrop Figure_3.pdf Figure_3.pdf
    ")


