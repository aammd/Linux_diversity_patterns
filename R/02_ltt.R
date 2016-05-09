
# read data, packages ---------------------------------

library(lubridate)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

gldt_time <- read_csv("data/distro_time.csv",
                      col_types = "ccccc") %>% 
  mutate(Start = ymd(Start),
         Stop = ymd(Stop))


# calculate start time --------------------------------

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
make_timeline <- function(.gldt_time){
  gldt_eternal <- .gldt_time %>% 
    mutate(Stop = max(Stop)) %>% 
    mutate(int = interval(Start, Stop))
  
  n_dist_time_eternal <- sapply(monthly_21c, is_distro(gldt_eternal))
  
  ## counting extinctions
  gldt_extinctions <- .gldt_time %>% 
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
  ggtitle('A') +
  ylab("Number of distros") + 
  xlab("Time") 
ggsave("figures/distro_ltt.pdf", width = 10, height = 4)

## change in distribution number over time (instantaneous growth)
distro_timeline %>% 
  group_by(clade) %>%  
  mutate(speciation = (speciation - lag(speciation,12)),
         extinction = (extinction - lag(extinction,12))) %>% 
  filter(time >= ymd("2001-01-01")) %>% 
  gather(living, number, -time, -clade) %>% 
  ungroup %>% 
  ggplot(aes(x = time, y = number, color = living, group = living)) +
  # geom_point() + 
  geom_line() +
  facet_grid(~clade) +
  theme_bw() + 
  scale_color_discrete(guide_legend(title = "", position = "top")) +
  theme(legend.position="top", plot.title=element_text(hjust=-0.05)) +
  ggtitle('B') + 
  ylab("Instantaneous rate") + 
  xlab("Time") 
ggsave("figures/distro_growth.pdf", width = 10, height = 4)

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
    pdfnup --nup 1x2 distro_ltt.pdf distro_growth.pdf --outfile evolution.pdf
    ")


