# ------------------------------------------------------------------------------
# Description: Here we create the plots of diversification through time
# in the three major Linux families.
#
# Authors: A Andrew M MacDonald, Petr Keil
# emails: a.a.m.macdonald@gmail.com, pkeil@seznam.cz
# ------------------------------------------------------------------------------

# read packages

library(lubridate)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(geiger)
library(gridExtra)

# read in time data as character, then convert to dates via lubridate

gldt_time <- read_csv("../data/distro_time.csv", col_types = "ccccc") %>% 
             mutate(Start = ymd(Start),
                    Stop = ymd(Stop))

# calculate start time ---------------------------------------------------------

# convert dates to an "interval" format
gldt_int <- gldt_time %>% 
  mutate(int = interval(Start, Stop))

# create a vector of months from 2000 to 2013
monthly_21c <- ymd("2000-01-01") + months(0:(12*13))

# function which counts how many distros are "alive" (that
# is, between their Start and Stop dates) at any given date.
is_distro <- function(dataset){
  function(s) sum(s %within% dataset$int)
} 

# create the vector of "extant" distros for every month
n_dist_time <-  sapply(monthly_21c, is_distro(gldt_int))

# place these values into a data frame. 
data_frame(time = monthly_21c,
           ndist = n_dist_time) %>% 
  ggplot(aes(x = time, y = ndist)) +
  geom_point() + geom_line() 

lines(monthly_21c, n_dist_time)

## what is the distribution of long dead things?
gldt_extinct <- gldt_int %>% 
  filter(Stop < ymd("2013-01-01")) %>% 
  mutate(int = interval(Start, Stop))


n_dist_time_ext <- sapply(monthly_21c, is_distro(gldt_extinct))

data_frame(time = monthly_21c,
           ndist = n_dist_time_ext)  %>% 
  ggplot(aes(x = time, y = ndist)) +
  geom_point() + geom_line() 


# ------------------------------------------------------------------------------
# FUNCTION FOR CUMULATIVE DIVERSIFICATION AND EXTINCTION THROUGHT TIME PLOTS

## assuming all species last forever
make_timeline <- function(.gldt_time)
{
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

# ------------------------------------------------------------------------------

make_timeline(gldt_time)

## create a master timeline dataset
distro_timeline <- gldt_time %>% 
  split(gldt_time$clade) %>% 
  lapply(make_timeline) %>% 
  bind_rows(.id = "clade") 
# distro_timeline <- data.frame(distro_timeline, n_dist_time)

# ------------------------------------------------------------------------------ 
# adjust the data for better plotting in ggplot2

dat_pk <- distro_timeline %>% mutate(richness = speciation - extinction) %>% 
  group_by(clade) %>%  
  mutate(spec.rate = (speciation - lag(speciation,12))/n_dist_time,
         ext.rate = (extinction - lag(extinction,12))/n_dist_time) %>%
  filter(time >= ymd("2001-01-01")) 
dat_pk


# ------------------------------------------------------------------------------
# PLOTS OF THE RELATIONSHIP BETWEEN NUMBER OF DISTROS AT A GIVEN MONTH, AND THE
# SPECIATION, EXTINCTION AND DIVERSIFICATION RATE (NOT PLOTTED IN THE PAPER)

p1 <- ggplot(dat_pk, aes(x=richness, y=spec.rate)) +
  geom_point(shape=21, aes( colour=clade)) + 
  geom_smooth(method="gam", formula=y~s(x, bs="ps", k=5), aes( colour=clade)) +
  labs(x="# of distros", 
       y="Speciation rate", 
       title="(a)") +
  theme_bw()+
  theme(legend.position = c(0.8, 0.7)) 


p2 <- ggplot(dat_pk, aes(x=richness, y=ext.rate)) +
  geom_point(shape=21, aes( colour=clade)) + 
  geom_smooth(method="gam", formula=y~s(x, bs="ps", k=5), aes( colour=clade)) +
  labs(x="# of distros", 
       y="Extinction rate", 
       title="(b)") +
  theme_bw() +
  theme(legend.position='none')


p3 <- ggplot(dat_pk, aes(x=richness, y=spec.rate - ext.rate)) +
  geom_point(shape=21, aes( colour=clade)) + 
  geom_smooth(method="gam", formula=y~s(x, bs="ps", k=5), aes( colour=clade)) +
  labs(x="# of distros", 
       y="Speciation rate - Extinction rate", 
       title="(c)") +
  theme_bw() +
  theme(legend.position='none')


# export the figures to a file 
pdf("../figures/Figure_rates.pdf", width=10, height=3.3)
  gridExtra::grid.arrange(p1, p2, p3, nrow=1, ncol=3)
dev.off()


# ------------------------------------------------------------------------------
# PLOTS OF SPECIATION, EXTINCTION AND DIVERSIFICATION THROUGH TIME

p1 <- distro_timeline %>% 
  gather(living, number, -time, -clade) %>% 
  ggplot(aes(x = time, y = number, color = living)) +
  geom_line() +
  facet_wrap(~clade) + 
  theme_bw() + 
  theme(legend.position=c(0.9,0.67)) +
  scale_color_discrete(guide_legend(title = "")) +
  ggtitle('(b)') +
  ylab("Cumulative no. of distros\n") +
  scale_x_date(breaks = as.Date(c("2000-01-01", "2005-01-01", "2010-01-01")),
               limits=as.Date(c("2000-01-01", "2013-01-01")), 
               date_labels = "%Y" ) +
  xlab("Time") 


p2 <- distro_timeline %>% 
  group_by(clade) %>%  
  mutate(speciation = (speciation - lag(speciation,12))/n_dist_time,
         extinction = (extinction - lag(extinction,12))/n_dist_time) %>%
  filter(time >= ymd("2001-01-01")) %>% 
  gather(living, number, -time, -clade) %>% 
  ungroup %>% 
  ggplot(aes(x = time, y = number, color = living, group = living)) +
  geom_line() +
  facet_grid(~clade) +
  theme_bw() + 
  theme(legend.position=c(0.9,0.7), legend.title = element_blank()) +
  ggtitle('(c)') + 
  ylab("Rate\n") + 
  scale_x_date(breaks = as.Date(c("2000-01-01", "2005-01-01", "2010-01-01")),
               limits=as.Date(c("2000-01-01", "2013-01-01")), 
               date_labels = "%Y" ) +
  xlab("Time") 


# ------------------------------------------------------------------------------
# plots with fitted smoother
for.p3 <- distro_timeline %>% 
  group_by(clade) %>%  
  mutate(speciation = (speciation - lag(speciation,12))/n_dist_time,
         extinction = (extinction - lag(extinction,12))/n_dist_time) %>%
  mutate(diversification = speciation - extinction) %>%
  filter(time >= ymd("2001-01-01")) %>% 
  gather(living, number, -time, -clade) %>% 
  ungroup 

for.p3 <- for.p3[for.p3$living=="diversification",]

p3 <- ggplot(data= for.p3, aes(x = time, y = number, group = living)) +
  geom_line(colour="darkgrey") +
  geom_smooth(colour="black") + 
  facet_grid(~clade) +
  geom_hline(yintercept = 0, colour="grey") +
  theme_bw() + 
  scale_color_discrete(guide=FALSE) +
  theme(legend.position="top") +
  ggtitle('(d)') + 
  ylab("Speciation rate - Extinction rate\n") + 
  scale_x_date(breaks = as.Date(c("2000-01-01", "2005-01-01", "2010-01-01")),
               limits=as.Date(c("2000-01-01", "2013-01-01")), 
               date_labels = "%Y" ) +
  xlab("Time") 

# export the plots to a file
pdf("../figures/Figure_3BCD.pdf")
  gridExtra::grid.arrange(p1, p2, p3, ncol=1, nrow=3)
dev.off()
