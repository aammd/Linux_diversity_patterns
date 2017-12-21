# ------------------------------------------------------------------------------
# Description: Here we plot & analyze the species-abundance distributions,
# and the Taylor's power law.

# Author: Petr Keil
# pkeil@seznam.cz
# ------------------------------------------------------------------------------

# DATA AND PACKAGES

library(plyr)
library(vegan)
library(ggplot2)
library(gridExtra)

# LOAD LINUXCOUNTER DATA

dat.lct <- read.csv("../data/data_added_after_review/linuxcounter_2017_11_27.csv") 
# remove Android and 'other' categories
dat.lct <- dat.lct[dat.lct$Distro != "Android",]
dat.lct <- dat.lct[dat.lct$Distro != "other",]
dat.lct <- data.frame(dat.lct, Source="LinuxCounter", 
                      Rank = max(rank(dat.lct$Machines, 
                                      ties.method="first")) + 1 - rank(dat.lct$Machines, ties.method="first"))

# LOAD DISTROWATCH DATA

dat.yearly <- read.csv("../data/linux_yearly_data.csv")
dat.recent <- read.csv("../data/popularity_last_clean_12.csv")
dat.recent <- data.frame(dat.recent, Source="DistroWatch", 
                         Rank = max(rank(dat.recent$HPD, 
                                         ties.method="first")) + 1 - rank(dat.recent$HPD, ties.method="first"))
names(dat.recent) <- names(dat.lct)


# LOAD WIKIMEDIA DATA

dat.squid.1 <- read.csv("../data/data_added_after_review/squid_2010_2011.csv")
dat.squid.2 <- read.csv("../data/data_added_after_review/squid_2011_2014.csv")

# remove the weird distros that seem to be duplicates
dat.squid.2 <- dat.squid.2[dat.squid.2$Distro != "Linux Xubuntu",]
dat.squid.2 <- dat.squid.2[dat.squid.2$Distro != "Linux openSUSE",]


################################################################################
# 1. SPECIES-ABUNDANCE DISTRIBUTIONS (SADs)
################################################################################

# some prelimiary explorations and plots

ggplot(data=rbind(dat.recent, dat.lct), aes(x=log10(Machines))) +
       geom_histogram(aes(fill=Source)) +
       theme_bw()

ggplot(data=rbind(dat.recent, dat.lct), aes(x=log10(Rank), y=log10(Machines))) +
       geom_point(aes(colour=Source)) + 
       theme_bw()



# RANK-ABUNDANCE CURVES

source("0_logseries_estimation.r")

# LinuxCounter data --------

X <- dat.lct$Machines
XX <- fit.distr(X)
X.1 <- data.frame(Rank=length(X) + 1 - rank(X, ties.method="first"), X)

# linear plot
LC.plot <- ggplot(X.1, aes(x=Rank, y=X)) +
           geom_point() +
           geom_line(data=XX, aes(x=Rank, y=Mean, colour=Model)) +
           labs(x="Rank", y="no. of machines", 
                title="(a)", 
                subtitle="Source: LinuxCounter") +
           theme_bw() +
           theme(legend.position = c(0.7, 0.7)) 

# log-log plot
LC.plot.log <- ggplot(X.1, aes(x=log10(Rank), y=log10(X))) +
  geom_point() +
  geom_line(data=XX, aes(x=log10(Rank), y=log10(Mean), colour=Model)) +
  labs(x=expression(paste(log[10], " Rank")), 
       y=expression(paste(log[10], " no. of machines")), 
       title="(b)", 
       subtitle="Source: LinuxCounter") +
  ylim(0, 5) +
  theme_bw() +
  theme(legend.position='none')

LC.plot.log


# DistroWatch data --------

X <- dat.recent$Machines
XX <- fit.distr(X)
X.1 <- data.frame(Rank=length(X) + 1 - rank(X, ties.method="first"), X)

# linear plot
DW.plot <- ggplot(X.1, aes(x=Rank, y=X)) +
  geom_point() +
  geom_line(data=XX, aes(x=Rank, y=Mean, colour=Model)) +
  labs(x="Rank", y="HPD", 
       title="(c)", 
       subtitle="Source: DistroWatch") +
  theme_bw() +
  theme(legend.position='none')

# log-log plot
DW.plot.log <- ggplot(X.1, aes(x=log10(Rank), y=log10(X))) +
  geom_point() +
  geom_line(data=XX, aes(x=log10(Rank), y=log10(Mean), colour=Model)) +
  labs(x=expression(paste(log[10], " Rank")), 
       y=expression(paste(log[10], " HPD")), 
       title="(d)", subtitle="Source: DistroWatch") +
  theme_bw() +
  theme(legend.position='none')


# export the plots to a file
pdf("../figures/SADs.pdf", width=5, height=5)
  grid.arrange(LC.plot, LC.plot.log, DW.plot, DW.plot.log, nrow=2, ncol=2)
dev.off()


################################################################################
# 2. TAYLOR'S POWER LAW
################################################################################

# Prepare the Distrowatch data 
dat <- data.frame(dat.yearly, Ones=rep(1, times=nrow(dat.yearly)))
dat$Distribution <- as.character(dat$Distribution)

# measure temporal duration of each distro
duration <- ddply(.data=dat,
                  .variables=.(Distribution),
                  .fun=summarize,
                  Duration = length(Ones))

# merge the two frames
dat <- merge(dat, duration, by="Distribution")

# subset the main data to only contain distributions with 
# > 10 years of duration
dat.T <- dat[dat$Duration >= 10, ]

# Temporal means and variances in the three data sources

# Distrowatch
DW <- ddply(.data=dat.T,
            .variables=.(Distribution),
            .fun=summarize,
            Mean=mean(H.P.D.),
            Var=var(H.P.D.),
            Source = "DistroWatch")

# Squid coutner
SQ1 <- ddply(.data=dat.squid.1,
            .variables=.(Distro),
            .fun=summarize,
            Mean=mean(Count),
            Var=var(Count),
            Source="Wikipedia 2010-2011")

# Squid counter
SQ2 <- ddply(.data=dat.squid.2,
             .variables=.(Distro),
             .fun=summarize,
             Mean=mean(Count),
             Var=var(Count),
             Source="Wikipedia 2012-2014")

names(DW) = names(SQ1) 

Taylor <- rbind(DW, SQ1, SQ2)


# PLOT THE TAYLOR'S POWER LAW 

T.plot <- ggplot(data=Taylor, aes(x=log10(Mean), y=log10(Var))) +
  geom_abline(intercept=0.5, slope=1, col="grey", linetype=2) +
  geom_abline(intercept=0.5, slope=2, col="grey", linetype=2) +
  geom_smooth(aes(colour=Source), method="lm", se=FALSE, size=0.5) + 
  geom_point(aes(colour=Source, shape=Source), size=2, alpha=0.6) +
  labs(x = expression(paste(log[10], " Mean")),
       y = expression(paste(log[10], " Variance")))+
  theme_bw()

# export the figure to a file
pdf("../figures/Taylor.pdf", width=5, height=3.2)
 print(T.plot)
dev.off()


# *-----------------------------------------------------------------------------
# Get TPL parameter estimates using linear regression in a log-log space

library(plyr)
library(dplyr)

# function that applies the linear regression model to each dataset in the 
# X dataframe (produced above)
my.lm <- function(X)
{
  summary(lm(log(Var) ~ log(Mean), data=X)  )
}

# apply the function
dlply(.data=Taylor,
      .variables = "Source",
      .fun = my.lm)






