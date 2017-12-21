# ------------------------------------------------------------------------------
# Description: Here we calculate the relationships between niche breadth
# and the three different measures of niche breadth.
#
# Author: Petr Keil
# pkeil@seznam.cz
# ------------------------------------------------------------------------------

# Loading the necessary libraries

library(readxl)
library(gridExtra)
library(ggplot2)

# Loading the trait data

traits_sheet <- read_excel("../data/Linux_traits.xlsx",
                           col_types = c("text", 
                                         "numeric", "numeric","numeric", "numeric", 
                                         "numeric", "numeric", 
                                         "text", "text", "text", "text"), 
                           na = c("NA"))

# ------------------------------------------------------------------------------
# DATA PROCESSING

# For simplicity, we will work with a `dat` object
dat <- traits_sheet

# remove NAs
dat <- dat[is.na(dat$Packages) == FALSE,]
dat <- dat[is.na(dat$Scope_width) == FALSE,]
dat <- dat[is.na(dat$Scope_special) == FALSE,]

hist(log10(dat$Packages))

# do logarithms prior to analyses (sometimes it makes things easier)
dat$logHPD <- log(dat$HPD)
dat$logPackages <- log(dat$Packages)

# count number of applications
library(plyr)
Scope <- dat$Scope
a <- Scope[1]

# function that parses the scopes from a string to a vector
scope.parser <- function(scope.string)
{
  parsed <- strsplit(scope.string, ",")
  parsed <- gsub(pattern=" ", x=parsed[[1]], replacement="")
  return(parsed)  
}

# apply the function to the scope data
splitted.list <- lapply(X=Scope, FUN=scope.parser)
splitted.data <- unlist(splitted.list)

# extract the categories list that will be copy-pasted directly to the ms text
categories <- unique(sort(splitted.data))
categories.for.paper <- paste(categories[], collapse=", ")
categories.for.paper

# ---------------------------------------------------------------------
# THE FIGURE
# ---------------------------------------------------------------------

####################################
##### HPD vs # of PACKAGES #########
####################################

# remove distros with package lists that come from repositories (BSD, Debian)
dat2 <- dat[dat$Packages < 8000,]

pA <- ggplot(dat2, aes(x=Packages, y=HPD)) +
  geom_point(alpha=0.5) +
  geom_smooth(method="glm", method.args=list(family="quasipoisson"), colour="red") +
  scale_y_log10() + scale_x_log10() +
  labs(title="(a)", 
       x="no. of packages", 
       y="HPD", 
       subtitle="Source: DistroWatch") +
  theme_bw()

pD <- ggplot(dat2, aes(x=Packages, y=LinuxCounter)) +
  geom_point(alpha=0.5) +
  scale_y_log10() + scale_x_log10() +
  labs(title="(d)", 
       x="no. of packages", 
       y="no. of machines", 
       subtitle="Source: LinuxCounter") +
  theme_bw()

# Fitting the model
summary( glm(HPD~logPackages, data = dat2, family="quasipoisson"))
summary( glm(LinuxCounter~logPackages, data = dat2, family="quasipoisson"))


# Standardized Pearson correlation
cor(scale(dat2$logHPD), scale(dat2$logPackages))

########################################
##### HPD vs # of applications #########
########################################

pB <- ggplot(dat, aes(x=jitter(Scope_width, factor=0.5), y=HPD)) +
  geom_point(alpha=0.5) +
  geom_smooth(method="glm", method.args=list(family="quasipoisson"), colour="red") +
  scale_y_log10() + 
  labs(title="(b)", 
       x="no. of applications", 
       y="HPD", 
       subtitle="Source: DistroWatch") +
  theme_bw()

pE <- ggplot(dat2, aes(x=jitter(Scope_width, factor=0.5), y=LinuxCounter)) +
  geom_point(alpha=0.5) +
 # geom_smooth(method="glm", method.args=list(family="quasipoisson"), colour="black") +
  scale_y_log10() + #scale_x_log10() +
  labs(title="(e)", 
       x="no. of applications", 
       y="no. of machines", 
       subtitle="Source: LinuxCounter") +
  theme_bw()
pE

# Fitting models
summary( glm(HPD~Scope_width, data = dat, family="quasipoisson"))
summary( glm(LinuxCounter~Scope_width, data = dat, family="quasipoisson"))



# Standardized Pearson correlation
cor(scale(dat$logHPD), scale(dat$Scope_width))

####################################################
##### HPD vs # of specialized applications #########
####################################################

pC <- ggplot(dat, aes(x=jitter(Scope_special, factor=0.5), y=HPD)) +
  geom_point(alpha=0.5) +
  #geom_smooth(method="glm", method.args=list(family="quasipoisson"), colour="black") +
  scale_y_log10() + 
  labs(title="(c)", 
       x="no. of special applications", 
       y="HPD", 
       subtitle="Source: DistroWatch") +
  theme_bw()

pF <- ggplot(dat, aes(x=jitter(Scope_special, factor=0.5), y=LinuxCounter)) +
  geom_point(alpha=0.5) +
  #geom_smooth(method="glm", method.args=list(family="quasipoisson"), colour="black") +
  scale_y_log10() + 
  labs(title="(f)", 
       x="no. of special applications", 
       y="no. of machines", 
       subtitle="Source: LinuxCounter") +
  theme_bw()


# Fitting model s
summary( glm(HPD~Scope_special, data = dat, family="quasipoisson"))
summary( glm(LinuxCounter~Scope_special, data = dat, family="quasipoisson"))



#  -----------------------------------------------------------------------------

# export the figure to a file
pdf(file = "../figures/Figure_2.pdf", width=8, height=6)
grid.arrange(pA, pB, pC, pD, pE, pF,  ncol=3, nrow=2)
dev.off()






