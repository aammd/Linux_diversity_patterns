# ---------------------------------------------------------------------
# Author: Petr Keil
# 2016-05-03 13:31:37 CEST
# pkeil@seznam.cz
# ---------------------------------------------------------------------

# Loading the necessary libraries
library(readxl)

# Loading the trait data
traits_sheet <- read_excel("data/Linux_traits.xlsx",
                           col_types = c("text", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", 
                                         "text", "text", "text", "text"), 
                           na = c("NA"))

# For simplicity, we will work with a `dat` object
dat <- traits_sheet

# remove NAs
dat <- dat[is.na(dat$Packages) == FALSE,]
dat <- dat[is.na(dat$Scope_width) == FALSE,]
dat <- dat[is.na(dat$Scope_special) == FALSE,]

# do logarithms prior to analyses (sometimes it makes things easier)
dat$logHPD <- log10(dat$HPD)
dat$logPackages <- log10(dat$Packages)



# ---------------------------------------------------------------------
# THE FIGURE
# ---------------------------------------------------------------------

# Initialize .pdf output
pdf(file = "figures/niche_breath.pdf", width=10, height=3.5)
par(mfrow = c(1,3), bty="l")

##############################################
##### Panel A - HPD vs # of PACKAGES #########
##############################################

plot(log10(HPD)~log10(Packages), 
     data=dat, col=rgb(0,0,0,alpha=0.5) ,
     xlab=expression(paste(log[10], " # packages")), 
     ylab=expression(paste(log[10], " HPD")) )
mtext("A", adj=-0.14, font=2)

# Fitting normal linear model (a.k.a. ordinary least-square regression)
m1 <- lm(logHPD~logPackages, data = dat)
summary(m1)

# Predictions of the model
newdata <- data.frame(logPackages =seq(1, 5, by=0.1))
preds <- predict(m1, newdata=newdata, se.fit=TRUE )

# Plotting the predictions
lines(newdata$logPackages, preds$fit, col="red", lwd=2)
lines(newdata$logPackages, preds$fit + preds$se.fit , 
      col="red", lwd=1, lty=2)
lines(newdata$logPackages, preds$fit - preds$se.fit , 
      col="red", lwd=1, lty=2)

##################################################
##### Panel B - HPD vs # of applications #########
##################################################

plot(log10(HPD) ~ jitter(Scope_width, factor=0.5), 
     data=dat, col=rgb(0,0,0,alpha=0.5) ,
     xlab="# applications", 
     ylab=expression(paste(log[10], " HPD"))  )
mtext("B", adj=-0.14, font=2)

# Fitting linear model of increasing complexity
m0 <- lm(logHPD ~ 1, data = dat) # the 'null' model
m1 <- lm(logHPD~Scope_width, data = dat)
m2 <- lm(logHPD~poly(Scope_width, 2), data=dat)
m3 <- lm(logHPD~poly(Scope_width, 3), data=dat)

# Model selection using AIC
AIC(m0, m1, m2, m3)
summary(m1) 

# Predictions of the best model
newdata <- data.frame(Scope_width =seq(1, 6, by=0.1))
preds <- predict(m1, newdata=newdata, se.fit=TRUE )
# Plotting the predictions
lines(newdata$Scope_width, preds$fit, col="red", lwd=2)
lines(newdata$Scope_width, preds$fit + preds$se.fit , 
      col="red", lwd=1, lty=2)
lines(newdata$Scope_width, preds$fit - preds$se.fit , 
      col="red", lwd=1, lty=2)

##############################################################
##### Panel C - HPD vs # of specialized applications #########
##############################################################

plot(log10(HPD) ~ jitter(Scope_special, factor=0.5), 
     data=dat, col=rgb(0,0,0,alpha=0.5) ,
     xlab="# specialized applications", 
     ylab=expression(paste(log[10], " HPD"))   )
mtext("C", adj=-0.14, font=2)

# Fitting linear model of increasing complexity
m0 <- lm(logHPD ~ 1, data = dat) # the 'null' model
m1 <- lm(logHPD~Scope_special, data = dat)
m2 <- lm(logHPD~poly(Scope_special, 2), data=dat)
m3 <- lm(logHPD~poly(Scope_special, 3), data=dat)

# Model selection using AIC
AIC(m0, m1, m2, m3)
summary(m1)

# SINCE NO MODEL PERFORMED WELL (ACCORGING TO AIC), WE
# ARE NOT PLOTTING THE PREDICTIONS IN PANEL C
# newdata <- data.frame(Scope_special =seq(0, 5, by=0.1))
# preds <- predict(m1, newdata=newdata, se.fit=TRUE )
# lines(newdata$Scope_special, preds$fit, col="red", lwd=2)
# lines(newdata$Scope_special, preds$fit + preds$se.fit , 
#       col="red", lwd=1, lty=2)
# lines(newdata$Scope_special, preds$fit - preds$se.fit , 
#       col="red", lwd=1, lty=2)


# Close the .pdf output
dev.off()




# ---------------------------------------------------------------
# SIMPLE BOXPLOT SHOWING POPULARITY BY PARENT DISTRIBUTION
# ---------------------------------------------------------------

# Note: This is not directly relevant to the project, but
#       I find it interesting nevertheless.

pdf(file="figures/parents.pdf", width=15, height=5)
boxplot(log10(HPD) ~ Parent_simple, data=dat, 
        ylab="log_10 HPD", col="grey",
        main="Popularity by parent distributions",
        at=rank(tapply(dat$HPD, dat$Parent_simple, median)))
abline(a=mean(log10(dat$HPD)), b=0, lty=1, col="grey")
dev.off()




