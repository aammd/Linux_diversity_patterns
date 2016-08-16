# ---------------------------------------------------------------------
# Author: Petr Keil
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
dat$logHPD <- log(dat$HPD)
dat$logPackages <- log(dat$Packages)

# DEFINING GGPLOT-LIKE COLORS
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
my.red <- gg_color_hue(4)[1]
my.blue <- gg_color_hue(4)[3]

# COUNT NUMBER OF APPLICATION CATEGORIES
library(plyr)
Scope <- dat$Scope
a <- Scope[1]

scope.parser <- function(scope.string)
{
  parsed <- strsplit(scope.string, ",")
  parsed <- gsub(pattern=" ", x=parsed[[1]], replacement="")
  return(parsed)  
}

splitted.list <- lapply(X=Scope, FUN=scope.parser)
splitted.data <- unlist(splitted.list)
categories <- unique(sort(splitted.data))
categories.for.paper <- paste(categories[], collapse=", ")
categories.for.paper

# ---------------------------------------------------------------------
# THE FIGURE
# ---------------------------------------------------------------------

# Initialize .pdf output
pdf(file = "figures/Figure_2.pdf", width=10, height=3.5)
par(mfrow = c(1,3), bty="l")

##############################################
##### Panel A - HPD vs # of PACKAGES #########
##############################################

plot(log(HPD)~log(Packages), 
     data=dat, col=rgb(0,0,0,alpha=0.3) , pch=19,
     xlab=expression(paste(log, " # packages")), 
     ylab=expression(paste(log, " HPD")), las=1 )
mtext("A", adj=-0.14, font=2)

# Fitting the model
m1 <- glm(HPD~logPackages, data = dat, family="quasipoisson")
summary(m1)

# Standardized Pearson correlation
cor(scale(dat$logHPD), scale(dat$logPackages))

# Predictions of the model
newdata <- data.frame(logPackages =seq(1, 11, by=0.1))
preds <- predict(m1, newdata=newdata, se.fit=TRUE , scale="link" )

# Plotting the predictions
lines(newdata$logPackages, preds$fit, col=my.red, lwd=2)
lines(newdata$logPackages, preds$fit + preds$se.fit , 
      col=my.red, lwd=1, lty=2)
lines(newdata$logPackages, preds$fit - preds$se.fit , 
      col=my.red, lwd=1, lty=2)

##################################################
##### Panel B - HPD vs # of applications #########
##################################################

plot(log(HPD) ~ jitter(Scope_width, factor=0.5), 
     data=dat, col=rgb(0,0,0,alpha=0.3) , pch=19,
     xlab="# applications", las=1,
     ylab=expression(paste(log, " HPD"))  )
mtext("B", adj=-0.14, font=2)

# Fitting model
m1 <- glm(HPD~Scope_width, data = dat, family="quasipoisson")

summary(m1) 

# Standardized Pearson correlation
cor(scale(dat$logHPD), scale(dat$Scope_width))

# Predictions of the best model
newdata <- data.frame(Scope_width =seq(1, 6, by=0.1))
preds <- predict(m1, newdata=newdata, se.fit=TRUE , scale="link")
# Plotting the predictions
lines(newdata$Scope_width, preds$fit, col=my.red, lwd=2)
lines(newdata$Scope_width, preds$fit + preds$se.fit , 
      col=my.red, lwd=1, lty=2)
lines(newdata$Scope_width, preds$fit - preds$se.fit , 
      col=my.red, lwd=1, lty=2)

##############################################################
##### Panel C - HPD vs # of specialized applications #########
##############################################################

plot(log(HPD) ~ jitter(Scope_special, factor=0.5), 
     data=dat, col=rgb(0,0,0,alpha=0.3) , 
     pch=19, las=1, #col="darkgrey",
     xlab="# specialized applications", 
     ylab=expression(paste(log, " HPD"))   )
mtext("C", adj=-0.14, font=2)

# Fitting model 
m1 <- glm(HPD~Scope_special, data = dat, family="quasipoisson")

# Model selection using AIC
summary(m1)

# Close the .pdf output
dev.off()

# convert the .pdf to .png (only works on a Unix machine with 'convert' installed)
system(" cd figures
convert -density 150 Figure_2.pdf Figure_2.png
")


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




