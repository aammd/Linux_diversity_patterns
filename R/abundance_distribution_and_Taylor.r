# ---------------------------------------------------------------------
# Author: Petr Keil
# 2016-05-03 13:31:37 CEST
# pkeil@seznam.cz
# ---------------------------------------------------------------------


# DATA AND PACKAGES

library(plyr)
dat.yearly <- read.csv("data/linux_yearly_data.csv")
dat.recent <- read.csv("data/popularity_last_clean_12.csv")


# THE FIGURE ---------------------------------------------------------

# Initialize .pdf output
pdf(file="figures/abundance_distribution.pdf", width=10, height=3.5)
par(mfrow=c(1,3), bty="l")


##############################################
##### Panel A - Rank-abundance curve #########
##############################################

plot(sort(dat.recent$HPD), xlab="Rank", ylab="HPD")
mtext("A", adj=-0.18,  font=2)


#############################################
##### Panel B - Abundance histogram #########
#############################################

hist(log10(dat.recent$HPD), col="grey", main="", 
     xlab=expression(paste(log[10], " HPD")) )
mtext("B", adj=-0.18, font=2)


#############################################
##### Panel C - Taylor's Power Law  #########
#############################################

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
# > 6 years of duration
dat8 <- dat[dat$Duration >= 6, ]

# Temporal variances
vars <- ddply(.data=dat8,
              .variables=.(Distribution),
              .fun=summarize,
              Var=var(H.P.D.))

# Temporal means
means <- ddply(.data=dat8,
               .variables=.(Distribution),
               .fun=summarize,
               Mean=mean(H.P.D.))

MV <- merge(means, vars, by="Distribution")

# Fit linear regression in log-log to estimate TPL slope
TPL <- lm(log10(Var) ~ log10(Mean), data=MV)

# Plot everything

plot(log10(Var) ~ log10(Mean), data=MV, 
     xlab=expression(paste(log[10], " Mean HPD")),
     ylab=expression(paste(log[10], " Variance of HPD")))
a <- round(coef(TPL)[1], 2)
b <- round(coef(TPL)[2], 2)

# add the regression line
abline(TPL)

# Insert TPL equation
legend("topleft", legend=paste("y = ", a, " + ", b, "x" ), box.col=NA)


mtext("C", adj=-0.18, font=2)



# Close the .pdf output
dev.off()






