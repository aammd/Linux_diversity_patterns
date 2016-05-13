# ---------------------------------------------------------------------
# Author: Petr Keil
# 2016-05-03 13:31:37 CEST
# pkeil@seznam.cz
# ---------------------------------------------------------------------


# DATA AND PACKAGES

library(plyr)

dat.yearly <- read.csv("data/linux_yearly_data.csv")
dat.recent <- read.csv("data/popularity_last_clean_12.csv")

# DEFINING GGPLOT-LIKE COLORS
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
my.red <- gg_color_hue(4)[1]
my.blue <- gg_color_hue(4)[3]

# THE FIGURE ---------------------------------------------------------

# Initialize .pdf output
pdf(file="figures/abundance_distribution.pdf", width=10, height=3.5)
par(mfrow=c(1,3), bty="l")


##############################################
##### Panel A - Rank-abundance curve #########
##############################################

plot(sort(dat.recent$HPD, decreasing=TRUE), 
     xlab="Rank", ylab="HPD", pch=19, col="darkgrey", las=1)
mtext("A", adj=-0.18,  font=2)

# plot the theoretical log-normal distribution
N=length(dat.recent$HPD)
draws <- matrix(0, nrow=N, ncol=1000)
for(i in 1:ncol(draws))
{
  draws[,i]  <- sort(rlnorm(N, meanlog=mean(log(dat.recent$HPD)), 
                               sdlog=sqrt(var(log(dat.recent$HPD)))))
}
means <- sort(rowMeans(draws), decreasing=TRUE)
lines(means, col=my.red, cex=0.8, lwd=1.5, lty=1)

# Optional 95% quantiles
# low95 <- sort(apply(draws, 1, quantile, probs=0.025), decreasing=TRUE)
# up95 <- sort(apply(draws, 1, quantile, probs=0.975), decreasing=TRUE)
# lines(up95, col="red", cex=0.8, lwd=1, lty=2)
# lines(low95, col="red", cex=0.8, lwd=1, lty=2)



#############################################
##### Panel B - Abundance histogram #########
#############################################

hist(dat.recent$HPD, col="darkgrey", main="", 
     xlab=expression(paste("HPD")), freq=FALSE , breaks=40,
     border = "lightgrey", las=1, ylim=c(0, 0.007))
mtext("B", adj=-0.18, font=2)


# plot the theoretical log-normal distribution
x <- seq(0, max(dat.recent$HPD))
y <- dlnorm(x, meanlog=mean(log(dat.recent$HPD)), sdlog=sqrt(var(log(dat.recent$HPD))))
lines(x, y, col=my.red, lwd=1.5)




# show lognormal curve


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
# > 10 years of duration
dat.T <- dat[dat$Duration >= 10, ]

# Temporal variances
vars <- ddply(.data=dat.T,
              .variables=.(Distribution),
              .fun=summarize,
              Var=var(H.P.D.))

# Temporal means
means <- ddply(.data=dat.T,
               .variables=.(Distribution),
               .fun=summarize,
               Mean=mean(H.P.D.))

MV <- merge(means, vars, by="Distribution")
nrow(MV)

# Fit linear regression in log-log to estimate TPL slope
TPL <- lm(log(Var) ~ log(Mean), data=MV)
summary(TPL)

# Plot everything

plot(log(Var) ~ log(Mean), data=MV, 
     xlab=expression(paste(log, " Mean HPD")),
     ylab=expression(paste(log, " Variance of HPD")), las=1,
     col="darkgrey", pch=19)
a <- round(coef(TPL)[1], 2)
b <- round(coef(TPL)[2], 2)



# add the regression line
abline(TPL, lwd=1.5, col=my.blue)

# Insert TPL equation
legend("topleft", legend=paste("y = ", a, " + ", b, "x"), 
  box.col=NA, text.col=my.blue, cex=1.2)


mtext("C", adj=-0.18, font=2)


# Close the .pdf output
dev.off()




### PLOT LOG-TRANSFORMED HISTOGRAM
pdf("figures/log_abund.pdf", width=3, height=3)
par(mai=c(0.8,0.1,0.1,0.1))
hist(log(dat.recent$HPD), col="darkgrey", main="", 
     xlab=expression(paste(log, " HPD")), freq=FALSE , breaks=20,
     border = "lightgrey", las=1, axes=FALSE, ylab=NA)
axis(side=1)
# plot the theoretical log-normal distribution
x <- seq(0, 9, by=0.1)
y <- dnorm(x, mean=mean(log(dat.recent$HPD)), sd=sqrt(var(log(dat.recent$HPD))))
lines(x, y, col=my.red, lwd=1.5)
dev.off()

