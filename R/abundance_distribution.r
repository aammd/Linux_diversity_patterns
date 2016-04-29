# ---------------------------------------------------------

dat <- read.csv("data/popularity_last_clean_12.csv")

pdf(file="figures/abundance_distribution.pdf", width=10, height=3.5)
par(mfrow=c(1,3))


plot(sort(dat$HPD), xlab="Rank", ylab="HPD", frame=FALSE)
mtext("A", adj=-0.18,  font=2)
#hist(dat$HPD, col="grey", main="", xlab="HPD")
#mtext("b", adj=-0.18, font=2)
hist(log10(dat$HPD), col="grey", main="", 
     xlab=expression(paste(log[10], " HPD")) )
mtext("B", adj=-0.18, font=2)
#dev.off()


library(plyr)
dat <- read.csv("data/linux_yearly_data.csv")
head(dat)

dat <- data.frame(dat, Ones=rep(1, times=nrow(dat)))
dat$Distribution <- as.character(dat$Distribution)



# measure temporal duration of each distro
duration <- ddply(.data=dat,
                  .variables=.(Distribution),
                  .fun=summarize,
                  Duration = length(Ones))
#hist(duration$Duration, col="grey")

# merge the two frames
dat <- merge(dat, duration, by="Distribution")

# subset the main data to only contain distributions with 
# > 8 years of duration
dat8 <- dat[dat$Duration >= 6, ]

vars <- ddply(.data=dat8,
              .variables=.(Distribution),
              .fun=summarize,
              Var=var(H.P.D.))

means <- ddply(.data=dat8,
               .variables=.(Distribution),
               .fun=summarize,
               Mean=mean(H.P.D.))

MV <- merge(means, vars, by="Distribution")


TPL <- lm(log10(Var) ~ log10(Mean), data=MV)

#pdf(file="Taylor_Power_Law.pdf")
plot(log10(Var) ~ log10(Mean), data=MV, frame=FALSE,
     xlab=expression(paste(log[10], " Mean HPD")),
     ylab=expression(paste(log[10], " Variance of HPD")))
a <- round(coef(TPL)[1], 2)
b <- round(coef(TPL)[2], 2)
#title(paste("y = ", a, " + ", b, "x" ), font=1, font.main=1)
legend("topleft", legend=paste("y = ", a, " + ", b, "x" ))

abline(TPL)
mtext("C", adj=-0.18, font=2)

dev.off()

