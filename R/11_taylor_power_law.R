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
hist(duration$Duration, col="grey")

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

pdf(file="Taylor_Power_Law.pdf")
plot(log10(Var) ~ log10(Mean), data=MV, pch=19, frame=FALSE)
a <- round(coef(TPL)[1], 2)
b <- round(coef(TPL)[2], 2)
title(paste("y = ", a, " + ", b, "x" ))
abline(TPL)
dev.off()













