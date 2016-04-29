

dat <- read.csv("Linux_traits.csv")
dat$Scope <- as.character(dat$Scope)
head(dat)

# remove NAs
dat <- dat[is.na(dat$Packages) == FALSE,]

pdf(file="niche_breath.pdf", width=10, height=3.5)
par(mfrow=c(1,3))

# ---------------------------------------------------------------------
##### HPD vs # of PACKAGES #########
plot(log10(HPD)~log10(Packages), data=dat, frame=FALSE,
     xlab="log_10 # packages", ylab="log_10 HPD")
mtext("A", adj=-0.14, font=2)

logHPD <- log10(dat$HPD)
logPackages <- log10(dat$Packages)

#m1 <- loess(logHPD~logPackages)
m1 <- lm(logHPD~logPackages)
newdata <- data.frame(logPackages =seq(1, 5, by=0.1))
preds <- predict(m1, newdata=newdata, se.fit=TRUE )
lines(newdata$logPackages, preds$fit, col="red", lwd=2)
lines(newdata$logPackages, preds$fit + preds$se.fit , 
      col="red", lwd=1, lty=2)
lines(newdata$logPackages, preds$fit - preds$se.fit , 
      col="red", lwd=1, lty=2)




# summary(lm(log10(HPD)~log10(Packages), data=dat))


# ---------------------------------------------------------------------

plot(log10(HPD) ~ jitter(Scope_width, factor=0.5), data=dat, frame=FALSE,
     xlab="# applications", ylab="log_10 HPD")
mtext("B", adj=-0.14, font=2)

ScopeWidth <- dat$Scope_width

m0 <- lm(logHPD ~ 1)
m1 <- lm(logHPD~ScopeWidth)
m2 <- lm(logHPD~poly(ScopeWidth, 2), data=na.omit(dat))
m3 <- lm(logHPD~poly(ScopeWidth, 3), data=na.omit(dat))

AIC(m0, m1, m2, m3)

newdata <- data.frame(ScopeWidth =seq(1, 5, by=0.1))
preds <- predict(m1, newdata=newdata, se.fit=TRUE )
lines(newdata$ScopeWidth, preds$fit, col="red", lwd=2)
lines(newdata$ScopeWidth, preds$fit + preds$se.fit , 
      col="red", lwd=1, lty=2)
lines(newdata$ScopeWidth, preds$fit - preds$se.fit , 
      col="red", lwd=1, lty=2)

# ---------------------------------------------------------------

plot(log10(HPD) ~ jitter(Scope_special, factor=0.5), data=dat, frame=FALSE,
     xlab="# specialized applications", ylab="log_10 HPD")
mtext("C", adj=-0.14, font=2)

ScopeSpec <- dat$Scope_width

m0 <- lm(logHPD ~ 1)
m1 <- lm(logHPD~ScopeSpec)
m2 <- lm(logHPD~poly(ScopeSpec, 2), data=na.omit(dat))
m3 <- lm(logHPD~poly(ScopeSpec, 3), data=na.omit(dat))

AIC(m0, m1, m2, m3)

newdata <- data.frame(ScopeSpec =seq(0, 5, by=0.1))
preds <- predict(m1, newdata=newdata, se.fit=TRUE )
lines(newdata$ScopeSpec, preds$fit, col="red", lwd=2)
lines(newdata$ScopeSpec, preds$fit + preds$se.fit , 
      col="red", lwd=1, lty=2)
lines(newdata$ScopeSpec, preds$fit - preds$se.fit , 
      col="red", lwd=1, lty=2)

summary(lm(log10(HPD)~Scope_width, data=dat))
dev.off()

# ---------------------------------------------------------------

pdf(file="parents.pdf", width=43, height=5)
boxplot(log10(HPD) ~ Parent, data=dat, 
        ylab="log_10 HPD", col="grey")
dev.off()




