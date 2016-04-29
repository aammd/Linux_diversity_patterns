
library(readxl)
traits_sheet <- read_excel("data/Linux_traits.xlsx",
                           col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "numeric"), na = c("NA"))

dat <- traits_sheet

dat$Scope <- as.character(dat$Scope)
head(dat)

# remove NAs
dat <- dat[is.na(dat$Packages) == FALSE,]

pdf(file = "figures/niche_breath.pdf", width=10, height=3.5)
par(mfrow = c(1,3))

# ---------------------------------------------------------------------
##### HPD vs # of PACKAGES #########
plot(log10(HPD)~log10(Packages), data=dat, frame=FALSE,
     xlab="log_10 # packages", ylab="log_10 HPD")
mtext("A", adj=-0.14, font=2)

dat$logHPD <- log10(dat$HPD)
dat$logPackages <- log10(dat$Packages)

#m1 <- loess(logHPD~logPackages)
m1 <- lm(logHPD~logPackages, data = dat)
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

hpd_scope <- dat[names(dat) %in% c("logHPD", "Scope_width")]

m0 <- lm(logHPD ~ 1, data = hpd_scope)
m1 <- lm(logHPD~Scope_width, data = hpd_scope)
m2 <- lm(logHPD~poly(Scope_width, 2), data=na.omit(hpd_scope))
m3 <- lm(logHPD~poly(Scope_width, 3), data=na.omit(hpd_scope))

AIC(m0, m1, m2, m3)

newdata <- data.frame(Scope_width =seq(1, 5, by=0.1))
preds <- predict(m1, newdata=newdata, se.fit=TRUE )
lines(newdata$Scope_width, preds$fit, col="red", lwd=2)
lines(newdata$Scope_width, preds$fit + preds$se.fit , 
      col="red", lwd=1, lty=2)
lines(newdata$Scope_width, preds$fit - preds$se.fit , 
      col="red", lwd=1, lty=2)

# ---------------------------------------------------------------

plot(log10(HPD) ~ jitter(Scope_special, factor=0.5), data=dat, frame=FALSE,
     xlab="# specialized applications", ylab="log_10 HPD")
mtext("C", adj=-0.14, font=2)

# ScopeSpec <- dat$Scope_width

hpd_spec <- dat[names(dat) %in% c("logHPD", "Scope_special")]

m0 <- lm(logHPD ~ 1, data = hpd_spec)
m1 <- lm(logHPD~Scope_special, data = hpd_spec)
m2 <- lm(logHPD~poly(Scope_special, 2), data=na.omit(hpd_spec))
m3 <- lm(logHPD~poly(Scope_special, 3), data=na.omit(hpd_spec))

AIC(m0, m1, m2, m3)

newdata <- data.frame(Scope_special =seq(0, 5, by=0.1))
preds <- predict(m1, newdata=newdata, se.fit=TRUE )
lines(newdata$Scope_special, preds$fit, col="red", lwd=2)
lines(newdata$Scope_special, preds$fit + preds$se.fit , 
      col="red", lwd=1, lty=2)
lines(newdata$Scope_special, preds$fit - preds$se.fit , 
      col="red", lwd=1, lty=2)

dev.off()

summary(lm(log10(HPD)~Scope_width, data=dat))


# ---------------------------------------------------------------

pdf(file="figures/parents.pdf", width=43, height=5)
boxplot(log10(HPD) ~ Parent, data=dat, 
        ylab="log_10 HPD", col="grey")
dev.off()




