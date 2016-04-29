# this little script is for creating random order at which 
# the distros are sampled
# The probability of each distro being sampled is proportional to its
# HPD, so that distros with high HPD are sampled more likely. This is done
# to cover the entire HPD gradient on log scale

dat <- read.csv("../1_popularity/popularity_last_clean_12.csv")
Rank <- 1:nrow(dat)
RandomOrder <- sample(x=Rank, prob=dat$HPD, replace=FALSE)
dat <- data.frame(dat, Rank, RandomOrder)



write.csv(dat, file="popularity_last_clean_2_sample.csv", row.names=FALSE)
