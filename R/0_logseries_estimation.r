# ------------------------------------------------------------------------------
# Description: Function that fits the logseries and lognormal model to the rank-abundance data.
# Author: Petr Keil, pkeil@sezam.cz, Nov 28 2017
# ------------------------------------------------------------------------------

# Argument: X - vector of the abundances

# Value: a data frame with ranked abunance (Rank), mean abundance from the 
# re-orderings (Mean), and indicator variable for the model used (Model)

# ------------------------------------------------------------------------------

fit.distr <- function(X)
{
  require(sads) # package for fitting of the logseries model
 
  # fit the logseries model
  lsfit <- fitls(X)

  N=length(X)
  J=sum(X)
  reps = 500
  draws.ln <- matrix(0, nrow=N, ncol=reps)
  draws.ls <- matrix(0, nrow=N, ncol=reps)

  # draw multiple realizations of the model, rank them, and take the rank
  # averages
  for(i in 1:reps)
  {
    message(i)
    draws.ln[,i] <- sort(rlnorm(N, meanlog=mean(log(X)), 
                             sdlog=sqrt(var(log(X)))))
    draws.ls[,i]  <- sort(sads::rls(n=N, N=J, alpha=lsfit@coef))
  }
  
  # calcluate the means and put them to a data frame
  means.ln <- data.frame(Mean=sort(rowMeans(draws.ln), decreasing=TRUE), 
                            Model="Lognormal", Rank = 1:N)

  means.ls <- data.frame(Mean=sort(rowMeans(draws.ls), decreasing=TRUE), 
                         Model="Logseries", Rank = 1:N)

  res <- rbind(means.ln, means.ls)
  return(res)
}





