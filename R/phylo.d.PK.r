phylo.d.PK <- function (data, phy, names.col, binvar, permut = 1000, rnd.bias = NULL) 
{
  if (!missing(data)) {
    if (!inherits(data, "comparative.data")) {
      if (missing(names.col)) 
        stop("names column is missing")
      names.col <- deparse(substitute(names.col))
      data <- caicStyleArgs(data = data, phy = phy, names.col = names.col)
    }
  }
  #binvar <- deparse(substitute(binvar))
  bininds <- match(binvar, names(data$data))
  if (is.na(bininds)) 
    (stop("'", binvar, "' is not a variable in data."))
  ds <- data$data[, bininds]
  if (any(is.na(ds))) 
    stop("'", binvar, "' contains missing values.")
  if (is.character(ds)) 
    ds <- as.factor(ds)
  if (length(unique(ds)) > 2) 
    stop("'", binvar, "' contains more than two states.")
  if (length(unique(ds)) < 2) 
    stop("'", binvar, "' only contains a single state.")
  propStates <- unclass(table(ds))
  propState1 <- propStates[1]/sum(propStates)
  names(dimnames(propStates)) <- binvar
  if (is.factor(ds)) 
    ds <- as.numeric(ds)
  if (!is.numeric(permut)) 
    (stop("'", permut, "' is not numeric."))
  if (!is.null(rnd.bias)) {
    rnd.bias <- deparse(substitute(rnd.bias))
    rnd.ind <- match(rnd.bias, names(data$data))
    if (is.na(rnd.ind)) 
      (stop("'", rnd.bias, "' is not a variable in data."))
    rnd.bias <- data$data[, rnd.bias]
  }
  el <- data$phy$edge.length
  elTip <- data$phy$edge[, 2] <= length(data$phy$tip.label)
  if (any(el[elTip] == 0)) 
    stop("Phylogeny contains pairs of tips on zero branch lengths, cannot currently simulate")
  if (any(el[!elTip] == 0)) 
    stop("Phylogeny contains zero length internal branches. Use di2multi.")
  ds.ran <- replicate(permut, sample(ds, prob = rnd.bias))
  if (is.null(data$vcv)) {
    vcv <- VCV.array(data$phy)
  }
  else {
    vcv <- data$vcv
  }
  ds.phy <- rmvnorm(permut, sigma = unclass(vcv))
  ds.phy <- as.data.frame(t(ds.phy))
  ds.phy.thresh <- apply(ds.phy, 2, quantile, propState1)
  ds.phy <- sweep(ds.phy, 2, ds.phy.thresh, "<")
  ds.phy <- as.numeric(ds.phy)
  dim(ds.phy) <- dim(ds.ran)
  ds.ran <- cbind(Obs = ds, ds.ran)
  ds.phy <- cbind(Obs = ds, ds.phy)
  dimnames(ds.ran) <- dimnames(ds.phy) <- list(data$phy$tip.label, 
                                               c("Obs", paste("V", 1:permut, sep = "")))
  phy <- reorder(data$phy, "pruningwise")
  ds.ran.cc <- contrCalc(vals = ds.ran, phy = phy, ref.var = "V1", 
                         picMethod = "phylo.d", crunch.brlen = 0)
  ds.phy.cc <- contrCalc(vals = ds.phy, phy = phy, ref.var = "V1", 
                         picMethod = "phylo.d", crunch.brlen = 0)
  ransocc <- colSums(ds.ran.cc$contrMat)
  physocc <- colSums(ds.phy.cc$contrMat)
  if (round(ransocc[1], digits = 6) != round(physocc[1], digits = 6)) 
    stop("Problem with character change calculation in phylo.d")
  obssocc <- ransocc[1]
  ransocc <- ransocc[-1]
  physocc <- physocc[-1]
  soccratio <- (obssocc - mean(physocc))/(mean(ransocc) - mean(physocc))
  soccpval1 <- sum(ransocc < obssocc)/permut
  soccpval0 <- sum(physocc > obssocc)/permut
  dvals <- list(DEstimate = soccratio, Pval1 = soccpval1, Pval0 = soccpval0, 
                Parameters = list(Observed = obssocc, MeanRandom = mean(ransocc), 
                                  MeanBrownian = mean(physocc)), StatesTable = propStates, 
                Permutations = list(random = ransocc, brownian = physocc), 
                NodalVals = list(observed = ds.ran.cc$nodVals[, 1, drop = FALSE], 
                                 random = ds.ran.cc$nodVals[, -1, drop = FALSE], brownian = ds.phy.cc$nodVals[, 
                                                                                                              -1, drop = FALSE]), binvar = binvar, data = data, 
                nPermut = permut, rnd.bias = rnd.bias)
  class(dvals) <- "phylo.d"
  return(dvals)
}
