# -------------------------------------------------------------------------------
# Description: Here we analyze the phylogenetic singal in Linux software
# packages.

# Author:
# Petr Keil, pkeil@seznam.cz
# ------------------------------------------------------------------------------

library(ape)
library(dplyr)
library(plyr)
library(tidyr)
library(vegan) # for beta diversity indices
library(ggtree)
library(gridExtra)

# ------------------------------------------------------------------------------
# read the Debian tree
  Deb.phylo <- readLines("../data/Debian.newick") %>% 
               paste0(., sep=";") %>% 
               read.tree(text=.)

# show the phylogeny
  plot(Deb.phylo)

# show tip labels of the phylogeny
  sort(Deb.phylo$tip.label)

# correct the distro names that have confusing characters such as "/"
  Deb.phylo$tip.label[Deb.phylo$tip.label=="A/V"] <- "AV"

# read the trait files
  Deb.distros <- list.files("../data/Debian_traits")

# trim the phylogenty so that it only contains the distros for which we have 
# the data
  to.be.dropped <- Deb.phylo$tip.label[Deb.phylo$tip.label %in% Deb.distros == FALSE]
  Deb.phylo.trimmed <- drop.tip(Deb.phylo, tip=to.be.dropped)

# ------------------------------------------------------------------------------
# function that cuts off the versioning parts of the package names
  shorten.name <- function(long.name)
  {
    short.name <- strsplit(long.name, split=c("^"), fixed=TRUE)[[1]][1]
    short.name <- strsplit(short.name, split=c(":"), fixed=TRUE)[[1]][1]
    short.name <- strsplit(short.name, split="-[[:digit:]]")[[1]][1]
    short.name <- strsplit(short.name, split="[[:digit:]]+.[[:digit:]]")[[1]][1]
    short.name <- gsub(pattern="[[:digit:]]", replacement="", x=short.name)
    short.name <- gsub(pattern=" (", replacement="", x=short.name, fixed=TRUE)
    return(short.name)
  }
        # testing the function on various strings
          shorten.name("linux-headers-3.12.19avl2-pae")
          shorten.name("python-wxgtk2.8")
          shorten.name("libwinpr-interlocked0.1")
          shorten.name("libxapian22v5")
          shorten.name("libxcursor1" )
          shorten.name("libxcb-render-util0")
          shorten.name("conserver-client (")   

# ------------------------------------------------------------------------------
# function that reads a package list for a given distro
# and cleans package names
  read.paclist <- function(distro)
  {
    paclist <- read.csv(file=paste("../data/Debian_traits/", distro, sep=""), 
                        header=FALSE)[,1] %>% 
                        as.character %>%
                        sapply(X=., FUN = shorten.name) %>%
                        unique
    names(paclist) <- NULL
    
    return(paclist)
  }
  
        # test the function with Ubuntu trait data
          read.paclist("Ubuntu")

# ------------------------------------------------------------------------------
# create empty storage list for package files
  Deb.paclists <- list()

# loop through distro names and load the package files
  i <- 1
  for(distro in Deb.distros)
  {
    Deb.paclists[[i]] <- data.frame(package=read.paclist(distro))
    i <- i+1
  }
  names(Deb.paclists) <- Deb.distros
  

# ------------------------------------------------------------------------------
# create the TRAIT x DISTRO binary matrix
  Deb.data <- ldply(Deb.paclists) %>%
                    data.frame(. , ones=rep(1, nrow(.))  ) %>%
                    spread(data=., key=.id, value=ones, fill=0)
    
  rownames(Deb.data) <- Deb.data$package
  Deb.data <- Deb.data[,-1]
  Deb.data <- t(Deb.data)
  Deb.data[1:10, 1:10]
  
# ------------------------------------------------------------------------------  
# REMOVE DEBIAN FROM TRAIT MATRIX
  Deb.data <- Deb.data[rownames(Deb.data)!="Debian",]
  Deb.data <- Deb.data[, colSums(Deb.data) != 0]
  
# REMOVE DEBIAN FROM THE PHYLOGENY  
  Deb.phylo.trimmed <- drop.tip(Deb.phylo.trimmed, tip="Debian")
  
  image(as.matrix(Deb.data))
  
# EXPORT THE FILES
  write.tree(Deb.phylo.trimmed, file="../data/traits_vs_phylogeny/Deb_clean_phylo.newick")
  write.csv(data.frame(DISTRO=rownames(Deb.data), Deb.data), 
            file="../data/traits_vs_phylogeny/Deb_clean_traits.csv",
            row.names = FALSE)
  
# ------------------------------------------------------------------------------  
# package caper and the statistics D
  library(caper)
  
  # remove traits which are in all distros
  Deb.data <- Deb.data[,colSums(Deb.data) != nrow(Deb.data)]
  
  for.caper <- data.frame(NAME=rownames(Deb.data), Deb.data) %>%
               comparative.data(names.col=NAME, phy=Deb.phylo.trimmed)
  
  # modify the function phylo.d so that it does not do the weird states
  # phylo.d.PK <- phylo.d
  # fix(phylo.d.PK)
  source("phylo.d.PK.r")
  
  # example use of one trait
  a <- phylo.d.PK(data=for.caper, binvar="acl")

  # loop through all traits
  res <- list()
  for(i in 1:ncol(Deb.data))
  {
    trait <- names(for.caper$data)[i]
    message(trait); message(i)  
    D <- phylo.d.PK(data=for.caper, binvar=trait, permut=400)
    
    res[[trait]] <- c(D = D$DEstimate, P0=D$Pval0, P1=D$Pval1)
  }

  res2 <- ldply(res, .id="package")
  write.csv(res2, file="../data/D_stats.csv", row.names = FALSE)
  
  # ----------------------------------------------------------------------------
  # plot the results
  # ----------------------------------------------------------------------------
  
  D <- read.csv("../data/D_stats.csv")
  
  # proportion of traits with p <= 0.05
  sum(D$P1 <= 0.05)/nrow(D)
  sum(D$P1 <= 0.10)/nrow(D)
  
  library(ggplot2)
  library(ggExtra)
  p <- ggplot(D, aes(x=D.Obs, y=P1)) + 
       geom_point(alpha=0.2, aes(colour=P1)) +
       geom_vline(xintercept = c(1)) + 
    xlim(c(-12, 12)) +
    theme_bw() +
    labs(x="D", y="P") +
  theme(legend.position = c(0.8, 0.2)) 
       ggMarginal(p, type="histogram")
  
 P.D <- ggplot(D, aes(x=D.Obs)) +
   geom_histogram(fill="darkgrey", binwidth=0.5) +
   geom_vline(xintercept = 1, colour="red") +
   geom_segment(aes(x = -5, y = 1000, xend = -3, yend = 600),
                arrow = arrow(length = unit(0.3, "cm"))) +
   xlim(c(-11,11)) +
   labs(x="D statistic", title="(d)") +
   theme_bw()
       
       
 P.P <- ggplot(D, aes(x=P1)) +
    geom_histogram(fill="darkgrey", binwidth=0.02) +
    geom_vline(xintercept = 0.05, colour="blue") +
    labs(x="P value", title="(e)") +
    theme_bw()

       

       

  
# ------------------------------------------------------------------------------
# create beta diversity distance matrices based on binary trait matrices
  
 # Jaccard dissimilarity:
  Deb.beta.j <- 1 - betadiver(Deb.data, method="j")
# Beta_sim dissimilarity:
  Deb.beta.sim <- betadiver(Deb.data, method="sim")
  plot(Deb.beta.j, Deb.beta.sim)

# ------------------------------------------------------------------------------
# create phylogenetic distance matrix
# and rescale it to years
  Deb.phylo.dist <- cophenetic(Deb.phylo.trimmed)/365

# sort the rows and columns alphabetically
  new.ord <- order(rownames(Deb.phylo.dist))
  Deb.phylo.dist <- as.dist(Deb.phylo.dist[new.ord,new.ord])
  
# ------------------------------------------------------------------------------
# Spearman correlations of trait composition with phylogenetic distance
  r.sim <- cor(Deb.phylo.dist, Deb.beta.sim, method="spearman") %>% round(.,3)
  r.j <- cor(Deb.phylo.dist, Deb.beta.j, method="spearman") %>% round(.,3)
  
# Mantel tests  
  p.sim <- mantel.test(as.matrix(Deb.phylo.dist), as.matrix(Deb.beta.sim), 
                       nperm=9999, graph=FALSE)$p
  p.j <- mantel.test(as.matrix(Deb.phylo.dist), as.matrix(Deb.beta.j), 
                     nperm=9999, graph=FALSE)$p 
  
# ------------------------------------------------------------------------------
# plots of phylogenetic distance vs. trait similarity
 library(ggtree)

# plot the mantel correlatio with beta sim
  
  beta.data <- data.frame(Betasim = as.vector(Deb.beta.sim),
                          Betajac = as.vector(Deb.beta.j),
                          Dist = as.vector(Deb.phylo.dist))
  
  P.betasim <- ggplot(beta.data, aes(x=Dist, y=Betasim)) +
    geom_point(alpha=0.5, shape=21) +
    theme_bw() +
    labs(x="Phylogenetic distance [years * 2]", y=~beta[Sim], title="(b)") +
    annotate(geom="text", label=paste("Rho =", r.sim, "**"), x=8, y=1)
  P.betasim
  
  P.betajac <- ggplot(beta.data, aes(x=Dist, y=Betajac)) +
    geom_point(alpha=0.5, shape=21) +
    theme_bw() +
    labs(x="Phylogenetic distance [years * 2]", y=~beta[Jaccard], title="(c)") +
    annotate(geom="text", label=paste("Rho =", r.j, "**"), x=8, y=1)
  P.betajac
  
  
  # plot the tree:
  P.tree <-  ggtree(Deb.phylo.trimmed) + 
    geom_tiplab(hjust=1, vjust=-.3, size=2.3)+
    labs(title="(a)") 
  
  
  MAT <-  matrix(c(1,2, 3,
                   1,4, 5), nrow=2, ncol=3, byrow=TRUE)
  
pdf("../figures/phylo_signal.pdf", width=10, height=7)
 grid.arrange(P.tree, P.betasim, P.betajac, P.D, P.P,
              layout_matrix=MAT)
dev.off()
  
  