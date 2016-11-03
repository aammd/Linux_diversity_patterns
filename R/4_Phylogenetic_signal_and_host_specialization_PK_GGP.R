# ANALYSIS OF PHYLOGENETIC SIGNAL IN DEBIAN PACKAGES

# Authors:
# Petr Keil, pkeil@seznam.cz
# Gabriel Garcia Pena (the host specialization part)

library(ape)
library(dplyr)
library(plyr)
library(tidyr)
library(vegan) # for beta diversity indices

# ------------------------------------------------------------------------------
# read the Debian tree
  Deb.phylo <- readLines("data/Debian.newick") %>% 
               paste0(., sep=";") %>% 
               read.tree(text=.)

# show the phylogeny
  plot(Deb.phylo)

# show tip labels of the phylogeny
  sort(Deb.phylo$tip.label)

# correct the distro names that have confusing characters such as "/"
  Deb.phylo$tip.label[Deb.phylo$tip.label=="A/V"] <- "AV"

# read the trait files
  Deb.distros <- list.files("data/Debian_traits")

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
    paclist <- read.csv(file=paste("data/Debian_traits/", distro, sep=""), 
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
  
  ## Write out debian trait data ---
  
  readr::write_csv(Deb.data, "data/debian_trait_data.csv")    
  
  
    
  rownames(Deb.data) <- Deb.data$package
  Deb.data <- Deb.data[,-1]
  Deb.data <- t(Deb.data)
  Deb.data[1:10, 1:10]
  
  image(as.matrix(Deb.data))
  


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

  # read results of the specialist/generalist analysis 
  D <- read.csv("data/specialists_vs_generalists.csv")
  
  
pdf("figures/Figure_4.pdf", width=9.5, height=7)
  layout(matrix(c(1,2,4,1,3,5), nrow=2, ncol=3, byrow=TRUE))
 
  par(mai=c(0.1,0.1,0.3,0.1))
  plot(Deb.phylo.trimmed, cex=1.05)
  mtext("A", adj=0, padj=-0.5, font=2)
  
  par(bty="l", mai=c(1,0.8,0.3,0.1))
  plot(Deb.phylo.dist, Deb.beta.sim, col=rgb(0,0,0,alpha=0.2), 
       pch=19, las=1, ylim=c(0,1),
       xlab="Phylogenetic distance [years * 2]",
       ylab=~beta[sim])
  legend("bottomright", legend=paste("Rho =", r.sim, "**"),
         bg="white" )
  mtext("B", adj=-0.14, padj=-0.5, font=2)
 
  barplot(table(D$host.specialist), col="grey", border=NA,
          names=c("Host generalists", "Host specialists"),
          ylab="# of packages")
  mtext("D", adj=-0.2, padj=-0.5, font=2)
  
  plot(Deb.phylo.dist, Deb.beta.j, col=rgb(0,0,0,alpha=0.2), 
       pch=19, las=1, ylim=c(0,1),
       xlab="Phylogenetic distance [years * 2]",
       ylab=~beta[Jaccard])
  legend("bottomright", legend=paste("Rho =", r.j, "***"), 
         bg="white")
  mtext("C", adj=-0.14, padj=-0.5, font=2)
dev.off()

system("
convert -density 150 figures/Figure_4.pdf figures/Figure_4.png
       ")


# ------------------------------------------------------------------------------
# PHYLOGENETIC GENERALIST or SPECIALIST

par(cex=0.5, mai=c(1,3,3,1))
Imagine(Deb.data, fill=F, xlab="", ylab="",
        col=c("beige", "darkslateblue"))


# TEST FOR THE PHYLOGENETIC SIGNAL (HOST SPECIALIZATION) OF EACH PACKAGE-
# classification is based on D$mpd.obs.p=<0.05)
# a Z-test on whether mpd.obs = mpd.rand.mean
# TAKES ABOUT 10min TO CALCULATE!!
X<-ses.mpd(t(com), cophenetic(Deb.phylo.trimmed), 
           null.model="phylogeny.pool", abundance.weighted=FALSE)
D<-data.frame(package.name=colnames(com), X)
D$host.specialist<-ifelse(D$mpd.obs.p>=0.05, 0,1)

write.csv(D, "data/specialists_vs_generalists.csv", row.names=FALSE)

pdf("figures/host.specialist.packgs.pdf", width=8, height=8)
layout(matrix(c(1,2,1,3), nrow=1, ncol=1, byrow=TRUE))

  par(mai=c(0,0,0.5,0), cex=1)
  tree<-Deb.phylo.trimmed
  plot(tree, cex=0.7, type="c", use.edge.length=F,label.offset=1)
  par(cex=0.7, font=1)
  title("phylogeny of distros")
  grid(nx=20, ny=20, col="lightsteelblue", lty="dotted")
  
  par(cex=0.5, mai=c(1,3,3,1))
  Imagine(Deb.data, fill=F, xlab="", ylab="",
          col=c("beige", "darkslateblue"))
  
  par(cex=1, mai=c(1,1,1,1))
  barplot(table(D$host.specialist), col="firebrick", 
          names=c("generalist, p > 0.05", "specialist, p < 0.05"))
  par(cex=0.9, font=2)
  title("packages")
  
dev.off()

  




