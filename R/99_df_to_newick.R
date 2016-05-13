## converting to newick format

library(dplyr)
library(readr)
library(picante)

distros <- read_csv("data/distro_time.csv")

glimpse(distros)

## what are the extant ones?

distros %>% 
  filter(Stop == max(Stop))

## ok first step, get their branch lengths

distros %>% 
  mutate(dur = as.numeric(Stop - Start)) %>% 
  filter(Stop == max(Stop)) %>% 
  arrange(Parent) %>% 
  mutate(tip = paste0(Name, ":", dur)) %>% 
  select(-dur) %>% 
  View

## let's work with just decendents of ubuntu

ubuntus <- distros %>% 
  filter(clade == "Debian") %>% 
  filter(Parent == "Ubuntu" | Name == "Ubuntu") %>% 
  mutate(dur = as.numeric(Stop - Start),
         age = as.numeric(max(Stop) - Start)) %>% 
  # filter(Stop == max(Stop)) %>% 
  arrange(Parent) %>% 
  mutate(tip = paste0(Name, ":", dur))

ubuntus %>% 
  arrange(dur) %>% 
  View


## maybe need to join the earliest and the latest together? not sure
## let's first try just combining things in oder 

tips <- ubuntus %>% 
  arrange(age) %>% 
  filter(Parent != "Debian")

## base:shortest
phylo <- "Ubuntu:113"

for (i in 1:(nrow(tips) - 1)) {
  phylo <- paste0(phylo, ",", tips$tip[[i]]) %>% 
    paste0("(", ., "):", tips$age[[i + 1]] - tips$age[[i]])
}

test <- paste0(phylo, "UBUNTU;")
# cat(phylo, file = "ubuntu.tre", sep = "\n")
# ubuntu_tree <- read.tree("ubuntu.tre")

mytree <- read.tree(text = test)
## if you limit to just extant this looks like the Debian symbol! how awesome
pdf(file = "figures/ubuntu_tree.pdf", height = 10, width = 7)
plot(mytree, label.offset = 0.5, cex = 0.6, 
     edge.color = "darkred", type="phylogram", edge.width = 2)
dev.off()

plot(mytree, type = "unrooted", label.offset = 0.5, cex = 0.6, 
     edge.color = "darkred", edge.width = 2)



# make it a function ----------------------------------

## split up a clade based on parents
## do that for each parent

distros %>% 
  filter(clade == "Debian") %>% 
  mutate(age = as.numeric(max(Stop) - Start)) %>% 
  split(., .$Parent)
  
## OK fine, split by parent -- but you need to pair the most recent node with the parent
## have any parents not surivved? probably. in which case, the branch lenght of the parent must be smaller than the descendant. How do we show that? 
## well in such cases its age would be larger than its duration. find the difference between those numbers, and subtract that number from the age of the youngest taxon.
