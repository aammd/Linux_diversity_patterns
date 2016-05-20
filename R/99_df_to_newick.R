## converting to newick format

library(dplyr)
library(readr)
library(picante)
library(purrr)

distros <- read_csv("data/distro_time.csv")

## fix goddamned parentheses

distros <- distros %>% 
  mutate(Name = gsub("\\(|\\)", ".", Name),
         Parent = gsub("\\(|\\)", ".", Parent))

glimpse(distros)

## what are the extant ones?

distros %>% 
  filter(Stop == max(Stop))

## ok first step, get their branch lengths

distros %>% 
  mutate(dur = as.numeric(Stop - Start)) %>% 
  arrange(dur) %>% 
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

df_to_newick <- function(dataset, parentdata){
  tips <- dataset %>% 
    arrange(age)
  
  assertthat::assert_that(nrow(parentdata) == 1)
  ## actually put the parent here, with the same branch length as the youngest tip
  ## or maybe more correctly, age of parent - duration of parent , then subtract that number from age of youngest tip.
  ## base:shortest
  parentlife <- parentdata$age - parentdata$dur
  phylo <- paste0(parentdata$Name, ":", tips$age[[1]] - parentlife)
  
  if (nrow(tips) == 1){
    phylo <- paste0(phylo, ",", tips$tip[[1]]) %>% 
      paste0("(", ., "):", parentdata$age - tips$age[[nrow(tips)]])
  } else if (nrow(tips) > 1) {
    for (i in 1:(nrow(tips) - 1)) {
      phylo <- paste0(phylo, ",", tips$tip[[i]]) %>% 
        paste0("(", ., "):", tips$age[[i + 1]] - tips$age[[i]])
    }
    phylo <- paste0("(", phylo, ",", tips$tip[[nrow(tips)]], "):", parentdata$age - tips$age[[nrow(tips)]]) 
    ## then right here, at the end , add : followed by parent age - age (not duration) of oldest offspring
  }
  
  return(phylo)
}

plot_tree <- function(dat, ...){
  
  dat %>% 
    paste0(";") %>% 
    read.tree(text = .) %>% 
    plot(., ...)
}

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


library(stringr)


create_total_phylogeny <- function(stem, .distros){
  split_distros <- .distros %>% 
    filter(clade == stem) %>% 
    mutate(dur = as.numeric(Stop - Start),
           age = as.numeric(max(Stop) - Start)) %>% 
    # filter(Stop == max(Stop)) %>% 
    arrange(Parent, age) %>% 
    mutate(tip = paste0(Name, ":", dur)) %>% 
    split(., .$Parent)
  
  
  
  ## are all parents extant?
  
  split_parents <- .distros %>% 
    filter(Name %in% (split_distros %>% names)) %>% 
    mutate(dur = as.numeric(Stop - Start),
           age = as.numeric(max(Stop) - Start)) %>% 
    # filter(Stop == max(Stop)) %>% 
    arrange(Parent, age) %>% 
    mutate(tip = paste0(Name, ":", dur)) %>% 
    split(., .$Name)
  
  stopifnot(identical(names(split_distros), names(split_parents)))
  
  ### try it out ######
  newicks <- split_distros%>% 
    map2(split_parents, safely(df_to_newick)) %>% 
    transpose %>% 
    simplify_all() %>% 
    compact 
  
  ### fill in the rents
  
  replacers <- split_parents %>% 
    sort_by("age") %>% 
    rev %>% 
    .[-1] 
  
  if (stem == "Redhat") {
    stem <- "Red Hat"
  }
  master <- newicks$result[[stem]]
  
  for (x in replacers) {
    master <- str_replace(master, paste0(",",x$Name), paste0(",",newicks$result[[x$Name]]))
  }
  
  return(master)
  
}


all_trees <- distros$clade %>% 
  unique %>% 
  as.list %>% 
  map(create_total_phylogeny, distros)

pdf("all_distros.pdf")
walk2(all_trees, 
      distros$clade %>% 
        unique, 
      ~ plot_tree(.x, main = .y, show.tip.label = FALSE))
dev.off()

walk2(all_trees, 
      distros$clade %>% 
        unique, 
      ~ writeLines(.x, paste0("data/", .y, ".newick")))


create_total_phylogeny("Redhat", distros)


master %>% plot_tree(show.tip.label = FALSE)

%>% 
  map(~ print(str_replace(master, .$Name, newicks$result[[.$Name]]))) %>% 
  map(nchar)


newicks$result %>% 
  as.list %>% 
  walk(plot_tree)

newicks$result %>% 
  keep(~ nchar(.) > 20)

newicks$result[["Knoppix"]] 


pdf("~/Desktop/Ubuntu.pdf")
newicks$result[["Ubuntu"]] %>% 
  plot_tree
dev.off()



## OK fine, split by parent -- but you need to pair the most recent node with the parent
## have any parents not surivved? probably. in which case, the branch lenght of the parent must be smaller than the descendant. How do we show that? 
## well in such cases its age would be larger than its duration. find the difference between those numbers, and subtract that number from the age of the youngest taxon.

ubuntutree <- newicks$result[["Ubuntu"]] %>% 
  paste0(";") %>% 
  read.tree(text = .)

ubuntutree$tip.label

"(A:0.1,B:0.2,(C:0.3,D:0.4):0.5);" %>% 
  read.tree(text = .) %>% 
  plot


"(((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((Debian:184,SprezzOS:184):4,Cultix:188):27,YunoHost:215):17,
Xamin:232):43,AtlasX:275):22,srvRX live:297):17,SolusOS:314):128,
OpenMediaVault:442):92,Snowlinux:534):34,Red Ribbon:568):131,
Liquid Lemur:699):14,Conducit:713):2,SalineOS:715):17,GNUGuitarINUX:732):38,
Progress:770):77,Linux Mint Debian:847):98,DoudouLinux:945):92,Sword OS:116):85,
Semplice:1122):9,Matriux:1131):157,Tails:1288):18,Canaima:1306):113,Nova:1419):73,
Metamorphose:1492):134,Inquisitor:1626):39,Estrella Roja:1665):57,Proxmox:1722):14,
GALPon MiniNo:1736):31,PelicanHPC:1767):330,Webconverger:2097):86,BOSS:2183):47,
sidux:2230):111,PureOS:2341):135,Librassoc:666):22,Vyatta:2498):13,Epidemic:2511):56,
NepaLinux:2567):42,ThinClientOS:2609):9,OS2005:2618):61,Tuquito:2679):41,Elive:2720):46,
VENENUX:2766):1,LliureX:2767):65,Trisquel:2832):46,Voyage:2878):13,BlankOn:2891):7,Resulinux:2898):41,
MoLinux:2939):14,Arco-Debian:2953):7,BeatriX:66):35,Ubuntu:2995):49,UserLinux:242):1,grml:3045):91,
Underground Desktop:865):31,Clonezilla Live:3167):30,DRBL:3197):0,DeadCD:791):19,Euronode:668):18,
Guadalinex:3234):10,K-DEMar:3244):13,Amber:790):26,Bluewall:761):20,Xebian:559):46,Impi:792):31,ASLinux:3380):153,
MEPIS:3533):47,miniwoody:305):11,BlackRhino:1519):93,Rxart:3684):175,gnuLiNex:3492):312,Lindows:2558):13,DeMuDi:1627):17,
Skolelinux:4201):84,Progeny:2212):162,LEAF:4447):29,Knoppix:4476):61,
Gibraltar:4537):181,Omoikane Arma:4718):62,Storm:487):22,Libranet:1994):69,Corel:3258)" %>%   plot_tree
