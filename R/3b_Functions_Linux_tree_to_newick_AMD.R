# Author: Andrew M McDonald
# email: a.a.m.macdonald@gmail.com

## functions to process the csv into a phylogeny

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

