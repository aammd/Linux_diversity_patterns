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