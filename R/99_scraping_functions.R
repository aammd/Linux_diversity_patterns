
get_packages <- function(site){
  
  pkgs <- read_html(site)
  
  distro_tab <- pkgs %>% 
    html_table(fill = TRUE)
  
  penultimate <- length(distro_tab) - 1
  
  test <- distro_tab[[penultimate]] %>%
    # str_sub(end = 5000) %>% 
    str_split("\\n")
  
  all_pkgs <- test %>% 
    .[[1]] %>% 
    .[-1] %>% 
    str_sub(6)
  
  return(all_pkgs)
}
