## scraping page hit data

library(rvest)

tables <- read_html("http://distrowatch.com/dwres.php?resource=popularity") %>% 
  html_nodes(".NewsText table")

tables[[1]]


read_html("http://distrowatch.com/table.php?distribution=ubuntu") %>% 
  html_nodes(".TablesInvert") %>% 
  html_text()

read_html("http://distrowatch.com/table.php?distribution=ubuntu") %>% 
  html_nodes(".Date") %>% 
  html_text()


## obtain links to the package list for all releases
read_html("http://distrowatch.com/table.php?distribution=ubuntu") %>% 
  html_nodes(":nth-child(15) a") %>% 
  html_attrs()

## 

data_frame(distro = read_html(url) %>% 
             html_nodes(".phr2") %>% 
             html_text(),
           hpd = read_html(url) %>% 
             html_nodes(".phr3") %>% 
             html_text()
)

get_distro_data <- function(yr) {
  url <- paste0("http://distrowatch.com/index.php?dataspan=", yr)
  
  ## ugh this is inelegant; i protest
  # if (yr >= 2013) {
  #   dl_node <- "td.News:nth-child(3)"
  # } else {
  #   dl_node <-  ".phr3"
  # }
  # 
  list(distro = read_html(url) %>% 
         html_nodes("td:nth-child(3) :nth-child(3) :nth-child(2)") %>% 
         html_text(),
       
       dl = read_html(url) %>% 
         html_nodes(".phr3") %>% 
         html_text()
  )
  
}

testlist <- get_distro_data(2011)

testlist$distro
testlist$dl


read_html("http://distrowatch.com/index.php?dataspan=2012") %>% 
  html_nodes("td:nth-child(3) :nth-child(3) :nth-child(2)") %>% 
  html_text()

read_html("http://distrowatch.com/index.php?dataspan=2013") %>% 
  html_nodes("td.News:nth-child(3)") %>% 
  html_text()
  

pkgs <- read_html("http://distrowatch.com/table.php?distribution=ubuntu&pkglist=true&version=15.10")



distro_tab <- pkgs %>% 
  html_table(fill = TRUE)

library(stringr)

test <- distro_tab[[16]] %>%
  # str_sub(end = 5000) %>% 
  str_split("\\n")

test %>% 
  .[[1]] %>% 
  .[-1] %>% 
  str_sub(6)

get_hab <- . %>% 
  read_html() %>% 
  html_nodes(".TablesTitle li a , .TablesTitle li b") %>% 
  html_text()

get_hab("http://distrowatch.com/table.php?distribution=ubuntu")
