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

get_distro_data <- function(yr){
  url <- paste0("http://distrowatch.com/index.php?dataspan=", yr)
  
  list(distro = read_html(url) %>% 
         html_nodes("td:nth-child(3) :nth-child(3) :nth-child(2)") %>% 
         html_text(),
       
       dl = read_html(url) %>% 
         html_nodes("td.News:nth-child(3)") %>% 
         html_text()
  )
  
}

get_distro_data(2005)

read_html("http://distrowatch.com/index.php?dataspan=2006") %>% 
  html_nodes("td:nth-child(3) :nth-child(3) :nth-child(2)") %>% 
  html_text()

read_html("http://distrowatch.com/index.php?dataspan=2006") %>% 
  html_nodes("td.News:nth-child(3)") %>% 
  html_text()
  

