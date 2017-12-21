
library(rvest)
library(magrittr) 
library(lubridate)
library(plyr)
library(stringr)
library(taRifx)

# process the raw data from squid counter https://stats.wikimedia.org/archive/squid_reports/

# PERIOD 1 ---------------------------------------------------------------------

adr.first <- "https://stats.wikimedia.org/archive/squid_reports/"
adr.last <- "/SquidReportOperatingSystems.htm"
year <- rep(as.character(2010:2014), each=12)
month <- rep(c("01","02","03","04","05","06","07","08","09","10","11","12"), times=5)
dates <- paste(year, month, sep="-")
ADDR <- paste(adr.first, dates, adr.last, sep="")

dates2 <- ymd("2010-01-01") + months(0:(12*5-1))

res1 <- list()

for(i in 1:length(dates))
{
  message(paste(i, dates2[i]))
  lin <- read_html(ADDR[i]) %>%
    html_nodes(css = "table") %>% extract(4) %>%
  html_table(fill=TRUE) %>% extract(1)
  
  lin <- lin[[1]][16:23,1:2]
  lin[,2] <- as.numeric(gsub(pattern=",", replacement="", x=lin[,2] ))
  res1[[i]] <- lin
}
names(res1) <- dates2

res <- ldply(res1)[1:168,]
names(res) <- c("Date","Distro","Count")
write.csv(res, file="squid_2010_2011.csv", row.names = FALSE)

# PERIOD 2 ---------------------------------------------------------------------

X <- read.csv("squid_wikimedia_2014_plus.csv")
clean <- gsub(pattern="k", replacement="000", x=X$Count) %>%
         gsub(pattern="M", replacement="000000") %>%
         gsub(pattern=".", replacement="", fixed=TRUE) %>%
         gsub(pattern=",", replacement="", fixed=TRUE) %>%
         str_replace_all(pattern=" ", repl="" ) %>%
         destring()

X$Count <- clean


X<- data.frame(X, Date=make_date(year = X$Year, month=X$Month))
X <- X[X$Distro!="Linux Other",]

X <- dplyr::select(X, Date, Distro, Count)

write.csv(X, file="squid_2011_2014.csv", row.names = FALSE)


