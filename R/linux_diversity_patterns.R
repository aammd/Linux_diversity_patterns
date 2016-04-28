## "range"


# load packages and data ------------------------------


library(dplyr)
library(googlesheets)
library(tidyr)
library(stringr)

traits_sheet <- gs_title("Linux_Traits.xlsx")

traits <- gs_read_csv(traits_sheet)


# splits scopes and trims -----------------------------


distro_scope <- traits %>% 
  filter(!is.na(Packages)) %>% 
  group_by(Distribution) %>% 
  do(data_frame(scope = str_split(.$Scope, ","))) %>% 
  ungroup %>% 
  unnest(scope) %>% 
  mutate(scope = str_trim(scope))

# generalist specialist -------------------------------


rare_scopes <- distro_trim %>% 
  filter(!str_detect(scope, "Desktop"),
         !str_detect(scope, "Live"))
## betware fucking whitespace
  

distscop %>% 
  group_by(Distribution) %>% 
  tally %>% 
  rename(abd = n) %>% 
  group_by(abd) %>% 
  tally %>% 
  arrange(n)

