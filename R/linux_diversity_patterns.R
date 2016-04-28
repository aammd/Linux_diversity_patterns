## "range"

library(dplyr)
library(googlesheets)
library(tidyr)
library(stringr)

traits_sheet <- gs_title("Linux_Traits.xlsx")

traits <- gs_read_csv(traits_sheet)

distro_scope <- traits %>% 
  filter(!is.na(Packages)) %>% 
  group_by(Distribution) %>% 
  do(data_frame(scope = str_split(.$Scope, ","))) %>% 
  ungroup %>% 
  unnest(scope) %>% 
  mutate(scope = str_trim(scope))


distscop %>% 
  group_by(Distribution) %>% 
  tally %>% 
  rename(abd = n) %>% 
  group_by(abd) %>% 
  tally %>% 
  arrange(n)


# generalist specialist -------------------------------

distro_trim <- distscop


rare_scopes <- distro_trim %>% 
  filter(!str_detect(scope, "Desktop"),
         !str_detect(scope, "Live"))
## betware fucking whitespace
  


