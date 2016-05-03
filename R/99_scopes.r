## "range"


# load packages and data ------------------------------


library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(ggplot2)


traits_sheet <- read_excel("data/Linux_traits.xlsx")


# splits scopes and trims -----------------------------

traits_sheet %>% 
  filter(is.na(Packages)) %>% 
  View

distro_scope <- traits_sheet %>% 
  filter(!is.na(Packages)) %>% 
  group_by(Distribution) %>% 
  do(data_frame(scope = str_split(.$Scope, ","))) %>% 
  ungroup %>% 
  unnest(scope) %>% 
  mutate(scope = str_trim(scope))

# generalist specialist -------------------------------


rare_scopes <- distro_scope %>% 
  filter(!str_detect(scope, "Desktop"),
         !str_detect(scope, "Live"))



# SAR -------------------------------------------------

## log log?

## vector of sample sizes
## 
scopelist <- rare_scopes %>% 
  select(scope) %>% 
  unique %>% 
  .[[1]]


pairs <- replicate(100, sample(scopelist, 2), simplify = FALSE)


pairs %>% 
  purrr::map_df(~ rare_scopes %>% 
                  group_by(Distribution) %>% 
                  filter(scope %in% .x),
                .id = "sampletime"
  ) %>% 
  View

# plots -----------------------------------------------


distscop %>% 
  group_by(Distribution) %>% 
  tally %>% 
  rename(abd = n) %>% 
  group_by(abd) %>% 
  tally %>% 
  arrange(n) %>% 
  ggplot(aes(x = abd, y = n)) + 
  geom_point() +
  geom_line() +
  xlab("number of Scopes") +
  ylab("number of Distribution")


distro_scope %>% 
  select(scope) %>% 
  distinct

distscop %>% 
  group_by(Distribution) %>% 
  tally %>% 
  rename(abd = n) %>% 
  arrange(desc(abd)) %>% 
  View



dist_wide <- distro_scope %>% 
  mutate(abd = 1) %>% 
  spread(scope, abd, fill = 0)
distmat <- as.matrix(dist_wide[-1])
rownames(distmat) <- dist_wide$Distribution

library(metacom)
Imagine(as.matrix(dist_wide[-1]), fill = FALSE)
library(bipartite)
plotweb(distmat)

