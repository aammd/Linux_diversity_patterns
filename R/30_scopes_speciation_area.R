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


# ok maybe we consider countries ----------------------

traits_nonglobal <- traits_sheet %>% 
  filter(Country != "Global")
  


traits_nonglobal %>% 
  group_by(Country) %>% 
  tally %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(x = seq_along(n), y = n)) + geom_point()


dist_wide <- traits_nonglobal %>% 
  select(Distribution, Country) %>% 
  mutate(abd = 1) %>% 
  spread(Country, abd, fill = 0)
distmat <- as.matrix(dist_wide[-1])
rownames(distmat) <- dist_wide$Distribution

library(metacom)
Imagine(as.matrix(dist_wide[-1]), fill = FALSE)
library(bipartite)
plotweb(distmat)


# fix country names -----------------------------------

library(countrycode)
library(geonames)

country_traits_code <- traits_nonglobal %>% 
  select(Country) %>% 
  distinct %>% 
  mutate(cc = countrycode(Country, "country.name", "iso2c"))

geonames::GNcountryInfo("CN")

country_size <- country_traits_code %>% 
  group_by(Country, cc) %>% 
  do(GNcountryInfo(.$cc))

sizes <- country_size %>% 
  select(Country, cc, areaInSqKm, population)

traits_nonglobal %>%
  group_by(Country) %>% 
  tally %>% 
  left_join(sizes) %>% 
  mutate(area = as.numeric(population)/1000000) %>% 
  arrange(area) %>% 
  mutate(cum_n = n,
         cum_a = area) %>% 
  ggplot(aes(x = cum_a, y = cum_n)) + 
  scale_y_log10() +
  scale_x_log10() +
  theme_minimal() +
  stat_smooth(method = "glm",  method.args = list(family = "poisson"), colour = "darkgrey") + 
  geom_point() +
  xlab("Population (millions)") +
  ylab("Number of distros")

ggsave("figures/speciation_area.png", width = 5, height = 3.5)
