## "range"


# load packages and data ------------------------------


library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(ggplot2)


traits_sheet <- read_excel("data/Linux_traits.xlsx")



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
