## "range"


# load packages and data ------------------------------


library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(ggplot2)
library(metacom)
library(bipartite)


traits_sheet <- read_excel("data/Linux_traits.xlsx")

sizes <- read.csv("data/country_sizes.csv")

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

Imagine(as.matrix(dist_wide[-1]), fill = FALSE)

plotweb(distmat)


# fix country names -----------------------------------



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
  theme_classic() +
  #theme(plot.title=element_text(hjust=-0.05)) +
  #ggtitle('A') + 
  stat_smooth(method = "glm",  method.args = list(family = "poisson"), 
              color = "red") + 
  geom_point(shape=1) +
  xlab("Population (millions)") +
  ylab("Number of diversification events")

ggsave("figures/speciation_area.pdf", width = 5, height = 3.5)



