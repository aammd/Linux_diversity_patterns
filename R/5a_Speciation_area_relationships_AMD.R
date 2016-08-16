## "range"


# load packages and data ------------------------------


library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(ggplot2)
library(metacom)
library(bipartite)

# DEFINING GGPLOT-LIKE COLORS
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
my.red <- gg_color_hue(4)[1]
my.blue <- gg_color_hue(4)[3]


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


for.glm <-  traits_nonglobal %>%
  group_by(Country) %>% 
  tally %>% 
  left_join(sizes) %>% 
  mutate(area = as.numeric(population)) %>% 
  arrange(area) %>% 
  mutate(cum_n = n,
         cum_a = log(area))
  summary(glm(cum_n~cum_a, family="quasipoisson", data=for.glm))


traits_nonglobal %>%
  group_by(Country) %>% 
  tally %>% 
  left_join(sizes) %>% 
  mutate(pop = log(as.numeric(population))) %>% 
  arrange(pop) %>% 
  mutate(cum_n = n,
         cum_a = pop) %>% 
  ggplot(aes(x = cum_a, y = cum_n)) + 
  ylim(0,75) +
  #scale_y_log10(limits=c(1, 75)) + 
  #scale_x_log10() +
  theme_bw() +
  theme(plot.title=element_text(hjust=-0.05)) +
  ggtitle('A') + 
  stat_smooth(method = "glm",  method.args = list(family = "quasipoisson"), 
              color = my.red) + 
  geom_point(shape=19, color="darkgrey") +
  xlab("log Country human population") +
  ylab("# of diversification events")

ggsave("figures/spec_pop.pdf", width = 4, height = 4)


for.glm <-  traits_nonglobal %>%
  group_by(Country) %>% 
  tally %>% 
  left_join(sizes) %>% 
  mutate(area = log(as.numeric(areaInSqKm))) %>% 
  arrange(area) %>% 
  mutate(cum_n = n,
         cum_a = area)
  summary(glm(cum_n~cum_a, family="quasipoisson", data=for.glm))

traits_nonglobal %>%
  group_by(Country) %>% 
  tally %>% 
  left_join(sizes) %>% 
  mutate(area = log(as.numeric(areaInSqKm))) %>% 
  arrange(area) %>% 
  mutate(cum_n = n,
         cum_a = area) %>% 
  ggplot(aes(x = cum_a, y = cum_n)) + 
  #scale_y_log10(limits=c(1, 100)) + 
  ylim(0, 75) +
  #scale_x_log10() +
  theme_bw() +
  theme(plot.title=element_text(hjust=-0.05)) +
  ggtitle('B') + 
  stat_smooth(method = "glm",  method.args = list(family = "quasipoisson"), 
              color = my.red) + 
  geom_point(shape=19, color="darkgrey") +
  xlab("log Country area [km^2]") +
  ylab("")

ggsave("figures/spec_area.pdf", width = 4, height = 4)


system("
    cd figures
    # pdfcrop spec_area.pdf spec_area.pdf
    # pdfcrop spec_pop.pdf spec_pop.pdf
    pdfnup --nup 2x1 spec_pop.pdf spec_area.pdf --outfile hard_to_test.pdf
    #pdfcrop hard_to_test.pdf hard_to_test.pdf
    convert hard_to_test.pdf hard_to_test.png
    ")
