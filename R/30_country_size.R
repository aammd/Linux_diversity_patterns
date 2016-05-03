## this script obtains data on the size of different countries
## note that you have to have a geonames username in order to use this, see ?geonames


# load packages ---------------------------------------

library(countrycode)
library(geonames)
library(dplyr)
library(readxl)


traits_sheet <- read_excel("data/Linux_traits.xlsx")


traits_nonglobal <- traits_sheet %>% 
  filter(Country != "Global")


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

readr::write_csv(sizes, "data/country_sizes.csv")
