# create map of race by county

## Dependencies
### tidyverse
library(dplyr)
library(ggplot2)
library(readr)

### mapping
library(classInt)
library(sf)
library(tigris)

### other
library(prener)

## Create state outline
### download missouri
mo <- states(cb = FALSE, resolution = "20m")

### convert to sf object
mo <- st_as_sf(mo)

### subset observations
mo <- filter(mo, STATEFP == 29)

### download missouri counties
moCounties <- counties(state = "MO", cb = FALSE, resolution = "20m")

### convert to sf object
moCounties <- st_as_sf(moCounties)

### subset columns
moCounties <- select(moCounties, GEOID, COUNTYFP, NAMELSAD, ALAND)

### read in tabular data
birthData <- read_csv("2016/data/MO_HEALTH_lowBirth.csv")

### clean up data
birthData %>%
  rename(lowBirths = `Number of Low Births`) %>%
  rename(liveBirths = `Number of Live Births`) %>%
  mutate(pctLow = (lowBirths/liveBirths)*100) %>%
  mutate(FIPS = as.character(FIPS)) %>%
  select(FIPS, pctLow) %>%
  cp_breaks(var = pctLow, newvar = lowJenks, classes = 5, style = "jenks") -> lowBirth
  
### combine spatial and geometric data
birthMap <- left_join(moCounties, lowBirth, by = c("GEOID" = "FIPS"))

## base map
base <- ggplot() + 
  geom_sf(data = mo, fill = "#ffffff", color = NA) + 
  geom_sf(data = birthMap, mapping = aes(fill = lowJenks), color = NA) +
  geom_sf(data = mo, fill = NA, color = "#000000", size = .25) +
  scale_fill_brewer(palette = "RdPu", name = "Percent",
                    labels = c("3.83 - 6.68", "6.69 - 7.80", "7.81 - 8.84", "8.85 - 10.60", "10.61 - 13.70")) +
  labs(
    title = "Low Birth Weight Births by County, 2016",
    subtitle = "Live Births in Missouri with Weight Under 5 lbs., 8 oz.",
    caption = "Data via County Health Rankings \nMap by Christopher Prener, Ph.D."
  ) 

cp_plotSave(filename = "2016/results/lbwMap16-base.png", plot = base, preset = "lg", dpi = 500)

## map with white background
map01 <- base +
  cp_sequoiaTheme(background = "white", map = TRUE)

cp_plotSave(filename = "2016/results/lbwMap16-white.png", plot = map01, preset = "lg", dpi = 500)

## map with transparent background
map02 <- base +
  cp_sequoiaTheme(background = "transparent", map = TRUE)

cp_plotSave(filename = "2016/results/lbwMap16-trans.png", plot = map02, preset = "lg", dpi = 500)
