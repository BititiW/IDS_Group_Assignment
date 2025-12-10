### Loading the necessary packages
library(tidyverse)
library(dplyr)
library(ggplot2)
# install.packages("sf")
library(sf)

### Loading the datasets
continents <- read.csv("continents-according-to-our-world-in-data.csv",
                       stringsAsFactors = F)
View(continents)

gdp_per_capita <- read.csv("gdp-per-capita-worldbank.csv",
                           stringsAsFactors = F)
View(gdp_per_capita_continents)

education_employment_training <- read.csv("youth-not-in-education-employment-training.csv",
                                           stringsAsFactors = F)
View(education_employment_training)

### Building the necessary data frames by manipulating gdp_per_capita data
# Adding continents and filtering for 2010 onwards
gdp_per_capita_continents <- gdp_per_capita %>% left_join(continents %>% select (Code, Continent), by = "Code") %>% filter(Year >= 2010)

# Renaming gdp_per_capita column
gdp_per_capita_continents <- gdp_per_capita_continents %>% 
  rename(gdp_per_capita = GDP.per.capita..PPP..constant.2017.international...)

# Adding gdp_growth_per_annum column
gdp_per_capita_continents <- gdp_per_capita_continents %>%
  group_by(Entity) %>%
  mutate(gdp_last_year = lag(gdp_per_capita))

gdp_per_capita_continents <- gdp_per_capita_continents %>%
  group_by(Entity) %>%
  mutate(gdp_growth_per_annum = (gdp_per_capita - gdp_last_year / gdp_last_year * 100)

gdp_growth_by_continent <- gdp_per_capita_continents %>%
  group_by(Year, Continent) %>%
  summarise(avg_growth_per_annum = mean(gdp_growth_per_annum, na.rm = TRUE)) %>%
  ungroup() %>%
  drop_na(Continent, avg_growth_per_annum)
View(gdp_growth_by_continent)

# Filtering out years before 2015
gdp_growth_by_continent_corrected <- gdp_growth_by_continent %>%
  filter(Year >= 2015)

View(gdp_growth_by_continent_corrected)

### Plotting a line graph
ggplot(gdp_growth_by_continent_corrected, aes(x = Year, y = avg_growth_per_annum, colour = Continent)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_line(size = 1) +
  scale_colour_brewer(palette = "Dark2") +
  labs(
    title = "GDP Per Capita Growth per Annum by Continent (2015-2021)",
    subtitle = ("Average GDP Per Capita Growth per Annum for all Countries in each Continent"),
    x = "Year",
    y = "Annual GDP Per Capita Growth Rate (%)",
    colour = "Continent"
  ) +
  scale_x_continuous(breaks = seq(2011, 2021, 1)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85")
  )

### Creating a box plot of annual GDP per Capita growth by Continent
## Selecting columns to simplify data frame, grouping by Continent and calculating the average growth for each country
gdp_per_capita_avg <- gdp_per_capita_continents %>%
  select(Entity, Year, Continent, gdp_growth_per_annum) %>%
  group_by(Entity, Continent) %>%
  summarise(avg_growth = mean(gdp_growth_per_annum, na.rm = TRUE)) %>%
  drop_na()
View(gdp_per_capita_avg)

## Plotting the boxplot
ggplot(gdp_per_capita_avg, aes(x = Continent, y = avg_growth, fill = Continent)) +
  geom_boxplot() +
  labs(
    title = "Average GDP per Capita Growth per Country (2015-2019)",
    subtitle = "Grouped by Continent",
    x = "Continent",
    y = "Average GDP per Capita Growth (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85"),
  )

### Heat maps showing annual GDP per capita growth for each continent on the map
## Loading the necessary packages
library(sf)
library(dplyr)
# install.packages("rnaturalearth")
library(rnaturalearth)
# install.packages("rnaturalearthdata")
library(rnaturalearthdata)

## Creating an _sf data frame with the geometries of each continent
continents_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  group_by(continent) %>%
  summarise(geometry = st_union(geometry)) %>%
  filter(continent != "Antarctica")
View(continents_sf)

## Plotting the heat maps for each year from 2015-2021
# 2015
gdp_growth_2015 <- gdp_growth_by_continent %>%
  filter(Year == 2015)

View(gdp_growth_2015)

gdp_growth_2015_sf <- continents_sf %>%
  left_join(gdp_growth_2015, by = c("continent" = "Continent"))

gdp_growth_2015_sf$growth_cat <- ifelse(gdp_growth_2015_sf$avg_growth_per_annum > 0,
                                        "Positive",
                                        "Negative")
View(gdp_growth_2015_sf)

ggplot(gdp_growth_2015_sf) +
  geom_sf(aes(fill = growth_cat)) +
  labs(
    title = "2015 GDP Per Capita Annual Growth (%) by Continent",
    x = "Degrees Latitude",
    y = "Degrees Longitude"
  ) +
  scale_fill_manual(
    name = "Legend",
    values = c("Negative" = "red", "Positive" = "green"),
    labels = c("Negative" = "Negative",
               "Positive" = "Positive"),
    na.translate = FALSE
  ) +
  theme_minimal()

# 2016
gdp_growth_2016 <- gdp_growth_by_continent %>%
  filter(Year == 2016)
View(gdp_growth_2016)

gdp_growth_2016_sf <- continents_sf %>%
  left_join(gdp_growth_2015, by = c("continent" = "Continent"))

gdp_growth_2016_sf$growth_cat <- ifelse(gdp_growth_2016_sf$avg_growth_per_annum > 0,
                                        "Positive",
                                        "Negative")
View(gdp_growth_2016_sf)

ggplot(gdp_growth_2016_sf) +
  geom_sf(aes(fill = growth_cat)) +
  labs(
    title = "2016 GDP Per Capita Annual Growth (%) by Continent",
    x = "Degrees Latitude",
    y = "Degrees Longitude"
  ) +
  scale_fill_manual(
    name = "Legend",
    values = c("Negative" = "red", "Positive" = "green"),
    labels = c("Negative" = "Negative",
               "Positive" = "Positive"),
    na.translate = FALSE
  ) +
  theme_minimal()

# 2017
gdp_growth_2017 <- gdp_growth_by_continent %>%
  filter(Year == 2017)
View(gdp_growth_2017)

gdp_growth_2017_sf <- continents_sf %>%
  left_join(gdp_growth_2015, by = c("continent" = "Continent"))

gdp_growth_2017_sf$growth_cat <- ifelse(gdp_growth_2017_sf$avg_growth_per_annum > 0,
                                        "Positive",
                                        "Negative")
View(gdp_growth_2017_sf)

ggplot(gdp_growth_2017_sf) +
  geom_sf(aes(fill = growth_cat)) +
  labs(
    title = "2017 GDP Per Capita Annual Growth (%) by Continent",
    x = "Degrees Latitude",
    y = "Degrees Longitude"
  ) +
  scale_fill_manual(
    name = "Legend",
    values = c("Negative" = "red", "Positive" = "green"),
    labels = c("Negative" = "Negative",
               "Positive" = "Positive"),
    na.translate = FALSE
  ) +
  theme_minimal()

# 2018
gdp_growth_2018 <- gdp_growth_by_continent %>%
  filter(Year == 2018)
View(gdp_growth_2018)

gdp_growth_2018_sf <- continents_sf %>%
  left_join(gdp_growth_2015, by = c("continent" = "Continent"))

gdp_growth_2018_sf$growth_cat <- ifelse(gdp_growth_2018_sf$avg_growth_per_annum > 0,
                                        "Positive",
                                        "Negative")
View(gdp_growth_2018_sf)

ggplot(gdp_growth_2018_sf) +
  geom_sf(aes(fill = growth_cat)) +
  labs(
    title = "2018 GDP Per Capita Annual Growth (%) by Continent",
    x = "Degrees Latitude",
    y = "Degrees Longitude"
  ) +
  scale_fill_manual(
    name = "Legend",
    values = c("Negative" = "red", "Positive" = "green"),
    labels = c("Negative" = "Negative",
               "Positive" = "Positive"),
    na.translate = FALSE
  ) +
  theme_minimal()

# 2019
gdp_growth_2019 <- gdp_growth_by_continent %>%
  filter(Year == 2019)

View(gdp_growth_2019)

gdp_growth_2019_sf <- continents_sf %>%
  left_join(gdp_growth_2015, by = c("continent" = "Continent"))

gdp_growth_2019_sf$growth_cat <- ifelse(gdp_growth_2019_sf$avg_growth_per_annum > 0,
                                        "Positive",
                                        "Negative")
View(gdp_growth_2019_sf)

ggplot(gdp_growth_2019_sf) +
  geom_sf(aes(fill = growth_cat)) +
  labs(
    title = "2019 GDP Per Capita Annual Growth (%) by Continent",
    x = "Degrees Latitude",
    y = "Degrees Longitude"
  ) +
  scale_fill_manual(
    name = "Legend",
    values = c("Negative" = "red", "Positive" = "green"),
    labels = c("Negative" = "Negative",
               "Positive" = "Positive"),
    na.translate = FALSE
  ) +
  theme_minimal()

# 2020
gdp_growth_2020 <- gdp_growth_by_continent %>%
  filter(Year == 2020)
View(gdp_growth_2020)

gdp_growth_2020_sf <- continents_sf %>%
  left_join(gdp_growth_2020, by = c("continent" = "Continent"))

gdp_growth_2020_sf$growth_cat <- ifelse(gdp_growth_2020_sf$avg_growth_per_annum > 0,
                                        "Positive",
                                        "Negative")
View(gdp_growth_2020_sf)

ggplot(gdp_growth_2020_sf) +
  geom_sf(aes(fill = growth_cat)) +
  labs(
    title = "2020 GDP Per Capita Annual Growth (%) by Continent",
    x = "Degrees Latitude",
    y = "Degrees Longitude"
  ) +
  scale_fill_manual(
    name = "Legend",
    values = c("Negative" = "red", "Positive" = "green"),
    labels = c("Negative" = "Negative",
               "Positive" = "Positive"),
    na.translate = FALSE
  ) +
  theme_minimal()

# 2021
gdp_growth_2021 <- gdp_growth_by_continent %>%
  filter(Year == 2021)
View(gdp_growth_2021)

gdp_growth_2021_sf <- continents_sf %>%
  left_join(gdp_growth_2021, by = c("continent" = "Continent"))

gdp_growth_2021_sf$growth_cat <- ifelse(gdp_growth_2021_sf$avg_growth_per_annum > 0,
                                        "Positive",
                                        "Negative")
View(gdp_growth_2021_sf)

ggplot(gdp_growth_2021_sf) +
  geom_sf(aes(fill = growth_cat)) +
  labs(
    title = "2021 GDP Per Capita Annual Growth (%) by Continent",
    x = "Degrees Latitude",
    y = "Degrees Longitude"
  ) +
  scale_fill_manual(
    name = "Legend",
    values = c("Negative" = "red", "Positive" = "green"),
    labels = c("Negative" = "Negative",
               "Positive" = "Positive"),
    na.translate = FALSE
  ) +
  theme_minimal()