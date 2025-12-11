# SECTION 1 target 2 2015-2020 bar chart

library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggthemes)
#library packages needed

data <- read.csv("youth-not-in-education-employment-training.csv")
country <- read.csv("continents-according-to-our-world-in-data.csv")

data1 <- data %>% left_join(country, by = "Code")
#join two datasets together
data1 <- data1 %>% select(-matches("Entity.y|Year.y"))
#remove extra columns

data1 <- data1 %>%
  mutate(Year = as.numeric(Year))

result <- data1 %>% # Ensure Year is numeric and filter only relevant years 
  filter(Year %in% c(2014, 2015, 2020, 2021)) %>% 
  group_by(Entity, Code, Continent) %>% summarise(
    baseline_year = ifelse(any(Year == 2015), 2015, 2014), 
    # Get baseline: 2015 if exists, else 2014 
    baseline_rate = Share.of.youth.not.in.education..employment.or.training..total....of.youth.population.[Year == baseline_year][1], 
    # End: prefer 2020, else 2021 
    end_year = ifelse(any(Year == 2020), 2020, 2021), 
    end_rate = Share.of.youth.not.in.education..employment.or.training..total....of.youth.population.[Year == end_year][1], 
    rate_decreased = baseline_rate - end_rate,.groups = "drop"
  )

# List of all six continents
all_continents <- data.frame(
  Continent = c("Africa", "Asia", "Europe", "North America", "South America", "Oceania"),
  stringsAsFactors = FALSE
)

continent_counts <- result %>%
  filter(rate_decreased > 3) %>%
  group_by(Continent) %>%
  summarise(n_countries = n(),.groups = "drop")

# Ensure all six continents are present, add row with n_countries = 0 if missing
continent_counts <- all_continents %>%
  left_join(continent_counts, by = "Continent") %>%
  mutate(n_countries = ifelse(is.na(n_countries), 0, n_countries))

plot <- ggplot(continent_counts, aes(x = Continent, y = n_countries, fill = Continent)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Number of Countries per Continent with Rate Decreased > 3 from 2015 to 2020",
    x = "Continent",
    y = "Number of Countries"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  )

print(plot)

# SECTION 2 target 2 2015-2019 bar chart

library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggthemes)
#library packages needed

data <- read.csv("youth-not-in-education-employment-training.csv")
country <- read.csv("continents-according-to-our-world-in-data.csv")

data1 <- data %>% left_join(country, by = "Code")
#join two datasets together
data1 <- data1 %>% select(-matches("Entity.y|Year.y"))
#remove extra columns

data1 <- data1 %>%
  mutate(Year = as.numeric(Year))

result <- data1 %>% # Ensure Year is numeric and filter only relevant years 
  filter(Year %in% c(2014, 2015, 2019, 2020)) %>% 
  group_by(Entity, Code, Continent) %>% summarise(
    baseline_year = ifelse(any(Year == 2015), 2015, 2014), 
    # Get baseline: 2015 if exists, else 2014 
    baseline_rate = Share.of.youth.not.in.education..employment.or.training..total....of.youth.population.[Year == baseline_year][1], 
    # End: prefer 2019, else 2020 
    end_year = ifelse(any(Year == 2019), 2019, 2020), 
    end_rate = Share.of.youth.not.in.education..employment.or.training..total....of.youth.population.[Year == end_year][1], 
    rate_decreased = baseline_rate - end_rate,.groups = "drop"
  )

# List of all six continents
all_continents <- data.frame(
  Continent = c("Africa", "Asia", "Europe", "North America", "South America", "Oceania"),
  stringsAsFactors = FALSE
)

continent_counts <- result %>%
  filter(rate_decreased > 3) %>%
  group_by(Continent) %>%
  summarise(n_countries = n(),.groups = "drop")

# Ensure all six continents are present, add row with n_countries = 0 if missing
continent_counts <- all_continents %>%
  left_join(continent_counts, by = "Continent") %>%
  mutate(n_countries = ifelse(is.na(n_countries), 0, n_countries))

plot <- ggplot(continent_counts, aes(x = Continent, y = n_countries, fill = Continent)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Number of Countries per Continent with Rate Decreased > 3 from 2015 to 2019",
    x = "Continent",
    y = "Number of Countries"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  )

print(plot)

# SECTION 3 target 1

### Loading the necessary packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(sf)

### Loading the datasets
continents <- read.csv("continents-according-to-our-world-in-data.csv", stringsAsFactors = FALSE)
gdp_per_capita <- read.csv("gdp-per-capita-worldbank.csv", stringsAsFactors = FALSE)
education_employment_training <- read.csv("youth-not-in-education-employment-training.csv", stringsAsFactors = FALSE)

### Building the necessary data frames by manipulating gdp_per_capita data
# Adding continents and filtering for 2010 onwards
gdp_per_capita_continents <- gdp_per_capita %>%
  left_join(continents %>% select(Code, Continent), by = "Code") %>%
  filter(Year >= 2010) %>%
  rename(gdp_per_capita = GDP.per.capita..PPP..constant.2017.international...)

# Adding gdp_growth_per_annum column
gdp_per_capita_continents <- gdp_per_capita_continents %>%
  group_by(Entity) %>%
  arrange(Year) %>%
  mutate(gdp_last_year = lag(gdp_per_capita)) %>%
  mutate(gdp_growth_per_annum = (gdp_per_capita - gdp_last_year) / gdp_last_year * 100) %>%
  ungroup()

gdp_growth_by_continent <- gdp_per_capita_continents %>%
  group_by(Year, Continent) %>%
  summarise(avg_growth_per_annum = mean(gdp_growth_per_annum, na.rm = TRUE)) %>%
  ungroup() %>%
  drop_na(Continent, avg_growth_per_annum)

# Filtering out years before 2015
gdp_growth_by_continent_corrected <- gdp_growth_by_continent %>%
  filter(Year >= 2015)

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
gdp_per_capita_avg <- gdp_per_capita_continents %>%
  select(Entity, Year, Continent, gdp_growth_per_annum) %>%
  group_by(Entity, Continent) %>%
  summarise(avg_growth = mean(gdp_growth_per_annum, na.rm = TRUE)) %>%
  drop_na()
View(gdp_per_capita_avg)

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
    panel.grid.major = element_line(color = "grey85")
  )

### Heat maps showing annual GDP per capita growth for each continent on the map
library(sf)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)

continents_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  group_by(continent) %>%
  summarise(geometry = st_union(geometry)) %>%
  filter(continent != "Antarctica")

for (year in 2015:2021) {
  gdp_growth_year <- gdp_growth_by_continent %>% filter(Year == year)
  gdp_growth_year_sf <- continents_sf %>%
    left_join(gdp_growth_year, by = c("continent" = "Continent"))
  gdp_growth_year_sf$growth_cat <- ifelse(gdp_growth_year_sf$avg_growth_per_annum > 0, "Positive", "Negative")
  print(
    ggplot(gdp_growth_year_sf) +
      geom_sf(aes(fill = growth_cat)) +
      labs(
        title = paste0(year, " GDP Per Capita Annual Growth (%) by Continent"),
        x = "Degrees Latitude",
        y = "Degrees Longitude"
      ) +
      scale_fill_manual(
        name = "Legend",
        values = c("Negative" = "red", "Positive" = "green"),
        labels = c("Negative" = "Negative", "Positive" = "Positive"),
        na.translate = FALSE
      ) +
      theme_minimal()
  )
}

# SECTION 4

library(tidyverse)
library(dplyr)
library(ggplot2)
# 0. Load data sets
# setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Downloads/data sets")
continents <- read.csv("continents-according-to-our-world-in-data.csv")
GDP_per_capita <- read.csv("gdp-per-capita-worldbank.csv")
Youth <- read.csv("youth-not-in-education-employment-training.csv")  # not used here, but fine to keep
Income_group_classification <- read.csv("World Bank data csv.csv")

# 1. Clean income classification file and merge with continents
Income_group_classification <- Income_group_classification[-c(1:4), ]
Income_group_classification <- Income_group_classification %>%
  select(X, World.Bank.Analytical.Classifications, X.20:X.38)

joined_data1 <- left_join(Income_group_classification, continents, by = c("X" = "Code")) %>%
  select(-Year)

names(joined_data1)[3:21] <- as.character(joined_data1[1, 3:21])
joined_data1 <- joined_data1[-1, ]
joined_data1 <- joined_data1 %>%
  rename(Code = X, Country = World.Bank.Analytical.Classifications)

GDP_per_capita <- GDP_per_capita %>% filter(Year >= 2006)

joined_data1_bottom <- joined_data1 %>%
  pivot_longer(cols = -c(Code, Country, Continent), names_to = "Year", values_to = "Income_Classification")

joined_data1_bottom$Year <- as.integer(joined_data1_bottom$Year)

joined_data2 <- inner_join(joined_data1_bottom, GDP_per_capita, by = c("Code", "Year")) %>%
  select(-Entity) %>%
  rename(`GDP per capita` = GDP.per.capita..PPP..constant.2017.international...)

low_income_growth <- joined_data2 %>%
  filter(Income_Classification == "L") %>%              # low-income only
  arrange(Country, Year) %>%
  group_by(Country, Continent) %>%
  mutate(
    GDP_Growth = (`GDP per capita` - lag(`GDP per capita`)) /
      lag(`GDP per capita`) * 100             # % growth
  ) %>%
  ungroup()

LDC_continent_avg <- low_income_growth %>%
  group_by(Year, Continent) %>%
  summarise(
    Avg_Growth = mean(GDP_Growth, na.rm = TRUE),.groups = "drop"
  )

ggplot(LDC_continent_avg,
       aes(x = Year, y = Avg_Growth, colour = Continent)) +
  geom_line(linewidth = 1.1) +
  geom_hline(yintercept = 7,
             linetype = "dashed",
             colour = "black",
             linewidth = 1.2) +
  labs(
    title = "Average GDP Growth of Low-Income Countries by Continent",
    subtitle = "Dashed line shows SDG Target 8.1: 7% annual GDP growth for LDCs",
    x = "Year",
    y = "Average GDP per capita growth (%)",
    colour = "Continent"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")    

# SECTION 5

library(tidyverse)
library(dplyr)
library(ggplot2)

# Load data sets
continents <- read.csv("continents-according-to-our-world-in-data.csv")
GDP_per_capita <- read.csv("gdp-per-capita-worldbank.csv")
Youth <- read.csv("youth-not-in-education-employment-training.csv")
Income_group_classification <- read.csv("World Bank data csv.csv")

Income_group_classification <- Income_group_classification[-c(1:4), ]
Income_group_classification <- Income_group_classification %>%
  select(X, World.Bank.Analytical.Classifications, X.20:X.38)

joined_data1 <- left_join(Income_group_classification, continents, by = c("X" = "Code")) %>%
  select(-Year)

names(joined_data1)[3:21] <- as.character(joined_data1[1, 3:21])
joined_data1 <- joined_data1[-1, ]
joined_data1 <- joined_data1 %>%
  rename(Code = X, Country = World.Bank.Analytical.Classifications)

GDP_per_capita <- GDP_per_capita %>% filter(Year >= 2015)

joined_data1_bottom <- joined_data1 %>%
  pivot_longer(cols = -c(Code, Country, Continent), names_to = "Year", values_to = "Income_Classification")
joined_data1_bottom$Year <- as.integer(joined_data1_bottom$Year)

joined_data2 <- inner_join(joined_data1_bottom, GDP_per_capita, by = c("Code", "Year")) %>%
  select(-Entity) %>%
  rename(`GDP per capita` = GDP.per.capita..PPP..constant.2017.international...)

# Keep only low income countries
joined_data2_Low_Income <- joined_data2 %>%
  filter(Income_Classification == "L")

# Calculate GDP growth for each low income country
joined_data2_Growth_Rates <- joined_data2 %>%
  filter(Income_Classification == "L") %>%
  group_by(Country) %>%
  summarise(
    GDP_Growth = (`GDP per capita` - lag(`GDP per capita`))*100/lag(`GDP per capita`)
  )

# Check whether they meet target growth of 7%
Low_income_growth_rates <- joined_data2_Low_Income %>%
  mutate(GDP_Growth_Rate = joined_data2_Growth_Rates$GDP_Growth,
         Result = case_when(
           GDP_Growth_Rate >= 7 ~ "Meets 7% GDP growth per annum",
           TRUE ~ "Does not meet 7% GDP growth per annum"
         ))

# Calculate average growth for all years for each country
Avg_Growth <- Low_income_growth_rates %>%
  group_by(Country) %>%
  summarise(Avg_Growth = mean(GDP_Growth_Rate, na.rm = TRUE))

# Join average growth data set to original data set (of low income countries only)
Low_income_growth_rates <- Low_income_growth_rates %>%
  full_join(Avg_Growth, by = "Country")

# Remove columns of GDP per capita and GDP growth for individual years
Only_Avg_Growth <- Low_income_growth_rates %>%
  select(Code, Country, Continent, Income_Classification, Avg_Growth) %>%
  filter(Country != "Zambia") %>%
  distinct()

# Create separate data sets for each continent (with low income countries)
Africa <- Only_Avg_Growth %>% filter(Continent == "Africa")
Asia <- Only_Avg_Growth %>% filter(Continent == "Asia")
Oceania <- Only_Avg_Growth %>% filter(Continent == "Oceania")
North_America <- Only_Avg_Growth %>% filter(Continent == "North America")

# Calculate number of low income countries in each continent that reach target growth of 7%
Countries_meeting_target_Africa <- nrow(subset(Africa, Avg_Growth >= 7))
Countries_meeting_target_Asia <- nrow(subset(Asia, Avg_Growth >= 7))
Countries_meeting_target_Oceania <- nrow(subset(Oceania, Avg_Growth >= 7))
Countries_meeting_target_North_America <- nrow(subset(North_America, Avg_Growth >= 7))

# Calculate number of low income countries in each continent
Number_low_income_countries_Africa <- nrow(Africa)
Number_low_income_countries_Asia <- nrow(Asia)
Number_low_income_countries_Oceania <- nrow(Oceania)
Number_low_income_countries_North_America <- nrow(North_America)

# Create a data set with the two above calculated variables for each continent
Countries_meeting_target <- data.frame(
  Continent = c("Africa", "Asia", "North America", "Oceania"),
  Number_low_income_countries = c(Number_low_income_countries_Africa,
                                  Number_low_income_countries_Asia,
                                  Number_low_income_countries_North_America,
                                  Number_low_income_countries_Oceania),
  Number_meeting_target = c(Countries_meeting_target_Africa,
                            Countries_meeting_target_Asia,
                            Countries_meeting_target_North_America,
                            Countries_meeting_target_Oceania)
)

# -----------------------------------------------------------------------------

### Low
Avg_Growth_Low <- mean(Only_Avg_Growth$Avg_Growth)

## Calculate average growth of all L countries by Continent
Avg_Growth_All_L_by_Continent <- Only_Avg_Growth %>%
  group_by(Continent) %>%
  summarise(Avg_Growth_All_L_by_Continent = mean(Avg_Growth))

### LM

# Keep only low middle income countries
joined_data2_LM <- joined_data2 %>%
  filter(Income_Classification == "LM")

# Calculate GDP growth for each low middle income country
joined_data2_Growth_Rates_LM <- joined_data2 %>%
  filter(Income_Classification == "LM") %>%
  group_by(Country) %>%
  summarise(
    GDP_Growth = (`GDP per capita` - lag(`GDP per capita`))*100/lag(`GDP per capita`)
  )

# Add growth rate column to original data set
LM_growth_rates <- joined_data2_LM %>%
  mutate(GDP_Growth_Rate = joined_data2_Growth_Rates_LM$GDP_Growth)

# Calculate average growth for all years for each LM country
Avg_Growth_LM <- LM_growth_rates %>%
  group_by(Country) %>%
  summarise(Avg_Growth_LM = mean(GDP_Growth_Rate, na.rm = TRUE))

# Join average growth data set to original data set (of LM countries only)
LM_growth_rates <- LM_growth_rates %>%
  full_join(Avg_Growth_LM, by = "Country")

# Remove columns of GDP per capita and GDP growth for individual years
Only_Avg_Growth_LM <- LM_growth_rates %>%
  select(Code, Country, Continent, Income_Classification, Avg_Growth_LM) %>%
  filter(!Country %in% c("Belarus", "Belize", "Lebanon", "Tonga")) %>%
  distinct()

# Calculate average growth of all LM countries
Avg_Growth_All_LM <- mean(Only_Avg_Growth_LM$Avg_Growth_LM)

## Calculate average growth of all LM countries by Continent
Avg_Growth_All_LM_by_Continent <- Only_Avg_Growth_LM %>%
  group_by(Continent) %>%
  summarise(Avg_Growth_All_LM_by_Continent = mean(Avg_Growth_LM))

### UM

# Keep only upper middle income countries
joined_data2_UM <- joined_data2 %>%
  filter(Income_Classification == "UM")

# Calculate GDP growth for each upper middle income country
joined_data2_Growth_Rates_UM <- joined_data2 %>%
  filter(Income_Classification == "UM") %>%
  group_by(Country) %>%
  summarise(
    GDP_Growth = (`GDP per capita` - lag(`GDP per capita`))*100/lag(`GDP per capita`)
  )

# Add growth rate column to original data set
UM_growth_rates <- joined_data2_UM %>%
  mutate(GDP_Growth_Rate = joined_data2_Growth_Rates_UM$GDP_Growth)

# Calculate average growth for all years for each UM country
Avg_Growth_UM <- UM_growth_rates %>%
  group_by(Country) %>%
  summarise(Avg_Growth_UM = mean(GDP_Growth_Rate, na.rm = TRUE))

# Join average growth data set to original data set (of UM countries only)
UM_growth_rates <- UM_growth_rates %>%
  full_join(Avg_Growth_UM, by = "Country")

# Remove columns of GDP per capita and GDP growth for individual years
Only_Avg_Growth_UM <- UM_growth_rates %>%
  select(Code, Country, Continent, Income_Classification, Avg_Growth_UM) %>%
  filter(!Country %in% c("Indonesia", "Mongolia", "Oman", "Angola", "Croatia", "Sri Lanka")) %>%
  distinct()

# Calculate average growth of all UM countries
Avg_Growth_All_UM <- mean(Only_Avg_Growth_UM$Avg_Growth_UM)

## Calculate average growth of all UM countries by Continent
Avg_Growth_All_UM_by_Continent <- Only_Avg_Growth_UM %>%
  group_by(Continent) %>%
  summarise(Avg_Growth_All_UM_by_Continent = mean(Avg_Growth_UM))

### High

# Keep only high income countries
joined_data2_H <- joined_data2 %>%
  filter(Income_Classification == "H")

# Calculate GDP growth for each high income country
joined_data2_Growth_Rates_H <- joined_data2 %>%
  filter(Income_Classification == "H") %>%
  group_by(Country) %>%
  summarise(
    GDP_Growth = (`GDP per capita` - lag(`GDP per capita`))*100/lag(`GDP per capita`)
  )

# Add growth rate column to original data set
H_growth_rates <- joined_data2_H %>%
  mutate(GDP_Growth_Rate = joined_data2_Growth_Rates_H$GDP_Growth)

# Calculate average growth for all years for each H country
Avg_Growth_H <- H_growth_rates %>%
  group_by(Country) %>%
  summarise(Avg_Growth_H = mean(GDP_Growth_Rate, na.rm = TRUE))

# Join average growth data set to original data set (of H countries only)
H_growth_rates <- H_growth_rates %>%
  full_join(Avg_Growth_H, by = "Country")

# Remove columns of GDP per capita and GDP growth for individual years
Only_Avg_Growth_H <- H_growth_rates %>%
  select(Code, Country, Continent, Income_Classification, Avg_Growth_H) %>%
  filter(Country != "Argentina") %>%
  distinct()

# Calculate average growth of all H countries
Avg_Growth_All_H <- mean(Only_Avg_Growth_H$Avg_Growth_H)

## Calculate average growth of all H countries by Continent
Avg_Growth_All_H_by_Continent <- Only_Avg_Growth_H %>%
  group_by(Continent) %>%
  summarise(Avg_Growth_All_H_by_Continent = mean(Avg_Growth_H))

# ----------------------------------------------------------------------------

### Bar Charts

## Low Income countries
Avg_Growth_All_L_by_Continent <- Avg_Growth_All_L_by_Continent %>%
  add_row(Continent = "World", Avg_Growth_All_L_by_Continent = Avg_Growth_Low) %>%
  arrange(Avg_Growth_All_L_by_Continent)

ggplot(Avg_Growth_All_L_by_Continent, 
       aes(x = reorder(Continent, Avg_Growth_All_L_by_Continent), 
           y = Avg_Growth_All_L_by_Continent, 
           fill = Continent)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("North America" = "red",
                               "Africa" = "red",
                               "Oceania" = "red",
                               "World" = "navy",
                               "Asia" = "green")) +
  labs(title = "Average Growth of Low Income Countries by Continent",
       x = "Continent",
       y = "Average percentage GDP per capita Growth")

## Low Middle Income Countries
Avg_Growth_All_LM_by_Continent <- Avg_Growth_All_LM_by_Continent %>%
  add_row(Continent = "World", Avg_Growth_All_LM_by_Continent = Avg_Growth_All_LM) %>%
  arrange(Avg_Growth_All_LM_by_Continent)

ggplot(Avg_Growth_All_LM_by_Continent, 
       aes(x = reorder(Continent, Avg_Growth_All_LM_by_Continent), 
           y = Avg_Growth_All_LM_by_Continent,
           fill = Continent)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("North America" = "green",
                               "Africa" = "red",
                               "Oceania" = "green",
                               "World" = "navy",
                               "South America" = "red",
                               "Asia" = "green",
                               "Europe" = "red")) +
  labs(title = "Average Growth of Low Middle Income Countries by Continent",
       x = "Continent",
       y = "Average percentage GDP per capita Growth")

## Upper Middle Income countries
Avg_Growth_All_UM_by_Continent <- Avg_Growth_All_UM_by_Continent %>%
  add_row(Continent = "World", Avg_Growth_All_UM_by_Continent = Avg_Growth_All_UM) %>%
  arrange(Avg_Growth_All_UM_by_Continent)

ggplot(Avg_Growth_All_UM_by_Continent, 
       aes(x = reorder(Continent, Avg_Growth_All_UM_by_Continent), 
           y = Avg_Growth_All_UM_by_Continent,
           fill = Continent)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("North America" = "red",
                               "Africa" = "red",
                               "Oceania" = "red",
                               "World" = "navy",
                               "South America" = "green",
                               "Asia" = "green",
                               "Europe" = "green")) +
  labs(title = "Average Growth of Upper Middle Income Countries by Continent",
       x = "Continent",
       y = "Average percentage GDP per capita Growth")

## High income countries
Avg_Growth_All_H_by_Continent <- Avg_Growth_All_H_by_Continent %>%
  add_row(Continent = "World", Avg_Growth_All_H_by_Continent = Avg_Growth_All_H) %>%
  arrange(Avg_Growth_All_H_by_Continent)

ggplot(Avg_Growth_All_H_by_Continent, 
       aes(x = reorder(Continent, Avg_Growth_All_H_by_Continent), 
           y = Avg_Growth_All_H_by_Continent,
           fill = Continent)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("North America" = "red",
                               "World" = "navy",
                               "Africa" = "green",
                               "Oceania" = "green",
                               "South America" = "green",
                               "Asia" = "green",
                               "Europe" = "green")) +
  labs(title = "Average Growth of High Income Countries by Continent",
       x = "Continent",
       y = "Average percentage GDP per capita Growth")

# SECTION 6

library(tidyverse)
library(dplyr)
library(ggplot2)

# 0. Load data sets
continents <- read.csv("continents-according-to-our-world-in-data.csv")
GDP_per_capita <- read.csv("gdp-per-capita-worldbank.csv")
Youth <- read.csv("youth-not-in-education-employment-training.csv")  # not used here, but fine to keep
Income_group_classification <- read.csv("World Bank data csv.csv")

# 1. Clean income classification file and merge with continents
Income_group_classification <- Income_group_classification[-c(1:4), ]
Income_group_classification <- Income_group_classification %>%
  select(X, World.Bank.Analytical.Classifications, X.20:X.38)

joined_data1 <- left_join(Income_group_classification, continents, by = c("X" = "Code")) %>%
  select(-Year)

names(joined_data1)[3:21] <- as.character(joined_data1[1, 3:21])
joined_data1 <- joined_data1[-1, ]
joined_data1 <- joined_data1 %>%
  rename(Code = X, Country = World.Bank.Analytical.Classifications)

# 2. Prepare GDP data and merge with income classifications
GDP_per_capita <- GDP_per_capita %>% filter(Year >= 2015)

joined_data1_bottom <- joined_data1 %>%
  pivot_longer(cols = -c(Code, Country, Continent), names_to = "Year", values_to = "Income_Classification")
joined_data1_bottom$Year <- as.integer(joined_data1_bottom$Year)

joined_data2 <- inner_join(joined_data1_bottom, GDP_per_capita, by = c("Code", "Year")) %>%
  select(-Entity) %>%
  rename(`GDP per capita` = GDP.per.capita..PPP..constant.2017.international...)

# 3. LDC GDP growth by continent over time
# Compute GDP growth per year for each low-income country
low_income_growth <- joined_data2 %>%
  filter(Income_Classification == "L") %>%              # low-income only
  arrange(Country, Year) %>%
  group_by(Country, Continent) %>%
  mutate(
    GDP_Growth = (`GDP per capita` - lag(`GDP per capita`)) /
      lag(`GDP per capita`) * 100             # % growth
  ) %>%
  ungroup()

# Average growth per continent per year
LDC_continent_avg <- low_income_growth %>%
  group_by(Year, Continent) %>%
  summarise(
    Avg_Growth = mean(GDP_Growth, na.rm = TRUE),.groups = "drop"
  )

# Plot: one line per continent + 7% SDG target line
ggplot(LDC_continent_avg,
       aes(x = Year, y = Avg_Growth, colour = Continent)) +
  geom_line(linewidth = 1.1) +
  geom_hline(yintercept = 7,
             linetype = "dashed",
             colour = "black",
             linewidth = 1.2) +
  xlim(2016, NA) +
  labs(
    title = "Average GDP Growth of Low-Income Countries by Continent",
    subtitle = "Dashed line shows SDG Target 8.1: 7% annual GDP growth for LDCs",
    x = "Year",
    y = "Average GDP per capita growth (%)",
    colour = "Continent"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")