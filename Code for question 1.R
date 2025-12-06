library(tidyverse)
library(dplyr)
library(ggplot2)

# Load data sets
continents <- read.csv("continents-according-to-our-world-in-data.csv")
GDP_per_capita <- read.csv("gdp-per-capita-worldbank.csv")
Youth <- read.csv("youth-not-in-education-employment-training.csv")
Income_group_classification <- read.csv("World Bank data csv.csv")


# Remove certain rows and columns
Income_group_classification <- Income_group_classification[-c(1:4), ]
Income_group_classification <- Income_group_classification[!(rownames(Income_group_classification) %in% c("229":"239")), ]
Income_group_classification <- Income_group_classification %>% 
  select(X, World.Bank.Analytical.Classifications, X.20:X.38)

# Join two data sets by country code
joined_data1 <- Income_group_classification %>%
  left_join(continents,
             join_by("X" == "Code")) %>%
  select(-Year)

# Rename certain columns
names(joined_data1)[3:21] <- as.character(joined_data1[1, 3:21])

# Correct country names, remove duplicate columnn of names, and rename certain columns
joined_data1 <- joined_data1 %>% 
  mutate(World.Bank.Analytical.Classifications = ifelse(row_number() >= 7, Entity, World.Bank.Analytical.Classifications)) %>%
  filter(!is.na(World.Bank.Analytical.Classifications)) %>%
  select(-Entity) %>%
  rename(Code = X, Country = World.Bank.Analytical.Classifications)

# Remove row 1 of joined data set
joined_data1 <- joined_data1[-1, ]


# Remove rows with Years before 2006
GDP_per_capita <- GDP_per_capita %>% filter(Year >= 2006)

# Split joined_data1 into two parts
joined_data1_top <- joined_data1 %>% slice(1:5)        # rows before cutoff
joined_data1_bottom <- joined_data1 %>% slice(6:n())   # rows from cutoff onward


# Change joined_data1_bottom to long format
joined_data1_bottom <- joined_data1_bottom %>%
  pivot_longer(cols = -Code & -Country & -Continent,
               names_to = "Year")

joined_data1_bottom$Year <- as.integer(joined_data1_bottom$Year)

# Join bottom part of joined_data1 to GDP_per_capita
joined_data2 <- joined_data1_bottom %>%
  inner_join(GDP_per_capita,
             join_by("Code", "Year")) %>%
  select(-Entity) %>%
  rename(`Income Classification` = `value`,
         `GDP per capita` = GDP.per.capita..PPP..constant.2017.international...)


# Keep only low income countries
joined_data2_Low_Income <- joined_data2 %>%
  filter(`Income Classification` == "L")

# Calculate GDP growth for each low income country
joined_data2_Growth_Rates <- joined_data2 %>%
  filter(`Income Classification` == "L") %>%
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
  full_join(Avg_Growth,
            join_by(Country))


# Remove columns of GDP per capita and GDP growth for individual years
Only_Avg_Growth <- Low_income_growth_rates %>%
  select(Code, Country, Continent, `Income Classification`, Avg_Growth) %>%
  distinct()


# Create separate data sets for each continent (with low income countries)
Africa <- Only_Avg_Growth %>%
  filter(Continent == "Africa")

Asia <- Only_Avg_Growth %>%
  filter(Continent == "Asia")


Oceania <- Only_Avg_Growth %>%
  filter(Continent == "Oceania")

North_America <- Only_Avg_Growth %>%
  filter(Continent == "North America")


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
Countries_meeting_target <- data.frame(Continent = c("Africa", "Asia", "North America", "Oceania"),
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
  filter(`Income Classification` == "LM")

# Calculate GDP growth for each low middle income country
joined_data2_Growth_Rates_LM <- joined_data2 %>%
  filter(`Income Classification` == "LM") %>%
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
  full_join(Avg_Growth_LM,
            join_by(Country))

# Remove columns of GDP per capita and GDP growth for individual years
Only_Avg_Growth_LM <- LM_growth_rates %>%
  select(Code, Country, Continent, `Income Classification`, Avg_Growth_LM) %>%
  filter(Country != "Belarus") %>%
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
  filter(`Income Classification` == "UM")

# Calculate GDP growth for each upper middle income country
joined_data2_Growth_Rates_UM <- joined_data2 %>%
  filter(`Income Classification` == "UM") %>%
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
  full_join(Avg_Growth_UM,
            join_by(Country))

# Remove columns of GDP per capita and GDP growth for individual years
Only_Avg_Growth_UM <- UM_growth_rates %>%
  select(Code, Country, Continent, `Income Classification`, Avg_Growth_UM) %>%
  filter(Country != "Indonesia" & Country != "Mongolia" & Country != "Oman") %>%
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
  filter(`Income Classification` == "H")

# Calculate GDP growth for each high income country
joined_data2_Growth_Rates_H <- joined_data2 %>%
  filter(`Income Classification` == "H") %>%
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
  full_join(Avg_Growth_H,
            join_by(Country))

# Remove columns of GDP per capita and GDP growth for individual years
Only_Avg_Growth_H <- H_growth_rates %>%
  select(Code, Country, Continent, `Income Classification`, Avg_Growth_H) %>%
  distinct()

# Calculate average growth of all UM countries
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
       y = "Average GDP per capita Growth")



## Low Middle Income Countries

Avg_Growth_All_LM_by_Continent <- Avg_Growth_All_LM_by_Continent %>%
  add_row(Continent = "World", Avg_Growth_All_LM_by_Continent = Avg_Growth_All_LM) %>%
  arrange(Avg_Growth_All_LM_by_Continent)

ggplot(Avg_Growth_All_LM_by_Continent, 
       aes(x = reorder(Continent, Avg_Growth_All_LM_by_Continent), 
           y = Avg_Growth_All_LM_by_Continent,
           fill = Continent)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("North America" = "red",
                               "Africa" = "red",
                               "Oceania" = "red",
                               "World" = "navy",
                               "South America" = "green",
                               "Asia" = "green",
                               "Europe" = "green")) +
  labs(title = "Average Growth of Low Middle Income Countries by Continent",
       x = "Continent",
       y = "Average GDP per capita Growth")



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
       y = "Average GDP per capita Growth")



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
       y = "Average GDP per capita Growth")



