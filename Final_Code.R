# Load necessary libraries
library(tidyverse)
library(corrplot)

# Read the energy consumption data
World.Energy.Consumption <- read.csv("J:/TY_Sem5/R Programming/R Mini Project/energy-data.csv")

# Filter the data for relevant columns and years
df <- World.Energy.Consumption %>%
  select(country, year, biofuel_electricity, hydro_electricity, nuclear_electricity, 
         solar_electricity, wind_electricity, other_renewable_electricity, coal_electricity, 
         gas_electricity, oil_electricity) %>%
  mutate(total = rowSums(across(biofuel_electricity:oil_electricity), na.rm = TRUE)) %>%
  drop_na(total)

str(df)
summary(df)

# Calculate percentage of renewable and non-renewable energy sources
df <- df %>%
  mutate(biofuel_perc = 100 * biofuel_electricity / total,
         hydro_perc = 100 * hydro_electricity / total,
         nuclear_perc = 100 * nuclear_electricity / total,
         solar_perc = 100 * solar_electricity / total,
         wind_perc = 100 * wind_electricity / total,
         other_renewable_perc = 100 * other_renewable_electricity / total,
         coal_perc = 100 * coal_electricity / total,
         gas_perc = 100 * gas_electricity / total,
         oil_perc = 100 * oil_electricity / total,
         renewable = rowSums(across(c(biofuel_perc, hydro_perc, nuclear_perc, solar_perc, wind_perc, other_renewable_perc)), na.rm = TRUE),
         not_renewable = rowSums(across(c(coal_perc, gas_perc, oil_perc)), na.rm = TRUE))

# Filter the data for the specified years
years_to_analyze <- c(1970, 1990, 2000, 2010, 2020, 2023)
df_years <- df %>%
  filter(year %in% years_to_analyze)

# Select countries with more than 50% renewable energy
high_renewable_countries <- df_years %>% 
  filter(renewable > 50) %>%
  distinct(country)

# Select countries with more than 50% non-renewable energy
high_nonrenewable_countries <- df_years %>% 
  filter(renewable < 50) %>%
  distinct(country)

# Plot renewable energy consumption over time for selected countries (5-7)
selected_countries <- df_years %>%
  filter(country %in% c("India", "USA", "Germany", "China", "Brazil", "France", "Japan")) %>%
  ggplot(aes(x = year, y = renewable, color = country)) +
  geom_line(linewidth = 1) +  # Changed size to linewidth
  labs(title = "Renewable Energy Consumption Over Time (1970-2023)",
       x = "Year", y = "Percentage of Renewable Energy") +
  theme_minimal()

selected_countries

# Plot energy consumption distribution in 2020 for renewable vs non-renewable energy
df_2020 <- df_years %>% filter(year == 2020)

ggplot(df_2020, aes(x = reorder(country, renewable), y = renewable, fill = "Renewable")) +
  geom_bar(stat = "identity") +
  geom_bar(aes(y = not_renewable, fill = "Non-Renewable"), stat = "identity", position = "stack") +
  coord_flip() +
  labs(title = "Renewable vs Non-Renewable Energy Consumption in 2020",
       x = "Country", y = "Energy Consumption Percentage") +
  scale_fill_manual(values = c("Renewable" = "#4CAF50", "Non-Renewable" = "#F44336")) +
  theme_minimal()

df_2020

# Pie chart for energy consumption in 2020 for selected countries
ggplot(df_2020 %>% filter(country %in% c("India", "USA", "Germany")), 
       aes(x = "", y = renewable, fill = country)) +
  geom_col() +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Renewable Energy Consumption in 2020") +
  scale_fill_brewer(palette = "Set3")

# Correlation plot of energy consumption variables for selected years
corr_data <- df_years %>%
  select(biofuel_perc:oil_perc) %>%
  cor(use = "complete.obs")

corr_data

# Plot correlation matrix
corrplot(corr_data, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45, title = "Correlation Matrix of Energy Consumption Variables")

# Data analysis for all countries based on selected years
df_analysis_all_countries <- df_years %>%
  group_by(year) %>%
  summarise(average_renewable = mean(renewable, na.rm = TRUE), 
            average_non_renewable = mean(not_renewable, na.rm = TRUE)) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = average_renewable, color = "Renewable"), linewidth = 1) +  # Changed size to linewidth
  geom_line(aes(y = average_non_renewable, color = "Non-Renewable"), linewidth = 1) +  # Changed size to linewidth
  labs(title = "Average Renewable vs Non-Renewable Energy Consumption Over Time (1970-2023)",
       x = "Year", y = "Average Energy Consumption Percentage") +
  scale_color_manual(values = c("Renewable" = "blue", "Non-Renewable" = "#F44336")) +
  theme_minimal()

# Display the plot
df_analysis_all_countries
