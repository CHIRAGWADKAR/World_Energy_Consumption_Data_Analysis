# Load necessary libraries
library(tidyverse)
library(corrplot)

# Read the energy consumption data
World.Energy.Consumption <- 
  read.csv("J:/TY_Sem5/R Programming/R Mini Project/energy-data.csv")

# Filter the data for relevant columns and years
df <- World.Energy.Consumption %>%
  select(country, 
         year, 
         biofuel_electricity, 
         hydro_electricity, 
         nuclear_electricity, 
         solar_electricity, 
         wind_electricity, 
         other_renewable_electricity, 
         coal_electricity, 
         gas_electricity, 
         oil_electricity) %>%
  mutate(total = rowSums(across(biofuel_electricity:oil_electricity), 
                         na.rm = TRUE)) %>%
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
         renewable = rowSums(across(c(biofuel_perc, 
                                      hydro_perc, 
                                      nuclear_perc, 
                                      solar_perc, 
                                      wind_perc, 
                                      other_renewable_perc)), 
                             na.rm = TRUE),
         not_renewable = rowSums(across(c(coal_perc, gas_perc, oil_perc)), 
                                 na.rm = TRUE))

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

# Assign the plot to selected_countries and display it
selected_countries <- df_years %>%
  filter(country %in% c("India", 
                        "United States", 
                        "Germany", 
                        "China", 
                        "Brazil", 
                        "France", 
                        "Japan", 
                        "South Korea")) %>%
  ggplot(aes(x = year, y = renewable, color = country)) +
  geom_line(linewidth = 1) +  # Changed size to linewidth
  labs(title = "Renewable Energy Consumption Over Time (1970-2023)",
       x = "Year", y = "Percentage of Renewable Energy") +
  theme_minimal()

# Display the plot
selected_countries

# Plot energy consumption distribution in 2020 for renewable vs non-renewable energy
df_2020 <- df_years %>% filter(year == 2020)

ggplot(df_2020, aes(x = reorder(country, renewable), 
                    y = renewable, 
                    fill = "Renewable")) +
  geom_bar(stat = "identity") +
  geom_bar(aes(y = not_renewable, 
               fill = "Non-Renewable"), 
           stat = "identity", 
           position = "stack") +
  coord_flip() +
  labs(title = "Renewable vs Non-Renewable Energy Consumption in 2020",
       x = "Country", y = "Energy Consumption Percentage") +
  scale_fill_manual(values = c("Renewable" = "#4CAF50", 
                               "Non-Renewable" = "#F44336")) +
  theme_minimal()

df_2020

# Pie chart for energy consumption in 2020 for selected countries 
# with percentage labels
df_2020_selected <- df_2020 %>%
  filter(country %in% c("India", 
                        "United States", 
                        "Germany", 
                        "China", 
                        "Japan"))

# Calculate percentages for renewable energy
df_2020_selected <- df_2020_selected %>%
  mutate(renewable_perc = round(renewable / sum(renewable) * 100, 1))

# Pie chart with percentages
ggplot(df_2020_selected, aes(x = "", y = renewable, fill = country)) +
  geom_col(width = 1, color = "white") +  # Adjust width for better pie shape
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(renewable_perc, "%")), 
            position = position_stack(vjust = 0.5), 
            size = 4) +  #Add percentage labels
  labs(title = "Renewable Energy Consumption in 2020 for Selected Countries") +
  theme_void() +  # Remove axis lines and ticks
  scale_fill_brewer(palette = "Set3") +  # Add color palette
  theme(legend.position = "right")  # Place legend to the right

# Pie chart for non-renewable energy consumption in 2020 
# for selected countries with percentage labels
df_2020_selected_nonrenewable <- df_2020 %>%
  filter(country %in% c("India", 
                        "United States", 
                        "Germany", 
                        "China", 
                        "Japan"))

# Calculate percentages for non-renewable energy
df_2020_selected_nonrenewable <- df_2020_selected_nonrenewable %>%
  mutate(nonrenewable_perc = round(not_renewable / sum(not_renewable) * 100, 1))

# Create the pie chart for non-renewable energy with percentages
ggplot(df_2020_selected_nonrenewable, aes(x = "", y = not_renewable, fill = country)) +
  geom_col(width = 1, color = "white") +  # Adjust width for pie chart
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(nonrenewable_perc, "%")), 
            position = position_stack(vjust = 0.5), size = 4) +  # Add percentage labels
  labs(title = "Non-Renewable Energy Consumption in 2020 for Selected Countries") +
  theme_void() +  # Remove axis lines and ticks
  scale_fill_brewer(palette = "Set3") +  # Add color palette
  theme(legend.position = "right")  # Place legend to the right


# Boxplot to show the distribution of renewable and non-renewable 
# energy consumption for selected countries

boxplot_data <- df_years %>%
  filter(country %in% c("India", 
                        "USA", 
                        "Germany", 
                        "China", 
                        "Brazil", 
                        "France", 
                        "Japan", 
                        "South Korea")) %>%
  select(country, year, renewable, not_renewable)

# Plotting the boxplot
ggplot(boxplot_data, aes(x = country, y = renewable, fill = country)) +
  geom_boxplot() +
  labs(title = "Boxplot of Renewable Energy Consumption by Country",
       x = "Country", y = "Renewable Energy Consumption (%)") +
  theme_minimal() +
  theme(legend.position = "none")

# Similarly, a boxplot for non-renewable energy consumption
ggplot(boxplot_data, aes(x = country, y = not_renewable, fill = country)) +
  geom_boxplot() +
  labs(title = "Boxplot of Non-Renewable Energy Consumption by Country",
       x = "Country", y = "Non-Renewable Energy Consumption (%)") +
  theme_minimal() +
  theme(legend.position = "none")

# Correlation plot of energy consumption variables for selected years
corr_data <- df_years %>%
  select(biofuel_perc:oil_perc) %>%
  cor(use = "complete.obs")

# Boxplot of distribution of various energy resources 
# across selected countries and years
df_energy_long <- df_years %>%
  pivot_longer(cols = biofuel_perc:oil_perc, 
               names_to = "energy_source", 
               values_to = "percentage")

# Create boxplot to show the distribution of energy resources
ggplot(df_energy_long, aes(x = energy_source, y = percentage, 
                           fill = energy_source)) +
  geom_boxplot() +
  labs(title = "Distribution of Various Energy Resources (1970-2023)",
       x = "Energy Source",
       y = "Percentage of Total Energy Consumption") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")  # Change color palette


# Plot correlation matrix
corrplot(corr_data, 
         method = "circle", 
         type = "upper", 
         tl.col = "black", 
         tl.srt = 45)

# Add title separately
title("Correlation Matrix of Energy Consumption Variables", 
      line = 2, cex.main = 1.2)

# Adjust margins if needed
par(mar = c(1, 1, 4, 1))  # This adjusts the top margin 
                          # to ensure the title is visible

# Data analysis for all countries based on selected years
df_analysis_all_countries <- df_years %>%
  group_by(year) %>%
  summarise(average_renewable = mean(renewable, na.rm = TRUE), 
            average_non_renewable = mean(not_renewable, na.rm = TRUE)) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = average_renewable, color = "Renewable"), 
            linewidth = 1) +  # Changed size to linewidth
  geom_line(aes(y = average_non_renewable, color = "Non-Renewable"), 
            linewidth = 1) +  # Changed size to linewidth
  labs(title = "Avg Renewable vs Non-Renewable Energy Consumption 
                        (1970-2023)",
       x = "Year", 
       y = "Average Energy Consumption Percentage") +
  scale_color_manual(values = c("Renewable" = "blue", 
                                "Non-Renewable" = "#F44336")) +
  theme_minimal()

# Display the plot
df_analysis_all_countries