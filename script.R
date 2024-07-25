#Project Question:
#How do air quality trends vary across different regions and sub-regions, 
#and what are the potential factors driving these variations?



# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(forecast)
library(readr)

# Load datasets
aqi_df <- read.csv("data_date.csv")
country_df <- read.csv("continents2.csv")
population_df <- read.csv("country_population.csv")  # Update the filename to your new population file

# Inspect the data sets
head(aqi_df)
head(country_df)
head(population_df)

# Select relevant columns from the country_df
country_df <- country_df %>% 
  select(name, sub_region = `sub.region`, region)

# Select the relevant columns from population_df
population_df <- population_df %>% 
  select(Country, Population_2022, Population_2023, Yearly_Growth = `Yearly_Growth...`)

# Merge aqi_df with country_df
merged_df <- aqi_df %>%
  inner_join(country_df, by = c("Country" = "name"))

# Merge the population data with the merged_df
final_df <- merged_df %>%
  inner_join(population_df, by = "Country")

# Select only the columns of interest
final_df <- final_df %>% 
  select(Date, Country, Status, `AQI_Value` = `AQI.Value`, region, sub_region, Population_2022, Population_2023, Yearly_Growth)

# Convert the Date column to date type
final_df$Date <- as.Date(final_df$Date, format = "%Y-%m-%d")

# Inspect the final_df to ensure it contains the correct columns
head(final_df)


# Create bar chart to compare how AQI in each region, change color fill to fit with the ranking
ggplot(data = final_df) + 
  geom_bar(mapping = aes(x = region, fill = Status)) + 
  scale_fill_manual(values = c(
    "Good" = "green", 
    "Moderate" = "yellow", 
    "Unhealthy for Sensitive Groups" = "orange", 
    "Unhealthy" = "red", 
    "Very Unhealthy" = "purple", 
    "Hazardous" = "brown"
  )) +
  ggtitle("AQI by Region") 


# Calculate the mean AQI_Value by region
mean_aqi_by_region <- final_df %>%
  group_by(region) %>%
  summarise(mean_aqi = mean(`AQI_Value`, na.rm = TRUE)) %>%
  ungroup()

# Create a named vector of mean AQI_Values
mean_aqi_vector <- setNames(round(mean_aqi_by_region$mean_aqi, 2), mean_aqi_by_region$region)

# Create a box plot of AQI_Values by region
ggplot(final_df, aes(x = reorder(region, `AQI_Value`), y = `AQI_Value`, fill = region)) +
  geom_boxplot() +
  labs(title = "AQI_Values by Region",
       x = "Region",
       y = "AQI_Value") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3", guide = guide_legend(title = "Region (Mean AQI)")) +
  scale_fill_manual(
    values = scales::brewer_pal(palette = "Set3")(length(mean_aqi_vector)),
    labels = paste(names(mean_aqi_vector), "(Mean AQI:", mean_aqi_vector, ")")
  )

# AQI trend
final_df <- final_df %>%
  mutate(Month = floor_date(Date, "month")) %>%
  group_by(Month, region) %>%
  summarise(mean_aqi = mean(`AQI_Value`, na.rm = TRUE)) %>%
  ungroup()

# Create a line graph of mean AQI_Values by region over time
ggplot(final_df, aes(x = Month, y = mean_aqi, color = region)) +
  geom_line() +
  labs(title = "Trend Analysis of Mean AQI_Values by Region",
       x = "Date",
       y = "Mean AQI_Value",
       color = "Region") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, NA))  # Ensure y-axis starts from 0


# Function to create box plot for a given region
create_region_box_plot <- function(region_name) {
  # Filter data for the specified region
  region_df <- final_df %>%
    filter(region == region_name)
  
  # Calculate mean AQI_Values for each sub-region in the specified region
  mean_aqi_by_sub_region <- region_df %>%
    group_by(sub_region) %>%
    summarise(mean_aqi = mean(`AQI_Value`, na.rm = TRUE)) %>%
    ungroup()
  
  # Create a named vector of mean AQI_Values for sub-regions
  mean_aqi_sub_vector <- setNames(round(mean_aqi_by_sub_region$mean_aqi, 2), mean_aqi_by_sub_region$sub_region)
  
  # Create a box plot of AQI_Values by sub-region
  ggplot(region_df, aes(x = reorder(sub_region, `AQI_Value`), y = `AQI_Value`, fill = sub_region)) +
    geom_boxplot() +
    labs(title = paste("AQI_Values by Sub-region in", region_name),
         x = "Sub-region",
         y = "AQI_Value") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set3", guide = guide_legend(title = "Sub-region (Mean AQI)")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(
      values = scales::brewer_pal(palette = "Set3")(length(mean_aqi_sub_vector)),
      labels = paste(names(mean_aqi_sub_vector), "(Mean AQI:", mean_aqi_sub_vector, ")")
    )
}

#box plot for Asia region
create_region_box_plot("Asia")

#box plot for Africa region
create_region_box_plot("Africa")

#box plot for Europe region
create_region_box_plot("Europe")

#box plot for Americas region
create_region_box_plot("Americas")

#box plot for Oceania region
create_region_box_plot("Oceania") 



