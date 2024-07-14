#Project Question:
#How do air quality trends vary across different regions and sub-regions, 
#and what are the potential factors driving these variations?



#library
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(forecast)

#load dataset
aqi_df <- read_csv("data_date.csv")
country_df <- read_csv("continents2.csv")

#inspect the data sets
head(aqi_df)
head(country_df)

# Select relevant columns from the country_df
country_df <- country_df %>% 
  select(name, sub_region)

# Merge the datasets on the country and name columns
merged_df <- aqi_df %>%
  inner_join(country_df, by = c("Country" = "name"))

# Select only the columns of interest
final_df <- merged_df %>% 
  select(Date, Country, Status, `AQI Value`, `alpha-2`, region, `sub-region`)

#convert the data column to date type
final_df$Date <- as.Date(final_df$Date, formate = "%Y-%m-%d")

#EDA
# Calculate the mean AQI value by region
mean_aqi_by_region <- final_df %>%
  group_by(region) %>%
  summarise(mean_aqi = mean(`AQI Value`, na.rm = TRUE)) %>%
  ungroup()

# Create a bar plot of mean AQI values by region
ggplot(mean_aqi_by_region, aes(x = reorder(region, -mean_aqi), y = mean_aqi)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean AQI Values by Region",
       x = "Region",
       y = "Mean AQI Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Trend Analysis
# Aggregate the data by month and region to reduce the number of data points
final_df <- final_df %>%
  mutate(Month = floor_date(Date, "month")) %>%
  group_by(Month, region) %>%
  summarise(mean_aqi = mean(`AQI Value`, na.rm = TRUE)) %>%
  ungroup()

# Create a line graph of mean AQI values by region over time
ggplot(final_df, aes(x = Month, y = mean_aqi, color = region)) +
  geom_line() +
  labs(title = "Trend Analysis of Mean AQI Values by Region",
       x = "Date",
       y = "Mean AQI Value",
       color = "Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, NA))  # Ensure y-axis starts from 0

