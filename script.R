#Project Question:
#How do air quality trends vary across different regions and sub-regions, 
#and what are the potential factors driving these variations?



#library
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(forecast)
library(readr)

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


# Calculate the mean AQI value by region
mean_aqi_by_region <- final_df %>%
  group_by(region) %>%
  summarise(mean_aqi = mean(`AQI Value`, na.rm = TRUE)) %>%
  ungroup()

# Create a named vector of mean AQI values
mean_aqi_vector <- setNames(round(mean_aqi_by_region$mean_aqi, 2), mean_aqi_by_region$region)

# Create a box plot of AQI values by region
ggplot(final_df, aes(x = reorder(region, `AQI Value`), y = `AQI Value`, fill = region)) +
  geom_boxplot() +
  labs(title = "AQI Values by Region",
       x = "Region",
       y = "AQI Value") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3", guide = guide_legend(title = "Region (Mean AQI)")) +
  scale_fill_manual(
    values = scales::brewer_pal(palette = "Set3")(length(mean_aqi_vector)),
    labels = paste(names(mean_aqi_vector), "(Mean AQI:", mean_aqi_vector, ")")
  )



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
  theme_minimal() 
  scale_y_continuous(limits = c(0, NA))  # Ensure y-axis starts from 0



