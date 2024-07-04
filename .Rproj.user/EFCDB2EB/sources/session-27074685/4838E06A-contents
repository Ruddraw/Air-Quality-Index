#library
library(dplyr)
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

# Inspect the final dataframe
View(final_df)
