# Air Quality and Population Analysis

## Project Overview

This project aims to analyze how air quality trends vary across different regions and sub-regions, and what potential factors might be driving these variations. I performed various statistical analyses and visualizations by utilizing datasets on air quality, country information, and population statistics.

> [!CAUTION]
> This project is intended solely for practice purposes and should not be used to draw real-life conclusions.


## Datasets

-   **Air Quality Data:** Contains daily air quality index (AQI) values for various countries.
-   **Country Data:** Includes information on regions and sub-regions for each country.
-   **Population Data:** Provides population statistics and yearly growth rates for each country.

## Population Data Processing

### Data Sources

-   **Population Data for 2022 and 2023**: Retrieved from [Worldometers](https://www.worldometers.info/).
-   **Yearly Growth Rate**: Calculated using data from the above source in Excel.

### Process

1.  **Population Data Extraction**: Collected population figures for 2022 and 2023 from Worldometers.
2.  **Growth Rate Calculation**: Computed the yearly growth rate based on the population figures for 2022 and 2023 using Excel.
3.  **Population Projection for 2024**: Estimated the population for 2024 using the calculated yearly growth rate in R.

> [!NOTE] The population projection for 2024 is an estimate and may not be highly accurate. The methodology involves using historical growth rates, which may not account for sudden changes in demographic trends.

### Code Snippets for Visualization

Here are some of the key visualizations used in this analysis:

#### Bar Chart to Compare AQI in Each Region

``` r
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
```

![Bar Chart to Compare AQI in Each Region](plots/AQI%20by%20region.png)

### AQI Values by Region

I calculated and plotted the mean AQI values by region using a box plot.

``` r
mean_aqi_by_region <- final_df %>%
  group_by(region) %>%
  summarise(mean_aqi = mean(`AQI_Value`, na.rm = TRUE)) %>%
  ungroup()

mean_aqi_vector <- setNames(round(mean_aqi_by_region$mean_aqi, 2), 
                            mean_aqi_by_region$region)

ggplot(final_df, aes(x = reorder(region, `AQI_Value`), 
                     y = `AQI_Value`, fill = region)) +
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
```

![Boxplot for AQI by region](plots/Boxplot%20for%20AQI%20values.png)

### AQI Trend Analysis

I analyzed the trend of mean AQI values over time for each region.

``` r
final_df <- final_df %>%
  mutate(Month = floor_date(Date, "month")) %>%
  group_by(Month, region) %>%
  summarise(mean_aqi = mean(`AQI_Value`, na.rm = TRUE)) %>%
  ungroup()

ggplot(final_df, aes(x = Month, y = mean_aqi, color = region)) +
  geom_line() +
  labs(title = "Trend Analysis of Mean AQI_Values by Region",
       x = "Date",
       y = "Mean AQI_Value",
       color = "Region") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, NA))
```

![AQI Trend Analysis](plots/Trend%20Analysis%20of%20Mean%20AQI_Values%20by%20Region.png)

### Box Plot for each Region

I created a functions to shpow ox plots for AQI values by sub-region within each specified region.

``` r
create_region_box_plot <- function(region_name) {
  region_df <- final_df %>%
    filter(region == region_name)
  
  mean_aqi_by_sub_region <- region_df %>%
    group_by(sub_region) %>%
    summarise(mean_aqi = mean(`AQI_Value`, na.rm = TRUE)) %>%
    ungroup()
  
  mean_aqi_sub_vector <- setNames(round(mean_aqi_by_sub_region$mean_aqi, 2), 
                                  mean_aqi_by_sub_region$sub_region)
  
  ggplot(region_df, aes(x = reorder(sub_region, `AQI_Value`), 
                        y = `AQI_Value`, 
                        fill = sub_region)) +
    geom_boxplot() +
    labs(title = paste("AQI_Values by Sub-region in", region_name),
         x = "Sub-region",
         y = "AQI_Value") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set3", 
                      guide = guide_legend(title = "Sub-region (Mean AQI)")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(
      values = scales::brewer_pal(palette = "Set3")(length(mean_aqi_sub_vector)),
      labels = paste(names(mean_aqi_sub_vector), "(Mean AQI:", mean_aqi_sub_vector, ")")
    )
}

create_region_box_plot("Asia")
```

![box plot for each region](plots/AQI_Values%20by%20Sub-region%20in%20Asia.png)

``` r
visualize_trends_by_region <- function(data, region_name) {
  region_data <- data %>% filter(region == region_name)
  sub_regions <- unique(region_data$sub_region)
  plot_list <- list()
  
  for (sub_region_name in sub_regions) {
    sub_region_data <- region_data %>% filter(sub_region == sub_region_name)
    
    p <- ggplot(sub_region_data, aes(x = year(Date))) +
      geom_line(aes(y = AQI_Value, color = "AQI Value"), size = 1) +
      geom_line(aes(y = Population/1e6, color = "Population (Millions)"), 
                size = 1) + 
      labs(title = paste("AQI and Population Trends for", sub_region_name),
           x = "Year",
           y = "") +
      scale_y_continuous(
        sec.axis = sec_axis(~ . * 1e6, name = "Population")
      ) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      scale_color_manual(values = c("AQI Value" = "blue", 
                                    "Population (Millions)" = "red"))
    
    plot_list[[sub_region_name]] <- p
  }
  
  grid.arrange(grobs = plot_list, ncol = 1)
}

visualize_trends_by_region(final_df, "Asia")
```

![AQI and Population trends](plots/AQI%20and%20Population%20trends.png%20.png)

### regression model

I created a multiple regression model to find the statistical values

``` r
model <- lm(AQI_Value ~ Yearly_Growth + Population + factor(region) + 
              factor(sub_region), data = final_df)

# Summarize the model to see the results
summary(model)
```

### Interpretation of the Model (modified the language by CHATGPT):

1.  **Intercept (85.95):** This is the average AQI value when all other variables are zero. It represents the baseline AQI value.

2.  **Yearly Growth (-11.98):** The coefficient for yearly growth is -11.98, indicating that as the yearly population growth rate increases by one unit, the AQI value decreases by 11.98 units. This suggests a negative relationship between population growth rate and AQI.

3.  **Population (8.49e-08):** The coefficient for population is 8.49e-08, meaning that as the population increases by one unit, the AQI value increases by a very small amount (8.49e-08). This shows a positive but very weak relationship between population size and AQI.

4.  **Region and Sub-region Factors:** The model includes categorical variables for region and sub-region, which means it controls for the effects of different regions and sub-regions on AQI. For example, the coefficient for the Americas region is -37.63, indicating that the AQI in the Americas is, on average, 37.63 units lower than the baseline region.

5.  **Significance:** The significance levels (Pr(\>\|t\|)) for most coefficients are very low (p \< 0.05), indicating that these variables significantly contribute to predicting AQI values. For instance, the population and region variables have p-values \< 2e-16, showing strong significance.

6.  **Model Fit:** The R-squared value is 0.4927, which means that approximately 49.27% of the variability in AQI values can be explained by the model. The adjusted R-squared value (0.4871) accounts for the number of predictors in the model.
