# Air Quality and Population Analysis

## Project Overview
This project aims to analyze how air quality trends vary across different regions and sub-regions, and what potential factors might be driving these variations. I performed various statistical analyses and visualizations by utilizing datasets on air quality, country information, and population statistics.

## Datasets
- **Air Quality Data:** Contains daily air quality index (AQI) values for various countries.
- **Country Data:** Includes information on regions and sub-regions for each country.
- **Population Data:** Provides population statistics and yearly growth rates for each country.

## Population Data Processing

### Data Sources
- **Population Data for 2022 and 2023**: Retrieved from [Worldometers](https://www.worldometers.info/).
- **Yearly Growth Rate**: Calculated using data from the above source in Excel.

### Process
1. **Population Data Extraction**: Collected population figures for 2022 and 2023 from Worldometers.
2. **Growth Rate Calculation**: Computed the yearly growth rate based on the population figures for 2022 and 2023 using Excel.
3. **Population Projection for 2024**: Estimated the population for 2024 using the calculated yearly growth rate in R.

>[!NOTE]
>The population projection for 2024 is an estimate and may not be highly accurate. The methodology involves using historical growth rates, which may not account for sudden changes in demographic trends.
### Code Snippets for Visualization
Here are some of the key visualizations used in this analysis:

#### Bar Chart to Compare AQI in Each Region
```r
ggplot(data = final_df) + 
  geom_bar(mapping = aes(x = region, fill = Status)) + 
  scale_fill_manual(values = c(
    "Good" = "green", 
    "Moderate" = "yellow", 
    "Unhealthy for Sensitive Groups" = "orange", 
    "Unhealthy" = "red", 
    "Very Unhealthy" = "purple"![AQI by region](https://github.com/user-attachments/assets/e8f09e35-732d-42cd-bee9-0c39ce7d8675)
, 
    "Hazardous" = "brown"
  )) +
  ggtitle("AQI by Region")
```
![ Alt Text](/Users/ruddraw/Documents/Code/R_learning/Air-Quality-Index/plots).
