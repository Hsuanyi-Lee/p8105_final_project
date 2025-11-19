
restaurant <- readRDS("~/Desktop/Final_P8105/restaurant_clean.rds")
rat_data <- readRDS("~/Desktop/Final_P8105/rat_data_cleaned.rds")

#====================================================================
  # Normal distribution test
#====================================================================
library(ggplot2)
library(patchwork)
library(dplyr)

restaurant <- readRDS("~/Desktop/Final_P8105/restaurant_clean.rds")
rat_data <- readRDS("~/Desktop/Final_P8105/rat_data_cleaned.rds")

# Zipcode
restaurant <- restaurant %>% mutate(ZIPCODE_fill = as.character(ZIPCODE_fill))
rat_data <- rat_data %>% mutate(Zip = as.character(Zip))

cuisine_summary <- restaurant %>%
  select(CUISINE_CATEGORY, ZIPCODE_fill) %>%
  filter(!is.na(ZIPCODE_fill) & ZIPCODE_fill != "") %>%
  group_by(CUISINE_CATEGORY, ZIPCODE_fill) %>%
  summarise(restaurant_count = n(), .groups = 'drop') %>%
  left_join(
    rat_data %>%
      filter(!is.na(Zip) & Zip != "") %>%
      group_by(Zip) %>%
      summarise(rat_count = n()),
    by = c("ZIPCODE_fill" = "Zip")
  ) %>%
  mutate(rat_count = replace_na(rat_count, 0)) %>%
  group_by(CUISINE_CATEGORY) %>%
  summarise(
    total_restaurants = sum(restaurant_count),
    total_rats = sum(rat_count),
    .groups = 'drop'
  )

# skewness 
calculate_skewness <- function(x) {
  n <- length(x)
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  (sum((x - mean_x)^3, na.rm = TRUE) / n) / (sd_x^3)
}

# Restaurant
cat("Restaurant distribution\n")
cat("===================\n")

restaurant_stats <- cuisine_summary %>%
  summarise(
    n = n(),
    mean = mean(total_restaurants),
    median = median(total_restaurants),
    sd = sd(total_restaurants),
    min = min(total_restaurants),
    max = max(total_restaurants),
    skewness = calculate_skewness(total_restaurants),
    shapiro_p = shapiro.test(total_restaurants)$p.value
  )

print(restaurant_stats)

# Rat
cat("\nRat distribution\n")
cat("===================\n")

rat_stats <- cuisine_summary %>%
  summarise(
    n = n(),
    mean = mean(total_rats),
    median = median(total_rats),
    sd = sd(total_rats),
    min = min(total_rats),
    max = max(total_rats),
    skewness = calculate_skewness(total_rats),
    shapiro_p = shapiro.test(total_rats)$p.value
  )

print(rat_stats)

# figure
p1 <- ggplot(cuisine_summary, aes(x = total_restaurants)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "steelblue", alpha = 0.7) +
  geom_density(color = "darkblue", linewidth = 1) +
  stat_function(fun = dnorm, 
                args = list(mean = restaurant_stats$mean, 
                            sd = restaurant_stats$sd),
                color = "red", linewidth = 1, linetype = "dashed") +
  labs(
    title = paste("Distribution of Total Restaurants per Cuisine\n",
                  "Shapiro–Wilk p =", round(restaurant_stats$shapiro_p, 4)),
    x = "Number of Restaurants per Cuisine",
    y = "Density"
  ) +
  theme_minimal(base_size = 13)

p2 <- ggplot(cuisine_summary, aes(x = total_rats)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "coral", alpha = 0.7) +
  geom_density(color = "darkred", linewidth = 1) +
  stat_function(fun = dnorm, 
                args = list(mean = rat_stats$mean, 
                            sd = rat_stats$sd),
                color = "red", linewidth = 1, linetype = "dashed") +
  labs(
    title = paste("Distribution of Total Rat Sightings per Cuisine\n",
                  "Shapiro–Wilk p =", round(rat_stats$shapiro_p, 4)),
    x = "Number of Rat Sightings per Cuisine",
    y = "Density"
  ) +
  theme_minimal(base_size = 13)


combined_plot <- p1 + p2
print(combined_plot)


  
#====================================================================
# Rat sightings ranking by cuisine 1.0 adjusted for restaurants
#====================================================================
# Standardize zip data types
restaurant <- restaurant %>% mutate(ZIPCODE_fill = as.character(ZIPCODE_fill))
rat_data <- rat_data %>% mutate(Zip = as.character(Zip))

# Basic data aggregation
cuisine_rat_analysis <- restaurant %>%
  select(CUISINE_CATEGORY, ZIPCODE_fill) %>%
  filter(!is.na(ZIPCODE_fill) & ZIPCODE_fill != "") %>%
  group_by(CUISINE_CATEGORY, ZIPCODE_fill) %>%
  summarise(Restaurant_Count = n(), .groups = 'drop') %>%
  left_join(
    rat_data %>%
      filter(!is.na(Zip) & Zip != "") %>%
      group_by(Zip) %>%
      summarise(Rat_Count = n()),
    by = c("ZIPCODE_fill" = "Zip")
  ) %>%
  mutate(Rat_Count = replace_na(Rat_Count, 0))

# Log-standardized ratio method
log_standardized_ranking <- cuisine_rat_analysis %>%
  group_by(CUISINE_CATEGORY) %>%
  summarise(
    Total_Rats = sum(Rat_Count),
    Total_Restaurants = sum(Restaurant_Count),
    Unique_ZIPs = n_distinct(ZIPCODE_fill),
    Rats_per_Restaurant = Total_Rats / Total_Restaurants
  ) %>%
  filter(Total_Restaurants >= 50) %>%
  
  # log transformation and standardization
  mutate(
    Log_Restaurants = log(Total_Restaurants + 1),
    Standardized_Rats = (Total_Rats - mean(Total_Rats, na.rm = TRUE)) / sd(Total_Rats, na.rm = TRUE),
    Standardized_Log_Restaurants = (Log_Restaurants - mean(Log_Restaurants, na.rm = TRUE)) / sd(Log_Restaurants, na.rm = TRUE),
    Standardized_Rat_Density = (Rats_per_Restaurant - mean(Rats_per_Restaurant, na.rm = TRUE)) / sd(Rats_per_Restaurant, na.rm = TRUE),
    Log_Standardized_Ratio = Standardized_Rats / Standardized_Log_Restaurants,
    Log_Standardized_Ratio = ifelse(
      is.infinite(Log_Standardized_Ratio) | is.nan(Log_Standardized_Ratio), 
      0, 
      Log_Standardized_Ratio
    ),
    Log_Standardized_Ratio = ifelse(
      abs(Log_Standardized_Ratio) > 10, 
      sign(Log_Standardized_Ratio) * 10, 
      Log_Standardized_Ratio
    )
  ) %>%
  
  # min–max normalization
  mutate(
    Normalized_Ratio = (Log_Standardized_Ratio - min(Log_Standardized_Ratio)) /
      (max(Log_Standardized_Ratio) - min(Log_Standardized_Ratio))
  ) %>%
  
  # Add ranking
  mutate(
    Rank_Log_Ratio = rank(-Log_Standardized_Ratio)
  ) %>%
  arrange(Rank_Log_Ratio)

# results
cat("Log-Standardized ratio method ranking\n")
cat("====================================================================\n\n")

print(log_standardized_ranking, n = 21, width = Inf)


#====================================================================
# Rat sightings ranking by cuisine 2.0 + zipcode weighted
#====================================================================
# Standardize zipcode data types
restaurant <- restaurant %>% mutate(ZIPCODE_fill = as.character(ZIPCODE_fill))
rat_data <- rat_data %>% mutate(Zip = as.character(Zip))

# Basic data aggregation
cuisine_rat_analysis <- restaurant %>%
  select(CUISINE_CATEGORY, ZIPCODE_fill) %>%
  filter(!is.na(ZIPCODE_fill) & ZIPCODE_fill != "") %>%
  group_by(CUISINE_CATEGORY, ZIPCODE_fill) %>%
  summarise(Restaurant_Count = n(), .groups = 'drop') %>%
  left_join(
    rat_data %>%
      filter(!is.na(Zip) & Zip != "") %>%
      group_by(Zip) %>%
      summarise(Rat_Count = n()),
    by = c("ZIPCODE_fill" = "Zip")
  ) %>%
  mutate(Rat_Count = replace_na(Rat_Count, 0))

# zip weighted rat sightings ranking
log_standardized_ranking <- cuisine_rat_analysis %>%
  group_by(CUISINE_CATEGORY) %>%
  summarise(
    Total_Rats = sum(Rat_Count),
    Total_Restaurants = sum(Restaurant_Count),
    Unique_ZIPs = n_distinct(ZIPCODE_fill),
    Rats_per_Restaurant = Total_Rats / Total_Restaurants
  ) %>%
  filter(Total_Restaurants >= 50) %>%
  
  mutate(
    Weighted_Rats = Total_Rats * Unique_ZIPs
  ) %>%
  
  # standarized weighted_rats and log(total_restaurants) 
  mutate(
    Log_Restaurants = log(Total_Restaurants + 1),
    Standardized_Weighted_Rats = (Weighted_Rats - mean(Weighted_Rats, na.rm = TRUE)) /
      sd(Weighted_Rats, na.rm = TRUE),
    Standardized_Log_Restaurants = (Log_Restaurants - mean(Log_Restaurants, na.rm = TRUE)) /
      sd(Log_Restaurants, na.rm = TRUE),
    
    # zip weighted log-standardized ratio
    Log_Standardized_Ratio = Standardized_Weighted_Rats / Standardized_Log_Restaurants,
    
    # extreme value
    Log_Standardized_Ratio = ifelse(
      is.infinite(Log_Standardized_Ratio) | is.nan(Log_Standardized_Ratio),
      0, Log_Standardized_Ratio
    ),
    Log_Standardized_Ratio = ifelse(
      abs(Log_Standardized_Ratio) > 10,
      sign(Log_Standardized_Ratio) * 10, Log_Standardized_Ratio
    )
  ) %>%
  
  # min–max normalization
  mutate(
    Normalized_Ratio = (Log_Standardized_Ratio - min(Log_Standardized_Ratio)) /
      (max(Log_Standardized_Ratio) - min(Log_Standardized_Ratio))
  ) %>%
  
  # rank
  mutate(
    Rank_Log_Ratio = rank(-Log_Standardized_Ratio)
  ) %>%
  arrange(Rank_Log_Ratio)


cat("zip weighted rat sightings ranking by cuisine\n")
cat("==========================================================\n\n")

print(log_standardized_ranking, n = 25, width = Inf)
