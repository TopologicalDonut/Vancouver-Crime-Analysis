library(tidyverse)
library(lubridate)
library(broom)
library(modelsummary)
library(sandwich)
library(lmtest)
### This is for the qmd file of my blog post.
# library(quartoExtra)

merged_data_clean <- read_csv("Data/Processed/merged_data_clean.csv")

# ---- Table ----

# Show first 10 rows of cleaned data
data_summary <- merged_data_clean %>%
  select(date, num_property_crime_perht, num_violent_crime_perht, 
         avg_temperature, rain, days_from_dst, dst_dummy, day_of_week) %>%
  head(10)  

kbl(data_summary,
    col.names = (c("Date", "Property Crime per 100k", "Violent Crime per 100k",
                   "Average Temperature", "Rain", "Days from DST", "DST Dummy",
                   "Day of Week")),
    align = c('l', 'c', 'c', 'c', 'c', 'c', 'c', 'c'),
    booktabs = T,
    linesep = "",
    digits = 3,
    caption = "First 10 Rows of Cleaned Data") %>%
  column_spec(1, width = "8em") %>%
  column_spec(2, width = "8em") %>%
  column_spec(3, width = "8em")

# ---- Plots ----

### This is also for my blog post. Not needed if running this script independently.
# darkmode_theme_set(
#   dark = ggthemes::theme_stata(scheme = "s1rcolor"),
#   light = ggthemes::theme_stata(scheme = "s1color")
# )

# Grouping crime into average per day so that the graphs are readable. Otherwise there'd be too many data points.
avg_crime_by_day <- merged_data_clean %>%
  group_by(days_from_dst, dst_dummy) %>%
  summarise(
    avg_property_crime = mean(num_property_crime_perht),
    avg_violent_crime = mean(num_violent_crime_perht),
    .groups = "drop"
  )

# Need long format for graphs
avg_crime_by_day_long <- avg_crime_by_day %>%
  pivot_longer(
    cols = c(avg_property_crime, avg_violent_crime),
    names_to = "crime_type",
    values_to = "crime_rate"
  ) %>%
  mutate(crime_type = case_when(
    crime_type == "avg_property_crime" ~ "Property Crime",
    crime_type == "avg_violent_crime" ~ "Violent Crime"
  ))

ggplot(avg_crime_by_day_long, aes(x = days_from_dst, y = crime_rate)) +
  geom_point(alpha = 0.5, colour = "#e69a0e") +
  geom_smooth(data = filter(avg_crime_by_day_long, dst_dummy == 0), 
              se = FALSE, colour = "#a011ed", linewidth = 0.5) +
  geom_smooth(data = filter(avg_crime_by_day_long, dst_dummy == 1), 
              se = FALSE, colour = "red", linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "darkslategrey") +
  facet_wrap(~crime_type, scales = "free_y", ncol = 1) +
  labs(x = "Days from DST Start", 
       y = "Crimes per 100k",
       title = "Average Rate of Crimes Around DST")

# ---- Model Estimation ----

run_model_linear <- function(outcome, bandwidth) {
  formula <- as.formula(paste(outcome, "~ days_from_dst*dst_dummy + day_of_week + rain + avg_temperature"))
  lm(formula, data = merged_data_clean, subset = abs(days_from_dst) <= bandwidth)
}

# 3 week bandwidth for initial results.
model_property <- run_model_linear("num_property_crime_perht", 21)
model_violent <- run_model_linear("num_violent_crime_perht", 21)

# vcov for robust SE.
coef_property <- tidy(coeftest(model_property, vcov = vcovHC), conf.int = TRUE, conf.level = 0.95)
coef_violent <- tidy(coeftest(model_violent, vcov = vcovHC), conf.int = TRUE, conf.level = 0.95)

combined_results <- bind_rows(
  coef_property %>% 
    filter(term == "dst_dummy") %>%
    mutate(crime_type = "Property Crime"),
  coef_violent %>%
    filter(term == "dst_dummy") %>%
    mutate(crime_type = "Violent Crime")
)

# ---- Graph and Table of Results ----

ggplot(combined_results, aes(y = crime_type, x = estimate)) +
  geom_point() +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), 
                    width = 0.2)

modelsummary(
  list("Property Crime" = model_property, "Violent Crime" = model_violent),
  coef_map = c("dst_dummy" = "DST Effect"),
  vcov = "robust",
  gof_map = c("nobs", "vcov.type"),
  statistic = c("conf.int", "s.e. = {std.error}"),
  stars = TRUE,
  title = "Effect of DST on Crime per 100k"
)

# ---- Robustness Check ----

run_model_quad <- function(outcome, bandwidth) {
  formula <- as.formula(paste(outcome, "~ poly(days_from_dst, 2, raw=TRUE)*dst_dummy +
                             day_of_week + rain + avg_temperature"))
  lm(formula, data = merged_data_clean, subset = abs(days_from_dst) <= bandwidth)
}

# Bandwidths are the range of days around start of DST.
bandwidths <- seq(14, 56, by = 7)

get_models_results <- function(bandwidth) {
  linear_prop <- run_model_linear("num_property_crime_perht", bandwidth)
  linear_violent <- run_model_linear("num_violent_crime_perht", bandwidth)
  quad_prop <- run_model_quad("num_property_crime_perht", bandwidth)
  quad_violent <- run_model_quad("num_violent_crime_perht", bandwidth)
  
  results <- bind_rows(
    tidy(coeftest(linear_prop, vcov = vcovHC), conf.int = TRUE) %>% 
      filter(term == "dst_dummy") %>%
      mutate(crime_type = "Property Crime",
             model_type = "Linear",
             bandwidth = bandwidth),
    tidy(coeftest(linear_violent, vcov = vcovHC), conf.int = TRUE) %>%
      filter(term == "dst_dummy") %>%
      mutate(crime_type = "Violent Crime",
             model_type = "Linear",
             bandwidth = bandwidth),
    tidy(coeftest(quad_prop, vcov = vcovHC), conf.int = TRUE) %>% 
      filter(term == "dst_dummy") %>%
      mutate(crime_type = "Property Crime",
             model_type = "Quadratic",
             bandwidth = bandwidth),
    tidy(coeftest(quad_violent, vcov = vcovHC), conf.int = TRUE) %>%
      filter(term == "dst_dummy") %>%
      mutate(crime_type = "Violent Crime",
             model_type = "Quadratic",
             bandwidth = bandwidth)
  )
  return(results)
}

all_results <- map_dfr(bandwidths, get_models_results)

ggplot(all_results, 
       aes(x = bandwidth, y = estimate, 
           ymin = conf.low, ymax = conf.high,
           color = model_type)) +
  geom_point(position = position_dodge(width = 3)) +
  geom_errorbar(position = position_dodge(width = 3), width = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkslategrey") +
  facet_wrap(~crime_type, scales = "free_y") +
  labs(x = "Bandwidth (Days)",
       y = "Estimated DST Effect",
       color = "Model Type",
       title = "DST Effects Across Different Bandwidths",
       subtitle = "Point Estimates and 95% Confidence Intervals") +
  scale_x_continuous(breaks = bandwidths)