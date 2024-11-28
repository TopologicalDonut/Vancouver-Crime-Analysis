library(tidyverse)
library(lubridate)
library(kableExtra)
library(broom)
library(modelsummary)
library(sandwich)
library(lmtest)

merged_data_clean <- read_csv("data/processed/merged_data_clean.csv")

# ---- Function Definitions ----

#' This function fits a linear regression designed to estimate the effect of
#' `dst_dummy` on the specified outcome. Note that the formula is hard-coded.
#'
#' @param outcome     Character string of the outcome variable name.
#' @param bandwidth   Numeric value specifying the bandwidth (number of days from DST).
#' @param degree      Numeric value specifying the polynomial degree for `days_from_dst`.
#'
#' @return            An object of class `lm`.
run_model <- function(outcome, bandwidth, degree) {
  polynomial_term <- if (degree == 1) {
    "days_from_dst"
  } else {
    paste0("poly(days_from_dst, ", degree, ", raw=TRUE)")
  }
  
  formula <- as.formula(
    paste(
      outcome, "~",
      polynomial_term, "* dst_dummy + day_of_week + rain + avg_temperature"
    )
  )
  
  lm(
    formula, 
    data = merged_data_clean, 
    subset = abs(days_from_dst) <= bandwidth
  )
}

#' This function returns a string label for the model type based on the polynomial degree.
#'
#' @param degree   Numeric value of the polynomial degree.
#'
#' @return         A character string representing the model type (e.g., "Linear", "Quadratic").
get_model_type_label <- function(degree) {
  if (degree == 1) {
    return("Linear")
  } else if (degree == 2) {
    return("Quadratic")
  } else if (degree == 3) {
    return("Cubic")
  } else {
    return(paste0("Degree ", degree, " Polynomial"))
  }
}

#' This function executes `run_model` for each outcome variable and polynomial degree,
#' extracts the coefficient for `dst_dummy`, and compiles the results.
#'
#' @param outcomes     Named character vector where names are variable names in the data,
#'                     and values are their corresponding labels.
#' @param bandwidth.   Numeric value specifying the bandwidth (number of days from DST).
#' @param degrees      Numeric vector of polynomial degrees to use in the models.
#'
#' @return             A data frame with the results for each model.
get_models_results <- function(outcomes, bandwidth, degrees) {
  results_list <- list()
  
  # Note that names(outcomes) will return the variable names,
  # and outcomes[[outcome]] returns the labels.
  for (outcome in names(outcomes)) {
    crime_label <- outcomes[[outcome]]
    
    for (degree in degrees) {
      model <- run_model(outcome, bandwidth, degree)
      
      tidy_result <- tidy(coeftest(model, vcov = vcovHC), conf.int = TRUE) %>%
        filter(term == "dst_dummy") %>%
        mutate(
          crime_type = crime_label,
          model_type = get_model_type_label(degree),
          bandwidth = bandwidth
        )
      
      results_list <- append(results_list, list(tidy_result))
    }
  }
  
  results <- bind_rows(results_list)
  return(results)
}

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

# Grouping crime into average per day so that the graphs are readable. Otherwise
# there'd be too many data points.
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

# 3 week bandwidth for initial results.
model_property <- run_model("num_property_crime_perht", 21, degree = 1)
model_violent <- run_model("num_violent_crime_perht", 21, degree = 1)

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

outcomes <- c(
  num_property_crime_perht = "Property Crime",
  num_violent_crime_perht = "Violent Crime"
)
bandwidths <- seq(14, 56, by = 7)
degrees <- c(1, 2)

all_results <- map_dfr(
  bandwidths, ~ get_models_results(outcomes, .x, degrees)
)

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