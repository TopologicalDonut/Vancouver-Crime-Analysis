library(tidyverse)
library(lubridate)
library(patchwork)
library(kableExtra)
library(broom)
### This is for the qmd file of my blog post.
# library(quartoExtra)


# ---- Merge Data ----

data_crime <- read.csv("Data/Raw/crimedata_csv_AllNeighbourhoods_AllYears.csv")
data_pop <- read.csv("Data/Processed/VancouverPop.csv")
data_weather <- read.csv("Data/Raw/weatherstats_vancouver_daily.csv")

data_weather_relevant <- data_weather %>%
  select(date, avg_temperature, rain) %>%
  mutate(date = as_date(date))

data_crime_with_date <- data_crime %>%
  mutate(date = make_date(YEAR,MONTH,DAY))

merged_data <- data_crime_with_date %>%
  left_join(data_pop, by = c("YEAR" = "Year")) %>%
  left_join(data_weather_relevant, by = "date")

write_csv(merged_data, "Data/Processed/merged_data.csv")

# ---- Data Cleaning ----

merged_data <- read_csv("Data/Processed/merged_data.csv")

get_dst_start <- function(date) {
  as.Date(sapply(date, function(date) {
    if (as.integer(format(date, "%Y")) < 2007) {
      # Before 2007, DST started on the first Sunday in April
      dst_date <- as.Date(paste0(format(date, "%Y"), "-04-01"))
      dst_date + (7 - as.integer(format(dst_date, "%u")))
    } else {
      # From 2007 onwards, DST starts on the second Sunday in March
      dst_date <- as.Date(paste0(format(date, "%Y"), "-03-01"))
      dst_date + (7 - as.integer(format(dst_date, "%u"))) + 7
    }
  }))
}

merged_data_clean <- merged_data %>%
  group_by(date, Population, avg_temperature, rain) %>%
  summarise(
    num_property_crime = sum(TYPE %in% c("Break and Enter Commercial", "Break and Enter Residential/Other", 
                  "Other Theft", "Theft from Vehicle", "Theft of Bicycle", "Theft of Vehicle")),
    num_violent_crime = sum(TYPE %in% c("Homicide", "Offence Against a Person")),
    .groups = "drop"
    ) %>%
  mutate(num_property_crime_perht = num_property_crime / Population * 100000,
         num_violent_crime_perht = num_violent_crime / Population * 100000,
         dst_start_date = get_dst_start(date),
         days_from_dst = as.integer(date - dst_start_date),
         dst_dummy = as.integer(days_from_dst >= 0),
         day_of_week = as.factor(wday(date))) %>%  # Sunday is considered the first day with this function
  filter(days_from_dst >= -60 & days_from_dst <= 60) %>%
  drop_na() # Some of the population data isn't available for 2024 yet.

write_csv(merged_data_clean, "Data/Processed/merged_data_clean.csv")

# ---- Table ----

merged_data_clean <- read_csv("Data/Processed/merged_data_clean.csv")

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

property_crime_plot <- ggplot(avg_crime_by_day, aes(x = days_from_dst, avg_property_crime)) +
  geom_point(alpha = 0.5, colour = "#e69a0e") +
  geom_smooth(data = filter(avg_crime_by_day, dst_dummy == 0), 
              aes(x = days_from_dst, y = avg_property_crime), se = FALSE, colour = "#a011ed", linewidth = 0.5) +
  geom_smooth(data = filter(avg_crime_by_day, dst_dummy == 1), 
              aes(x = days_from_dst, y = avg_property_crime), se = FALSE, colour = "red", linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "darkslategrey") +
  labs(title = "Avg Property Crimes Around DST", x = "Days from DST Start", y = "Property Crimes per 100k")

violent_crime_plot <- ggplot(avg_crime_by_day, aes(x = days_from_dst, avg_violent_crime)) +
  geom_point(alpha = 0.5, colour = "#e69a0e") +
  geom_smooth(data = filter(avg_crime_by_day, dst_dummy == 0), 
              aes(x = days_from_dst, y = avg_violent_crime), se = FALSE, colour = "#a011ed", linewidth = 0.5) +
  geom_smooth(data = filter(avg_crime_by_day, dst_dummy == 1), 
              aes(x = days_from_dst, y = avg_violent_crime), se = FALSE, colour = "red", linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "darkslategrey") +
  labs(title = "Avg Violent Crimes Around DST", x = "Days from DST Start", y = "Violent Crimes per 100k")

property_crime_plot / violent_crime_plot

# ---- Model Estimation ----

run_model <- function(outcome, bandwidth) {
  formula <- as.formula(paste(outcome, "~ days_from_dst*dst_dummy + day_of_week + rain + avg_temperature"))
  lm(formula, data = merged_data_clean, subset = abs(days_from_dst) <= bandwidth)
}

model_property <- run_model("num_property_crime_perht", 60)
model_violent <- run_model("num_violent_crime_perht", 60)

coef_property <- tidy(model_property) %>% 
  filter(term == "dst_dummy") %>% 
  select(estimate, std.error, statistic, p.value)

coef_violent <- tidy(model_violent) %>% 
  filter(term == "dst_dummy") %>% 
  select(estimate, std.error, statistic, p.value)

coef_table <- rbind(
  data.frame(Crime = "Property", coef_property),
  data.frame(Crime = "Violent", coef_violent)
)

kbl(coef_table, 
    col.names = c("Crime Type", "Estimate", "Std. Error", "t-statistic", "p-value"),
    align = c('l', 'c', 'c', 'c', 'c'),
    digits = 3,
    caption = "Effect of DST on Crime per 100k",
    booktabs = T) %>%
  add_header_above(c(" " = 1, "Statistics" = 4))

# ---- Robustness Check ----

model_property_cubic <- lm(num_property_crime_perht ~ poly(days_from_dst, 3, raw=TRUE)*dst_dummy +
                             day_of_week + rain + avg_temperature, data = merged_data_clean)
model_violent_cubic <- lm(num_violent_crime_perht ~ poly(days_from_dst, 3, raw=TRUE)*dst_dummy 
                          + day_of_week + rain + avg_temperature, data = merged_data_clean)
model_property_altband <- run_model("num_property_crime_perht", 14)
model_violent_altband <- run_model("num_violent_crime_perht", 14)

extract_dst_coef <- function(model, model_name) {
  tidy(model) %>% 
    filter(term == "dst_dummy") %>% 
    select(estimate, std.error, statistic, p.value) %>%
    mutate(Model = model_name)
}

coef_table <- bind_rows(
  extract_dst_coef(model_violent_cubic, "Violent - Cubic"),
  extract_dst_coef(model_property_cubic, "Property - Cubic"),
  extract_dst_coef(model_violent_altband, "Violent - 14 Day Bandwidth"),
  extract_dst_coef(model_property_altband, "Property - 14 Day Bandwidth")
) %>%
  mutate(Crime_Type = ifelse(grepl("Violent", Model), "Violent", "Property"),
         Model_Type = gsub("Violent - |Property - ", "", Model)) %>%
  select(Model_Type, Crime_Type,everything(), -Model)

kbl(coef_table[2:6], 
    col.names = c("Crime Type", "Estimate", "Std. Error", "t-statistic", "p-value"),
    digits = 3,
    align = c('l', 'c', 'c', 'c', 'c'),
    caption = "Effect of DST Across Different Models") %>%
  pack_rows("Cubic Model", 1, 2) %>%
  pack_rows("14 Day Bandwidth", 3, 4) %>%
  add_header_above(c(" " = 1, "Statistics" = 4))