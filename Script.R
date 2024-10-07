library(tidyverse)
library(lubridate)
library(ggplot2)
library(patchwork)
library(kableExtra)
library(ggdag)
library(broom)

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

# ---- DAG ----

coord_dag <- list(
  x = c(DST = 0, light = 2, crime = 4, day = 2, weather = 3 ),
  y = c(DST = 0, light = 0, crime = 0, day = 0.5, weather = -0.5 )
)

dag <- dagify(
  crime ~ light + weather + day, # Y = crime, X = light, W = DST, Z = rain, V = day of week
  light ~ weather + DST,
  DST ~ day,
  exposure = "DST",
  outcome = "crime",
  coords = coord_dag
)
ggdag_status(dag, node_size = 20) + theme_dag()

# ---- Table ----

data_summary <- merged_data_clean %>%
  select(date, num_property_crime_perht, num_violent_crime_perht, 
         avg_temperature, rain, days_from_dst, dst_dummy, day_of_week) %>%
  head(10)  # Show first 10 rows

kbl(data_summary,
    col.names = (c("Date", "Property Crime per 100k", "Violent Crime per 100k",
                   "Average Temperature", "Rain", "Days from DST", "DST Dummy",
                   "Day of Week")),
    align = c('l', 'c', 'c', 'c', 'c', 'c', 'c', 'c'),
    booktabs = T,
    linesep = "",
    digits = 3,
    caption = "First 10 Rows of Cleaned Data") %>%
  kable_styling(latex_options = c("striped", "hold_position", "scale_down"))

# ---- Plots ----

avg_crime_by_day <- merged_data_clean %>%
  group_by(days_from_dst, dst_dummy) %>%
  summarise(
    avg_property_crime = mean(num_property_crime_perht),
    avg_violent_crime = mean(num_violent_crime_perht),
    .groups = "drop"
  )

property_crime_plot <- ggplot(avg_crime_by_day, aes(x = days_from_dst, avg_property_crime)) +
  geom_point(alpha = 0.5) +
  geom_smooth(data = filter(avg_crime_by_day, dst_dummy == 0), 
              aes(x = days_from_dst, y = avg_property_crime), se = FALSE, colour = "blue", linewidth = 0.5) +
  geom_smooth(data = filter(avg_crime_by_day, dst_dummy == 1), 
              aes(x = days_from_dst, y = avg_property_crime), se = FALSE, colour = "red", linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Avg Property Crimes Around DST", x = "Days from DST Start", y = "Property Crimes per 100k") + 
  theme_minimal()

violent_crime_plot <- ggplot(avg_crime_by_day, aes(x = days_from_dst, avg_violent_crime)) +
  geom_point(alpha = 0.5) +
  geom_smooth(data = filter(avg_crime_by_day, dst_dummy == 0), 
              aes(x = days_from_dst, y = avg_violent_crime), se = FALSE, colour = "blue", linewidth = 0.5) +
  geom_smooth(data = filter(avg_crime_by_day, dst_dummy == 1), 
              aes(x = days_from_dst, y = avg_violent_crime), se = FALSE, colour = "red", linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Avg Violent Crimes Around DST", x = "Days from DST Start", y = "Violent Crimes per 100k") + 
  theme_minimal()

combined_plot <- property_crime_plot + violent_crime_plot
print(combined_plot)

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
  kable_styling(latex_options = "hold_position") %>%
  add_header_above(c(" " = 1, "Statistics" = 4))

# ---- Robustness Check ----

model_property_cubic <- lm(num_property_crime_perht ~ poly(days_from_dst,3)*dst_dummy 
                           + day_of_week + rain + avg_temperature, data = merged_data_clean)
model_violent_cubic <- lm(num_violent_crime_perht ~ poly(days_from_dst,3)*dst_dummy 
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
  extract_dst_coef(model_violent, "Violent - Default"),
  extract_dst_coef(model_property, "Property - Default"),
  extract_dst_coef(model_violent_cubic, "Violent - Cubic"),
  extract_dst_coef(model_property_cubic, "Property - Cubic"),
  extract_dst_coef(model_violent_altband, "Violent - 14 Day Bandwidth"),
  extract_dst_coef(model_property_altband, "Property - 14 Day Bandwidth")
  ) %>%
  mutate(Crime_Type = ifelse(grepl("Violent", Model), "Violent", "Property"),
         Model_Type = gsub("Violent - |Property - ", "", Model)) %>%
  select(Model_Type, Crime_Type,everything(), -Model)

# Create table using kable
kbl(coef_table, 
      col.names = c("Model", "Crime Type", "Estimate", "Std. Error", "t-statistic", "p-value"),
      digits = 3,
      caption = "Effect of DST Across Different Models",
      booktabs = T,
      linesep = "") %>%
  kable_styling(latex_options = "hold_position") %>%
  collapse_rows(1:2) %>%
  add_header_above(c(" " = 2, "Statistics" = 4))