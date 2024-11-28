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