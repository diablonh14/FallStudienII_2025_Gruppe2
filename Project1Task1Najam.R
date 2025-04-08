library(tidyverse)
library(lubridate)
library(zoo)

setwd("D:/M.Sc Material/Case Studies Summer 2025/")
file_paths <- unzip("Haushalte mit Waermepumpe OHNE PV.zip", list = TRUE)$Name
all_data <- map_dfr(file_paths, ~read_csv(
  unz("Haushalte mit Waermepumpe OHNE PV.zip", .x),
  col_types = cols(index = col_datetime())
) %>% 
  mutate(household = str_extract(.x, "\\d+")))

power_data <- all_data %>% 
  select(index, HAUSHALT_TOT, household) %>% 
  mutate(
    date = as.Date(index),
    hour = hour(index),
    weekday = weekdays(date),
    day_of_year = yday(date)
  ) %>% 
  rename(power = HAUSHALT_TOT)

smoothed_data_a <- power_data %>%
  arrange(index) %>%
  group_by(household, date) %>%
  mutate(
    smoothed_power = rollapply(power, width = 7,
                               FUN = mean, align = "center", fill = NA)
  ) %>%
  ungroup()

daily_data_a <- smoothed_data_a %>% 
  filter(!is.na(smoothed_power)) %>% 
  group_by(household, date, weekday) %>% 
  summarize(
    daily_power = sum(smoothed_power, na.rm = TRUE),
    .groups = "drop"
  )

power_data <- power_data %>%
  mutate(weekday = factor(weekdays(date), 
                          levels = c("Monday", "Tuesday", "Wednesday", 
                                     "Thursday", "Friday", "Saturday", "Sunday"),
                          ordered = TRUE))

smoothed_data_b <- power_data %>%
  arrange(household, index) %>%
  group_by(household, weekday, hour) %>%
  mutate(
    smoothed_power_b = rollapply(power, 
                                 width = list(c(-3:3)*5),
                                 FUN = mean, 
                                 align = "center", 
                                 fill = NA,
                                 na.rm = TRUE)
  ) %>%
  ungroup()

daily_data_b <- smoothed_data_b %>% 
  filter(!is.na(smoothed_power_b)) %>%
  group_by(household, date, weekday) %>%
  summarize(
    daily_power = sum(smoothed_power_b, na.rm = TRUE),
    .groups = "drop"
  )

final_dataset <- daily_data_a %>% 
  rename(daily_power_a = daily_power) %>% 
  left_join(daily_data_b %>% rename(daily_power_b = daily_power),
            by = c("household", "date", "weekday")) %>% 
  mutate(
    day_of_week = factor(weekday, levels = c("Monday", "Tuesday", "Wednesday",
                                             "Thursday", "Friday", "Saturday", "Sunday")),
    day_of_year = yday(date),
    household = factor(household)
  ) %>% 
  select(household, date, day_of_week, day_of_year, daily_power_a, daily_power_b)

final_dataset %>% 
  filter(household == "27") %>% 
  ggplot(aes(x = date, y = daily_power_a))+
  geom_line()+
  facet_wrap(~day_of_week, scales = "free_x") +
  labs(title = "Smoothed Daily Power Consumption with Neighboring Hours",
       subtitle = "Household 27",
       x = "Date", y = "Power Consumption (kWh)")

final_dataset %>% 
  filter(household == "27") %>% 
  ggplot(aes(x = date, y = daily_power_b))+
  geom_line()+
  facet_wrap(~day_of_week, scales = "free_x") +
  labs(title = "Smoothed Daily Power Consumption with Neighboring Days",
       subtitle = "Household 27",
       x = "Date", y = "Power Consumption (kWh)")