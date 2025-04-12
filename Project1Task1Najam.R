## Fallstudien II Sommer Semester 2025 ##

## Project 1 Task 1 ##

library(tidyverse)
library(lubridate)
library(zoo)
library(patchwork)

## Loading the data ##

setwd("D:/M.Sc Material/Case Studies Summer 2025/") # Adjust it accordingly
file_paths <- unzip("Haushalte mit Waermepumpe OHNE PV.zip", list = TRUE)$Name
all_data <- map_dfr(file_paths, ~read_csv(
  unz("Haushalte mit Waermepumpe OHNE PV.zip", .x),
  col_types = cols(index = col_datetime())
) %>% 
  mutate(household = str_extract(.x, "\\d+")))

## Adding date/time features and extracting the relevant columns ##

power_data <- all_data %>% 
  select(index, HAUSHALT_TOT, household) %>% 
  mutate(
    date = as.Date(index),
    hour = hour(index),
    weekday = weekdays(date),
    day_of_year = yday(date)
  ) %>% 
  rename(power = HAUSHALT_TOT)

## Part a: Smoothing the data by neighboring hours (±3 hrs)

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

## Adding the weekday column for part b

power_data <- power_data %>%
  mutate(weekday = factor(weekdays(date), 
                          levels = c("Monday", "Tuesday", "Wednesday", 
                                     "Thursday", "Friday", "Saturday", "Sunday"),
                          ordered = TRUE))

## Part b: Smoothing the data by neighboring weekdays (±2 weeks)

smoothed_data_b <- power_data %>%
  arrange(household, index) %>%
  group_by(household, weekday, hour) %>%
  mutate(
    smoothed_power_b = rollapply(power, 
                                 width = list(c(-3:3)*3),
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

## Plotting the datasets for visual representation ##

plot_a <- daily_data_a %>%
  filter(household == "3") %>% # We can change it to any household between 3-40
  ggplot(aes(x = date, y = daily_power)) +
  geom_line(color = "blue", linewidth = 1.0) +
  labs(title = "Part (a)",
       subtitle = "01.01.2019 - 31.12.2019",
       x = "Date", 
       y = "Power Consumption (Watt-hours)") +
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_b <- daily_data_b %>%
  filter(household == "3") %>% # For consistency, make sure it is the same as above
  ggplot(aes(x = date, y = daily_power)) +
  geom_line(color = "red", linewidth = 1.0) +
  labs(title = "Part (b)",
       subtitle = "01.01.2019 - 31.12.2019",
       x = "Date", 
       y = "Power Consumption (Watt-hours)") +
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Combine and plot the smoothed power consumptions for comparison ##

combined_plot <- daily_data_a %>%
  filter(household == "3") %>%
  mutate(method = "Part (a)") %>%
  bind_rows(
    daily_data_b %>%
      filter(household == "3") %>%
      mutate(method = "Part (b)")
  ) %>%
  ggplot(aes(x = date, y = daily_power, color = method)) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "Household 3 - Smoothing Data Comparison",
       x = "Date", y = "Power Consumption (Watt-hours)",
       color = "Method") +
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

combined_plot

## Just in case, I'm keeping this code to join both the smoothing datasets ##

#final_dataset <- daily_data_a %>% 
  #rename(daily_power_a = daily_power) %>% 
  #left_join(daily_data_b %>% rename(daily_power_b = daily_power),
            #by = c("household", "date", "weekday")) %>% 
  #mutate(
    #day_of_week = factor(weekday, levels = c("Monday", "Tuesday", "Wednesday",
                                             #"Thursday", "Friday", "Saturday", "Sunday")),
    #day_of_year = yday(date),
    #household = factor(household)
  #) %>% 
  #select(household, date, day_of_week, day_of_year, daily_power_a, daily_power_b)

