library(tidyverse)
library(lubridate)
library(car)
library(lme4)

setwd("D:/M.Sc Material/Case Studies Summer 2025/Data")
smoothed_data <- read_csv("all_households_smoothed.csv") %>% 
  mutate(
    date = as.Date(date),
    day_of_year = yday(date),
    summer_solstice = yday(as.Date("2019-06-21")),
    
    seasonal_sin = sin(2*pi*(day_of_year - summer_solstice)/365),
    seasonal_cos = cos(2*pi*(day_of_year - summer_solstice)/365),
    
    household = as.factor(household),
    weekday = as.factor(weekdays(date))
  )
#select(date, yb_0, yb_1, yb_2, yb_3, yb_4, yb_5, yb_6, yb_7,
       #yb_8, yb_9, yb_10, yb_11, yb_12, yb_13, yb_14, yb_15,
       #yb_16, yb_17, yb_18, yb_19, yb_20, yb_21, yb_22, yb_23,
       #household, weekday, seasonal_sin)

set.seed(123)
selected_households <- sample(levels(smoothed_data$household), 30)
filtered_data <- smoothed_data %>% 
  filter(household %in% selected_households)

manova_model <- manova(
  cbind(yb_0, yb_1, yb_2, yb_3, yb_4, yb_5, yb_6, yb_7,
        yb_8, yb_9, yb_10, yb_11, yb_12, yb_13, yb_14, yb_15,
        yb_16, yb_17, yb_18, yb_19, yb_20, yb_21, yb_22, yb_23) ~
    household + weekday + seasonal_sin, data = filtered_data
)

summary(manova_model, test = "Pillai")

manova_interaction <- manova(
  cbind(yb_0, yb_1, yb_2, yb_3, yb_4, yb_5, yb_6, yb_7,
        yb_8, yb_9, yb_10, yb_11, yb_12, yb_13, yb_14, yb_15,
        yb_16, yb_17, yb_18, yb_19, yb_20, yb_21, yb_22, yb_23) ~
    household + weekday * seasonal_sin,
  data = filtered_data
)

anova(manova_model, manova_interaction)

final_model <- manova_model #simpler model in case interactions are insignificant

noon <- aov(yb_12 ~ household + weekday + seasonal_sin, data = filtered_data)
summary(noon)

TukeyHSD(noon, which = "weekday")
)
)
