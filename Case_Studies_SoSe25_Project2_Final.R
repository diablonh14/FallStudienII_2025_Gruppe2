##################### Case Studies Summer Semester 2025 ######################
##################### Project 2 ##############################################
##################### Group 2 ################################################

### Loading the libraries ###

library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stargazer)
library(changepoint)
library(strucchange)
library(viridis)

### Loading the data ###

setwd("D:/M.Sc Material/Case Studies Summer 2025/Data")
grau <- read_excel("../data/Tankbuch_grau.xlsx")
karriert <- read_excel("../data/Tankbuch_karriert.xlsx")

grau_backup <- grau 
karriert_backup <- karriert

### Addressing some outliers and missing values ###

grau$Datum <- as.Date(grau$Datum)
karriert$Datum <- as.Date(karriert$Datum)

karriert <- karriert %>%
  filter(!is.na(Datum))

impute_neighbor_mean <- function(df, date_col = "Datum", value_col = "Kilometerstand"){ 
  df <- df[order(df[[date_col]]), ]
  n <- nrow(df)
  vals <- df[[value_col]]
  for (i in which(is.na(vals))) {
    prev_idx <- max(which(!is.na(vals[1:(i-1)])), na.rm = TRUE)
    next_idx <- min(which(!is.na(vals[(i+1):n])) + i, na.rm = TRUE)
    if (!is.infinite(prev_idx) && !is.infinite(next_idx)) {
      vals[i] <- mean(c(vals[prev_idx], vals[next_idx]), na.rm = TRUE)
    }
  }
  df[[value_col]] <- vals
  return(df)
}
karriert <- impute_neighbor_mean(karriert, date_col = "Datum", value_col = "Kilometerstand")
grau <- impute_neighbor_mean(grau, date_col = "Datum", value_col = "Kilometerstand")

karriert_backup <- impute_neighbor_mean(karriert_backup, date_col = "Datum", value_col = "Kilometerstand")
grau_backup <- impute_neighbor_mean(grau_backup, date_col = "Datum", value_col = "Kilometerstand")

grau <- grau %>%
  mutate(
    Währung = ifelse(Datum == as.Date("1997-07-23") & is.na(Währung), "italienische Lira", Währung),
    Währung = ifelse(Datum == as.Date("2010-07-10") & is.na(Währung), "Zloty", Währung),
    Währung = ifelse(Datum == as.Date("2010-07-11") & is.na(Währung), "Zloty", Währung),
  )

dates_to_update <- as.Date(c("2010-07-10", "2010-07-11"))

grau$Preis[grau$Datum %in% dates_to_update] <- 
  grau$Preis[grau$Datum %in% dates_to_update] / 10

karriert <- karriert %>%
  mutate(
    post_EUR  = ifelse(Datum >= as.Date("2002-01-01"), 1, 0)
  )

grau <- grau %>%
  mutate(
    post_EUR  = ifelse(Datum >= as.Date("2002-01-01"), 1, 0)
  )


### Task 1 ###

grau <- grau %>% 
  mutate(Buch = "grau")

karriert <- karriert %>% 
  mutate(Verbrauch = NA_real_, Buch = "karriert")

## Currency Exchange ##

karriert <- karriert %>%
  mutate(
    Preis_DEM = case_when(
      Währung == "Dinar" & Datum %in% as.Date(c("1987-08-18", "1987-08-19", "1987-08-20", "1987-08-21")) ~ Preis * (100 / 78045.16),
      Währung == "Drachme" & Datum %in% as.Date(c("1987-08-22", "1987-08-24")) ~ Preis / 76.176,
      Währung == "Drachme" & Datum %in% as.Date(c("1987-09-02", "1987-09-09", "1987-09-12", "1987-09-14")) ~ Preis / 76.478,
      Währung == "Forint" & Datum %in% as.Date(c("1987-09-17", "1987-09-18")) ~ Preis / 26.0756,
      Währung == "Gulden" & Datum == as.Date("1986-08-30") ~ Preis * (88.705 / 100),
      Währung == "irische Pfund" & Datum %in% as.Date(c("1986-09-13", "1986-09-18")) ~ Preis * 2.747,
      Währung == "österreichische Schillinge" & Datum == as.Date("1987-08-17") ~ Preis * (14.223 / 100),
      Währung == "österreichische Schillinge" & Datum == as.Date("1987-09-18") ~ Preis * (14.209 / 100),
      Währung == "britische Pfund" & Datum == as.Date("1986-08-31") ~ Preis * 3.0308,
      Währung == "britische Pfund" & Datum %in% as.Date(c(
        "1986-09-01", "1986-09-03", "1986-09-10", 
        "1986-09-12", "1986-09-19", "1986-09-20"
      )) ~ Preis * 2.9352,
      Währung == "dänische Kronen" & Datum %in% as.Date(c("1994-08-12", "1994-08-20")) ~ Preis * (100 / 394.34),
      Währung == "DM" ~ Preis,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    Preis_EUR = case_when(
      Währung == "Euro" ~ Preis,
      !is.na(Preis_DEM) ~ Preis_DEM / 1.95583,
      TRUE ~ NA_real_
    )
  )

grau <- grau %>%
  mutate(
    Preis_EUR = case_when(
      Währung == "Euro" ~ Preis,
      Währung == "DM" ~ Preis / 1.95583,
      Währung == "österreichische Schillinge" ~ Preis / 13.7603,
      Währung == "tschechische Krone" & Datum %in% as.Date(c("2001-08-04", "2001-08-05")) ~ Preis / 34.235,
      Währung == "Zloty" & Datum %in% as.Date(c("2005-07-16", "2005-07-30", "2005-07-31")) ~ Preis / 4.0758,
      Währung == "Zloty" & Datum %in% as.Date(c("2010-07-10", "2010-07-11")) ~ Preis / 4.008,
      Währung == "britische Pfund" & Datum == as.Date("2002-08-03") ~ Preis / 0.63395,
      Währung == "dänische Kronen" & Datum %in% as.Date(c("2007-08-25", "2007-08-26")) ~ Preis / 7.4491,
      Währung == "isländische Kronen" & Datum %in% as.Date(c("2007-08-10", "2007-08-11", "2007-08-14", "2007-08-20")) ~ Preis / 86.12,
      Währung == "norwegische Kronen" & Datum == as.Date("2006-07-31") ~ Preis / 7.8687,
      Währung == "norwegische Kronen" & Datum %in% as.Date(c("2006-08-07", "2006-08-13", "2006-08-17")) ~ Preis / 8.07,
      Währung == "norwegische Kronen" & Datum %in% as.Date(c("2007-08-04", "2007-08-08")) ~ Preis / 7.9278,
      Währung == "italienische Lira" & Datum %in% as.Date("1997-07-23") ~ Preis / 1936.27,
      TRUE ~ NA_real_
    )
  )



karriert_clean <- karriert %>% filter(!is.na(Preis_EUR))
grau_clean <- grau %>% filter(!is.na(Preis_EUR))

## Consumption (L/100km) calculation ##

consumption <- function(df){
  df <- df %>% 
    arrange(Datum) %>% 
    mutate(
      km_diff = Kilometerstand - lag(Kilometerstand),
      Verbrauch_calc = ifelse(!is.na(km_diff),
                              abs((Liter / km_diff) * 100), NA_real_)
    )
  df$Verbrauch <- ifelse(is.na(df$Verbrauch), df$Verbrauch_calc, df$Verbrauch)
  df
}
grau <- consumption(grau)
karriert <- consumption(karriert)

karriert_clean <- karriert %>% filter(!is.na(Verbrauch_calc),
                                      Verbrauch_calc >= 4,
                                      Verbrauch_calc <= 20,
                                      !is.na(Datum),
                                      km_diff > 0,
                                      km_diff <= 750)
grau_clean <- grau %>% filter(!is.na(Verbrauch_calc),
                              Verbrauch_calc >= 4,
                              Verbrauch_calc <= 20,
                              !is.na(Datum),
                              km_diff > 0,
                              km_diff <= 750)
combined_consumption <- bind_rows(karriert_clean, grau_clean)

ggplot(combined_consumption, aes(x = Datum, y = Verbrauch_calc, color = Buch)) +
  geom_line(linewidth = 1.1) +
  scale_color_viridis_d(option = "B", end = 0.85) +
  labs(
    title = "Petrol Consumption Time Series",
    x = "Date",
    y = "Litre/100 KM",
    color = "Book"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

### Prices per Liter (EUR/L) calculation ###

grau <- grau %>%
  mutate(Preis_pro_Liter = ifelse(!is.na(Preis_EUR) & !is.na(Liter) & Liter > 0, Preis_EUR / Liter, NA_real_))

karriert <- karriert %>%
  mutate(Preis_pro_Liter = ifelse(!is.na(Preis_EUR) & !is.na(Liter) & Liter > 0, Preis_EUR / Liter, NA_real_))

karriert_clean_pl <- karriert %>% filter(!is.na(Preis_pro_Liter),
                                         !is.na(Verbrauch_calc),
                                         !is.na(Datum),
                                         Verbrauch_calc >= 4,
                                         Verbrauch_calc <= 20,
                                         km_diff > 0,
                                         km_diff <= 750)
grau_clean_pl <- grau %>% filter(!is.na(Preis_pro_Liter),
                                 !is.na(Verbrauch_calc),
                                 !is.na(Datum),
                                 Verbrauch_calc >= 4,
                                 Verbrauch_calc <= 20,
                                 km_diff > 0,
                                 km_diff <= 750)
combined_per_liter <- bind_rows(karriert_clean_pl, grau_clean_pl)

ggplot(combined_per_liter, aes(x = Datum, y = Preis_pro_Liter, color = Buch)) +
  geom_line(linewidth = 1.1) +
  scale_color_viridis_d(option = "E", end = 0.85) +
  labs(
    title = "Petrol Price per Liter Over Time",
    x = "Date",
    y = "Price (EUR) per Liter",
    color = "Book"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

### Task 2 ###

### For the change-point detection, we use OLS-CUSUM, ###
### OLS-MOSUM and PELT. The approach is replicated  ###
### for Task 4 ###

## Linear Regression Models for Trend ##

karriert_con_model <- lm(Verbrauch_calc ~ as.numeric(Datum), data = karriert_clean_pl)
summary(karriert_con_model)
stargazer(karriert_con_model, font.size = "normalsize", single.row = T)

grau_con_model <- lm(Verbrauch_calc ~ as.numeric(Datum), data =grau_clean_pl)
summary(grau_con_model)
stargazer(grau_con_model, font.size = "normalsize", single.row = T)

## Changepoint Detection (Karriert) ##

cp_karriert <- cpt.mean(karriert_clean_pl$Verbrauch_calc, method = "PELT")
change_dates_karriert <- karriert_clean_pl$Datum[cpts(cp_karriert)]

cusum_test <- efp(Verbrauch_calc ~ as.numeric(Datum), data = karriert_clean_pl, type = "OLS-CUSUM")
cusum_bp <- breakpoints(Verbrauch_calc ~ as.numeric(Datum), data = karriert_clean_pl)
cusum_bp_indices <- cusum_bp$breakpoints
change_dates_cusum <- karriert_clean$Datum[cusum_bp_indices]

mosum_test <- efp(Verbrauch_calc ~ as.numeric(Datum), data = karriert_clean_pl, type = "OLS-MOSUM")
mosum_bp <- breakpoints(Verbrauch_calc ~ as.numeric(Datum), data = karriert_clean_pl)
mosum_bp_indices <- mosum_bp$breakpoints
change_dates_mosum <- karriert_clean$Datum[mosum_bp_indices]

vline_data <- bind_rows(
  data.frame(Datum = change_dates_karriert, Method = "PELT"),
  data.frame(Datum = change_dates_cusum, Method = "CUSUM"),
  data.frame(Datum = change_dates_mosum, Method = "MOSUM")
)

ggplot(karriert_clean_pl, aes(x = Datum, y = Verbrauch_calc)) +
  geom_line(color = "#440154FF", linewidth = 1.1) +
  geom_smooth(method = "loess", se = FALSE, color = "#FDE725FF", linewidth = 1.2, linetype = "solid") + 
  geom_vline(
    data = vline_data,
    aes(xintercept = Datum, color = Method, linetype = Method),
    linewidth = 1.2,
    show.legend = TRUE
  ) +
  scale_color_manual(
    values = c(PELT = "#21908CFF", CUSUM = "#3F51B5", MOSUM = "#E57373"),
    name = "Change Point Method"
  ) +
  scale_linetype_manual(
    values = c(PELT = "dashed", CUSUM = "dotdash", MOSUM = "twodash"),
    name = "Change Point Method"
  ) +
  labs(
    title = "Change-Points in Karriert Book Consumption",
    x = "Date", y = "Litre/100 KM"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

change_table_karriert_cons <- bind_rows(
  data.frame(Method = "PELT", Change_Date = change_dates_karriert),
  data.frame(Method = "CUSUM", Change_Date = change_dates_cusum),
  data.frame(Method = "MOSUM", Change_Date = change_dates_mosum)
)

print(change_table_karriert_cons)

## Changepoint Detection (Grau) ##

cp_grau <- cpt.mean(grau_clean$Verbrauch_calc, method = "PELT")
change_dates_grau <- grau_clean$Datum[cpts(cp_grau)]

cusum_test_grau <- efp(Verbrauch_calc ~ Datum, data = grau_clean, type = "OLS-CUSUM")
cusum_bp_grau <- breakpoints(Verbrauch_calc ~ Datum, data = grau_clean)
cusum_bp_indices_grau <- cusum_bp_grau$breakpoints
change_dates_cusum_grau <- grau_clean$Datum[cusum_bp_indices_grau]


mosum_test_grau <- efp(Verbrauch_calc ~ Datum, data = grau_clean, type = "OLS-MOSUM")
mosum_bp_grau <- breakpoints(Verbrauch_calc ~ Datum, data = grau_clean)
mosum_bp_indices_grau <- mosum_bp_grau$breakpoints
change_dates_mosum_grau <- grau_clean$Datum[mosum_bp_indices_grau]

vline_data_grau <- bind_rows(
  data.frame(Datum = change_dates_grau, Method = "PELT"),
  data.frame(Datum = change_dates_cusum_grau, Method = "CUSUM"),
  data.frame(Datum = change_dates_mosum_grau, Method = "MOSUM")
)

ggplot(grau_clean_pl, aes(x = Datum, y = Verbrauch_calc)) +
  geom_line(color = "#440154FF", linewidth = 1.1) +
  geom_smooth(method = "loess", se = FALSE, color = "#FDE725FF", linewidth = 1.2, linetype = "solid") + 
  geom_vline(
    data = vline_data_grau,
    aes(xintercept = Datum, color = Method, linetype = Method),
    linewidth = 1.2,
    show.legend = TRUE
  ) +
  scale_color_manual(
    values = c(PELT = "#21908CFF", CUSUM = "#3F51B5", MOSUM = "#E57373"),
    name = "Change Point Method"
  ) +
  scale_linetype_manual(
    values = c(PELT = "dashed", CUSUM = "dotdash", MOSUM = "twodash"),
    name = "Change Point Method"
  ) +
  labs(
    title = "Change-Points in Grau Book Consumption",
    x = "Date", y = "Litre/100 KM"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )


change_table_grau_cons <- bind_rows(
  data.frame(Method = "PELT", Change_Date = change_dates_grau),
  data.frame(Method = "CUSUM", Change_Date = change_dates_cusum_grau),
  data.frame(Method = "MOSUM", Change_Date = change_dates_mosum_grau)
)

print(change_table_grau_cons)

### Task 3 ###

## Plotting odometer readings to show car change ##

ggplot(karriert_backup, aes(x = Datum, y = Kilometerstand)) +
  geom_line(color = "black", linewidth = 1.2) +
  geom_point(color = "black", size = 2, alpha = 0.7) +
  labs(
    title = "Odometer Reading (Karriert)",
    x = "Date",
    y = "Kilometers"
  ) +
  theme_minimal(base_size = 15)

ggplot(grau_backup, aes(x = Datum, y = Kilometerstand)) +
  geom_line(color = "black", linewidth = 1.2) +
  geom_point(color = "black", size = 2, alpha = 0.7) +
  labs(
    title = "Odometer Reading (Grau)",
    x = "Date",
    y = "Kilometers"
  ) +
  theme_minimal(base_size = 15)


### Task 4 ###

karriert_clean_pl <- karriert %>% filter(!is.na(Preis_pro_Liter))
grau_clean_pl <- grau %>% filter(!is.na(Preis_pro_Liter))

## Linear Regression Models for Trend ##

karriert_price_model <- lm(Preis_pro_Liter ~ as.numeric(Datum), data = karriert_clean_pl)
summary(karriert_price_model)
stargazer(karriert_price_model, font.size = "normalsize", single.row = T)

grau_price_model <- lm(Preis_pro_Liter ~ as.numeric(Datum), data = grau_clean_pl)
summary(grau_price_model)
stargazer(grau_price_model, font.size = "normalsize", single.row = T)

## Changepoint Detection (Karriert) ##

cp_karriert_price <- cpt.mean(karriert_clean_pl$Preis_pro_Liter, method = "PELT")
change_dates_karriert_price <- karriert_clean_pl$Datum[cpts(cp_karriert_price)]

cusum_test_price <- efp(Preis_pro_Liter ~ as.numeric(Datum), data = karriert_clean_pl, type = "OLS-CUSUM")
cusum_bp_price <- breakpoints(Preis_pro_Liter ~ as.numeric(Datum), data = karriert_clean_pl)
cusum_bp_indices_price <- cusum_bp_price$breakpoints
change_dates_cusum_price <- karriert_clean_pl$Datum[cusum_bp_indices_price]

mosum_test_price <- efp(Preis_pro_Liter ~ as.numeric(Datum), data = karriert_clean_pl, type = "OLS-MOSUM")
mosum_bp_price <- breakpoints(Preis_pro_Liter ~ as.numeric(Datum), data = karriert_clean_pl)
mosum_bp_indices_price <- mosum_bp_price$breakpoints
change_dates_mosum_price <- karriert_clean_pl$Datum[mosum_bp_indices_price]

vline_data_price <- bind_rows(
  data.frame(Datum = change_dates_karriert_price, Method = "PELT"),
  data.frame(Datum = change_dates_cusum_price, Method = "CUSUM"),
  data.frame(Datum = change_dates_mosum_price, Method = "MOSUM")
)

ggplot(karriert_clean_pl, aes(x = Datum, y = Preis_pro_Liter)) +
  geom_line(color = "#440154FF", linewidth = 1.1) +
  geom_smooth(method = "loess", se = FALSE, color = "#FDE725FF", linewidth = 1.2, linetype = "solid") + 
  geom_vline(
    data = vline_data_price,
    aes(xintercept = Datum, color = Method, linetype = Method),
    linewidth = 1.2,
    show.legend = TRUE
  ) +
  scale_color_manual(
    values = c(PELT = "#21908CFF", CUSUM = "#3F51B5", MOSUM = "#E57373"),
    name = "Change Point Method"
  ) +
  scale_linetype_manual(
    values = c(PELT = "dashed", CUSUM = "dotdash", MOSUM = "twodash"),
    name = "Change Point Method"
  ) +
  labs(
    title = "Change-Points in Karriert Book Price per Liter",
    x = "Date", y = "Price (EUR)"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

change_table_karriert_price <- bind_rows(
  data.frame(Method = "PELT", Change_Date = change_dates_karriert_price),
  data.frame(Method = "CUSUM", Change_Date = change_dates_cusum_price),
  data.frame(Method = "MOSUM", Change_Date = change_dates_mosum_price)
)

print(change_table_karriert_price)

## Changepoint Detection (Grau) ##

cp_grau_price <- cpt.mean(grau_clean_pl$Preis_pro_Liter, method = "PELT")
change_dates_grau_price <- grau_clean_pl$Datum[cpts(cp_grau_price)]

cusum_test_grau_price <- efp(Preis_pro_Liter ~ as.numeric(Datum), data = grau_clean_pl, type = "OLS-CUSUM")
cusum_bp_grau_price <- breakpoints(Preis_pro_Liter ~ as.numeric(Datum), data = grau_clean_pl)
cusum_bp_indices_grau_price <- cusum_bp_grau_price$breakpoints
change_dates_cusum_grau_price <- grau_clean_pl$Datum[cusum_bp_indices_grau_price]

mosum_test_grau_price <- efp(Preis_pro_Liter ~ as.numeric(Datum), data = grau_clean_pl, type = "OLS-MOSUM")
mosum_bp_grau_price <- breakpoints(Preis_pro_Liter ~ as.numeric(Datum), data = grau_clean_pl)
mosum_bp_indices_grau_price <- mosum_bp_grau_price$breakpoints
change_dates_mosum_grau_price <- grau_clean_pl$Datum[mosum_bp_indices_grau_price]

vline_data_grau_price <- bind_rows(
  data.frame(Datum = change_dates_cusum_grau_price, Method = "CUSUM"),
  data.frame(Datum = change_dates_mosum_grau_price, Method = "MOSUM")
)

ggplot(grau_clean_pl, aes(x = Datum, y = Preis_pro_Liter)) +
  geom_line(color = "#440154FF", linewidth = 1.1) +
  geom_smooth(method = "loess", se = FALSE, color = "#FDE725FF", linewidth = 1.2, linetype = "solid") + 
  geom_vline(
    data = vline_data_grau_price,
    aes(xintercept = Datum, color = Method, linetype = Method),
    linewidth = 1.2,
    show.legend = TRUE
  ) +
  scale_color_manual(
    values = c(PELT = "#21908CFF", CUSUM = "#3F51B5", MOSUM = "#E57373"),
    name = "Change Point Method"
  ) +
  scale_linetype_manual(
    values = c(PELT = "dashed", CUSUM = "dotdash", MOSUM = "twodash"),
    name = "Change Point Method"
  ) +
  labs(
    title = "Change-Points in Grau Book Price per Liter",
    x = "Date", y = "Price (EUR)"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

change_table_grau_price <- bind_rows(
  data.frame(Method = "CUSUM", Change_Date = change_dates_cusum_grau_price),
  data.frame(Method = "MOSUM", Change_Date = change_dates_mosum_grau_price)
)

print(change_table_grau_price)

### Task 5 ###

grau_clean_pl <- grau_clean_pl %>%
  mutate(days = as.numeric(Datum - min(Datum)))

karriert_clean_pl <- karriert_clean_pl %>% 
  mutate(days = as.numeric(Datum - min(Datum)))

## Dummy Variable Regression Models ##

grau_eur_model <- lm(Preis_pro_Liter ~ days + post_EUR, data = grau_clean_pl)
summary(grau_eur_model)
stargazer(grau_eur_model, font.size = "normalsize", single.row = T)

karriert_eur_model <- lm(Preis_pro_Liter ~ days + post_EUR, data = karriert_clean_pl)
summary(karriert_eur_model)
stargazer(karriert_eur_model, font.size = "normalsize", single.row = T)

## Chow Test (Grau) ##

break_date <- as.Date("2002-01-01")
break_index <- which.min(abs(as.numeric(grau_clean_pl$Datum - break_date)))
actual_break_date <- grau_clean_pl$Datum[break_index]
print(actual_break_date)

chow_result <- sctest(
  Preis_pro_Liter ~ as.numeric(Datum),
  type = "Chow",
  point = break_index,
  data = grau_clean_pl
)

print(chow_result)

## Chow Test (Karriert) ##

break_date_karriert <- as.Date("2002-01-01")
break_index_karriert <- which.min(abs(as.numeric(karriert_clean_pl$Datum - break_date_karriert)))
actual_break_date_karriert <- karriert_clean_pl$Datum[break_index_karriert]
print(actual_break_date_karriert)

chow_result_karriert <- sctest(
  Preis_pro_Liter ~ as.numeric(Datum),
  type = "Chow",
  point = break_index,
  data = karriert_clean_pl
)

print(chow_result_karriert)
