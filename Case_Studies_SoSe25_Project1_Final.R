################## CASE STUDIES SUMMER SEMESTER 2025 ###############
########################### PROJECT 1 ##############################
######## Author: Najam Ul Hasan ############

# Loading All the Required Libraries

library(tidyverse)
library(lubridate)
library(broom)
library(tidyr)
library(stringr)
library(dplyr)
library(ggplot2)
library(tibble)
library(purrr)
library(car) # linear regression and OLS
library(lme4) # MANOVA
library(systemfit) # SUR regression
library(cluster) # clustering
library(matrixStats) # creating centroids to assign clusters to 3 households

# Task 1 is covered in the Python file that does the
# smoothing using a 30-min window (part (a)), and 14-day
# window (part (b)), and generating the all_households_smoothed.csv
# as a result that will be used for the rest of the project.

############################ Task 2 ###########################

# Here we select 30 households at random, and check which model
# is the most feasible one, for the smoothed power output based on
# both the smoothing methods (Moving Averages)

# Data Transformation

setwd("D:/M.Sc Material/Case Studies Summer 2025/Data") # update to your data folder
smoothed_data <- read_csv("all_households_smoothed.csv") %>%
  mutate(
    date = as.Date(date),
    day_of_year = yday(date),
    march21 = yday(as.Date("2019-03-21")),
    seasonal_sin = sin(2 * pi * (day_of_year - march21) / 365),
    shifted_day = (day_of_year - march21) %% 365,
    household = as.factor(household),
    weekday = as.factor(weekdays(date)),
    is_weekend = case_when(
      weekday %in% c("Saturday", "Sunday") ~ 1,
      TRUE ~ 0
    ),
    day_of_week = as.factor(lubridate::wday(date, week_start = 1) - 1)
  )

og_df <- smoothed_data # this will be helpful to work with remaining 3 households

# To describe the data and compare it with the original,
# we take the summary statistics
# As an example, we compare for Household 12 (SFH12)

sfh12_df <- read_csv("SFH12.csv")

# Selecting SFH12 data from Smoothed Dataframe

sfh12_smoothed <- smoothed_data[smoothed_data$household == "SFH12", ]
ya_sum_cols <- grep("^ya_", names(sfh12_smoothed), value = TRUE)
yb_sum_cols <- grep("^yb_", names(sfh12_smoothed), value = TRUE)
ya_sum <- unlist(sfh12_smoothed[ya_sum_cols])
yb_sum <- unlist(sfh12_smoothed[yb_sum_cols])

summary(sfh12_df) # shows summary stats for original

summary(ya_sum) # shows summary stats for ya-smoothing
summary(yb_sum) # shows summary stats for yb-smoothing

# Randomizing the selection for 30 households

set.seed(123)
selected_households <- sample(levels(smoothed_data$household), 30)
filtered_data <- smoothed_data %>% 
  filter(household %in% selected_households)

filtered_data <- filtered_data %>% drop_na()

# Dependent variables (yb-smoothing)
dependent_vars <- filtered_data %>% 
  dplyr::select(starts_with("yb_")) %>%
  colnames()

# Independent variables
independent_vars <- c("household", "day_of_week", "seasonal_sin")

# MANOVA formulas
lhs <- paste0("cbind(", paste(dependent_vars, collapse = ", "), ")")
rhs_add <- paste(independent_vars, collapse = " + ")
rhs_int <- paste(independent_vars, collapse = " * ")
manova_formula <- as.formula(paste(lhs, "~", rhs_add))
manova_formula_int <- as.formula(paste(lhs, "~", rhs_int))

# MANOVA (no interaction)
manova_result_no_int <- manova(manova_formula, data = filtered_data)
summary_no_int <- summary(manova_result_no_int, test = "Wilks")

# MANOVA (with interaction)
manova_result_with_int <- manova(manova_formula_int, data = filtered_data)
summary_with_int <- summary(manova_result_with_int, test = "Wilks")

# MANOVA results
print(summary_no_int)
print(summary_with_int)

# Dependent variables (ya-smoothing)
dependent_vars_ya <- filtered_data %>% 
  dplyr::select(starts_with("ya_")) %>% 
  colnames()

# MANOVA formulas
lhs <- paste0("cbind(", paste(dependent_vars_ya, collapse = ", "), ")")
rhs_add <- paste(independent_vars, collapse = " + ")
rhs_int <- paste(independent_vars, collapse = " * ")
manova_formula_ya <- as.formula(paste(lhs, "~", rhs_add))
manova_formula_ya_int <- as.formula(paste(lhs, "~", rhs_int))

# MANOVA (no interaction)
manova_result_ya_no_int <- manova(manova_formula_ya, data = filtered_data)
summary_ya_no_int <- summary(manova_result_ya_no_int, test = "Wilks")

# MANOVA (with interaction)
manova_result_ya_with_int <- manova(manova_formula_ya_int, data = filtered_data)
summary_ya_with_int <- summary(manova_result_ya_with_int, test = "Wilks")

# MANOVA results 
print(summary_ya_no_int)
print(summary_ya_with_int)

# Not the best approach, as all values tend to inflate
# regardless of results and show up as significant

# OLS Approach, using the AIC to determine which model is better
# household variable dropped as it was mostly insignificant

ols_model <- function(df, target_prefix) {
  target_cols <- grep(paste0("^", target_prefix), colnames(df), value = TRUE)
  feature_cols <- c("day_of_week", "seasonal_sin")
  X <- df[, feature_cols, drop = FALSE]
  Y <- df[, target_cols, drop = FALSE]
  
  n_targets <- ncol(Y)
  n_samples <- nrow(Y)
  n_features <- ncol(X)
  Y_pred <- matrix(NA, nrow = n_samples, ncol = n_targets)
  
  for (j in seq_len(n_targets)) {
    dat <- cbind(target = Y[[j]], X)
    lm_fit <- lm(target ~ ., data = dat)
    Y_pred[, j] <- predict(lm_fit)
  }
  
  residuals <- as.matrix(Y) - Y_pred
  RSS <- sum(residuals ^ 2)
  
  k <- n_targets * (n_features + 1)
  
  sigma2 <- RSS / (n_samples * n_targets)
  log_likelihood <- -0.5 * n_samples * n_targets * (log(2 * pi * sigma2) + 1)
  
  AIC <- 2 * k - 2 * log_likelihood
  return(AIC)
}

ols_model_int <- function(df, target_prefix) {
  target_cols <- grep(paste0("^", target_prefix), colnames(df), value = TRUE)
  feature_cols <- c("day_of_week", "seasonal_sin")
  rhs_int <- paste(feature_cols, collapse = " * ")
  formula_str <- paste("target ~", rhs_int)
  
  n_targets <- length(target_cols)
  n_samples <- nrow(df)
  Y_pred <- matrix(NA, nrow = n_samples, ncol = n_targets)
  k_total <- 0
  
  for (j in seq_len(n_targets)) {
    dat <- df %>%
      dplyr::select(all_of(feature_cols)) %>%
      mutate(target = df[[target_cols[j]]])
    lm_fit <- lm(as.formula(formula_str), data = dat)
    Y_pred[, j] <- predict(lm_fit)
    k_total <- k_total + lm_fit$rank
  }
  
  Y <- as.matrix(df[, target_cols, drop = FALSE])
  residuals <- Y - Y_pred
  RSS <- sum(residuals ^ 2)
  
  sigma2 <- RSS / (n_samples * n_targets)
  log_likelihood <- -0.5 * n_samples * n_targets * (log(2 * pi * sigma2) + 1)
  AIC <- 2 * k_total - 2 * log_likelihood
  
  return(AIC)
}

# AIC for each dependent variable
AIC_ya_simple <- ols_model(filtered_data, "ya_")
AIC_yb_simple <- ols_model(filtered_data, "yb_")
AIC_ya_inter <- ols_model_int(filtered_data, "ya_")
AIC_yb_inter <- ols_model_int(filtered_data, "yb_")

aic_results <- tibble::tibble(
  Model = c("OLS (ya_, simple)", "OLS (ya_, interaction)", "OLS (yb_, simple)" , "OLS (yb_, interaction)"),
  AIC = c(AIC_ya_simple, AIC_yb_simple, AIC_ya_inter, AIC_yb_inter)
)

aic_results

## We nonetheless choose simple model over interaction model,
## despite interaction model having lower AIC value than simple.
## This is because using simple model ensures consistency and is
## less computationally expensive.


############################ Task 3 ###########################

# In this task, we build OLS Models to observe if the
# AIC score for different households based on both
# the smoothing methods, to see whether simple or interaction
# model is feasible.

# Building a SUR Regression Model

regression_model <- function(df) {
  results <- list()
  
  for (household_id in unique(df$household)) {
    df_house <- df %>% filter(household == household_id)
    df_house <- df_house %>% drop_na(all_of(ya_targets))
    
    if (nrow(df_house) == 0) next
    
    equations_no_inter <- lapply(ya_targets, function(target) {
      formula(paste(target, "~ as.factor(day_of_week) + seasonal_sin"))
    })
    names(equations_no_inter) <- ya_targets
    system_no_inter <- list()
    for (i in seq_along(ya_targets)) {
      system_no_inter[[ya_targets[i]]] <- equations_no_inter[[i]]
    }
    
    tryCatch({
      sur_fit <- systemfit(system_no_inter, data = df_house, method = "SUR")
      loglik <- logLik(sur_fit)
      nobs_val <- nobs(sur_fit)
      k <- length(sur_fit$coefficients)
      
      aic <- AIC(sur_fit)
      bic <- BIC(sur_fit)
      
      results[[length(results) + 1]] <- tibble(
        household = household_id,
        loglik = as.numeric(loglik),
        aic = as.numeric(aic),
        bic = as.numeric(bic),
        nobs = nobs_val
      )
    }, error = function(e) {
      message(sprintf("Error in SUR for household %s: %s", household_id, e$message))
    })
  }
  
  if (length(results) > 0) {
    results_df <- bind_rows(results)
  } else {
    results_df <- tibble(
      household = character(),
      loglik = numeric(),
      aic = numeric(),
      bic = numeric(),
      nobs = integer()
    )
  }
  return(results_df)
}

yb_targets <- paste0("yb_", 0:23)
ya_targets <- paste0("ya_", 0:23)

ols_model <- function(df, targets) {
  results <- list()
  coefficients <- list()
  
  for (household_id in unique(df$household)) {
    df_house <- df %>% filter(household == household_id)
    
    for (target in targets) {
      formula_no_interaction <- as.formula(paste(target, "~ seasonal_sin + is_weekend"))
      formula_with_interaction <- as.formula(paste(target, "~ seasonal_sin * is_weekend"))
      
      tryCatch({
        model_no_inter <- lm(formula_no_interaction, data = df_house)
        model_inter <- lm(formula_with_interaction, data = df_house)
        
        results[[length(results) + 1]] <- tibble(
          household = household_id,
          target = target,
          AIC_no_inter = AIC(model_no_inter),
          AIC_with_inter = AIC(model_inter),
          interaction_helpful = AIC(model_inter) - AIC(model_no_inter) < 10
        )
        
        coeff <- as.list(coef(model_no_inter))
        coeff$household <- household_id
        coeff$target <- target
        coefficients[[length(coefficients) + 1]] <- as_tibble_row(coeff)
      }, error = function(e) {
        message(sprintf("Error processing household %s, target %s: %s", household_id, target, e$message))
      })
    }
  }
  
  results_df <- bind_rows(results)
  coeffs_df <- bind_rows(coefficients)
  return(list(results_df = results_df, coeffs_df = coeffs_df))
}

# Coefficients for ya's and yb's (simple model)

results_b_02 <- ols_model(filtered_data, yb_targets)
results_df_b_02 <- results_b_02$results_df
coeffs_df_b_02 <- results_b_02$coeffs_df # yb coefficients for clustering

results_a_02 <- ols_model(filtered_data, ya_targets)
results_df_a_02 <- results_a_02$results_df
coeffs_df_a_02 <- results_a_02$coeffs_df # ya coefficients for clustering

# Determining which model is feasible for each household

# Ya Smoothing

summary_ya <- results_df_a_02 %>% 
  group_by(household) %>% 
  summarize(
    avg_AIC_change = mean(AIC_with_inter - AIC_no_inter, na.rm = TRUE),
    interaction = mean(AIC_with_inter < AIC_no_inter, na.rm = TRUE) > 0.5
  )

# Yb Smoothing

summary_yb <- results_df_b_02 %>% 
  group_by(household) %>% 
  summarize(
    avg_AIC_change = mean(AIC_with_inter - AIC_no_inter, na.rm = TRUE),
    interaction = mean(AIC_with_inter < AIC_no_inter, na.rm = TRUE) > 0.5
  )

# Merging the results and showing the proportion

summary_all <- left_join(
  summary_ya %>% rename(avg_AIC_change_ya = avg_AIC_change, interaction_ya = interaction),
  summary_yb %>% rename(avg_AIC_change_yb = avg_AIC_change, interaction_yb = interaction),
  by = "household"
)

mean(summary_ya$interaction) # shows %age of households that require interaction model, from ya smoothing
mean(summary_yb$interaction) # ditto, but from yb smoothing


## We observe here, that no households in ya require interaction,
## but around 73% (22 households) in yb require in their model.
## However for consistency and less computationally expensive procedure,
## we stick with simple model.


############################ Task 4 ###########################

## In the following task, we take the 30 selected households,
## and cluster them based on the selected OLS model

## Clustering initially for ya's and yb's for all days

# Function to create a wide dataframe

wide_df <- function(coeffs_df) {
  colnms <- colnames(coeffs_df)
  intercept_col <- if ("(Intercept)" %in% colnms) "(Intercept)" else "Intercept"
  vals <- intersect(c(intercept_col, "seasonal_sin", "is_weekend"), colnms)
  if (!all(c("household", "target") %in% colnms)) {
    stop("Coefficients dataframe must have 'household' and 'target' columns.")
  }
  
  coeffs_long <- coeffs_df[, c("household", "target", vals), drop = FALSE]
  coeffs_long$household <- as.factor(coeffs_long$household)
  coeffs_long$target <- as.factor(coeffs_long$target)
  
  df_wide <- tidyr::pivot_wider(
    coeffs_long,
    id_cols = "household",
    names_from = "target",
    values_from = vals,
    names_glue = "{.value}_{target}"
  )
  return(df_wide)
}

## Coefficients converted to a wide matrix, with 
## dimensions of 30 x 73 (incl. household)

yb_coeffs_wide <- wide_df(coeffs_df_b_02)
ya_coeffs_wide <- wide_df(coeffs_df_a_02)

# Function to scale the wide data frame

data_scale <- function(df_wide) {
  X <- df_wide %>% dplyr::select(-household)
  X[] <- lapply(X, function(col) as.numeric(as.character(col)))
  X <- X[, colSums(is.na(X)) < nrow(X), drop = FALSE]
  X <- X[complete.cases(X), , drop = FALSE]
  X_scaled <- scale(X)
  list(X_scaled = X_scaled, scaler = attr(X_scaled, "scaled:scale"))
}

# Scaling ya's

ya_scaled_obj <- data_scale(ya_coeffs_wide)
ya_scaled <- ya_scaled_obj$X_scaled
scaler_a <- ya_scaled_obj$scaler

# Function for Elbow and Silhouette plots (pre-split)

elbow_silhouette <- function(X_scaled) {
  require(cluster)
  require(ggplot2)
  require(factoextra)
  require(gridExtra)
  
  n_points <- nrow(X_scaled)
  max_k <- min(5, n_points - 1)
  if (max_k < 2) {
    stop("Not enough data points for clustering (need at least 3 rows).")
  }
  k_range <- 2:max_k
  
  inertia <- c()
  silhouette_scores <- c()
  
  for (k in k_range) {
    set.seed(42)
    km <- kmeans(X_scaled, centers = k, nstart = 25)
    inertia <- c(inertia, km$tot.withinss)
    sil <- silhouette(km$cluster, dist(X_scaled)) # Silhouette score, use only if Elbow doesn't help
    silhouette_scores <- c(silhouette_scores, mean(sil[, 3]))
  }
  
  # Plotting the graphs
  df <- data.frame(
    k = k_range,
    Inertia = inertia,
    Silhouette = silhouette_scores
  )
  
  p1 <- ggplot(df, aes(x = k, y = Inertia)) +
    geom_line(color = "deepskyblue3") + geom_point(color = "deepskyblue3") +
    ggtitle("Elbow Method For Optimal k") +
    xlab("Number of clusters") + ylab("Inertia") + theme_minimal()
  
  p2 <- ggplot(df, aes(x = k, y = Silhouette)) +
    geom_line(color = "darkorange") + geom_point(color = "darkorange") +
    ggtitle("Silhouette Scores For Different k") +
    xlab("Number of clusters") + ylab("Silhouette Score") + theme_minimal()
  
  # This helps to arrange the plots in one go, grid-like
  gridExtra::grid.arrange(p1, p2, ncol = 2)
}

elbow_silhouette(ya_scaled) # Plotting for ya's (pre-split)

dim(ya_scaled) # should be 30 x 72!

# Clustering for ya's

optimal_k <- 3
d_ya <- dist(ya_scaled)
hc_ya <- hclust(d_ya, method = "ward.D2")
cluster_assignments <- cutree(hc_ya, k = optimal_k)
ya_coeffs_wide$cluster <- cluster_assignments

# Sanity check

head(ya_coeffs_wide[, c("household", "cluster")])
colnames(ya_coeffs_wide)

# Plotting the Cluster Dendogram for ya's (pre-split)

plot(hc_ya, labels = ya_coeffs_wide$household)
rect.hclust(hc_ya, k = optimal_k, border = 2:5)

# PCA Projection Graph for ya's (pre-split)

plot_clusters <- function(X_scaled, df_wide) {
  require(ggplot2)
  
  pca <- prcomp(X_scaled)
  pca_df <- as.data.frame(pca$x[, 1:2])
  colnames(pca_df) <- c("PC1", "PC2")
  pca_df$cluster <- as.factor(df_wide$cluster)
  pca_df$household <- df_wide$household
  
  ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster, label = household)) +
    geom_point(size = 4, alpha = 0.8) +
    geom_text(vjust = -0.7, size = 3, alpha = 0.7) +
    labs(
      title = "Household Clusters based on Coefficients",
      x = "Principal Component 1",
      y = "Principal Component 2",
      color = "Cluster"
    ) +
    scale_color_brewer(palette = "Set1") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    )
}

plot_clusters(ya_scaled, ya_coeffs_wide)

# The same procedure, but now for yb's

yb_scaled_obj <- data_scale(yb_coeffs_wide)
yb_scaled <- yb_scaled_obj$X_scaled
scaler_yb <- yb_scaled_obj$scaler

# Function to create Elbow and Silhouette plots
# (the reason being it can have more than 5 pts. max)

elbow_silhouette <- function(X_scaled) {
  require(cluster)
  require(ggplot2)
  require(factoextra)
  require(gridExtra)
  
  n_unique <- nrow(unique(as.data.frame(X_scaled)))
  n_points <- nrow(X_scaled)
  max_k <- min(10, n_unique, n_points - 1)
  if (max_k < 2) {
    stop("Not enough unique data points for clustering (need at least 3 distinct rows).")
  }
  k_range <- 2:max_k
  
  inertia <- c()
  silhouette_scores <- c()
  
  for (k in k_range) {
    set.seed(42)
    km <- kmeans(X_scaled, centers = k, nstart = 25)
    inertia <- c(inertia, km$tot.withinss)
    sil <- silhouette(km$cluster, dist(X_scaled))
    silhouette_scores <- c(silhouette_scores, mean(sil[, 3]))
  }
  
  df <- data.frame(
    k = k_range,
    Inertia = inertia,
    Silhouette = silhouette_scores
  )
  
  p1 <- ggplot(df, aes(x = k, y = Inertia)) +
    geom_line(color = "deepskyblue3") + geom_point(color = "deepskyblue3") +
    ggtitle("Elbow Method For Optimal k") +
    xlab("Number of clusters") + ylab("Inertia") + theme_minimal()
  
  p2 <- ggplot(df, aes(x = k, y = Silhouette)) +
    geom_line(color = "darkorange") + geom_point(color = "darkorange") +
    ggtitle("Silhouette Scores For Different k") +
    xlab("Number of clusters") + ylab("Silhouette Score") + theme_minimal()
  
  gridExtra::grid.arrange(p1, p2, ncol = 2)
}

elbow_silhouette(yb_scaled)  # Elbow failed, so Silhouette works

# Clustering for yb's (pre-split)

optimal_k <- 2 # From the silhouette plot
d_yb <- dist(yb_scaled)
hc_yb <- hclust(d_yb, method = "ward.D2")
cluster_assignments <- cutree(hc_yb, k = optimal_k)
yb_coeffs_wide$cluster <- cluster_assignments

# Sanity check, yet again

head(yb_coeffs_wide[, c("household", "cluster")])
colnames(yb_coeffs_wide)

# Plotting the Cluster Dendogram for yb's (pre-split)

plot(hc_yb, labels = yb_coeffs_wide$household)
rect.hclust(hc_yb, k = optimal_k, border = 2:5)

# Function to plot the PCA Projection for yb's (pre-split)

plot_clusters(yb_scaled, yb_coeffs_wide)

# Creating another copy(s) of the original data frame(s) for both cases

df <- smoothed_data[, c(
  "date", 
  paste0("yb_", 0:23), 
  "household", "day_of_week", "day_of_year", "shifted_day", "seasonal_sin", "is_weekend"
)]

df_a <- smoothed_data[, c(
  "date", 
  paste0("ya_", 0:23), 
  "household", "day_of_week", "day_of_year", "shifted_day", "seasonal_sin", "is_weekend"
)]

# Spliting the data into non-weekend and weekend, 
# dropping 'is_weekend' column, and ensuring 30 households are selected

df_noweekend <- df[df$is_weekend == 0 & df$household %in% selected_households, ]
df_weekend   <- df[df$is_weekend == 1 & df$household %in% selected_households, ]

df_a_noweekend <- df_a[df_a$is_weekend == 0 & df_a$household %in% selected_households, ]
df_a_weekend   <- df_a[df_a$is_weekend == 1 & df_a$household %in% selected_households, ]


# Running our OLS models for each subset

results_df_b_now <- ols_model(df_noweekend, yb_targets)[[1]]
coeffs_df_b_now  <- ols_model(df_noweekend, yb_targets)[[2]]
results_df_b_w   <- ols_model(df_weekend, yb_targets)[[1]]
coeffs_df_b_w    <- ols_model(df_weekend, yb_targets)[[2]]

results_df_a_now <- ols_model(df_a_noweekend, ya_targets)[[1]]
coeffs_df_a_now  <- ols_model(df_a_noweekend, ya_targets)[[2]]
results_df_a_w   <- ols_model(df_a_weekend, ya_targets)[[1]]
coeffs_df_a_w    <- ols_model(df_a_weekend, ya_targets)[[2]]

# Converting to wide format

yb_coeffs_now <- wide_df(coeffs_df_b_now)
yb_coeffs_w   <- wide_df(coeffs_df_b_w)

ya_coeffs_now <- wide_df(coeffs_df_a_now)
ya_coeffs_w   <- wide_df(coeffs_df_a_w)

# View columns of wide data

colnames(yb_coeffs_w)
colnames(ya_coeffs_w)

colnames(yb_coeffs_now)
colnames(ya_coeffs_now)

# Scaling our split data for each smoothing method

yb_scaled_now_obj <- data_scale(yb_coeffs_now)
yb_scaled_now <- yb_scaled_now_obj$X_scaled # yb's Weekday

yb_scaled_w_obj <- data_scale(yb_coeffs_w)
yb_scaled_w <- yb_scaled_w_obj$X_scaled # yb's Weekend

ya_scaled_now_obj <- data_scale(ya_coeffs_now)
ya_scaled_now <- ya_scaled_now_obj$X_scaled # ya's Weekday

ya_scaled_w_obj <- data_scale(ya_coeffs_w)
ya_scaled_w <- ya_scaled_w_obj$X_scaled # ya's Weekend


# Creating a function for Elbow and Silhouette for split data

elbow_silhouette_split <- function(X_scaled) {
  require(cluster)
  require(ggplot2)
  require(gridExtra)
  
  if (is.list(X_scaled) && !is.null(X_scaled$X_scaled)) {
    X_scaled <- X_scaled$X_scaled
  }
  
  X_unique <- unique(as.data.frame(X_scaled)) 
  n_points <- nrow(X_unique)
  max_k <- min(10, n_points - 1)
  
  if (max_k < 2) {
    stop("Not enough unique data points for clustering (need at least 3 distinct rows).")
  }
  k_range <- 2:max_k
  
  inertia <- c()
  silhouette_scores <- c()
  
  for (k in k_range) {
    set.seed(42)
    km <- kmeans(X_scaled, centers = k, nstart = 25)
    inertia <- c(inertia, km$tot.withinss)
    sil <- silhouette(km$cluster, dist(X_scaled))
    silhouette_scores <- c(silhouette_scores, mean(sil[, 3]))
  }
  
  df <- data.frame(
    k = k_range,
    Inertia = inertia,
    Silhouette = silhouette_scores
  )
  
  p1 <- ggplot(df, aes(x = k, y = Inertia)) +
    geom_line(color = "deepskyblue3") + geom_point(color = "deepskyblue3") +
    ggtitle("Elbow Method For Optimal k") +
    xlab("Number of clusters") + ylab("Inertia") + theme_minimal()
  
  p2 <- ggplot(df, aes(x = k, y = Silhouette)) +
    geom_line(color = "darkorange") + geom_point(color = "darkorange") +
    ggtitle("Silhouette Scores For Different k") +
    xlab("Number of clusters") + ylab("Silhouette Score") + theme_minimal()
  
  gridExtra::grid.arrange(p1, p2, ncol = 2)
}

elbow_silhouette_split(yb_scaled_now)
elbow_silhouette_split(yb_scaled_w)

elbow_silhouette_split(ya_scaled_now)
elbow_silhouette_split(ya_scaled_w)

# k = 3 for ya's and k = 2 for yb's (in both cases)

# Scaling the coefficients post-split

# Ya's Weekday

optimal_k <- 3
d_ya_now <- dist(ya_scaled_now)
hc_ya_now <- hclust(d_ya_now, method = "ward.D2")
cluster_a_now <- cutree(hc_ya_now, k = optimal_k)
ya_coeffs_now$cluster <- cluster_a_now

plot_clusters(ya_scaled_now, ya_coeffs_now)


# Ya's Weekend

optimal_k <- 3
d_ya_w <- dist(ya_scaled_w)
hc_ya_w <- hclust(d_ya_w, method = "ward.D2")
cluster_a_w <- cutree(hc_ya_w, k = optimal_k)
ya_coeffs_w$cluster <- cluster_a_w

plot_clusters(ya_scaled_w, ya_coeffs_w)


# Yb's Weekday

optimal_k <- 2
d_yb_now <- dist(yb_scaled_now)
hc_yb_now <- hclust(d_yb_now, method = "ward.D2")
cluster_now <- cutree(hc_yb_now, k = optimal_k)
yb_coeffs_now$cluster <- cluster_now

plot_clusters(yb_scaled_now, yb_coeffs_now)


# Yb's Weekend

optimal_k <- 2
d_yb_w <- dist(yb_scaled_w)
hc_yb_w <- hclust(d_yb_w, method = "ward.D2")
cluster_w <- cutree(hc_yb_w, k = optimal_k)
yb_coeffs_w$cluster <- cluster_w

plot_clusters(yb_scaled_w, yb_coeffs_w)

# Task 4 Ends here... a lot of modular commands.

############################ Task 5 ###########################

# Here, we classify the remaining 3 households from the
# random selection, using the nearest centroids
# Be prepared for a lot of modular commands...

# Getting the three remaining households
household_3 <- og_df[!(og_df$household %in% selected_households), ]
household_3_nonas <- household_3[complete.cases(household_3), ] # ensuring no NAs are there

# Running the OLS models for yb and ya targets
household_3_results_df_b_02 <- ols_model(household_3_nonas, yb_targets)[[1]]
household_3_coeffs_df_b_02  <- ols_model(household_3_nonas, yb_targets)[[2]]
household_3_results_df_a_02 <- ols_model(household_3_nonas, ya_targets)[[1]]
household_3_coeffs_df_a_02  <- ols_model(household_3_nonas, ya_targets)[[2]]

household_3_yb_coeffs_wide <- wide_df(household_3_coeffs_df_b_02)
household_3_ya_coeffs_wide <- wide_df(household_3_coeffs_df_a_02)

# Removing the household column and preparing them for scaling
ya_features <- colnames(ya_scaled)
yb_features <- colnames(yb_scaled)

household_3_ya_coeffs_wide_1 <- household_3_ya_coeffs_wide[, ya_features, drop = FALSE]
household_3_yb_coeffs_wide_1 <- household_3_yb_coeffs_wide[, yb_features, drop = FALSE]

household_3_ya_coeffs_wide_1[] <- lapply(household_3_ya_coeffs_wide_1, as.numeric)
household_3_yb_coeffs_wide_1[] <- lapply(household_3_yb_coeffs_wide_1, as.numeric)

# Scaling the 3 Remaining Households' Coefficients for both ya's and yb's

new_data_scaled_a <- scale(
  household_3_ya_coeffs_wide_1,
  center = attr(ya_scaled, "scaled:center"),
  scale  = attr(ya_scaled, "scaled:scale")
)
new_data_scaled_b <- scale(
  household_3_yb_coeffs_wide_1,
  center = attr(yb_scaled, "scaled:center"),
  scale  = attr(yb_scaled, "scaled:scale")
)

# Assigning the clusters to new data using nearest centroids (from Hierarchical clustering)

get_hc_centroids <- function(X_scaled, clusters) {
  as.data.frame(X_scaled) |>
    dplyr::mutate(cluster = clusters) |>
    dplyr::group_by(cluster) |>
    dplyr::summarise_all(mean) |>
    dplyr::arrange(cluster)
}

assign_to_centroid <- function(new_data_scaled, centroids) {
  apply(as.matrix(new_data_scaled), 1, function(row) {
    dists <- apply(centroids[,-1], 1, function(centroid) sum((row - centroid)^2))
    centroids$cluster[which.min(dists)]
  })
}

# Getting our respective Centroids and assigning them to scaled data

ya_centroids <- get_hc_centroids(ya_scaled, ya_coeffs_wide$cluster)
yb_centroids <- get_hc_centroids(yb_scaled, yb_coeffs_wide$cluster)

new_clusters_a <- assign_to_centroid(new_data_scaled_a, ya_centroids)
household_3_ya_coeffs_wide$cluster <- new_clusters_a # for ya's

new_clusters_b <- assign_to_centroid(new_data_scaled_b, yb_centroids)
household_3_yb_coeffs_wide$cluster <- new_clusters_b # for yb's

# Sanity check
head(household_3_yb_coeffs_wide[, c("household", "cluster")])
head(household_3_ya_coeffs_wide[, c("household", "cluster")])

# Visualizing the remaining households' clusters with existing ones

# Combining the data from here and Task 4 (ya)

combined_data_ya <- rbind(ya_scaled, new_data_scaled_a)
combined_labels_ya <- c(ya_coeffs_wide$cluster, new_clusters_a)
combined_source_ya <- c(rep("original", nrow(ya_scaled)), rep("new", nrow(new_data_scaled_a)))

# PCA Projection
pca_ya <- prcomp(combined_data_ya)
reduced_ya <- as.data.frame(pca_ya$x[, 1:2])
colnames(reduced_ya) <- c("PC1", "PC2")
reduced_ya$cluster <- as.factor(combined_labels_ya)
reduced_ya$source <- combined_source_ya
reduced_ya$household <- c(as.character(ya_coeffs_wide$household), as.character(household_3_ya_coeffs_wide$household))

# Plotting for ya's

ggplot(reduced_ya, aes(x = PC1, y = PC2, color = cluster, shape = source, label = household)) +
  geom_point(size = 3, alpha = 0.7, aes(fill = cluster)) +
  geom_text(vjust = -1, size = 3, alpha = 0.6, show.legend = FALSE) +
  scale_shape_manual(values = c("original" = 16, "new" = 4)) +
  labs(
    title = "PCA projection - Smoothing Method A",
    x = "Principal Component 1",
    y = "Principal Component 2",
    color = "Cluster",
    shape = "Source"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  guides(fill = "none")


# Combining the data from here and Task 4 (yb)

combined_data_yb <- rbind(yb_scaled, new_data_scaled_b)
combined_labels_yb <- c(yb_coeffs_wide$cluster, new_clusters_b)
combined_source_yb <- c(rep("original", nrow(yb_scaled)), rep("new", nrow(new_data_scaled_b)))

# PCA Projection

pca_yb <- prcomp(combined_data_yb)
reduced_yb <- as.data.frame(pca_yb$x[, 1:2])
colnames(reduced_yb) <- c("PC1", "PC2")
reduced_yb$cluster <- as.factor(combined_labels_yb)
reduced_yb$source <- combined_source_yb
reduced_yb$household <- c(as.character(yb_coeffs_wide$household), as.character(household_3_yb_coeffs_wide$household))

# 3. Plotting for yb's

ggplot(reduced_yb, aes(x = PC1, y = PC2, color = cluster, shape = source, label = household)) +
  geom_point(size = 3, alpha = 0.7, aes(fill = cluster)) +
  geom_text(vjust = -1, size = 3, alpha = 0.6, show.legend = FALSE) +
  scale_shape_manual(values = c("original" = 16, "new" = 4)) +
  labs(
    title = "PCA Projection - Smoothing Method B",
    x = "Principal Component 1",
    y = "Principal Component 2",
    color = "Cluster",
    shape = "Source"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  guides(fill = "none")


# Subset household_3 for weekend and non-weekend, drop 'is_weekend'

household_3_w   <- household_3[household_3$is_weekend == 1, ]
household_3_now <- household_3[household_3$is_weekend == 0, ]

# Run OLS model

household_3_yb_results_df_now <- ols_model(household_3_now, yb_targets)[[1]]
household_3_yb_coeffs_df_now   <- ols_model(household_3_now, yb_targets)[[2]]
household_3_yb_results_df_w   <- ols_model(household_3_w, yb_targets)[[1]]
household_3_yb_coeffs_df_w     <- ols_model(household_3_w, yb_targets)[[2]]

household_3_ya_results_df_now <- ols_model(household_3_now, ya_targets)[[1]]
household_3_ya_coeffs_df_now   <- ols_model(household_3_now, ya_targets)[[2]]
household_3_ya_results_df_w   <- ols_model(household_3_w, ya_targets)[[1]]
household_3_ya_coeffs_df_w     <- ols_model(household_3_w, ya_targets)[[2]]

household_3_yb_coeffs_wide_now <- wide_df(household_3_yb_coeffs_df_now)
household_3_yb_coeffs_wide_w   <- wide_df(household_3_yb_coeffs_df_w)

household_3_ya_coeffs_wide_now <- wide_df(household_3_ya_coeffs_df_now)
household_3_ya_coeffs_wide_w   <- wide_df(household_3_ya_coeffs_df_w)

# removing the 'household' column to scale

ya_now_features <- colnames(ya_scaled_now)
yb_now_features <- colnames(yb_scaled_now)

ya_w_features <- colnames(ya_scaled_w)
yb_w_features <- colnames(yb_scaled_w)

household_3_ya_coeffs_wide_1_now <- household_3_ya_coeffs_wide_now[, ya_now_features, drop = FALSE]
household_3_ya_coeffs_wide_1_w <- household_3_ya_coeffs_wide_w[, ya_w_features, drop = FALSE]
household_3_yb_coeffs_wide_1_now <- household_3_yb_coeffs_wide_now[, yb_now_features, drop = FALSE]
household_3_yb_coeffs_wide_1_w <- household_3_yb_coeffs_wide_w[, yb_w_features, drop = FALSE]

household_3_ya_coeffs_wide_1_now[] <- lapply(household_3_ya_coeffs_wide_1_now, as.numeric)
household_3_ya_coeffs_wide_1_w[] <- lapply(household_3_ya_coeffs_wide_1_w, as.numeric)
household_3_yb_coeffs_wide_1_now[] <- lapply(household_3_yb_coeffs_wide_1_now, as.numeric)
household_3_yb_coeffs_wide_1_w[] <- lapply(household_3_yb_coeffs_wide_1_w, as.numeric)

# Scaling the data

household_3_yb_new_scaled_now <- scale(
  household_3_yb_coeffs_wide_1_now, 
  center = attr(yb_scaled_now, "scaled:center"), 
  scale  = attr(yb_scaled_now, "scaled:scale")
)
household_3_yb_new_scaled_w <- scale(
  household_3_yb_coeffs_wide_1_w, 
  center = attr(yb_scaled_w, "scaled:center"), 
  scale  = attr(yb_scaled_w, "scaled:scale")
)
household_3_ya_new_scaled_now <- scale(
  household_3_ya_coeffs_wide_1_now, 
  center = attr(ya_scaled_now, "scaled:center"), 
  scale  = attr(ya_scaled_now, "scaled:scale")
)
household_3_ya_new_scaled_w <- scale(
  household_3_ya_coeffs_wide_1_w, 
  center = attr(ya_scaled_w, "scaled:center"), 
  scale  = attr(ya_scaled_w, "scaled:scale")
)

# Assigning the clusters to new data using nearest centroids (from Hierarchical clustering)

get_hc_centroids <- function(X_scaled, clusters) {
  as.data.frame(X_scaled) |>
    dplyr::mutate(cluster = clusters) |>
    dplyr::group_by(cluster) |>
    dplyr::summarise_all(mean) |>
    dplyr::arrange(cluster)
}

assign_to_centroid <- function(new_data_scaled, centroids) {
  apply(as.matrix(new_data_scaled), 1, function(row) {
    dists <- apply(centroids[,-1], 1, function(centroid) sum((row - centroid)^2))
    centroids$cluster[which.min(dists)]
  })
}

# Getting our respective Centroids and assigning them to scaled data

ya_now_centroids <- get_hc_centroids(ya_scaled_now, ya_coeffs_now$cluster)
ya_w_centroids <- get_hc_centroids(ya_scaled_w, ya_coeffs_w$cluster)

yb_now_centroids <- get_hc_centroids(yb_scaled_now, yb_coeffs_now$cluster)
yb_w_centroids <- get_hc_centroids(yb_scaled_w, yb_coeffs_w$cluster)

new_clusters_a_now <- assign_to_centroid(household_3_ya_new_scaled_now, ya_now_centroids)
household_3_ya_coeffs_wide_now$cluster <- new_clusters_a_now # Ya's Weekday
new_clusters_a_w <- assign_to_centroid(household_3_ya_new_scaled_w, ya_w_centroids)
household_3_ya_coeffs_wide_w$cluster <- new_clusters_a_w # Ya's Weekend

new_clusters_b_now <- assign_to_centroid(household_3_yb_new_scaled_now, yb_now_centroids)
household_3_yb_coeffs_wide_now$cluster <- new_clusters_b_now # Yb's Weekday
new_clusters_b_w <- assign_to_centroid(household_3_yb_new_scaled_w, yb_w_centroids)
household_3_yb_coeffs_wide_w$cluster <- new_clusters_b_w # Yb's Weekend

# Difference between weekday and weekend cluster assignments

cluster_difference_a <- household_3_ya_coeffs_wide_now$cluster - household_3_ya_coeffs_wide_w$cluster
cluster_difference_b <- household_3_yb_coeffs_wide_now$cluster - household_3_yb_coeffs_wide_w$cluster

# Show cluster difference

cluster_difference_a
cluster_difference_b

## For ya's, SFH17 and SFH25 belong to Cluster 1 on Weekdays,
## and Cluster 2 on Weekends. SFH39 remains as-is in either case.
## For yb's, no change observed with the households for any day of the week.

# Visualizing the Clusters

# Yb-Weekday

combined_data_b_wd <- rbind(yb_scaled_now, household_3_yb_new_scaled_now)
combined_labels_b_wd <- c(yb_coeffs_now$cluster, new_clusters_b_now)
combined_source_b_wd <- c(rep("original", nrow(yb_scaled_now)), rep("new", nrow(household_3_yb_new_scaled_now)))

pca_b_wd <- prcomp(combined_data_b_wd)
reduced_b_wd <- as.data.frame(pca_b_wd$x[, 1:2])
colnames(reduced_b_wd) <- c("PC1", "PC2")
reduced_b_wd$cluster <- as.factor(combined_labels_b_wd)
reduced_b_wd$source <- combined_source_b_wd

reduced_b_wd$household <- c(as.character(yb_coeffs_now$household), as.character(household_3_yb_coeffs_wide_now$household))

ggplot(reduced_b_wd, aes(x = PC1, y = PC2, color = cluster, shape = source, label=household)) +
  geom_point(size = 3, alpha = 0.8, aes(fill = cluster)) +
  geom_text(vjust = -1, size = 3, alpha = 0.6, show.legend = FALSE) +
  scale_shape_manual(values = c("original" = 16, "new" = 4)) +
  labs(
    title = "PCA Projection - Smoothing Method B (Weekday)",
    x = "Principal Component 1",
    y = "Principal Component 2",
    color = "Cluster",
    shape = "Source"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  guides(fill = "none")

# Yb-Weekend

combined_data_b_we <- rbind(yb_scaled_w, household_3_yb_new_scaled_w)
combined_labels_b_we <- c(yb_coeffs_w$cluster, new_clusters_b_w)
combined_source_b_we <- c(rep("original", nrow(yb_scaled_w)), rep("new", nrow(household_3_yb_new_scaled_w)))

pca_b_we <- prcomp(combined_data_b_we)
reduced_b_we <- as.data.frame(pca_b_we$x[, 1:2])
colnames(reduced_b_we) <- c("PC1", "PC2")
reduced_b_we$cluster <- as.factor(combined_labels_b_we)
reduced_b_we$source <- combined_source_b_we

reduced_b_we$household <- c(as.character(yb_coeffs_w$household), as.character(household_3_yb_coeffs_wide_w$household))

ggplot(reduced_b_we, aes(x = PC1, y = PC2, color = cluster, shape = source, label=household)) +
  geom_point(size = 3, alpha = 0.8, aes(fill = cluster)) +
  geom_text(vjust = -1, size = 3, alpha = 0.6, show.legend = FALSE) +
  scale_shape_manual(values = c("original" = 16, "new" = 4)) +
  labs(
    title = "PCA Projection - Smoothing Method B (Weekend)",
    x = "Principal Component 1",
    y = "Principal Component 2",
    color = "Cluster",
    shape = "Source"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  guides(fill = "none")

# Ya-Weekday

combined_data_a_wd <- rbind(ya_scaled_now, household_3_ya_new_scaled_now)
combined_labels_a_wd <- c(ya_coeffs_now$cluster, new_clusters_a_now)
combined_source_a_wd <- c(rep("original", nrow(ya_scaled_now)), rep("new", nrow(household_3_ya_new_scaled_now)))

pca_a_wd <- prcomp(combined_data_a_wd)
reduced_a_wd <- as.data.frame(pca_a_wd$x[, 1:2])
colnames(reduced_a_wd) <- c("PC1", "PC2")
reduced_a_wd$cluster <- as.factor(combined_labels_a_wd)
reduced_a_wd$source <- combined_source_a_wd

reduced_a_wd$household <- c(as.character(ya_coeffs_now$household), as.character(household_3_ya_coeffs_wide_now$household))

ggplot(reduced_a_wd, aes(x = PC1, y = PC2, color = cluster, shape = source, label=household)) +
  geom_point(size = 3, alpha = 0.8, aes(fill = cluster)) +
  geom_text(vjust = -1, size = 3, alpha = 0.6, show.legend = FALSE) +
  scale_shape_manual(values = c("original" = 16, "new" = 4)) +
  labs(
    title = "PCA Projection - Smoothing Method A (Weekday)",
    x = "Principal Component 1",
    y = "Principal Component 2",
    color = "Cluster",
    shape = "Source"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  guides(fill = "none")

# Ya-Weekend

combined_data_a_we <- rbind(ya_scaled_w, household_3_ya_new_scaled_w)
combined_labels_a_we <- c(ya_coeffs_w$cluster, new_clusters_a_w)
combined_source_a_we <- c(rep("original", nrow(ya_scaled_w)), rep("new", nrow(household_3_ya_new_scaled_w)))

pca_a_we <- prcomp(combined_data_a_we)
reduced_a_we <- as.data.frame(pca_a_we$x[, 1:2])
colnames(reduced) <- c("PC1", "PC2")
reduced_a_we$cluster <- as.factor(combined_labels_a_we)
reduced_a_we$source <- combined_source_a_we

reduced_a_we$household <- c(as.character(ya_coeffs_w$household), as.character(household_3_ya_coeffs_wide_w$household))

ggplot(reduced_a_we, aes(x = PC1, y = PC2, color = cluster, shape = source, label=household)) +
  geom_point(size = 3, alpha = 0.8, aes(fill = cluster)) +
  geom_text(vjust = -1, size = 3, alpha = 0.6, show.legend = FALSE) +
  scale_shape_manual(values = c("original" = 16, "new" = 4)) +
  labs(
    title = "PCA Projection - Smoothing Method A (Weekend)",
    x = "Principal Component 1",
    y = "Principal Component 2",
    color = "Cluster",
    shape = "Source"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  guides(fill = "none")


############################ Task 6 ###########################

## Here, we calculate the mean load profile for households,
## based on each smoothing method and cluster, using the
## seasonal_sin and day_of_week as our variables to consider

###################### Yb-Weekday #########################

df_noweekend_subset <- df_noweekend %>%
  dplyr::select(household, seasonal_sin, day_of_year)

merged_df_wd <- yb_coeffs_w %>%
  dplyr::inner_join(df_noweekend_subset, by = "household")

yb_coeffs_w

intercept_cols <- grep("^\\(Intercept\\)_yb_", colnames(merged_df_wd), value = TRUE)
season_cols <- grep("^seasonal_sin_yb_", colnames(merged_df_wd), value = TRUE)

df_intercept <- merged_df_wd %>%
  tidyr::pivot_longer(
    cols = all_of(intercept_cols),
    names_to = "hour",
    values_to = "intercept_value"
  )

df_season <- merged_df_wd %>%
  tidyr::pivot_longer(
    cols = all_of(season_cols),
    names_to = "hour",
    values_to = "season_value"
  )

df_season$season_value <- as.numeric(df_season$season_value)
df_intercept$intercept_value <- as.numeric(df_intercept$intercept_value)

df_intercept <- df_intercept %>%
  mutate(hour = as.integer(stringr::str_extract(hour, "(?<=\\(Intercept\\)_yb_)\\d+")))

df_season <- df_season %>%
  mutate(hour = as.integer(stringr::str_extract(hour, "(?<=seasonal_sin_yb_)\\d+")))

# Joining the data for Mean Load Profile calculation
df_combined <- df_intercept %>%
  dplyr::inner_join(
    df_season,
    by = c("household", "cluster", "day_of_year", "seasonal_sin", "hour")
  )

# Applying the provided formula: predicted load = 1 * intercept + s_D * season
df_combined <- df_combined %>%
  dplyr::mutate(mean_load = intercept_value + seasonal_sin * season_value)

print(head(df_combined))

household_counts <- df_combined %>%
  dplyr::group_by(day_of_year, cluster, hour) %>%
  dplyr::summarise(total_households = dplyr::n_distinct(household), .groups = "drop")

result_yb_wd <- df_combined %>%
  dplyr::select(household, day_of_year, cluster, hour, mean_load) %>%
  dplyr::left_join(household_counts, by = c("day_of_year", "cluster", "hour"))


# Sanity check, to make sure only households with clusters are selected
households_with_cluster <- result_yb_wd %>%
  dplyr::filter(!is.na(cluster)) %>%
  dplyr::pull(household) %>%
  unique()

df_noweekend_valid <- df_noweekend %>%
  dplyr::filter(household %in% households_with_cluster)

df_long <- df_noweekend_valid %>%
  tidyr::pivot_longer(
    cols = dplyr::starts_with("yb_"),
    names_to = "hour",
    values_to = "actual_load"
  ) %>%
  dplyr::mutate(
    hour = as.integer(stringr::str_extract(hour, "(?<=yb_)\\d+")),
    household = as.character(household),
    day_of_year = as.integer(day_of_year)
  )

# Making sure the hour was extracted as a numeric
if("hour" %in% names(result_yb_wd)) {
  result_yb_wd <- result_yb_wd %>%
    dplyr::mutate(hour = as.integer(hour))
} else if("hour" %in% names(result_yb_wd) == FALSE && "mean_load" %in% names(result_yb_wd)) {
  stop("result_yb_wd is missing the 'hour' column! Regenerate result_yb_wd with correct hour extraction.")
}

result_yb_wd_sub <- result_yb_wd %>%
  dplyr::filter(!is.na(cluster)) %>%
  dplyr::mutate(
    household = as.character(household),
    day_of_year = as.integer(day_of_year),
    hour = as.integer(hour)
  ) %>%
  dplyr::select(household, day_of_year, hour, mean_load, cluster, total_households)

# Merging the predicted mean_load and cluster by household,day_of_year & hour
merged_error_df_wd <- df_long %>%
  dplyr::inner_join(result_yb_wd_sub, by = c("household", "day_of_year", "hour"))

# Calculating error and variance (with provided formula)
merged_error_df_wd <- merged_error_df_wd %>%
  dplyr::mutate(
    error = abs(actual_load - mean_load),
    Variance = error * 3
  )

# Grouping and summarizing the Mean Load Profile data
df_mean_load_weekdays <- merged_error_df_wd %>%
  dplyr::group_by(cluster, day_of_year, hour) %>%
  dplyr::summarise(
    actual_load = mean(actual_load, na.rm = TRUE),
    mean_load = mean(mean_load, na.rm = TRUE),
    error = mean(error, na.rm = TRUE),
    total_households = mean(total_households, na.rm = TRUE),
    Variance = mean(Variance, na.rm = TRUE),
    .groups = "drop"
  )

df_mean_load_weekdays

# Visualizing the Mean Load Profile (yb-Weekday) 

set.seed(123)
sample_days <- sample(unique(df_mean_load_weekdays$day_of_year), 2)
clusters <- c(1, 2)

plot_df <- df_mean_load_weekdays %>%
  filter(day_of_year %in% sample_days, cluster %in% clusters)

plot_df <- plot_df %>%
  mutate(
    facet_label = paste0("Cluster ", cluster, " - Day ", day_of_year)
  )

all_combos <- expand.grid(
  cluster = clusters,
  day_of_year = sample_days,
  hour = 0:23
)
plot_df <- right_join(plot_df, all_combos, by = c("cluster", "day_of_year", "hour")) %>%
  mutate(
    facet_label = paste0("Cluster ", cluster, " - Day ", day_of_year)
  )

ggplot(plot_df, aes(x = hour, y = mean_load)) +
  geom_line(color = "blue", na.rm = TRUE) +
  geom_ribbon(aes(ymin = mean_load - Variance, ymax = mean_load + Variance), 
              fill = "blue", alpha = 0.3, na.rm = TRUE) +
  facet_grid(cluster ~ day_of_year, labeller = label_both) +
  labs(
    title = "Mean Load by Cluster and Day - Yb Weekdays",
    x = "Hour of Day",
    y = "Load"
  ) +
  scale_x_continuous(breaks = 0:23) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# For comparison, we plot the actual load for a random day
# SFH11 chosen as it belongs to largest cluster, 1

household_id <- "SFH11" 
sfh_data <- df_noweekend %>% filter(household == household_id)
random_day <- sample(unique(sfh_data$day_of_year), 1)

actual_row <- sfh_data %>% filter(day_of_year == random_day)
actual_load <- as.numeric(actual_row %>% select(starts_with("yb_")))

cluster_id <- yb_coeffs_now %>%
  filter(household == household_id) %>%
  pull(cluster) %>%
  unique()

mean_data <- df_mean_load_weekdays %>%
  filter(day_of_year == random_day, cluster == cluster_id) %>%
  arrange(hour)

hours <- mean_data$hour
mean_load <- mean_data$mean_load
std_dev <- mean_data$Variance

plot_lines <- data.frame(
  hour = rep(hours, 2),
  value = c(actual_load, mean_load),
  Series = rep(c("Actual Load", "Cluster Mean"), each = length(hours))
)

plot_ribbon <- data.frame(
  hour = hours,
  lower = mean_load - std_dev,
  upper = mean_load + std_dev
)

ggplot() +
  geom_ribbon(
    data = plot_ribbon,
    aes(x = hour, ymin = lower, ymax = upper, fill = "Variance"),
    alpha = 0.2
  ) +
  geom_line(
    data = plot_lines,
    aes(x = hour, y = value, color = Series, linetype = Series),
    size = 1.2
  ) +
  scale_color_manual(values = c("Actual Load" = "black", "Cluster Mean" = "blue")) +
  scale_linetype_manual(values = c("Actual Load" = "solid", "Cluster Mean" = "dashed")) +
  scale_fill_manual(values = c("Variance" = "blue")) +
  labs(
    title = paste0("Power Consumption on Day ", random_day, " (", household_id, " - Cluster ", cluster_id, ", Weekend)"),
    x = "Hour of Day",
    y = "Load",
    color = NULL,
    fill = NULL,
    linetype = NULL
  ) +
  scale_x_continuous(breaks = 0:23) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
####################### Yb-Weekend ########################

## For the rest of the parts, they follow the same as above,
## but are modular such that they take the scenario in account

df_weekend_subset <- df_weekend %>%
  dplyr::select(household, seasonal_sin, day_of_year)

merged_df_we <- yb_coeffs_w %>%
  dplyr::inner_join(df_weekend_subset, by = "household")

yb_coeffs_w # sanity check yet again

intercept_cols <- grep("^\\(Intercept\\)_yb_", colnames(merged_df_we), value = TRUE)
season_cols <- grep("^seasonal_sin_yb_", colnames(merged_df_we), value = TRUE)

df_intercept <- merged_df_we %>%
  tidyr::pivot_longer(
    cols = all_of(intercept_cols),
    names_to = "hour",
    values_to = "intercept_value"
  )

df_season <- merged_df_we %>%
  tidyr::pivot_longer(
    cols = all_of(season_cols),
    names_to = "hour",
    values_to = "season_value"
  )

df_season$season_value <- as.numeric(df_season$season_value)
df_intercept$intercept_value <- as.numeric(df_intercept$intercept_value)

df_intercept <- df_intercept %>%
  mutate(hour = as.integer(stringr::str_extract(hour, "(?<=\\(Intercept\\)_yb_)\\d+")))

df_season <- df_season %>%
  mutate(hour = as.integer(stringr::str_extract(hour, "(?<=seasonal_sin_yb_)\\d+")))

df_combined_w <- df_intercept %>%
  dplyr::inner_join(
    df_season,
    by = c("household", "cluster", "day_of_year", "seasonal_sin", "hour")
  )

df_combined_w <- df_combined_w %>%
  dplyr::mutate(mean_load = intercept_value + seasonal_sin * season_value)

print(head(df_combined_w))

household_counts <- df_combined_w %>%
  dplyr::group_by(day_of_year, cluster, hour) %>%
  dplyr::summarise(total_households = dplyr::n_distinct(household), .groups = "drop")

result_yb_we <- df_combined_w %>%
  dplyr::select(household, day_of_year, cluster, hour, mean_load) %>%
  dplyr::left_join(household_counts, by = c("day_of_year", "cluster", "hour"))

households_with_cluster_w <- result_yb_we %>%
  dplyr::filter(!is.na(cluster)) %>%
  dplyr::pull(household) %>%
  unique()

df_weekend_valid <- df_weekend %>%
  dplyr::filter(household %in% households_with_cluster)

df_long_w <- df_weekend_valid %>%
  tidyr::pivot_longer(
    cols = dplyr::starts_with("yb_"),
    names_to = "hour",
    values_to = "actual_load"
  ) %>%
  dplyr::mutate(
    hour = as.integer(stringr::str_extract(hour, "(?<=yb_)\\d+")),
    household = as.character(household),
    day_of_year = as.integer(day_of_year)
  )

if("hour" %in% names(result_yb_we)) {
  result_yb_wd <- result_yb_we %>%
    dplyr::mutate(hour = as.integer(hour))
} else if("hour" %in% names(result_yb_we) == FALSE && "mean_load" %in% names(result_yb_we)) {
  stop("result_yb_we is missing the 'hour' column! Regenerate result_yb_we with correct hour extraction.")
}

result_yb_we_sub <- result_yb_we %>%
  dplyr::filter(!is.na(cluster)) %>%
  dplyr::mutate(
    household = as.character(household),
    day_of_year = as.integer(day_of_year),
    hour = as.integer(hour)
  ) %>%
  dplyr::select(household, day_of_year, hour, mean_load, cluster, total_households)

merged_error_df_we <- df_long_w %>%
  dplyr::inner_join(result_yb_we_sub, by = c("household", "day_of_year", "hour"))

merged_error_df_we <- merged_error_df_we %>%
  dplyr::mutate(
    error = abs(actual_load - mean_load),
    Variance = error * 3
  )

df_mean_load_weekends <- merged_error_df_we %>%
  dplyr::group_by(cluster, day_of_year, hour) %>%
  dplyr::summarise(
    actual_load = mean(actual_load, na.rm = TRUE),
    mean_load = mean(mean_load, na.rm = TRUE),
    error = mean(error, na.rm = TRUE),
    total_households = mean(total_households, na.rm = TRUE),
    Variance = mean(Variance, na.rm = TRUE),
    .groups = "drop"
  )

df_mean_load_weekends

# Visualizing for the Mean Load Profile (yb-Weekend)

set.seed(123)
sample_days <- sample(unique(df_mean_load_weekends$day_of_year), 2)
clusters <- c(1, 2)

plot_df_w <- df_mean_load_weekends %>%
  filter(day_of_year %in% sample_days, cluster %in% clusters)

plot_df_w <- plot_df_w %>%
  mutate(
    facet_label = paste0("Cluster ", cluster, " - Day ", day_of_year)
  )

all_combos <- expand.grid(
  cluster = clusters,
  day_of_year = sample_days,
  hour = 0:23
)
plot_df_w <- right_join(plot_df_w, all_combos, by = c("cluster", "day_of_year", "hour")) %>%
  mutate(
    facet_label = paste0("Cluster ", cluster, " - Day ", day_of_year)
  )

ggplot(plot_df_w, aes(x = hour, y = mean_load)) +
  geom_line(color = "blue", na.rm = TRUE) +
  geom_ribbon(aes(ymin = mean_load - Variance, ymax = mean_load + Variance), 
              fill = "blue", alpha = 0.3, na.rm = TRUE) +
  facet_grid(cluster ~ day_of_year, labeller = label_both) +
  labs(
    title = "Mean Load by Cluster and Day - Yb Weekends",
    x = "Hour of Day",
    y = "Load"
  ) +
  scale_x_continuous(breaks = 0:23) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Comparative Graph

household_id <- "SFH11" 
sfh_data <- df_weekend %>% filter(household == household_id)
random_day <- sample(unique(sfh_data$day_of_year), 1)

actual_row <- sfh_data %>% filter(day_of_year == random_day)
actual_load <- as.numeric(actual_row %>% select(starts_with("yb_")))

cluster_id <- yb_coeffs_now %>%
  filter(household == household_id) %>%
  pull(cluster) %>%
  unique()

mean_data <- df_mean_load_weekends %>%
  filter(day_of_year == random_day, cluster == cluster_id) %>%
  arrange(hour)

hours <- mean_data$hour
mean_load <- mean_data$mean_load
std_dev <- mean_data$Variance

plot_lines <- data.frame(
  hour = rep(hours, 2),
  value = c(actual_load, mean_load),
  Series = rep(c("Actual Load", "Cluster Mean"), each = length(hours))
)

plot_ribbon <- data.frame(
  hour = hours,
  lower = mean_load - std_dev,
  upper = mean_load + std_dev
)

ggplot() +
  geom_ribbon(
    data = plot_ribbon,
    aes(x = hour, ymin = lower, ymax = upper, fill = "Variance"),
    alpha = 0.2
  ) +
  geom_line(
    data = plot_lines,
    aes(x = hour, y = value, color = Series, linetype = Series),
    size = 1.2
  ) +
  scale_color_manual(values = c("Actual Load" = "black", "Cluster Mean" = "blue")) +
  scale_linetype_manual(values = c("Actual Load" = "solid", "Cluster Mean" = "dashed")) +
  scale_fill_manual(values = c("Variance" = "blue")) +
  labs(
    title = paste0("Power Consumption on Day ", random_day, " (", household_id, " - Cluster ", cluster_id, ", Weekend)"),
    x = "Hour of Day",
    y = "Load",
    color = NULL,
    fill = NULL,
    linetype = NULL
  ) +
  scale_x_continuous(breaks = 0:23) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

################## Ya-Weekday ########################

df_a_noweekend_subset <- df_a_noweekend %>%
  dplyr::select(household, seasonal_sin, day_of_year)

merged_df_ya_wd <- ya_coeffs_w %>%
  dplyr::inner_join(df_a_noweekend_subset, by = "household")

intercept_cols_ya <- grep("^\\(Intercept\\)_ya_", colnames(merged_df_ya_wd), value = TRUE)
season_cols_ya <- grep("^seasonal_sin_ya_", colnames(merged_df_ya_wd), value = TRUE)

df_intercept_ya <- merged_df_ya_wd %>%
  tidyr::pivot_longer(
    cols = all_of(intercept_cols_ya),
    names_to = "hour",
    values_to = "intercept_value"
  )
df_season_ya <- merged_df_ya_wd %>%
  tidyr::pivot_longer(
    cols = all_of(season_cols_ya),
    names_to = "hour",
    values_to = "season_value"
  )

df_season_ya$season_value <- as.numeric(df_season_ya$season_value)
df_intercept_ya$intercept_value <- as.numeric(df_intercept_ya$intercept_value)

df_intercept_ya <- df_intercept_ya %>%
  mutate(hour = as.integer(str_extract(hour, "(?<=\\(Intercept\\)_ya_)\\d+")))
df_season_ya <- df_season_ya %>%
  mutate(hour = as.integer(str_extract(hour, "(?<=seasonal_sin_ya_)\\d+")))

df_combined_ya <- df_intercept_ya %>%
  inner_join(
    df_season_ya,
    by = c("household", "cluster", "day_of_year", "seasonal_sin", "hour")
  )

df_combined_ya <- df_combined_ya %>%
  mutate(mean_load = intercept_value + seasonal_sin * season_value)

household_counts_ya <- df_combined_ya %>%
  group_by(day_of_year, cluster, hour) %>%
  summarise(total_households = n_distinct(household), .groups = "drop")

result_ya_wd <- df_combined_ya %>%
  select(household, day_of_year, cluster, hour, mean_load) %>%
  left_join(household_counts_ya, by = c("day_of_year", "cluster", "hour"))

households_with_cluster_ya <- result_ya_wd %>%
  filter(!is.na(cluster)) %>%
  pull(household) %>%
  unique()

df_a_noweekend_valid <- df_a_noweekend %>%
  filter(household %in% households_with_cluster_ya)

df_long_ya <- df_a_noweekend_valid %>%
  pivot_longer(
    cols = starts_with("ya_"),
    names_to = "hour",
    values_to = "actual_load"
  ) %>%
  mutate(
    hour = as.integer(str_extract(hour, "(?<=ya_)\\d+")),
    household = as.character(household),
    day_of_year = as.integer(day_of_year)
  )

if("hour" %in% names(result_ya_wd)) {
  result_ya_wd <- result_ya_wd %>%
    mutate(hour = as.integer(hour))
} else if("hour" %in% names(result_ya_wd) == FALSE && "mean_load" %in% names(result_ya_wd)) {
  stop("result_ya_wd is missing the 'hour' column! Regenerate result_ya_wd with correct hour extraction.")
}

result_ya_wd_sub <- result_ya_wd %>%
  filter(!is.na(cluster)) %>%
  mutate(
    household = as.character(household),
    day_of_year = as.integer(day_of_year),
    hour = as.integer(hour)
  ) %>%
  select(household, day_of_year, hour, mean_load, cluster, total_households)

merged_error_df_ya_wd <- df_long_ya %>%
  inner_join(result_ya_wd_sub, by = c("household", "day_of_year", "hour"))

merged_error_df_ya_wd <- merged_error_df_ya_wd %>%
  mutate(
    error = abs(actual_load - mean_load),
    Variance = error * 3
  )

df_mean_load_weekdays_ya <- merged_error_df_ya_wd %>%
  group_by(cluster, day_of_year, hour) %>%
  summarise(
    actual_load = mean(actual_load, na.rm = TRUE),
    mean_load = mean(mean_load, na.rm = TRUE),
    error = mean(error, na.rm = TRUE),
    total_households = mean(total_households, na.rm = TRUE),
    Variance = mean(Variance, na.rm = TRUE),
    .groups = "drop"
  )

df_mean_load_weekdays_ya

# Visualizing for the Mean Load Profile (ya-Weekday)

set.seed(123)
sample_days_ya <- sample(unique(df_mean_load_weekdays_ya$day_of_year), 2)
clusters_ya <- sort(unique(df_mean_load_weekdays_ya$cluster))[1:2]

plot_df_ya <- df_mean_load_weekdays_ya %>%
  filter(day_of_year %in% sample_days_ya, cluster %in% clusters_ya) %>%
  mutate(
    facet_label = paste0("Cluster ", cluster, " - Day ", day_of_year)
  )

all_combos_ya <- expand.grid(
  cluster = clusters_ya,
  day_of_year = sample_days_ya,
  hour = 0:23
)
plot_df_ya <- right_join(plot_df_ya, all_combos_ya, by = c("cluster", "day_of_year", "hour")) %>%
  mutate(
    facet_label = paste0("Cluster ", cluster, " - Day ", day_of_year)
  )

ggplot(plot_df_ya, aes(x = hour, y = mean_load)) +
  geom_line(color = "blue", na.rm = TRUE) +
  geom_ribbon(aes(ymin = mean_load - Variance, ymax = mean_load + Variance), 
              fill = "blue", alpha = 0.3, na.rm = TRUE) +
  facet_grid(cluster ~ day_of_year, labeller = label_both) +
  labs(
    title = "Mean Load by Cluster and Day - Ya Weekdays",
    x = "Hour of Day",
    y = "Load"
  ) +
  scale_x_continuous(breaks = 0:23) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Comparative Graph

household_id <- "SFH11" 
sfh_data <- df_a_noweekend %>% filter(household == household_id)
random_day <- sample(unique(sfh_data$day_of_year), 1)

actual_row <- sfh_data %>% filter(day_of_year == random_day)
actual_load <- as.numeric(actual_row %>% select(starts_with("ya_")))

cluster_id <- ya_coeffs_w %>%
  filter(household == household_id) %>%
  pull(cluster) %>%
  unique()

mean_data <- df_mean_load_weekdays_ya %>%
  filter(day_of_year == random_day, cluster == cluster_id) %>%
  arrange(hour)

hours <- mean_data$hour
mean_load <- mean_data$mean_load
std_dev <- mean_data$Variance

plot_lines <- data.frame(
  hour = rep(hours, 2),
  value = c(actual_load, mean_load),
  Series = rep(c("Actual Load", "Cluster Mean"), each = length(hours))
)

plot_ribbon <- data.frame(
  hour = hours,
  lower = mean_load - std_dev,
  upper = mean_load + std_dev
)

ggplot() +
  geom_ribbon(
    data = plot_ribbon,
    aes(x = hour, ymin = lower, ymax = upper, fill = "Variance"),
    alpha = 0.2
  ) +
  geom_line(
    data = plot_lines,
    aes(x = hour, y = value, color = Series, linetype = Series),
    size = 1.2
  ) +
  scale_color_manual(values = c("Actual Load" = "black", "Cluster Mean" = "blue")) +
  scale_linetype_manual(values = c("Actual Load" = "solid", "Cluster Mean" = "dashed")) +
  scale_fill_manual(values = c("Variance" = "blue")) +
  labs(
    title = paste0("Power Consumption on Day ", random_day, " (", household_id, " - Cluster ", cluster_id, ", Weekday)"),
    x = "Hour of Day",
    y = "Load",
    color = NULL,
    fill = NULL,
    linetype = NULL
  ) +
  scale_x_continuous(breaks = 0:23) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )


#################### Ya-Weekend #####################

df_a_weekend_subset <- df_a_weekend %>%
  dplyr::select(household, seasonal_sin, day_of_year)

merged_df_ya_we <- ya_coeffs_now %>%
  dplyr::inner_join(df_a_weekend_subset, by = "household")

intercept_cols_ya_we <- grep("^\\(Intercept\\)_ya_", colnames(merged_df_ya_we), value = TRUE)
season_cols_ya_we <- grep("^seasonal_sin_ya_", colnames(merged_df_ya_we), value = TRUE)

df_intercept_ya_we <- merged_df_ya_we %>%
  tidyr::pivot_longer(
    cols = all_of(intercept_cols_ya_we),
    names_to = "hour",
    values_to = "intercept_value"
  )
df_season_ya_we <- merged_df_ya_we %>%
  tidyr::pivot_longer(
    cols = all_of(season_cols_ya_we),
    names_to = "hour",
    values_to = "season_value"
  )

df_season_ya_we$season_value <- as.numeric(df_season_ya_we$season_value)
df_intercept_ya_we$intercept_value <- as.numeric(df_intercept_ya_we$intercept_value)

df_intercept_ya_we <- df_intercept_ya_we %>%
  mutate(hour = as.integer(str_extract(hour, "(?<=\\(Intercept\\)_ya_)\\d+")))
df_season_ya_we <- df_season_ya_we %>%
  mutate(hour = as.integer(str_extract(hour, "(?<=seasonal_sin_ya_)\\d+")))

df_combined_ya_we <- df_intercept_ya_we %>%
  inner_join(
    df_season_ya_we,
    by = c("household", "cluster", "day_of_year", "seasonal_sin", "hour")
  )

df_combined_ya_we <- df_combined_ya_we %>%
  mutate(mean_load = intercept_value + seasonal_sin * season_value)

household_counts_ya_we <- df_combined_ya_we %>%
  group_by(day_of_year, cluster, hour) %>%
  summarise(total_households = n_distinct(household), .groups = "drop")

result_ya_we <- df_combined_ya_we %>%
  select(household, day_of_year, cluster, hour, mean_load) %>%
  left_join(household_counts_ya_we, by = c("day_of_year", "cluster", "hour"))

households_with_cluster_ya_we <- result_ya_we %>%
  filter(!is.na(cluster)) %>%
  pull(household) %>%
  unique()

df_a_weekend_valid <- df_a_weekend %>%
  filter(household %in% households_with_cluster_ya_we)

df_long_ya_we <- df_a_weekend_valid %>%
  pivot_longer(
    cols = starts_with("ya_"),
    names_to = "hour",
    values_to = "actual_load"
  ) %>%
  mutate(
    hour = as.integer(str_extract(hour, "(?<=ya_)\\d+")),
    household = as.character(household),
    day_of_year = as.integer(day_of_year)
  )

if("hour" %in% names(result_ya_we)) {
  result_ya_we <- result_ya_we %>%
    mutate(hour = as.integer(hour))
} else if("hour" %in% names(result_ya_we) == FALSE && "mean_load" %in% names(result_ya_we)) {
  stop("result_ya_we is missing the 'hour' column! Regenerate result_ya_we with correct hour extraction.")
}

result_ya_we_sub <- result_ya_we %>%
  filter(!is.na(cluster)) %>%
  mutate(
    household = as.character(household),
    day_of_year = as.integer(day_of_year),
    hour = as.integer(hour)
  ) %>%
  select(household, day_of_year, hour, mean_load, cluster, total_households)

merged_error_df_ya_we <- df_long_ya_we %>%
  inner_join(result_ya_we_sub, by = c("household", "day_of_year", "hour"))

merged_error_df_ya_we <- merged_error_df_ya_we %>%
  mutate(
    error = abs(actual_load - mean_load),
    Variance = error * 3
  )

df_mean_load_weekends_ya <- merged_error_df_ya_we %>%
  group_by(cluster, day_of_year, hour) %>%
  summarise(
    actual_load = mean(actual_load, na.rm = TRUE),
    mean_load = mean(mean_load, na.rm = TRUE),
    error = mean(error, na.rm = TRUE),
    total_households = mean(total_households, na.rm = TRUE),
    Variance = mean(Variance, na.rm = TRUE),
    .groups = "drop"
  )

df_mean_load_weekends_ya

# Visualizing for the Mean Load Profile (ya-Weekend)

set.seed(108)
sample_days_ya_we <- sample(unique(df_mean_load_weekends_ya$day_of_year), 2)
clusters_ya_we <- sort(unique(df_mean_load_weekends_ya$cluster))[1:2]

plot_df_ya_we <- df_mean_load_weekends_ya %>%
  filter(day_of_year %in% sample_days_ya_we, cluster %in% clusters_ya_we) %>%
  mutate(
    facet_label = paste0("Cluster ", cluster, " - Day ", day_of_year)
  )

all_combos_ya_we <- expand.grid(
  cluster = clusters_ya_we,
  day_of_year = sample_days_ya_we,
  hour = 0:23
)
plot_df_ya_we <- right_join(plot_df_ya_we, all_combos_ya_we, by = c("cluster", "day_of_year", "hour")) %>%
  mutate(
    facet_label = paste0("Cluster ", cluster, " - Day ", day_of_year)
  )

plot_df_ya_we <- plot_df_ya_we %>%
  filter(
    is.finite(mean_load),
    is.finite(Variance)
  )

ggplot(plot_df_ya_we, aes(x = hour, y = mean_load)) +
  geom_line(color = "blue", na.rm = TRUE) +
  geom_ribbon(aes(ymin = mean_load - Variance, ymax = mean_load + Variance), 
              fill = "blue", alpha = 0.3, na.rm = TRUE) +
  facet_grid(cluster ~ day_of_year, labeller = label_both) +
  labs(
    title = "Mean Load by Cluster and Day - Ya Weekends",
    x = "Hour of Day",
    y = "Load"
  ) +
  scale_x_continuous(breaks = 0:23) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Comparative Graph

household_id <- "SFH11" 
sfh_data <- df_a_weekend %>% filter(household == household_id)
random_day <- sample(unique(sfh_data$day_of_year), 1)

actual_row <- sfh_data %>% filter(day_of_year == random_day)
actual_load <- as.numeric(actual_row %>% select(starts_with("ya_")))

cluster_id <- ya_coeffs_now %>%
  filter(household == household_id) %>%
  pull(cluster) %>%
  unique()

mean_data <- df_mean_load_weekends_ya %>%
  filter(day_of_year == random_day, cluster == cluster_id) %>%
  arrange(hour)

hours <- mean_data$hour
mean_load <- mean_data$mean_load
std_dev <- mean_data$Variance

plot_lines <- data.frame(
  hour = rep(hours, 2),
  value = c(actual_load, mean_load),
  Series = rep(c("Actual Load", "Cluster Mean"), each = length(hours))
)

plot_ribbon <- data.frame(
  hour = hours,
  lower = mean_load - std_dev,
  upper = mean_load + std_dev
)

ggplot() +
  geom_ribbon(
    data = plot_ribbon,
    aes(x = hour, ymin = lower, ymax = upper, fill = "Variance"),
    alpha = 0.2
  ) +
  geom_line(
    data = plot_lines,
    aes(x = hour, y = value, color = Series, linetype = Series),
    size = 1.2
  ) +
  scale_color_manual(values = c("Actual Load" = "black", "Cluster Mean" = "blue")) +
  scale_linetype_manual(values = c("Actual Load" = "solid", "Cluster Mean" = "dashed")) +
  scale_fill_manual(values = c("Variance" = "blue")) +
  labs(
    title = paste0("Power Consumption on Day ", random_day, " (", household_id, " - Cluster ", cluster_id, ", Weekend)"),
    x = "Hour of Day",
    y = "Load",
    color = NULL,
    fill = NULL,
    linetype = NULL
  ) +
  scale_x_continuous(breaks = 0:23) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )


############################ Task 7 ###########################

# In the following task, we forecast for the next hour based
# on the 'past' values, utilizing the Mean Load Profiles 
# that we got from the smoothing and clustering, as well as
# using only the 'raw', actual values for the households
# We use both Kalman and Particle Filter for all the
# three cases, and realize which method is more feasible
# to determine the forecast

# From the random selection, the households we have are
# SFH17, SFH25, and SFH39


# Helper function, which allows to select hourly data
load_hourly <- function(filename) {
  df <- read_csv(filename) %>%
    mutate(
      index = ymd_hms(index),
      hour = hour(index),
      minute = minute(index),
      date = as.Date(index),
      day_of_year = yday(date)
    ) %>%
    filter(minute == 0) %>%
    arrange(index)
  return(df)
}

################ Kalman Filter #################

# Kalman Filter function (no individual load profile)

kalman_filter_uni <- function(z, theta_X=1, theta_Z=1, Q=0.01, sigma_Z2=0.01, c0=mean(z, na.rm=TRUE), rho0=1) {
  n <- length(z)
  a <- numeric(n)
  c <- numeric(n)
  b <- numeric(n)
  S <- numeric(n)
  rho <- numeric(n)
  
  c_prev <- c0
  rho_prev <- rho0
  for (t in 1:n) {
    a_t <- theta_X * c_prev
    p_t <- theta_X^2 * rho_prev + Q
    b_t <- theta_Z * a_t
    S_t <- theta_Z^2 * p_t + sigma_Z2
    if (is.na(z[t])) {
      c_t <- a_t
      rho_t <- p_t
    } else {
      K_t <- p_t * theta_Z / S_t
      c_t <- a_t + K_t * (z[t] - b_t)
      rho_t <- (1 - K_t * theta_Z) * p_t
    }
    a[t] <- a_t
    c[t] <- c_t
    b[t] <- b_t
    S[t] <- S_t
    rho[t] <- rho_t
    c_prev <- c_t
    rho_prev <- rho_t
  }
  list(predicted_state_mean = a, filtered_state_mean = c, predicted_obs_mean = b, predicted_obs_var = S, filtered_state_var = rho)
}

# Tuning the parameters using optim's L-BFGS-B

kalman_filter_loss <- function(par, z_train) {
  theta_X  <- par[1]
  theta_Z  <- par[2]
  Q <- par[3]
  sigma_Z2 <- par[4]
  
  fit <- kalman_filter_uni(z_train, theta_X=theta_X, theta_Z = theta_Z, Q = Q, sigma_Z2=sigma_Z2)
  mean((fit$filtered_state_mean - z_train)^2, na.rm=TRUE)
}

tune_kalman_optim <- function(z_train) {
  par_init <- c(theta_X=1, theta_Z = 1, Q = 0.01, sigma_Z2=0.01)
  lower    <- c(0.9, 0.9, 0.005, 0.005)
  upper    <- c(1.1, 1.1, 0.2, 0.2)
  res <- optim(
    par = par_init,
    fn = kalman_filter_loss,
    z_train = z_train,
    method = "L-BFGS-B",
    lower = lower,
    upper = upper
  )
  names(res$par) <- c("theta_X", "theta_Z", "Q", "sigma_Z2")
  as.list(res$par)
}

# Loop function
households <- c("SFH17", "SFH25", "SFH39")
filenames <- c("SFH17.csv", "SFH25.csv", "SFH39.csv")

results_kalman <- list()

for (i in seq_along(households)) {
  hh <- households[i]
  file <- filenames[i]
  df <- load_hourly(file)
  max_day <- max(df$day_of_year, na.rm=TRUE)
  train_df <- df %>% filter(day_of_year <= 183)
  test_df  <- df %>% filter(day_of_year >= 184)
  z_train <- train_df$HAUSHALT_TOT
  z_test  <- test_df$HAUSHALT_TOT
  
  params <- tune_kalman_optim(z_train)
  
  c0 <- mean(z_train, na.rm=TRUE)
  rho0 <- var(z_train, na.rm=TRUE)
  kf <- kalman_filter_uni(z_test, theta_X=params$theta_X, sigma_Z2=params$sigma_Z2, c0=c0, rho0=rho0)
  
  results_kalman[[hh]] <- tibble(
    date = test_df$date,
    hour = test_df$hour,
    actual = z_test,
    forecast = kf$predicted_obs_mean,
    filtered = kf$filtered_state_mean,
    forecast_sd = sqrt(kf$predicted_obs_var)
  )
}

head(results_kalman[["SFH17"]]) # can view the results if needed

# params = 0.9, 0.990, 0.2, 0.005

#############################################
# --- 1. Load and select hourly data ---    #
# load_hourly <- function(filename) {       #
#  #read_csv(filename) %>%                  #
#    #mutate(                               #
#      #index = ymd_hms(index),             #
#      #hour = hour(index),                 #
#      #minute = minute(index),             #
#      #date = as.Date(index),              #
#      #day_of_year = yday(date)            #
#    #) %>%                                 #
#    #filter(minute == 0) %>%               #
#    #arrange(index)                        #
#}                                          #
#############################################

# Kalman Filter function with exogenous Mean Load Profile from Classification 

kalman_filter_uni_exog <- function(z, v, theta_X=1, theta_v=1, theta_Z=1, Q=0.01, sigma_Z2=0.01, c0=mean(z, na.rm=TRUE), rho0=1) {
  n <- length(z)
  a <- numeric(n)
  c <- numeric(n)
  b <- numeric(n)
  S <- numeric(n)
  rho <- numeric(n)
  
  c_prev <- c0
  rho_prev <- rho0
  for (t in 1:n) {
    a_t <- theta_X * c_prev + theta_v * v[t]
    p_t <- theta_X^2 * rho_prev + Q
    b_t <- theta_Z * a_t
    S_t <- theta_Z^2 * p_t + sigma_Z2
    if (is.na(z[t]) || is.na(v[t])) {
      c_t <- a_t
      rho_t <- p_t
    } else {
      K_t <- p_t * theta_Z / S_t
      c_t <- a_t + K_t * (z[t] - b_t)
      rho_t <- (1 - K_t * theta_Z) * p_t
    }
    a[t] <- a_t
    c[t] <- c_t
    b[t] <- b_t
    S[t] <- S_t
    rho[t] <- rho_t
    c_prev <- c_t
    rho_prev <- rho_t
  }
  list(predicted_state_mean = a, filtered_state_mean = c, predicted_obs_mean = b, predicted_obs_var = S, filtered_state_var = rho)
}

# Tuning the Parameters with Mean Load Profile parameter

kalman_filter_loss_exog <- function(par, z_train, v_train) {
  theta_X  <- par[1]
  theta_v  <- par[2]
  theta_Z  <- par[3]
  Q <- par[4]
  sigma_Z2 <- par[5]
  
  fit <- kalman_filter_uni_exog(z_train, v_train, theta_X=theta_X, theta_v = theta_v, theta_Z = theta_Z, Q = Q, sigma_Z2=sigma_Z2)
  mean((fit$filtered_state_mean - z_train)^2, na.rm=TRUE)
}

# For Mean Load Profile B

tune_kalman_optim_exog_b <- function(z_train, v_train) {
  par_init <- c(theta_X=1, theta_v = 1, theta_Z = 1, Q = 0.01, sigma_Z2=0.01)
  lower    <- c(0.5, 0.5, 0.9, 0.005, 0.005)
  upper    <- c(1.1, 1.1, 1.1, 0.2, 0.2)
  res <- optim(
    par = par_init,
    fn = kalman_filter_loss_exog,
    z_train = z_train,
    v_train = v_train,
    method = "L-BFGS-B",
    lower = lower,
    upper = upper
  )
  names(res$par) <- c("theta_X", "theta_v", "theta_Z", "Q", "sigma_Z2")
  as.list(res$par)
}

# Loop for MLP B

mean_load_profile_b <- dplyr::bind_rows(result_yb_wd_sub, result_yb_we_sub) %>%
  dplyr::select(household, day_of_year, hour, mean_load) # From Task 6

households <- c("SFH17", "SFH25", "SFH39")
filenames <- c("SFH17.csv", "SFH25.csv", "SFH39.csv")

results_kalman_profile_b <- list() 

for (i in seq_along(households)) {
  hh <- households[i]
  file <- filenames[i]
  df <- load_hourly(file)
  max_day <- max(df$day_of_year, na.rm=TRUE)
  
  df <- df %>%
    mutate(household = hh) %>%
    left_join(mean_load_profile_b, by = c("household", "day_of_year", "hour"))
  
  train_df <- df %>% filter(day_of_year <= 183)
  test_df  <- df %>% filter(day_of_year >= 184)
  z_train <- train_df$HAUSHALT_TOT
  v_train <- train_df$mean_load
  z_test  <- test_df$HAUSHALT_TOT
  v_test  <- test_df$mean_load
  
  if (all(is.na(v_train))) v_train <- rep(mean(z_train, na.rm=TRUE), length(z_train))
  if (all(is.na(v_test))) v_test <- rep(mean(z_test, na.rm=TRUE), length(z_test))
  
  params <- tune_kalman_optim_exog_b(z_train, v_train)
  
  c0 <- mean(z_train, na.rm=TRUE)
  rho0 <- var(z_train, na.rm=TRUE)
  kf <- kalman_filter_uni_exog(z_test, v_test, theta_X=params$theta_X, theta_v=params$theta_v, Q = params$Q, sigma_Z2=params$sigma_Z2, c0=c0, rho0=rho0)
  
  results_kalman_profile_b[[hh]] <- tibble(
    date = test_df$date,
    hour = test_df$hour,
    actual = z_test,
    mean_load = v_test,
    forecast = kf$predicted_obs_mean,
    filtered = kf$filtered_state_mean,
    forecast_sd = sqrt(kf$predicted_obs_var)
  )
}

head(results_kalman_profile_b[["SFH17"]]) 

# params = 0.5, 0.5, 0.991, 0.2, 0.005

# For Mean Load Profile A

tune_kalman_optim_exog_a <- function(z_train, v_train) {
  par_init <- c(theta_X=1, theta_v = 1, theta_Z = 1, Q = 0.01, sigma_Z2=0.01)
  lower    <- c(0.5, 0.5, 0.9, 0.005, 0.005)
  upper    <- c(1.1, 1.1, 1.1, 0.2, 0.2)
  res <- optim(
    par = par_init,
    fn = kalman_filter_loss_exog,
    z_train = z_train,
    v_train = v_train,
    method = "L-BFGS-B",
    lower = lower,
    upper = upper
  )
  names(res$par) <- c("theta_X", "theta_v", "theta_Z", "Q", "sigma_Z2")
  as.list(res$par)
}

# Loop for MLP A

mean_load_profile_a <- dplyr::bind_rows(result_ya_wd_sub, result_ya_we_sub) %>%
  dplyr::select(household, day_of_year, hour, mean_load)

households <- c("SFH17", "SFH25", "SFH39")
filenames <- c("SFH17.csv", "SFH25.csv", "SFH39.csv")

results_kalman_profile_a <- list() 

for (i in seq_along(households)) {
  hh <- households[i]
  file <- filenames[i]
  df <- load_hourly(file)
  max_day <- max(df$day_of_year, na.rm=TRUE)
  
  df <- df %>%
    mutate(household = hh) %>%
    left_join(mean_load_profile_a, by = c("household", "day_of_year", "hour"))
  
  train_df <- df %>% filter(day_of_year <= 183)
  test_df  <- df %>% filter(day_of_year >= 184)
  z_train <- train_df$HAUSHALT_TOT
  v_train <- train_df$mean_load
  z_test  <- test_df$HAUSHALT_TOT
  v_test  <- test_df$mean_load
  
  if (all(is.na(v_train))) v_train <- rep(mean(z_train, na.rm=TRUE), length(z_train))
  if (all(is.na(v_test))) v_test <- rep(mean(z_test, na.rm=TRUE), length(z_test))
  
  params <- tune_kalman_optim_exog_a(z_train, v_train)
  
  c0 <- mean(z_train, na.rm=TRUE)
  rho0 <- var(z_train, na.rm=TRUE)
  kf <- kalman_filter_uni_exog(z_test, v_test, theta_X=params$theta_X, theta_v=params$theta_v, Q = params$Q, sigma_Z2=params$sigma_Z2, c0=c0, rho0=rho0)
  
  results_kalman_profile_a[[hh]] <- tibble(
    date = test_df$date,
    hour = test_df$hour,
    actual = z_test,
    mean_load = v_test,
    forecast = kf$predicted_obs_mean,
    filtered = kf$filtered_state_mean,
    forecast_sd = sqrt(kf$predicted_obs_var)
  )
}

head(results_kalman_profile_a[["SFH17"]])

# params = 0.5, 0.5, 0.997, 0.2, 0.005

################ Particle Filter #################

# Helper function, which allows to select hourly data

load_hourly <- function(filename) {
  df <- read_csv(filename) %>%
    mutate(
      index = ymd_hms(index),
      hour = hour(index),
      minute = minute(index),
      date = as.Date(index),
      day_of_year = yday(date)
    ) %>%
    filter(minute == 0) %>%
    arrange(index)
  return(df)
}

# Particle Filter function (no individual load profile)

particle_filter_meanfield <- function(
    z,
    theta_X = 1,
    theta_Z = 1,
    Q = 100,
    sigma_Z2 = 100,
    c0 = mean(z, na.rm=TRUE),
    rho0 = 1,
    M = 500
) {
  n <- length(z)
  x_particles <- matrix(NA, nrow = n, ncol = M)
  xbar <- numeric(n)
  zbar <- numeric(n)
  zvar <- numeric(n)
  
  # Initializing particles at t=1
  x_particles[1, ] <- rnorm(M, mean = c0, sd = sqrt(rho0))
  xbar[1] <- mean(x_particles[1, ])
  z_particles <- theta_Z * x_particles[1, ]
  zbar[1] <- mean(z_particles)
  zvar[1] <- var(z_particles)
  
  for (t in 2:n) {
    # Propagating using mean of previous particles, as in the lecture
    x_pred <- theta_X * xbar[t-1] + rnorm(M, mean = 0, sd = sqrt(Q))
    z_pred <- theta_Z * x_pred
    # Weighting
    if (!is.na(z[t])) {
      w_unnorm <- dnorm(z[t], mean = z_pred, sd = sqrt(sigma_Z2))
      w_unnorm <- w_unnorm + 1e-99
      w <- w_unnorm / sum(w_unnorm)
    } else {
      w <- rep(1/M, M)
    }
    idx_resample <- sample(1:M, size=M, replace=TRUE, prob=w) # resampling technique
    x_particles[t, ] <- x_pred[idx_resample] + rnorm(M, 0, sqrt(Q) * 0.2) # adds jitter
    xbar[t] <- mean(x_particles[t, ])
    z_particles <- theta_Z * x_particles[t, ]
    zbar[t] <- mean(z_particles)
    zvar[t] <- var(z_particles)
  }
  list(zbar = zbar, zvar = zvar, xbar = xbar)
}

# Tuning the parameters using optim's L-BFGS-B

particle_filter_loss <- function(par, z_train) {
  theta_X   <- par[1]
  theta_Z   <- par[2]
  Q         <- par[3]
  sigma_Z2  <- par[4]
  pf <- particle_filter_meanfield(
    z = z_train,
    theta_X = theta_X,
    theta_Z = theta_Z,
    Q = Q,
    sigma_Z2 = sigma_Z2,
    M = 500
  )
  mean((pf$zbar - z_train)^2, na.rm=TRUE)
}

tune_particle_optim <- function(z_train) {
  par_init <- c(theta_X = 1, theta_Z = 1, Q = 99.56, sigma_Z2 = 103.01)
  lower    <- c(0.99, 0.99, 99, 103)
  upper    <- c(1, 1, 100, 104)
  res <- optim(
    par = par_init,
    fn = particle_filter_loss,
    z_train = z_train,
    method = "L-BFGS-B",
    lower = lower,
    upper = upper
  )
  names(res$par) <- c("theta_X", "theta_Z", "Q", "sigma_Z2")
  as.list(res$par)
}

# Loop function

households <- c("SFH17", "SFH25", "SFH39")
filenames <- c("SFH17.csv", "SFH25.csv", "SFH39.csv")

results_particle <- list()

for (i in seq_along(households)) {
  hh <- households[i]
  file <- filenames[i]
  df <- load_hourly(file)
  max_day <- max(df$day_of_year, na.rm=TRUE)
  train_df <- df %>% filter(day_of_year <= 183)
  test_df  <- df %>% filter(day_of_year >= 184)
  z_train <- train_df$HAUSHALT_TOT
  z_test  <- test_df$HAUSHALT_TOT
  
  
  c0 <- mean(z_train, na.rm=TRUE)
  rho0 <- var(z_train, na.rm=TRUE)
  if (is.na(rho0) || rho0 == 0) rho0 <- 1
  
  params <- tune_particle_optim(z_train)
  pf <- particle_filter_meanfield(
    z_test,
    theta_X = params$theta_X,
    theta_Z = params$theta_Z,
    Q = params$Q,
    sigma_Z2 = params$sigma_Z2,
    c0 = c0,
    rho0 = rho0,
    M = 1000
  )
  
  results_particle[[hh]] <- tibble(
    date = test_df$date,
    hour = test_df$hour,
    actual = z_test,
    forecast = pf$zbar,
    forecast_sd = sqrt(pf$zvar)
  )
}

head(results_particle[["SFH17"]])

# params = 1, 1, 99.56, 103.01

# Particle Filter function with exogenous Mean Load Profile from Classification 

particle_filter_meanfield_exog <- function(
    z,
    v,
    theta_X = 0.65,
    theta_v = 0.60,
    theta_Z = 0.60,
    Q = 335,
    sigma_Z2 = 363.5,
    c0 = mean(z, na.rm=TRUE),
    rho0 = 1,
    M = 500
) {
  n <- length(z)
  x_particles <- matrix(NA, nrow = n, ncol = M)
  xbar <- numeric(n)
  zbar <- numeric(n)
  zvar <- numeric(n)
  
  x_particles[1, ] <- rnorm(M, mean = c0, sd = sqrt(rho0))
  xbar[1] <- mean(x_particles[1, ])
  z_particles <- theta_Z * x_particles[1, ]
  zbar[1] <- mean(z_particles)
  zvar[1] <- var(z_particles)
  
  for (t in 2:n) {
    v_t <- ifelse(is.na(v[t]), 0, v[t]) 
    x_pred <- theta_X * xbar[t-1] + theta_v * v_t + rnorm(M, mean = 0, sd = sqrt(Q))
    z_pred <- theta_Z * x_pred
    if (!is.na(z[t])) {
      w_unnorm <- dnorm(z[t], mean = z_pred, sd = sqrt(sigma_Z2))
      w_unnorm <- w_unnorm + 1e-99
      w <- w_unnorm / sum(w_unnorm)
    } else {
      w <- rep(1/M, M)
    }
    idx_resample <- sample(1:M, size=M, replace=TRUE, prob=w)
    x_particles[t, ] <- x_pred[idx_resample] + rnorm(M, 0, sqrt(Q) * 0.2)
    xbar[t] <- mean(x_particles[t, ])
    z_particles <- theta_Z * x_particles[t, ]
    zbar[t] <- mean(z_particles)
    zvar[t] <- var(z_particles)
  }
  list(zbar = zbar, zvar = zvar, xbar = xbar)
}

# Tuning the Parameters with Mean Load Profile parameter

particle_filter_loss_exog <- function(par, z_train, v_train) {
  theta_X   <- par[1]
  theta_v   <- par[2]
  theta_Z   <- par[3]
  Q         <- par[4]
  sigma_Z2  <- par[5]
  pf <- particle_filter_meanfield_exog(
    z = z_train,
    v = v_train,
    theta_X = theta_X,
    theta_v = theta_v,
    theta_Z = theta_Z,
    Q = Q,
    sigma_Z2 = sigma_Z2,
    M = 500
  )
  mean((pf$zbar - z_train)^2, na.rm=TRUE)
}

# For Mean Load Profile B

tune_particle_optim_exog_b <- function(z_train, v_train) {
  par_init <- c(theta_X = 0.65, theta_v = 0.60, theta_Z = 0.6, Q = 335, sigma_Z2 = 363.5)
  lower    <- c(0.60, 0.55, 0.50, 330, 360)
  upper    <- c(0.70, 0.65, 0.70, 340, 370)
  res <- optim(
    par = par_init,
    fn = particle_filter_loss_exog,
    z_train = z_train,
    v_train = v_train,
    method = "L-BFGS-B",
    lower = lower,
    upper = upper
  )
  names(res$par) <- c("theta_X", "theta_v", "theta_Z", "Q", "sigma_Z2")
  as.list(res$par)
}

# Loop for MLP B

# We already have Mean Load Profile dataframe loaded from Kalman

results_particle_profile_b <- list()

for (i in seq_along(households)) {
  hh <- households[i]
  file <- filenames[i]
  df <- load_hourly(file)
  max_day <- max(df$day_of_year, na.rm=TRUE)
  
  df <- df %>%
    mutate(household = hh) %>%
    left_join(mean_load_profile_b, by = c("household", "day_of_year", "hour"))
  
  train_df <- df %>% filter(day_of_year <= 183)
  test_df  <- df %>% filter(day_of_year >= 184)
  z_train <- train_df$HAUSHALT_TOT
  v_train <- train_df$mean_load
  z_test  <- test_df$HAUSHALT_TOT
  v_test  <- test_df$mean_load
  
  if (all(is.na(v_train))) v_train <- rep(mean(z_train, na.rm=TRUE), length(z_train))
  if (all(is.na(v_test))) v_test <- rep(mean(z_test, na.rm=TRUE), length(z_test))
  
  params <- tune_particle_optim_exog_b(z_train, v_train)
  
  c0 <- mean(z_train, na.rm=TRUE)
  rho0 <- var(z_train, na.rm=TRUE)
  if (is.na(rho0) || rho0 == 0) rho0 <- 1
  
  pf <- particle_filter_meanfield_exog(
    z = z_test,
    v = v_test,
    theta_X = params$theta_X,
    theta_v = params$theta_v,
    theta_Z = params$theta_Z,
    Q = params$Q,
    sigma_Z2 = params$sigma_Z2,
    c0 = c0,
    rho0 = rho0,
    M = 1000
  )
  
  results_particle_profile_b[[hh]] <- tibble(
    date = test_df$date,
    hour = test_df$hour,
    actual = z_test,
    mean_load = v_test,
    forecast = pf$zbar,
    forecast_sd = sqrt(pf$zvar)
  )
}

head(results_particle_profile_b[["SFH17"]])

# params = 0.65, 0.5999, 0.5999, 335.0034, 363.504 

# For Mean Load Profile A

tune_particle_optim_exog_a <- function(z_train, v_train) {
  par_init <- c(theta_X = 0.55, theta_v = 0.45, theta_Z = 0.50, Q = 335, sigma_Z2 = 360)
  lower    <- c(0.50, 0.35, 0.40, 330, 350)
  upper    <- c(0.60, 0.55, 0.60, 340, 370)
  res <- optim(
    par = par_init,
    fn = particle_filter_loss_exog,
    z_train = z_train,
    v_train = v_train,
    method = "L-BFGS-B",
    lower = lower,
    upper = upper
  )
  names(res$par) <- c("theta_X", "theta_v", "theta_Z", "Q", "sigma_Z2")
  as.list(res$par)
}

# Loop for MLP A

results_particle_profile_a <- list()

for (i in seq_along(households)) {
  hh <- households[i]
  file <- filenames[i]
  df <- load_hourly(file)
  max_day <- max(df$day_of_year, na.rm=TRUE)
  
  df <- df %>%
    mutate(household = hh) %>%
    left_join(mean_load_profile_a, by = c("household", "day_of_year", "hour"))
  
  train_df <- df %>% filter(day_of_year <= 183)
  test_df  <- df %>% filter(day_of_year >= 184)
  z_train <- train_df$HAUSHALT_TOT
  v_train <- train_df$mean_load
  z_test  <- test_df$HAUSHALT_TOT
  v_test  <- test_df$mean_load
  
  if (all(is.na(v_train))) v_train <- rep(mean(z_train, na.rm=TRUE), length(z_train))
  if (all(is.na(v_test))) v_test <- rep(mean(z_test, na.rm=TRUE), length(z_test))
  
  params <- tune_particle_optim_exog_a(z_train, v_train)
  
  c0 <- mean(z_train, na.rm=TRUE)
  rho0 <- var(z_train, na.rm=TRUE)
  if (is.na(rho0) || rho0 == 0) rho0 <- 1
  
  pf <- particle_filter_meanfield_exog(
    z = z_test,
    v = v_test,
    theta_X = params$theta_X,
    theta_v = params$theta_v,
    theta_Z = params$theta_Z,
    Q = params$Q,
    sigma_Z2 = params$sigma_Z2,
    c0 = c0,
    rho0 = rho0,
    M = 1000
  )
  
  results_particle_profile_a[[hh]] <- tibble(
    date = test_df$date,
    hour = test_df$hour,
    actual = z_test,
    mean_load = v_test,
    forecast = pf$zbar,
    forecast_sd = sqrt(pf$zvar)
  )
}

head(results_particle_profile_a[["SFH17"]])

# params = 0.55, 0.45, 0.50, 334.89, 359.77

################## Visualization #######################

########### Household No. 17 #############

###### Kalman Filter ######

# No Individual Load Profile

kalman_limit_17 <- results_kalman$SFH17 %>%
  mutate(day_of_year = yday(date)) %>%
  filter(day_of_year <= 213)

ggplot(kalman_limit_17, aes(x = date)) +
  geom_line(aes(y = actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = forecast, color = "Forecast"), linewidth = 1.25) +
  geom_ribbon(aes(ymin = forecast - forecast_sd, ymax = forecast + forecast_sd), alpha = 0.2) +
  labs(
    title = "SFH17: Actual vs Forecasted Load (Kalman)",
    y = "Load (kWh)",
    x = "Date",
    color = "Legend"
  ) +
  scale_color_manual(values = c("Actual" = "red", "Forecast" = "blue")) +
  theme_minimal()

# Mean Load Profile A

kalman_limit_17_mlp_a <- results_kalman_profile_a$SFH17 %>%
  mutate(day_of_year = yday(date)) %>%
  filter(day_of_year <= 213)

ggplot(kalman_limit_17_mlp_a, aes(x = date)) +
  geom_line(aes(y = actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = forecast, color = "Forecast"), linewidth = 1.25) +
  geom_ribbon(aes(ymin = forecast - forecast_sd, ymax = forecast + forecast_sd), alpha = 0.2) +
  labs(
    title = "SFH17: Actual vs Forecasted Load Mean Load Profile A (Kalman)",
    y = "Load (kWh)",
    x = "Date",
    color = "Legend"
  ) +
  scale_color_manual(values = c("Actual" = "red", "Forecast" = "blue")) +
  theme_minimal()

# Mean Load Profile B

kalman_limit_17_mlp_b <- results_kalman_profile_b$SFH17 %>%
  mutate(day_of_year = yday(date)) %>%
  filter(day_of_year <= 213)

ggplot(kalman_limit_17_mlp_b, aes(x = date)) +
  geom_line(aes(y = actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = forecast, color = "Forecast"), linewidth = 1.25) +
  geom_ribbon(aes(ymin = forecast - forecast_sd, ymax = forecast + forecast_sd), alpha = 0.2) +
  labs(
    title = "SFH17: Actual vs Forecasted Load Mean Load Profile B (Kalman)",
    y = "Load (kWh)",
    x = "Date",
    color = "Legend"
  ) +
  scale_color_manual(values = c("Actual" = "red", "Forecast" = "blue")) +
  theme_minimal()


###### Particle Filter ######

# No Individual Load Profile

particle_limit_17 <- results_particle$SFH17 %>%
  mutate(day_of_year = yday(date)) %>%
  filter(day_of_year <= 213)

ggplot(particle_limit_17, aes(x = date)) +
  geom_line(aes(y = actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = forecast, color = "Forecast"), linewidth = 1.25) +
  geom_ribbon(aes(ymin = forecast - forecast_sd, ymax = forecast + forecast_sd), alpha = 0.2) +
  labs(
    title = "SFH17: Actual vs Forecasted Load (Particle)",
    y = "Load (kWh)",
    x = "Date",
    color = "Legend"
  ) +
  scale_color_manual(values = c("Actual" = "red", "Forecast" = "blue")) +
  theme_minimal()

# Mean Load Profile A

particle_limit_17_mlp_a <- results_particle_profile_a$SFH17 %>%
  mutate(day_of_year = yday(date)) %>%
  filter(day_of_year <= 213)

ggplot(particle_limit_17_mlp_a, aes(x = date)) +
  geom_line(aes(y = actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = forecast, color = "Forecast"), linewidth = 1.25) +
  geom_ribbon(aes(ymin = forecast - forecast_sd, ymax = forecast + forecast_sd), alpha = 0.2) +
  labs(
    title = "SFH17: Actual vs Forecasted Load Mean Load Profile A (Particle)",
    y = "Load (kWh)",
    x = "Date",
    color = "Legend"
  ) +
  scale_color_manual(values = c("Actual" = "red", "Forecast" = "blue")) +
  theme_minimal()

# Mean Load Profile B

particle_limit_17_mlp_b <- results_particle_profile_b$SFH17 %>%
  mutate(day_of_year = yday(date)) %>%
  filter(day_of_year <= 213)

ggplot(particle_limit_17_mlp_b, aes(x = date)) +
  geom_line(aes(y = actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = forecast, color = "Forecast"), linewidth = 1.25) +
  geom_ribbon(aes(ymin = forecast - forecast_sd, ymax = forecast + forecast_sd), alpha = 0.2) +
  labs(
    title = "SFH17: Actual vs Forecasted Load Mean Load Profile B (Particle)",
    y = "Load (kWh)",
    x = "Date",
    color = "Legend"
  ) +
  scale_color_manual(values = c("Actual" = "red", "Forecast" = "blue")) +
  theme_minimal()

########### Household No. 25 #############

###### Kalman Filter ######

# No Individual Load Profile

kalman_limit_25 <- results_kalman$SFH25 %>%
  mutate(day_of_year = yday(date)) %>%
  filter(day_of_year <= 213)

ggplot(kalman_limit_25, aes(x = date)) +
  geom_line(aes(y = actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = forecast, color = "Forecast"), linewidth = 1.25) +
  geom_ribbon(aes(ymin = forecast - forecast_sd, ymax = forecast + forecast_sd), alpha = 0.2) +
  labs(
    title = "SFH25: Actual vs Forecasted Load (Kalman)",
    y = "Load (kWh)",
    x = "Date",
    color = "Legend"
  ) +
  scale_color_manual(values = c("Actual" = "red", "Forecast" = "blue")) +
  theme_minimal()

# Mean Load Profile A

kalman_limit_25_mlp_a <- results_kalman_profile_a$SFH25 %>%
  mutate(day_of_year = yday(date)) %>%
  filter(day_of_year <= 213)

ggplot(kalman_limit_25_mlp_a, aes(x = date)) +
  geom_line(aes(y = actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = forecast, color = "Forecast"), linewidth = 1.25) +
  geom_ribbon(aes(ymin = forecast - forecast_sd, ymax = forecast + forecast_sd), alpha = 0.2) +
  labs(
    title = "SFH25: Actual vs Forecasted Load Mean Load Profile A (Kalman)",
    y = "Load (kWh)",
    x = "Date",
    color = "Legend"
  ) +
  scale_color_manual(values = c("Actual" = "red", "Forecast" = "blue")) +
  theme_minimal()

# Mean Load Profile B

kalman_limit_25_mlp_b <- results_kalman_profile_b$SFH25 %>%
  mutate(day_of_year = yday(date)) %>%
  filter(day_of_year <= 213)

ggplot(kalman_limit_25_mlp_b, aes(x = date)) +
  geom_line(aes(y = actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = forecast, color = "Forecast"), linewidth = 1.25) +
  geom_ribbon(aes(ymin = forecast - forecast_sd, ymax = forecast + forecast_sd), alpha = 0.2) +
  labs(
    title = "SFH25: Actual vs Forecasted Load Mean Load Profile B (Kalman)",
    y = "Load (kWh)",
    x = "Date",
    color = "Legend"
  ) +
  scale_color_manual(values = c("Actual" = "red", "Forecast" = "blue")) +
  theme_minimal()


###### Particle Filter ######

# No Individual Load Profile

particle_limit_25 <- results_particle$SFH25 %>%
  mutate(day_of_year = yday(date)) %>%
  filter(day_of_year <= 213)

ggplot(particle_limit_25, aes(x = date)) +
  geom_line(aes(y = actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = forecast, color = "Forecast"), linewidth = 1.25) +
  geom_ribbon(aes(ymin = forecast - forecast_sd, ymax = forecast + forecast_sd), alpha = 0.2) +
  labs(
    title = "SFH25: Actual vs Forecasted Load (Particle)",
    y = "Load (kWh)",
    x = "Date",
    color = "Legend"
  ) +
  scale_color_manual(values = c("Actual" = "red", "Forecast" = "blue")) +
  theme_minimal()

# Mean Load Profile Ya

particle_limit_25_mlp_a <- results_particle_profile_a$SFH25 %>%
  mutate(day_of_year = yday(date)) %>%
  filter(day_of_year <= 213)

ggplot(particle_limit_25_mlp_a, aes(x = date)) +
  geom_line(aes(y = actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = forecast, color = "Forecast"), linewidth = 1.25) +
  geom_ribbon(aes(ymin = forecast - forecast_sd, ymax = forecast + forecast_sd), alpha = 0.2) +
  labs(
    title = "SFH25: Actual vs Forecasted Load Mean Load Profile A (Particle)",
    y = "Load (kWh)",
    x = "Date",
    color = "Legend"
  ) +
  scale_color_manual(values = c("Actual" = "red", "Forecast" = "blue")) +
  theme_minimal()

# Mean Load Profile Yb

particle_limit_25_mlp_b <- results_particle_profile_b$SFH25 %>%
  mutate(day_of_year = yday(date)) %>%
  filter(day_of_year <= 213)

ggplot(particle_limit_25_mlp_b, aes(x = date)) +
  geom_line(aes(y = actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = forecast, color = "Forecast"), linewidth = 1.25) +
  geom_ribbon(aes(ymin = forecast - forecast_sd, ymax = forecast + forecast_sd), alpha = 0.2) +
  labs(
    title = "SFH25: Actual vs Forecasted Load Mean Load Profile B (Particle)",
    y = "Load (kWh)",
    x = "Date",
    color = "Legend"
  ) +
  scale_color_manual(values = c("Actual" = "red", "Forecast" = "blue")) +
  theme_minimal()

########### Household No. 39 #############

###### Kalman Filter ######

# No Individual Load Profile

kalman_limit_39 <- results_kalman$SFH39 %>%
  mutate(day_of_year = yday(date)) %>%
  filter(day_of_year <= 213)

ggplot(kalman_limit_39, aes(x = date)) +
  geom_line(aes(y = actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = forecast, color = "Forecast"), linewidth = 1.25) +
  geom_ribbon(aes(ymin = forecast - forecast_sd, ymax = forecast + forecast_sd), alpha = 0.2) +
  labs(
    title = "SFH39: Actual vs Forecasted Load (Kalman)",
    y = "Load (kWh)",
    x = "Date",
    color = "Legend"
  ) +
  scale_color_manual(values = c("Actual" = "red", "Forecast" = "blue")) +
  theme_minimal()

# Mean Load Profile A

kalman_limit_39_mlp_a <- results_kalman_profile_a$SFH39 %>%
  mutate(day_of_year = yday(date)) %>%
  filter(day_of_year <= 213)

ggplot(kalman_limit_39_mlp_a, aes(x = date)) +
  geom_line(aes(y = actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = forecast, color = "Forecast"), linewidth = 1.25) +
  geom_ribbon(aes(ymin = forecast - forecast_sd, ymax = forecast + forecast_sd), alpha = 0.2) +
  labs(
    title = "SFH39: Actual vs Forecasted Load Mean Load Profile A (Kalman)",
    y = "Load (kWh)",
    x = "Date",
    color = "Legend"
  ) +
  scale_color_manual(values = c("Actual" = "red", "Forecast" = "blue")) +
  theme_minimal()

# Mean Load Profile B

kalman_limit_39_mlp_b <- results_kalman_profile_b$SFH39 %>%
  mutate(day_of_year = yday(date)) %>%
  filter(day_of_year <= 213)

ggplot(kalman_limit_39_mlp_b, aes(x = date)) +
  geom_line(aes(y = actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = forecast, color = "Forecast"), linewidth = 1.25) +
  geom_ribbon(aes(ymin = forecast - forecast_sd, ymax = forecast + forecast_sd), alpha = 0.2) +
  labs(
    title = "SFH39: Actual vs Forecasted Load Mean Load Profile B (Kalman)",
    y = "Load (kWh)",
    x = "Date",
    color = "Legend"
  ) +
  scale_color_manual(values = c("Actual" = "red", "Forecast" = "blue")) +
  theme_minimal()

###### Particle Filter ######

# No Individual Load Profile

particle_limit_39 <- results_particle$SFH39 %>%
  mutate(day_of_year = yday(date)) %>%
  filter(day_of_year <= 213)

ggplot(particle_limit_39, aes(x = date)) +
  geom_line(aes(y = actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = forecast, color = "Forecast"), linewidth = 1.25) +
  geom_ribbon(aes(ymin = forecast - forecast_sd, ymax = forecast + forecast_sd), alpha = 0.2) +
  labs(
    title = "SFH39: Actual vs Forecasted Load (Particle)",
    y = "Load (kWh)",
    x = "Date",
    color = "Legend"
  ) +
  scale_color_manual(values = c("Actual" = "red", "Forecast" = "blue")) +
  theme_minimal()

# Mean Load Profile A

particle_limit_39_mlp_a <- results_particle_profile_a$SFH39 %>%
  mutate(day_of_year = yday(date)) %>%
  filter(day_of_year <= 213)

ggplot(particle_limit_39_mlp_a, aes(x = date)) +
  geom_line(aes(y = actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = forecast, color = "Forecast"), linewidth = 1.25) +
  geom_ribbon(aes(ymin = forecast - forecast_sd, ymax = forecast + forecast_sd), alpha = 0.2) +
  labs(
    title = "SFH39: Actual vs Forecasted Load Mean Load Profile A (Particle)",
    y = "Load (kWh)",
    x = "Date",
    color = "Legend"
  ) +
  scale_color_manual(values = c("Actual" = "red", "Forecast" = "blue")) +
  theme_minimal()

# Mean Load Profile B

particle_limit_39_mlp_b <- results_particle_profile_b$SFH39 %>%
  mutate(day_of_year = yday(date)) %>%
  filter(day_of_year <= 213)

ggplot(particle_limit_39_mlp_b, aes(x = date)) +
  geom_line(aes(y = actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = forecast, color = "Forecast"), linewidth = 1.25) +
  geom_ribbon(aes(ymin = forecast - forecast_sd, ymax = forecast + forecast_sd), alpha = 0.2) +
  labs(
    title = "SFH39: Actual vs Forecasted Load Mean Load Profile B (Particle)",
    y = "Load (kWh)",
    x = "Date",
    color = "Legend"
  ) +
  scale_color_manual(values = c("Actual" = "red", "Forecast" = "blue")) +
  theme_minimal()

# Task 7 finally ends here... it could definitely use
# less amount of lines of code, but all a learning experience. :)

############################ Task 8 ###########################

# Now that we have our forecast values for all the mentioned
# cases, we compare forecast errors and do some diagnostics
# As a measure, we take RMSE and compare with all the households
# and check whether KF was a better fit or PF. That's where
# the MSE should be lower

# Function to calculate the RMSE (Forecast errors)

forecast_errors <- function(df, method, profile, household){
  df %>% 
    summarize(
      method = method,
      profile = profile,
      household = household,
      MSE = mean((actual - forecast)^2, na.rm = TRUE),
      RMSE = sqrt(mean((actual - forecast)^2, na.rm = TRUE))
    )
}

households <- c("SFH17", "SFH25", "SFH39") # from our remaining 3 households

# Executing the forecast errors
errors_all <- list()

for (hh in households){
  # Kalman, no individual load profile
  errors_all[[length(errors_all) + 1]] <- forecast_errors(results_kalman[[hh]], "Kalman", "None", hh)
  # Kalman, Mean Load Profile A
  errors_all[[length(errors_all) + 1]] <- forecast_errors(results_kalman_profile_a[[hh]], "Kalman", "Profile A", hh)
  # Kalman, Mean Load Profile B
  errors_all[[length(errors_all) + 1]] <- forecast_errors(results_kalman_profile_b[[hh]], "Kalman", "Profile B", hh)
  # Particle, no individual load profile
  errors_all[[length(errors_all) + 1]] <- forecast_errors(results_particle[[hh]], "Particle", "None", hh) 
  # Particle, Mean Load Profile A
  errors_all[[length(errors_all) + 1]] <- forecast_errors(results_particle_profile_a[[hh]], "Particle", "Profile A", hh)
  # Particle, Mean Load Profile B
  errors_all[[length(errors_all) + 1]] <- forecast_errors(results_particle_profile_b[[hh]], "Particle", "Profile B", hh) 
}

errors_df <- bind_rows(errors_all)
print(errors_df) # in one dataframe for easier access

# Visualizing the errors (RMSE in the y-axis for clarity)

ggplot(errors_df, aes(x = profile, y = RMSE, fill = method)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~ household) +
  labs(title = "Forecast RMSE by Household, Method, and Profile Type") +
  theme_minimal()

# In general, Mean Load Profile B brings least MSE in its class
# In terms of household, mixed results. SFH17 is inconclusive, 
# SFH25 shows Kalman is a better fit, and SFH39 shows Particle is a better fit
# Selection of filter depends on the data, in this case
# Missing dates are one issue too, so results could be affected
# Parameter optimization could be made better
# Although more expensive computationally, model with interaction
# could be more helpful determining better filter.

## Last-Minute Diagnostics

# Helper function for our residuals

extract_residuals_long <- function(results_list, method, profile) {
  bind_rows(
    lapply(names(results_list), function(hh) {
      df <- results_list[[hh]]
      tibble(
        error = df$actual - df$forecast,
        household = hh,
        method = method,
        profile = profile
      )
    })
  )
}

# Assembling all the cases and combining them in one

residuals_kalman_none      <- extract_residuals_long(results_kalman, "Kalman", "None")
residuals_kalman_a         <- extract_residuals_long(results_kalman_profile_a, "Kalman", "Profile A")
residuals_kalman_b         <- extract_residuals_long(results_kalman_profile_b, "Kalman", "Profile B")
residuals_particle_none    <- extract_residuals_long(results_particle, "Particle", "None")
residuals_particle_a       <- extract_residuals_long(results_particle_profile_a, "Particle", "Profile A")
residuals_particle_b       <- extract_residuals_long(results_particle_profile_b, "Particle", "Profile B")

residuals_long <- bind_rows(
  residuals_kalman_none,
  residuals_kalman_a,
  residuals_kalman_b,
  residuals_particle_none,
  residuals_particle_a,
  residuals_particle_b
)

residuals_long <- residuals_long %>% filter(!is.na(error)) # to ensure we have no NAs

# Q-Q Plot Function (to check if the errors are normally distributed)
ggplot(residuals_long, aes(sample = error, color = method)) +
  stat_qq(size = 0.8) +
  stat_qq_line() +
  facet_grid(household ~ profile) +
  labs(
    title = "Q-Q Plots of Forecast Residuals",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()


# Performing Box and Shapiro Tests

test_results <- residuals_long %>%
  group_by(household, method, profile) %>%
  summarise(
    shapiro_p = tryCatch(shapiro.test(error)$p.value, error = function(e) NA),
    box_p = tryCatch(Box.test(error, lag = 10, type = "Ljung-Box")$p.value, error = function(e) NA),
    .groups = "drop"
  )

# Null hypothesis of Shapiro test being that for p-value
# greater than 0.05, the errors are normally distributed
# Alt hypothesis rejects this
# For Box test, the null hypothesis is that for p < 0.05,
# the errors have no autocorrelation, and alt hypothesis
# rejects this

print(test_results)

# From these, we see that neither the errors are normally
# distributed, nor are they independent, as autocorrelation
# does seem to be present nonetheless
# The Q-Q plots confirm that while the errors may look
# normally distributed in case for Kalman Filter, that is definitely
# not the case with Particle Filter



############################### END ########################################

