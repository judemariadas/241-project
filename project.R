library(readr)
library(ggplot2)
#library(tidyverse)
#library(GGally)     # pairs plots
#library(janitor)    # variable summaries
library(knitr)
library(car)
library(broom)
library(boot)
library(dplyr)
library(MASS)

bike_day <- read.csv("bike+sharing+dataset/day.csv")
bike_hr <- read.csv("bike+sharing+dataset/hour.csv")


str(bike_day)
summary(bike_day)
summary(bike_hr)

# not standardized (data is normalized)
daily_model <- lm(cnt ~ temp + atemp + hum + windspeed, data= bike_day)
summary(daily_model)

hourly_model <- lm(cnt ~ temp + atemp + hum + windspeed, data= bike_hr)
summary(hourly_model)

par(mfrow = c(2, 2))
plot(daily_model)

quartz()
par(mfrow = c(1, 1))

## variable summary table
vars      <- names(bike_day)
classes   <- sapply(bike_day, function(x) class(x)[1])
n_unique  <- sapply(bike_day, function(x) length(unique(x)))
example   <- sapply(bike_day, function(x) as.character(x[1]))

var_tbl <- data.frame(
  variable = vars,
  class    = classes,
  n_unique = n_unique,
  example  = example,
  stringsAsFactors = FALSE,
  row.names = NULL
)

print(var_tbl)


## univariate graphics — HW 2
num_vars <- names(bike_day)[sapply(bike_day, is.numeric)]
cat_vars <- names(bike_day)[!sapply(bike_day, is.numeric)]

## histograms for numeric variables
if (length(num_vars)) {
  op <- par(mfrow = c(ceiling(length(num_vars) / 4), min(4, length(num_vars))),
            mar = c(4, 4, 2, 1))
  for (v in num_vars) {
    hist(bike_day[[v]],
         main = v,
         xlab = "",
         col  = "grey",
         border = "white")
  }
  par(op)
}

## bar plots for categorical variables
if (length(cat_vars)) {
  op <- par(mfrow = c(ceiling(length(cat_vars) / 4), min(4, length(cat_vars))),
            mar = c(4, 4, 2, 1))
  for (v in cat_vars) {
    barplot(table(bike_day[[v]]),
            main = v,
            xlab = "",
            ylab = "Count",
            col  = "grey",
            border = "white")
  }
  par(op)
}

# Pairs plot  (HW 2-Proj 3) – diagnostics
sel_vars  <- c("cnt","casual","registered",
               "temp","atemp","hum","windspeed",
               "season","weathersit","hr")

pairs_df  <- bike_hr[intersect(sel_vars, names(bike_hr))]

## scale numeric columns
num_cols <- sapply(pairs_df, is.numeric)
pairs_df[num_cols] <- lapply(pairs_df[num_cols],
                             function(x) as.numeric(scale(x)))

## scatter-plot matrix
pairs(pairs_df,
      pch = 19,
      col = adjustcolor("black", alpha.f = 0.25),
      main = "Pairs plot")

# HW3
cont_vars <- c("temp","atemp","hum","windspeed")    # continuous only
fit_cont  <- lm(cnt ~ ., data = bike_hr %>% select(cnt, all_of(cont_vars)))


# Coefficients, SEs, p-values, adj R²
## OLS with continuous predictors
cont_vars <- c("temp", "atemp", "hum", "windspeed")
fit_cont  <- lm(cnt ~ ., data = bike_hr[, c("cnt", cont_vars)])

## coefficient table and adjusted R²
coef_tbl <- cbind(
  Estimate  = coef(fit_cont),
  Std.Error = sqrt(diag(vcov(fit_cont))),
  t.value   = coef(fit_cont) / sqrt(diag(vcov(fit_cont))),
  Pr_t      = 2 * pt( abs(coef(fit_cont) / sqrt(diag(vcov(fit_cont)))) ,
                      df = df.residual(fit_cont),
                      lower.tail = FALSE)
)
print(coef_tbl, digits = 4)
cat("\nAdjusted R²:", summary(fit_cont)$adj.r.squared, "\n")

## default diagnostic plots
op <- par(mfrow = c(2, 2));  plot(fit_cont);  par(op)

## Breusch–Pagan (heteroscedasticity)
e        <- residuals(fit_cont)
bp_stat  <- summary(lm(e^2 ~ fitted(fit_cont)))$r.squared * length(e)
bp_p     <- pchisq(bp_stat, df = 1, lower.tail = FALSE)
cat("\nBreusch–Pagan χ² =", round(bp_stat, 3), "p =", signif(bp_p, 3), "\n")

## Durbin–Watson (serial correlation)
dw <- sum(diff(e)^2) / sum(e^2)
cat("Durbin–Watson =", round(dw, 3), "\n")

## Variance Inflation Factors
X   <- model.matrix(fit_cont)[, -1]            # drop intercept
vif <- sapply(seq_len(ncol(X)), function(i) {
  1 / (1 - summary(lm(X[, i] ~ X[, -i]))$r.squared)
})
names(vif) <- colnames(X)
print(vif, digits = 3)
# HW 4
# discrete-only model

bike_day$season     <- as.factor(bike_day$season)
bike_day$yr         <- as.factor(bike_day$yr)
bike_day$mnth       <- as.factor(bike_day$mnth)
bike_day$holiday    <- as.factor(bike_day$holiday)
bike_day$weekday    <- as.factor(bike_day$weekday)
bike_day$workingday <- as.factor(bike_day$workingday)
bike_day$weathersit <- as.factor(bike_day$weathersit)

# linear model
mod_disc <- lm(cnt ~ season + yr + mnth + holiday + weekday + workingday + weathersit, data = bike_day)
summary(mod_disc)

mm <- model.matrix(mod_disc)
aliased_terms <- attr(alias(mod_disc)$Complete, "dimnames")[[1]]
mm_df <- as.data.frame(mm[, !(colnames(mm) %in% c("(Intercept)", aliased_terms))])

vif_disc <- car::vif(lm(cnt ~ . - 1, data = cbind(cnt = bike_day$cnt, mm_df)))
print(vif_disc)    

# mixed model (cont + disc)
fit_mix <- lm(cnt ~ season + yr + mnth + holiday + weekday + workingday +
                weathersit + temp + atemp + hum + windspeed,
              data = bike_day)

summary(fit_mix)
# quick diagnostics
par(mfrow = c(2, 2));  plot(fit_mix)

# reduced model
fit_reduced <- lm(cnt ~ mnth + yr + holiday + weekday + weathersit +
                    temp + hum + windspeed, data = bike_day)
summary(fit_reduced)

# HW5 --------------------------------------------------------------

disc <- c("season","yr","mnth","holiday","weekday","workingday","weathersit")
bike_day[disc] <- lapply(bike_day[disc], factor)   # convert once to treat as categorical predictors

# MIXED MODEL

fit_mix <- lm(cnt ~ season + yr + mnth + holiday + weekday + workingday +
                weathersit + temp + atemp + hum + windspeed,
              data = bike_day)
orig_coef <- coef(fit_mix)                         
class_se  <- coef(summary(fit_mix))[ , "Std. Error"]

# function for bootstrap
statfun <- function(df, i) {
  m <- lm(cnt ~ season + yr + mnth + holiday + weekday + workingday +
            weathersit + temp + atemp + hum + windspeed,
          data = df[i, ])
  
  v <- rep(NA_real_, length(orig_coef))
  names(v) <- names(orig_coef)
  v[names(coef(m))] <- coef(m)    
  v                               
}

# table(is.na(statfun(bike_day, sample(nrow(bike_day), replace = TRUE))))

# BOOTSTRAP
set.seed(241)
boot_out <- boot(bike_day, statistic = statfun, R = 1000, parallel = "no")
colnames(boot_out$t) <- names(orig_coef)   
boot_se <- apply(boot_out$t, 2, sd, na.rm = TRUE)
class_se <- coef(summary(fit_mix))[ , "Std. Error"]

se_tbl <- data.frame(
  term      = names(class_se),
  classical = round(class_se, 3),
  bootstrap = round(boot_se[names(class_se)], 3)
)

print(se_tbl, row.names = FALSE)

-------------------------------------------------------#  testing the hypothesis that a reduced set would reduce SE
# Refit reduced model
fit_reduced <- lm(cnt ~ season + yr + holiday + weekday + weathersit + temp + hum + windspeed, data = bike_day)
# Extract SEs
orig_coef <- coef(fit_reduced)
class_se <- coef(summary(fit_reduced))[ , "Std. Error"]
# Bootstrap function
statfun_reduced <- function(df, i) {
  m <- lm(cnt ~ season + yr + holiday + weekday + weathersit + temp + hum + windspeed, data = df[i, ])
  v <- rep(NA_real_, length(orig_coef)); names(v) <- names(orig_coef)
  v[names(coef(m))] <- coef(m); v
}
# Run bootstrap
set.seed(241)
boot_out <- boot(bike_day, statfun_reduced, R = 1000)
colnames(boot_out$t) <- colnames(boot_out$t, do.NULL = FALSE)# Compute bootstrap SEs safely
boot_se <- rep(NA_real_, length(orig_coef))
names(boot_se) <- names(orig_coef)

boot_se[names(orig_coef)] <- apply(boot_out$t, 2, sd, na.rm = TRUE)

# Compare with classical SEs
se_tbl <- data.frame(
  term      = names(class_se),
  classical = round(class_se, 3),
  bootstrap = round(boot_se[names(class_se)], 3)
)

print(se_tbl, row.names = FALSE)

# results validate our hypothesis that yes in a reduced model, classical assumptions are more reliable- less collinearity, more stable estimation, fewer dropped terms
--------------------------------------------------------

# HW 6 ----------------------------------------------

# old model - with multicollinearity
fit_old <- lm(cnt ~ season + yr + mnth + holiday + weekday + workingday +
                  weathersit + temp + atemp + hum + windspeed,
                data = bike_day)
summary(fit_old)


# fix collinearity and nonlinearity)
### - Remove: atemp (collinear with temp), season (collinear with mnth), workingday (redundant)
### - Transform: temp and hum (nonlinear relationship with cnt)


bike_day$sqrt_temp <- sqrt(bike_day$temp)
bike_day$sqrt_hum  <- sqrt(bike_day$hum)

fit_new <- lm(cnt ~ yr + mnth + holiday + weekday + weathersit +
                sqrt_temp + sqrt_hum + windspeed,
              data = bike_day)
summary(fit_new)

# Model Comparison


# R-squared
r2_old <- summary(fit_old)$r.squared
r2_new <- summary(fit_new)$r.squared
cat("R-squared (old model):", round(r2_old, 4), "\n")
cat("R-squared (new model):", round(r2_new, 4), "\n\n")

# Standard errors
se_old <- coef(summary(fit_old))[ , "Std. Error"]
se_new <- coef(summary(fit_new))[ , "Std. Error"]

compare_se <- data.frame(
  Term     = names(se_new),
  SE_Old   = round(se_old[names(se_new)], 3),
  SE_New   = round(se_new, 3)
)
kable(compare_se, caption = "Comparison of Standard Errors (Old vs. New Model)")

# VIF
cat("\nVariance Inflation Factors (New Model):\n")
print(vif(fit_new))


par(mfrow=c(2,2))
plot(fit_new)
residualPlots(fit_new)
avPlots(fit_new)

# checking random hypothesis ============================================================


# Log transformation (add small constant to avoid log(0))
bike_day$log_temp <- log(bike_day$temp + 1e-6)
bike_day$log_hum  <- log(bike_day$hum + 1e-6)

# Quarter-root transformation
bike_day$qrt_temp <- bike_day$temp^(0.25)
bike_day$qrt_hum  <- bike_day$hum^(0.25)

### ------------------------------------------
### Case A: Log transformation model
fit_log <- lm(cnt ~ yr + mnth + holiday + weekday + weathersit +
                log_temp + log_hum + windspeed,
              data = bike_day)

### Case B: Quarter-root transformation model
fit_qrt <- lm(cnt ~ yr + mnth + holiday + weekday + weathersit +
                qrt_temp + qrt_hum + windspeed,
              data = bike_day)

### ------------------------------------------
### Compare summaries
cat("R² (log model):", round(summary(fit_log)$r.squared, 4), "\n")
cat("R² (qrt model):", round(summary(fit_qrt)$r.squared, 4), "\n")

### Optional: compare AIC
aic_log <- AIC(fit_log)
aic_qrt <- AIC(fit_qrt)
cat("AIC (log model):", round(aic_log, 2), "\n")
cat("AIC (qrt model):", round(aic_qrt, 2), "\n")

### Optional: Residual plots
par(mfrow=c(2,2)); plot(fit_log)
par(mfrow=c(2,2)); plot(fit_qrt)

avPlots(fit_qrt)
residualPlots(fit_qrt)

# HW 7 ==================================================================

# (a)

## full set = HW6 “fixed” predictors
full_form <- cnt ~ yr + mnth + holiday + weekday + weathersit + 
  I(temp^.5) + I(hum^.5) + windspeed

## forward stepwise by AIC
start_lm  <- lm(cnt ~ 1, data = bike_day)              # intercept only
full_lm   <- lm(full_form,  data = bike_day)           # upper scope
best_lm   <- step(start_lm, scope = list(upper = full_lm),
                  direction = "forward", trace = 0)

summary(best_lm)$adj.r.squared      # new model
tidy(best_lm)                       # coefficients + std errors
summary(best_lm)


# (b)
## threshold: upper quartile of demand
q50 <- quantile(bike_day$cnt, 0.5)
bike_day$high_cnt <- factor(ifelse(bike_day$cnt > q50, 1, 0))

## use SAME predictors selected above
best_pred <- attr(terms(best_lm), "term.labels")
log_form  <- as.formula(paste("high_cnt ~", paste(best_pred, collapse = " + ")))

logit_mod <- glm(log_form, data = bike_day, family = binomial)

tidy(logit_mod, conf.int = TRUE)    # coeffs, SE, 95 % CIs on log-odds
logit_mod


# ------------- reduced logit model since there was perfect separation
reduced_form <- log_form <- high_cnt ~ yr + holiday + weekday + weathersit + I(temp^0.5) + I(hum^0.5) + windspeed
logit_reduced <- glm(reduced_form, data = bike_day, family = binomial)
summary(logit_reduced)
# -----------------------------------------

# testing
base <- lm(cnt ~ mnth, data = bike_day)
extra <- lm( cnt ~ mnth + season, data = bike_day)
c(AIC(base), AIC(extra))
anova(base, extra)
# random plots to help

# 1. Check the distribution of high_cnt
cat("Distribution of high_cnt:\n")
table(bike_day$high_cnt)
prop.table(table(bike_day$high_cnt))

# 2. Investigate YEAR variable (most suspicious given large coefficient)
cat("\n=== YEAR INVESTIGATION ===\n")
year_table <- table(bike_day$yr, bike_day$high_cnt)
print(year_table)
cat("\nProportions by year:\n")
prop.table(year_table, margin = 1)  # row proportions

# 3. Investigate TEMPERATURE (largest coefficient)
cat("\n=== TEMPERATURE INVESTIGATION ===\n")
# Create temperature bins
bike_day$temp_bins <- cut(bike_day$temp, breaks = 10, labels = FALSE)
temp_table <- table(bike_day$temp_bins, bike_day$high_cnt)
print(temp_table)

cat("\nTemperature summary by high_cnt:\n")
bike_day %>% 
  group_by(high_cnt) %>% 
  summarise(
    min_temp = min(temp),
    q25_temp = quantile(temp, 0.25),
    median_temp = median(temp),
    q75_temp = quantile(temp, 0.75),
    max_temp = max(temp),
    .groups = 'drop'
  )

# 4. Check for perfect separation patterns
cat("\n=== PERFECT SEPARATION CHECKS ===\n")

# Check if any temperature threshold perfectly separates
temp_thresholds <- quantile(bike_day$temp, seq(0.1, 0.9, 0.1))
for(i in 1:length(temp_thresholds)) {
  thresh <- temp_thresholds[i]
  high_temp <- bike_day$temp > thresh
  separation_table <- table(high_temp, bike_day$high_cnt)
  
  # Check if any cell is zero (indicates separation)
  if(any(separation_table == 0)) {
    cat(sprintf("Temperature threshold %.3f shows separation:\n", thresh))
    print(separation_table)
    cat("\n")
  }
}


# Check temperature separation with median cutoff
q50 <- quantile(bike_day$cnt, 0.5)
bike_day$med_high_cnt <- factor(ifelse(bike_day$cnt > q50, 1, 0))

levels <- levels(bike_day$med_high_cnt)
summary_list <- list()

# each level (0 and 1)
for (lev in levels) {
  subset_data <- bike_day[bike_day$med_high_cnt == lev, ]
  summary_list[[lev]] <- data.frame(
    med_high_cnt = lev,
    min_temp     = min(subset_data$temp),
    max_temp     = max(subset_data$temp),
    median_temp  = median(subset_data$temp)
  )
}

# Combine results into one data frame
temp_summary <- do.call(rbind, summary_list)
print(temp_summary)

# Check for separation
low_demand_temps <- bike_day$temp[bike_day$med_high_cnt == 0]
high_demand_temps <- bike_day$temp[bike_day$med_high_cnt == 1]

cat("Max temp for low demand:", max(low_demand_temps), "\n")
cat("Min temp for high demand:", min(high_demand_temps), "\n")
