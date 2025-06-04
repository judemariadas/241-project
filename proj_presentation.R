library(readr)
library(ggplot2)
library(knitr)
library(car)
library(broom)
library(boot)
library(dplyr)
library(MASS)

bike <- read.csv("bike+sharing+dataset/hour.csv")

glimpse(bike)  # overview
summary(bike)  # basic stats
colnames(bike) # list of column names

# Ensure expected ranges
table(bike$season)
range(bike$temp)
range(bike$casual + bike$registered - bike$cnt)  # should be zero

# Outlier Detection
z_scores <- scale(bike$casual)
outliers_z <- bike[abs(z_scores) > 3, ]
nrow(outliers_z)

ggplot(bike, aes(x = "Temperature", y = temp)) +
  geom_boxplot(outlier.shape = 16, outlier.colour = "red") +
  ylab("Normalized Temperature") +
  xlab("") +
  ggtitle("Boxplot of Normalized Temperature")



model <- lm(cnt ~ season + holiday + workingday + weathersit + temp + atemp + hum + windspeed, data = bike)
vif(model)

# Step 4 Residual analysis 

fit <- lm(casual ~ season + holiday + workingday + weathersit + temp + hum + windspeed, data = bike)
summary(fit)

bike <- bike %>%
  mutate(registered_majority = ifelse(registered > casual, 1, 0))
table(bike$registered_majority)

# homoscedasticity check
plot(fit$fitted.values, fit$residuals,
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red", lty = 2)

# normality of residuals
shapiro.test(sample(fit$residuals, 5000))  # sample if too many obs


qqnorm(fit$residuals)
qqline(fit$residuals, col = "red")

durbinWatsonTest(fit)

# based on tests above we see funnel shaped residuals and normality is viaolated therefore we try log transform
fit_log <- lm(log1p(casual) ~ season + holiday + workingday + weathersit + temp + hum + windspeed, data = bike)

# round 2 diagnostics for log model
# Homoscedasticity check
plot(fit_log$fitted.values, fit_log$residuals,
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted Values (log1p model)")
abline(h = 0, col = "red", lty = 2)

# Normality of residuals
shapiro.test(sample(fit_log$residuals, 5000))  # sample if too many obs

# Q-Q plot
qqnorm(fit_log$residuals)
qqline(fit_log$residuals, col = "red")

# Autocorrelation check
durbinWatsonTest(fit_log)

# results show reduced funneling ; substantially better qq plot. but durbin watson still bad - strong positive autocorrelation

# let's try adding a lag term now.
bike <- bike %>%
  arrange(dteday, hr) %>%  # ensure time order
  mutate(casual_lag1 = lag(casual, 1))

# Refit model (excluding NA introduced by lag)
fit_lag <- lm(log1p(casual) ~ casual_lag1 + season + holiday + workingday + weathersit + temp + hum + windspeed, 
              data = bike %>% filter(!is.na(casual_lag1)))
# run diagnostics for lag term
plot(fit_lag$fitted.values, fit_lag$residuals,
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted Values (log1p + lag)")
abline(h = 0, col = "red", lty = 2)
shapiro.test(sample(fit_lag$residuals, 5000))
qqnorm(fit_lag$residuals)
qqline(fit_lag$residuals, col = "red")
durbinWatsonTest(fit_lag)

# Hypothetical example (won't run with your current libraries):
library(nlme)
# create day grouping
bike <- bike %>%
  arrange(dteday, hr) %>%
  group_by(dteday) %>%
  mutate(day_id = cur_group_id()) %>%
  ungroup()

gls_model <- gls(log1p(casual) ~ season + holiday + workingday +
                   weathersit + temp + hum + windspeed,
                 data = bike,
                 correlation = corAR1(form = ~ 1 | day_id))

summary(gls_model)
acf(residuals(gls_model))


# I think try splitting casual vs non-casual and see if different inferences come up
gls_casual <- gls(log1p(casual) ~ season + holiday + workingday + weathersit + temp + hum + windspeed,
                  data = bike,
                  correlation = corAR1(form = ~ 1 | day_id))

gls_registered <- gls(log1p(registered) ~ season + holiday + workingday + weathersit + temp + hum + windspeed,
                      data = bike,
                      correlation = corAR1(form = ~ 1 | day_id))
summary(gls_casual)
summary(gls_registered)

# diagnostic plots
# Casual
plot(fitted(gls_casual), resid(gls_casual),
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted (Casual)")
abline(h = 0, col = "red", lty = 2)

# Registered
plot(fitted(gls_registered), resid(gls_registered),
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted (Registered)")
abline(h = 0, col = "red", lty = 2)
# Casual
qqnorm(resid(gls_casual), main = "Normal Q-Q Plot (Casual)")
qqline(resid(gls_casual), col = "red")

# Registered
qqnorm(resid(gls_registered), main = "Normal Q-Q Plot (Registered)")
qqline(resid(gls_registered), col = "red")

# Casual
acf(resid(gls_casual), main = "ACF of Residuals (Casual)")

# Registered
acf(resid(gls_registered), main = "ACF of Residuals (Registered)")

# not great results ebcause of the autocorrelation which still exists. there is clear daily periodicity and limited long-range structure

bike <- bike %>%
arrange(dteday, hr) %>%
  mutate(casual_lag1 = lag(casual, 1),
         registered_lag1 = lag(registered, 1)) %>%
  filter(!is.na(casual_lag1) & !is.na(registered_lag1))

# Casual model
fit_casual <- lm(log1p(casual) ~ casual_lag1 + season + holiday + workingday +
                   weathersit + temp + hum + windspeed, data = bike)

# Registered model
fit_registered <- lm(log1p(registered) ~ registered_lag1 + season + holiday + workingday +
                       weathersit + temp + hum + windspeed, data = bike)

# Next Step ===========================================================
# bootstrap based feature importance stability
library(boot)

# Boot function for casual
boot_fn_casual <- function(data, indices) {
  d <- data[indices, ]
  model <- lm(log1p(casual) ~ casual_lag1 + season + holiday + workingday +
                weathersit + temp + hum + windspeed, data = d)
  return(coef(model))
}

# Boot function for registered
boot_fn_registered <- function(data, indices) {
  d <- data[indices, ]
  model <- lm(log1p(registered) ~ registered_lag1 + season + holiday + workingday +
                weathersit + temp + hum + windspeed, data = d)
  return(coef(model))
}

# Bootstrap
set.seed(241)

boot_casual <- boot(data = bike, statistic = boot_fn_casual, R = 500)
boot_registered <- boot(data = bike, statistic = boot_fn_registered, R = 500)

# compare coefficient stability
casual_ci <- apply(boot_casual$t, 2, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
registered_ci <- apply(boot_registered$t, 2, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))

casual_summary <- as.data.frame(t(casual_ci))
registered_summary <- as.data.frame(t(registered_ci))

casual_summary$term <- names(coef(fit_casual))
registered_summary$term <- names(coef(fit_registered))

casual_summary <- casual_summary %>%
  rename(CI_lower = `2.5%`, Median = `50%`, CI_upper = `97.5%`) %>%
  mutate(user_type = "Casual")

registered_summary <- registered_summary %>%
  rename(CI_lower = `2.5%`, Median = `50%`, CI_upper = `97.5%`) %>%
  mutate(user_type = "Registered")

# combine to compare
combined_summary <- bind_rows(casual_summary, registered_summary)
knitr::kable(combined_summary, digits = 3)

ggplot(combined_summary, aes(x = term, y = Median, color = user_type)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper),
                width = 0.2, position = position_dodge(width = 0.5)) +
  coord_flip() +
  labs(title = "Bootstrapped Coefficient Estimates",
       y = "Coefficient (log1p scale)", x = "Predictor",
       color = "User Type") +
  theme_minimal()

# checking interaction variables temp* hum
# Casual
fit_casual_int <- lm(log1p(casual) ~ casual_lag1 + season + holiday + workingday +
                       weathersit + temp * hum + windspeed, data = bike)

# Registered
fit_registered_int <- lm(log1p(registered) ~ registered_lag1 + season + holiday + workingday +
                           weathersit + temp * hum + windspeed, data = bike)

summary(fit_casual_int)
summary(fit_registered_int)


# =================
# Create a grid of temp and humidity values
grid <- expand.grid(
  temp = seq(min(bike$temp), max(bike$temp), length.out = 100),
  hum = seq(min(bike$hum), max(bike$hum), length.out = 3),  # select 3 hum levels
  casual_lag1 = mean(bike$casual_lag1, na.rm = TRUE),
  season = 1,  # you can vary these if needed
  holiday = 0,
  workingday = 1,
  weathersit = 1,
  windspeed = mean(bike$windspeed)
)

# Predict from casual model with interaction
grid$log_casual <- predict(fit_casual_int, newdata = grid)
grid$casual_count <- expm1(grid$log_casual)

# Plot
ggplot(grid, aes(x = temp, y = casual_count, color = factor(hum))) +
  geom_line(size = 1.2) +
  labs(title = "Interaction: Temperature × Humidity (Casual Riders)",
       x = "Temperature (normalized)", y = "Predicted Casual Count",
       color = "Humidity Level") +
  theme_minimal()

boot_fn_casual_int <- function(data, indices) {
  d <- data[indices, ]
  model <- lm(log1p(casual) ~ casual_lag1 + season + holiday + workingday +
                weathersit + temp * hum + windspeed, data = d)
  return(coef(model))
}

set.seed(241)
boot_casual_int <- boot(data = bike, statistic = boot_fn_casual_int, R = 500)

# CI summary
casual_int_ci <- apply(boot_casual_int$t, 2, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
casual_int_summary <- as.data.frame(t(casual_int_ci))
casual_int_summary$term <- names(coef(fit_casual_int))

# plot
casual_int_summary <- casual_int_summary %>%
  rename(CI_lower = `2.5%`, Median = `50%`, CI_upper = `97.5%`) %>%
  mutate(user_type = "Casual")

ggplot(casual_int_summary, aes(x = term, y = Median, color = user_type)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper),
                width = 0.2, position = position_dodge(width = 0.5)) +
  coord_flip() +
  labs(title = "Bootstrapped Coefficient Estimates with Interaction (Casual)",
       y = "Coefficient (log1p scale)", x = "Predictor",
       color = "User Type") +
  theme_minimal()
