require(car)
library(readr)
#day_dat <- read_csv("C:/Users/froze/Downloads/bike+sharing+dataset/day.csv")  # use this for desktop
day_dat <- read_csv("C:/Users/froze/Downloads/day.csv")  # use this for laptop

## PART 1
# categorical
day_dat$season <- factor(day_dat$season, ordered = FALSE)
day_dat$yr <- factor(day_dat$yr, ordered = FALSE)
day_dat$mnth <- factor(day_dat$mnth, ordered = FALSE)
day_dat$holiday <- as.factor(day_dat$holiday)
day_dat$weekday <- factor(day_dat$weekday, ordered = FALSE)
day_dat$workingday <- as.factor(day_dat$workingday)  # workingday is completely dependent on holiday and weekday
day_dat$weathersit <- factor(day_dat$weathersit, ordered = FALSE)

pairs(cnt ~ season + yr + mnth + holiday + weekday + workingday + weathersit, data = day_dat, pch=16)
# "fix" linearity by setting the categorical variables to factors
# can't really "fix" normality for categorical variables because e.g. "mean month" or "mean day type" doesn't really  make sense
# no real worries about outliers/influential points

fit1 <- lm(cnt ~ yr + mnth + holiday + weekday + weathersit, data = day_dat)  # remove working day, season
summary(fit1)
vif(fit1)  # season and month have high VIF---remove each individually and choose the one with best R^2?
# plot(fit1, which = 3)

# no month, no working day: R^2 = .7441
# no season, no working day: R^2 = .7823
# no season, add working day, no holiday, no weekday: R^2 = .7769

# continuous
# "fix" linearity by performing transformations
# normality... kind of a lost cause ;((((
# removed predictors for multicollinearity, as well as non-influential predictors
pairs(cnt ~ I(atemp^.5) + I(hum^.5), data = day_dat, pch=16)
fit2 <- lm(cnt ~ I(atemp^.5) + I(hum^.5), data = day_dat)  # remove temp
#plot(log(cnt) ~ log(hum), data = day_dat)
summary(fit2)
vif(fit2)  # temp and atemp have high VIF---remove each individually and choose the one with best R^2
residualPlots(fit2)
avPlots(fit2)
plot(fit2, which=2)

# no temp: R^2 = .461
# no atemp: R^2 = .4587

fit3 <- lm(cnt ~ yr + mnth + holiday + weekday + weathersit + I(temp^.5) + I(hum^.5) + windspeed, data = day_dat)
summary(fit3)
# vif(fit3)
plot(fit3, which=2)
