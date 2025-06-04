require(car)
library(readr)
day_dat <- read_csv("C:/Users/froze/Downloads/bike+sharing+dataset/day.csv")

## PART 1
# discrete variables: season, yr, mnth, holiday, weekday, workingday, weathersit

# first, set each variable to categorical:
day_dat$season <- factor(day_dat$season, ordered = FALSE)
day_dat$yr <- factor(day_dat$yr, ordered = FALSE)
day_dat$mnth <- factor(day_dat$mnth, ordered = FALSE)
day_dat$holiday <- as.factor(day_dat$holiday)
day_dat$weekday <- factor(day_dat$weekday, ordered = FALSE)
day_dat$workingday <- as.factor(day_dat$workingday)  # workingday is completely dependent on holiday and weekday
day_dat$weathersit <- factor(day_dat$weathersit, ordered = FALSE)

# assuming no interactions
fit <- lm(cnt ~ season + yr + mnth + holiday + weekday + workingday + weathersit, data = day_dat)
summary(fit)

# diagnostics
plot(fit, which = 1)  # check linearity # some clustering in parts... but still relatively random distribution
#durbinWatsonTest(fit)  # hmmm getting p-value = 0, but i don't trust it!
qqPlot(fit$residuals) # check normality # not normally distributed---lots of divergence negatively past -1 and positively past 2.5 or so
#residualPlots(fit) # check homoscedasticity # seem to have curvature/fanning for yr, holiday, workingday, maybe season?
plot(fit, which = 3)  # check homoscedasticity # may be some fanning? but roughly linear
plot(fit, which = 4)  # check outliers/influential points # cook's distances are all very small (max doesn't even hit .1)
vif(fit)  # error: aliased coefficients means multicollinearity
alias(fit)  # which predictors cause multicollinearity?


## PART 2
fit1 <- lm(cnt ~ season + yr + mnth + holiday + weekday + workingday + weathersit + temp + atemp + hum + windspeed, data = day_dat)
summary(fit1)

# diagnostics
plot(fit1, which = 1)  # eh relatively random distribution, maybe some curvature
durbinWatsonTest(fit1)
qqPlot(fit1$residuals)  # still seeing substantial deviation in the tails
plot(fit1, which = 3)  # oop ye looks to be fanning/positive slope after ~5000 on the x-axis
plot(fit1, which = 4)  # cook's distances still quite small, highest barely hitting 0.3 (not many influential observations)
vif(fit1)
alias(fit1)
plot(fit1)  # even with residuals vs leverage plot, all points are within cook's distance (although 595 is looking suspicious)

