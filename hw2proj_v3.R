require(car)
## PART 1
# graphical variable representations for day
par(mfrow = c(3, 3))
barplot(table(day_dat$season), names=c('winter','spring','summer','fall'), main='Season Data (Day)')  # pretty well-distributed, with the most counts during the summer
barplot(table(day_dat$yr), names=c('2011','2012'), main='Year Data (Day)')  # very evenly distributed (especially given that the paper used 2011 for training and 2012 for testing)
barplot(table(day_dat$mnth), names=c('jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec'), main='Month Data (Day)')  # some months have more points, but the standard deviation is definitely < +/- 10
barplot(table(day_dat$holiday), names=c('no','yes'), main='Is it a holiday? (Day)')  # heavily skewed---most days in a year are not holidays
barplot(table(day_dat$weekday), names=c('sun','mon','tues','weds','thurs','fri','sat'), main='Weekday Data (Day)')  # assuming week starts on sunday  # very evenly distributed
barplot(table(day_dat$workingday), names=c('no','yes'), main='Is it a working day? (Day)')  # fairly heavily-skewed---most days in a year are working days
barplot(table(day_dat$weathersit), names=c('clear/partly cloudy','misty/cloudy','light snow/rain'), main='Weather Data (Day)')  # pretty skewed---maybe the weather tends to be pretty clear most days in D.C.?
hist(day_dat$temp, xlab='Temperature [°C]', main='Normalized temperature (Day)')  # looking almost normally-distributed, at worst a little bimodal (may be because of binning)
hist(day_dat$atemp, xlab='Temperature [°C]', main='Normalized feeling temperature (Day)')  # looking kind of bimodal (more severe than actual temp)
par(mfrow = c(2, 2))
hist(day_dat$hum, xlab='Humidity (max 100)', main='Normalized humidity (Day)')  # treat humidity like a percentage/probability (max is 1)  # left-skewed like some sort of flipped poisson distribution
hist(day_dat$windspeed, xlab='Wind speed (max 67)', main='Normalized wind speed (Day)')  # treat windspeed like a percentage/probability (max is 1)  # right-skewed, kind of poissonian or chi-squared
hist(day_dat$casual, xlab='Count', main='Count of casual users (Day)')  # looking poissonian
hist(day_dat$registered, xlab='Count', main='Count of registered users (Day)')  #  idek what this distribution would be
par(mfrow = c(1, 1))
hist(day_dat$cnt, xlab='Count', main='Count of total rental bikes (Day)')  # target (response variable)  # seems to match up with registered user distribution

# graphical variable representations for hour
par(mfrow = c(3, 3))
barplot(table(hour_dat$season), names=c('winter','spring','summer','fall'), main='Season Data (Hour)')  # pretty evenly-distributed
barplot(table(hour_dat$yr), names=c('2011','2012'), main='Year Data (Hour)')  # very evenly distributed
barplot(table(hour_dat$mnth), names=c('jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec'), main='Month Data (Hour)')  # still pretty evenly distributed
barplot(table(hour_dat$hr), main='Hour Data (Hour)')  # very evenly distributed, although interesting to see a dip for 3am and 4am
barplot(table(hour_dat$holiday), names=c('no','yes'), main='Is it a holiday? (Hour)')  # obviously heavily skewed
barplot(table(hour_dat$weekday), names=c('sun','mon','tues','weds','thurs','fri','sat'), main='Weekday Data (Hour)')  # assuming week starts on sunday  # very evenly distributed
barplot(table(hour_dat$workingday), names=c('no','yes'), main='Is it a working day? (Hour)')  # again, obviously skewed
barplot(table(hour_dat$weathersit), names=c('clear/partly cloudy','misty/cloudy','light snow/rain','heavy snow/rain'), main='Weather Data (Hour)')  # quite skewed, but honestly, WHO is out there biking in rain/snow/storm???
hist(hour_dat$temp, xlab='Temperature (min -8, max +39) [°C]', main='Normalized temperature (Hour)')  # looking kind of bimodal (maybe trimodal at worst?)
par(mfrow = c(2, 3))
hist(hour_dat$atemp, xlab='Temperature (min -16, max +50) [°C]', main='Normalized feeling temperature (Hour)')  # yep, definintely trimodal
hist(hour_dat$hum, xlab='Humidity (max 100)', main='Normalized humidity (Hour)')  # treat humidity like a percentage/probability (max is 1)  # left-skewed
hist(hour_dat$windspeed, xlab='Wind speed (max 67)', main='Normalized wind speed (Hour)')  # treat windspeed like a percentage/probability (max is 1)  # heavily right-skewed (it probably doesn't get super windy in D.C. often)
hist(hour_dat$casual, xlab='Count', main='Count of casual users (Hour)')  # pretty poissonian again
hist(hour_dat$registered, xlab='Count', main='Count of registered users (Hour)')  # also quite poissonian
hist(hour_dat$cnt, xlab='Count', main='Count of total rental bikes (Hour)')  # target (response variable)  # still quite poissonian, unsurprisingly given the distribution of registered users

# bivariate pair plots (day)
#pairs(day_dat$cnt ~ day_dat$season, pch=16)  # summer/fall preference, with spring not far behind
#pairs(day_dat$cnt ~ day_dat$yr, pch=16)  # 2012 preference
#pairs(day_dat$cnt ~ day_dat$mnth, pch=16)  # whoa march! september is preferred
#pairs(day_dat$cnt ~ day_dat$holiday, pch=16)  # significant preference for non-holiday
#pairs(day_dat$cnt ~ day_dat$weekday, pch=16)  # looks like a weekend preference, if 0 = sun and 6 = sat
#pairs(day_dat$cnt ~ day_dat$workingday, pch=16)  # non-workday preference...!
#pairs(day_dat$cnt ~ day_dat$weathersit, pch=16)  # preference for good weather, of course
#pairs(day_dat$cnt ~ day_dat$temp, pch=16)  # higher temps -> more rentals, up to a certain point
#pairs(day_dat$cnt ~ day_dat$atemp, pch=16)  # same trends as regular temps
#pairs(day_dat$cnt ~ day_dat$hum, pch=16)  # concentrated between .5 and .7; outlier in bottom left?
#pairs(day_dat$cnt ~ day_dat$windspeed, pch=16)  # concentrated between .1 and .2

# bivariate pair plots (hour)
#pairs(hour_dat$cnt ~ hour_dat$season, pch=16)  # summer/fall preference, with spring not far behind
#pairs(hour_dat$cnt ~ hour_dat$yr, pch=16)  # lot more biking in 2012, seems like
#pairs(hour_dat$cnt ~ hour_dat$mnth, pch=16)  # october is super popular, november is super unpopular lol
#pairs(hour_dat$cnt ~ hour_dat$hr, pch=16)  # WHO is out biking at 4am??? midmorning and midafternoon most popular
#pairs(hour_dat$cnt ~ hour_dat$holiday, pch=16)  # preference for non-holiday biking
#pairs(hour_dat$cnt ~ hour_dat$weekday, pch=16)  # if weekdays are correctly listed... sat/sun not so popular? maybe people are biking to work/school...
#pairs(hour_dat$cnt ~ hour_dat$workingday, pch=16)  # working day preference, ye
#pairs(hour_dat$cnt ~ hour_dat$weathersit, pch=16)  # WHO is out here biking during snow/storms/heavy rain???
#pairs(hour_dat$cnt ~ hour_dat$temp, pch=16)  # trending upwards, though a pretty sharp cutoff around .9
#pairs(hour_dat$cnt ~ hour_dat$atemp, pch=16)  # cutoff closer to .8
#pairs(hour_dat$cnt ~ hour_dat$hum, pch=16)  # preference for middling humidities (outliers for really dry days?)
#pairs(hour_dat$cnt ~ hour_dat$windspeed, pch=16)  # preference for less-windy days


## from the paper: 'month, hour, working day and temperature are selected as most important features for hourly scale
## and month, working-day and temperature are selected as final features for daily scale' (result of feature selection)

## PART 2 (questions probably need to be reformulated)
# who is biking at ass-o'clock in the morning??? casual vs registered riders
# how is casual vs registered biking affected during holidays? (expect more casual riders?)
# how is casual vs registered biking affected during e.g. summer? (increased tourism?)
# at e.g. 1am on a rainy january morning, is it more likely that a registered or a casual rider will rent a bike?

## ACTUALLY PART 3!!!
pairs(day_dat$cnt ~ day_dat$mnth + day_dat$workingday + day_dat$temp, pch=16)
# month and temp: highly correlated (obviously---temperature changes with the time of year), obvious nonlinearity
# count and temp: nonlinear behavior (makes sense---people probably prefer to bike on a day where the weather is
# not too hot and not too cold)
# count and month: nonlinearity, but also makes sense for temperature reasons

pairs(hour_dat$cnt ~ hour_dat$mnth + hour_dat$hr + hour_dat$workingday + hour_dat$temp, pch=16)
# month and temp: same correlation and nonlinearity as in day.csv
# hour and temp: continued nonlinearity and correlation (temperature naturally changes with time of day)
# count and tempt: same behavior as in day.csv
# count and month: nonlinearity (temperature-dependent, presumably)
# count and hour: nonlinearity (who's out biking at 4am???)
