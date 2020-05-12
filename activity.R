##REad the csv file.

activity <- read.csv("activity.csv")

##hhistogram of the total steps per day.

Steps_day <- aggregate(steps ~ date, activity, sum, na.rm=TRUE)
hist(Steps_day$steps)

##Mean and median number of steps taken each day

mean_Steps_day <- mean(Steps_day$steps)
mean_Steps_day

median_Step_day <- median(Steps_day$steps)
median_Step_day


###Time series plot of the average number of steps taken

stepsperInterval <- aggregate(steps~interval, data = activity, mean, na.rm = TRUE)

plot(steps~interval, data = stepsperInterval, type="l", col="blue", main ="Average Daily Activity")

##The 5-minute interval that, on average, contains the maximum number of steps
MaxSteps <- stepsperInterval[which.max(stepsperInterval$steps),1]
MaxSteps

##Imputing missing values
totalNA <- sum(is.na(activity$steps))
totalNA


### Create a new dataframe from the original dataframe but with NO NA values.
source("getmeaninaterval.R")

NoNA_activity <- activity

totalrows <- nrow(NoNA_activity)

for (i in 1:totalrows) {
  if(is.na(NoNA_activity[i,]$steps)) {
    NoNA_activity[i,]$steps <- getmeaninaterval(NoNA_activity[i,]$interval, stepsperInterval)
  }
  
}

totalNA <- sum(is.na(NoNA_activity$steps))

totalNA


##hhistogram of the total steps per day. with no NA values.

Steps_day_NoNA <- aggregate(steps ~ date, NoNA_activity, sum)

hist(Steps_day_NoNA$steps)


##Mean and median number of steps taken each day in the dataframe with No NA values.

meanSteps_NoNA <- mean(Steps_day_NoNA$steps)
meanSteps_NoNA

medianSteps_NoNA <- median(Steps_day_NoNA$steps)
medianSteps_NoNA


##differences in activity patterns between weekdays and weekends

##formating the data and create a new variable named day.

NoNA_activity$date <- as.Date(strptime(NoNA_activity$date, format="%Y-%m-%d"))

NoNA_activity$day <- weekdays(NoNA_activity$date)


for (i in 1:totalrows) {
  if(NoNA_activity[i,]$day %in% c("Saturday","Sunday")) {
    NoNA_activity[i,]$day <- "weekend"
  }
  else {
    NoNA_activity[i,]$day <-  "weekday"
  }
  
}

NoNA_activity$day <- as.factor(NoNA_activity$day)


##Make a panel plot containing a time series plot
Steps_by_Day <- aggregate(NoNA_activity$steps ~ NoNA_activity$interval+NoNA_activity$day, NoNA_activity, mean)
names(Steps_by_Day) <- c("interval", "day", "steps")
library(lattice)

xyplot(steps ~ interval | day, Steps_by_Day, type = "l", layout = c(1, 2), ylab = "Number of Steps")