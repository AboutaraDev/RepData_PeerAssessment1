

zip_file <- "activity.zip"
extract_file <- "activity"

if(!dir.exists(extract_file)) {
   dir.create(extraxt_file)
}

# unzip file

unzip(zip_file, exdir=extract_file)
setwd("./activity")
dir()
## Loading and preprocessing the data

# 1- load the data
activity = read.csv("activity.csv")

# 2- Process/transform the data (if necessary) into a format suitable for your analysis
totalSteps <- aggregate(steps ~ date, activity, sum, na.rm=TRUE)

## What is mean total number of steps taken per day?
# 1- calculate the total number of steps taken per day
totalSteps

# 2- Make a histogram of the total number of steps taken each day
hist(totalSteps$steps)
# 3- calculate and report the mean and median total number of steps taken per day
mean(totalSteps$steps)
median(totalSteps$steps)

## What is the average daily activity pattern?
# 1- Make a time series plot(i.e type="l") of the 5-minute interval(x-axis) and the average number of steps taken, averaged across all days(y-axis)
stepsInterval <- aggregate(steps ~ interval, data=activity, mean, na.rm=TRUE)
plot(steps ~ interval, data=stepsInterval, type="l")
# 2- which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
stepsInterval[which.max(stepsInterval$steps),]$interval

## Imputing missing values
# 1- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(activity$steps))

# 2- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
interval2steps <-function(interval) {
   stepsInterval[stepsInterval$interval == interval, ]$steps
}

# 3- Create a new dataset that is equal to the original dataset but with the missing data filled in.
activityFilled <- activity
count = 0
for(i in 1:nrow(activityFilled)) {
  if(is.na(activityFilled[i,]$steps)) {
    activityFilled[i,]$steps <- interval2steps(activityFilled[i,]$interval)
    count = count + 1  
  }
}

cat("Total ", count, "NA values were filled.\n\r")

# 4- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
totalSteps2 <- aggregate(steps ~ date, data=activityFilled, sum)
hist(totalSteps2$steps)
mean(totalSteps2$steps)
median(totalSteps2$steps)
## Are there differences in activity patterns between weekdays and weekends?
# 1- Create a new factor variable in the dataset with two levels ??? ???weekday??? and ???weekend??? indicating whether a given date is a weekday or weekend day.
activityFilled$day = ifelse(as.POSIXlt(as.Date(activityFilled$date))$wday%%6 == 0, "weekend", "weekday")
# For Sunday and Saturday : weekend, Other days : weekday
activityFilled$day = factor(activityFilled$day, levels = c("weekday", "weekend"))
# 2- Make a panel plot containing a time series plot (i.e.type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
stepsInterval2 = aggregate(steps ~ interval + day, activityFilled, mean)
library(lattice)
xyplot(steps ~ interval | factor(day), data = stepsInterval2, aspect = 1/2, 
       type = "l")