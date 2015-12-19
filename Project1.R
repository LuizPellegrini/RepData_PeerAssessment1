## Loading and preprocessing the data
activity_file <- read.csv("activity.csv", header=TRUE, sep=",", na.strings="NA")
##
## What is mean total number of steps taken per day? (QUESTION 1)
##
## Eliminates the NA 
##
act_file_clean <- na.omit(activity_file)
## summarize total number os steps per day
sum_file <-as.data.frame(xtabs(act_file_clean$steps ~ act_file_clean$date, df))
##
## creates the histogram for the total number os steps per day
##
plot(sum_file$act_file_clean.date, sum_file$Freq, 
     type="o", main="Total steps per day", xlab="Date", 
     ylab="Number of steps")
summary(sum_file)
mean(sum_file$Freq)
median(sum_file$Freq)
##
## What is the average daily activity pattern? (QUESTION 2)
##
## Aggregate number os steps by interval
      sum_file2 <- as.data.frame(xtabs(act_file_clean$steps ~ act_file_clean$interval, df))
## Creates plot with Number os steps per interval
      plot(sum_file2$act_file_clean.interval, sum_file2$Freq, type="1", 
           xlab="Intervals",ylab="Total number of steps",
           main="Total number os steps per interval"
      )
## Gets the maximum number os steps per day,
## finds the row index for this value and displays interval with the max number os steps
max_step <- sum_file2$act_file_clean.interval[which.max(sum_file2$Freq)]
max_step
##
## QUESTION 3 Dealing with imput missing values
##
## number of steps with NA values
##
sum(is.na(activity_file$steps))
##
## find the mean of steps per day
##
agr_day <- aggregate(act_file_NA$step, list(act_file_NA$date), mean)
agr_day$Group.1 <- as.Date(agr_day$Group.1)
agr_day$x[is.na(agr_day$x)] <- 0
##
## fill in average day for NA intervals
##
act_file_NA <- activity_file 
act_file_NA$date <- as.Date(act_file_NA$date) 
## calculate the general mean
## mean_steps <- mean(agr_day$x)
##
## apply the general mean to NA values
##
##for(i in 1:700) {
for(i in 1:17568) {
  dif_days <- act_file_NA$date[i+1] - act_file_NA$date[1] + 1
  dif_days <- as.numeric(dif_days)
  act_file_NA$steps[i][is.na(act_file_NA$steps[i])] <- agr_day$x[dif_days]
}
## creates new file with the replaced NA
sum_file_WONA <-as.data.frame(xtabs(act_file_NA$steps ~ act_file_NA$date, df))  
plot(sum_file_WONA$act_file_NA.date, sum_file_WONA$Freq, 
     type="h", main="Total steps per day", xlab="Date", 
     ylab="Total number of steps")
summary(sum_file_WONA) 
mean(sum_file_WONA$Freq)
median(sum_file_WONA$Freq) 
##
## Compare the effects of replacing NA
##
mean_diff <- mean(sum_file_WONA$Freq) - mean(sum_file$Freq)
median_diff <- median(sum_file_WONA$Freq) - median(sum_file$Freq)
##
## part 4 - include column for weekend and weekdays
## plot 2 charts, type = 1, comparing weekdays and weekends by interval
##
sum_day_interval <- aggregate(act_file_clean$steps~act_file_clean$interval+
                                act_file_clean$date, FUN=mean)
sum_day_interval["weekend"] <- NA
sum_day_interval$`act_file_clean$date` <- 
  as.Date(sum_day_interval$`act_file_clean$date`)
sum_day_interval$weekend = 
  chron::is.weekend(sum_day_interval$`act_file_clean$date`)
##
agreg_weekday <- aggregate(sum_day_interval$`act_file_clean$steps`~
                             sum_day_interval$weekend + 
                             sum_day_interval$`act_file_clean$interval`, FUN=sum)
names(agreg_weekday) <- c("weekend", "interval", "steps")
##
dt_weekend <- subset(agreg_weekday, agreg_weekday$weekend=="TRUE") 
dt_weekday <- subset(agreg_weekday, agreg_weekday$weekend=="FALSE") 
par(mfrow=c(2,1))
plot.ts(dt_weekday$interval, dt_weekday$steps, type="h",
            main="Total steps per weekday", 
     xlab="Interval",ylab="Total number of steps")
plot.ts(dt_weekend$interval, dt_weekend$steps, 
             type="h", main="Total steps per weekend", 
     xlab="Interval",ylab="Total number of steps")