library(dplyr)

#load in data from working directory
activity_data <- read.csv("./activity.csv", stringsAsFactors = F)

#process data to take out missing values

comcase_activity <- activity_data[complete.cases(activity_data), ]

## combine the number of steps by day

summary_act <- comcase_activity %>% 
  group_by(date) %>% 
  summarise(daily_step_count = sum(steps))

### create a histogram that shows frequency of step ranges
hist(summary_act$daily_step_count, main= "Histogram of steps per day", xlab = "Range of daily step totals",
     ylab = "Number of days in Range", col= heat.colors(1), ylim= c(0,30))

##Calc the Mean and Median

mean_steps <- mean(summary_act$daily_step_count,na.rm=TRUE)
median_steps <- median(summary_act$daily_step_count,na.rm=TRUE)
mean_steps
median_steps


## Part 2 What is the daily activity pattern?
## group the number of steps by the interval group 
##and find the average amount of steps for each interval
interval_mean <- comcase_activity %>%
                group_by(interval) %>%
                summarise(avg_interval = mean(steps, na.rm=TRUE))

## Plot the interval section by the interval mean

plot(interval_mean$interval, interval_mean$avg_interval,
     main = "Avg Number of Steps in each Interval",
     type = "l",
     xlab = "Daily Interval",
     ylab = "Step Average"
)
## Find Which 5-minute interval, 
##on average across all the days in the dataset, 
##contains the maximum number of steps?
## use which.max function

interval_mean[which.max(interval_mean$avg_interval), ]

## interval 835 with 206.1698 average

##Calculate and report the total number of missing values 
##in the dataset (i.e. the total number of rows with NA)
## calculate number of rows in original data set (includes NA)
H <- nrow(activity_data)
## number of entries for complete cases
K <- nrow(comcase_activity)

total <-  H - K
percent_total <- total / H

## Devise strategy to fill in for missing values

## I am going to fill missing values with the total average from that specific interval
## IE if an 8am interval is missing, it will input the 8am average from the rest of the days

without_NA <- numeric(nrow(activity_data))
for (i in 1: nrow(activity_data))
{
  if (is.na(activity_data[i, "steps"]) == TRUE)
  {
    without_NA[i] <- filter(interval_mean, interval== activity_data[i, "interval"]) %>% select(avg_interval)
  }
  else
    {
      without_NA[i] <- activity_data[i, "steps"]
    
  }
}
activity_data_NoNA <- mutate(activity_data, steps_noNA = without_NA)
head(activity_data_NoNA)

##TEST
check <- filter(activity_data_NoNA,!is.na(steps)) %>% mutate(ok = (steps==steps_noNA))
mean(check$ok)

## Make a histogram of the total number of steps taken each day and Calculate and report
## the mean and median number of steps taken per day
total_day_noNA <- activity_data_NoNA %>% mutate(steps_noNA=as.numeric(steps_noNA)) %>%
  group_by(date) %>% 
  summarise(total_steps=sum(steps_noNA))
hist(total_day_noNA$total_steps, main= "Histogram of steps per day", xlab = "Range of daily step totals",
     ylab = "Number of days in Range", col= heat.colors(7), ylim= c(0,30), breaks = 8)

summary(total_day_noNA$total_steps)




## Are there any differences in activity patterns between weekdays and weekends?
## we need to seperate dates into weekend and weekday

activity_data_NoNA$date <- as.POSIXct(activity_data_NoNA$date)

activity_data_NoNA$dayType  <- ifelse(weekdays(activity_data_NoNA$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
activity_data_NoNA$dayType  <- as.factor(activity_data_NoNA$dayType)

## Now we need to sum up step totals by day type then by intervals

daytype_data  <- activity_data_NoNA %>% 
  mutate(steps_noNA=as.numeric(steps_noNA)) %>%
  group_by(dayType, interval) %>% 
  summarise(daily_step = sum(steps_noNA))

## use the ggplot2 package to plot two seperate time series by dayType
library(lattice)
with(daytype_data,
     xyplot(daily_step ~ interval | dayType,
            type= "l",
            main= "Total steps within each Interval",
            xlab= "Daily Intervals (Time)",
            ylab = "Total Steps")
     )
