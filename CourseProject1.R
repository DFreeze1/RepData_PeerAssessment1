getwd()

# Loading packages
library(ggplot2)
library(ggthemes)
library(dplyr)

# Loading the data

unzip("activity.zip")
activity <- read.csv("activity.csv")

str(activity)

activity$date <-  as.Date(activity$date)

# Total number of steps per day. First, group data by day. Second, sum for each group.

total_daily_steps <- activity |> 
    group_by(date) |> 
    summarise(daily_steps = sum(steps, na.rm = TRUE))

head(total_daily_steps)

# Histogram of daily steps. 

histogram <- ggplot(total_daily_steps, aes(daily_steps)) +
    geom_histogram(bins = 12, fill = "orange", col = "black") +
    xlab("Total Steps Per Day") +
    ylab("Frequency")

histogram

# Mean and Median of total steps taken per day

mean(total_daily_steps$daily_steps, na.rm = TRUE)
median(total_daily_steps$daily_steps, na.rm = TRUE)

# Question 2
# Time-series plot of steps taken each interval. First, group by interval. Second, average the steps
# taken each interval. 

avg_steps_per_interval <- activity |> 
    group_by(interval) |> 
    summarise(avg_steps = mean(steps, na.rm = TRUE))
head(avg_steps_per_interval)

time_series <- ggplot(avg_steps_per_interval, aes(x = interval, y = avg_steps)) +
    geom_line() +
    xlab("5-minute Interval") +
    ylab("average number of steps")

time_series

avg_steps_per_interval[which.max(avg_steps_per_interval$avg_steps),]$interval

# Inputting missing values

# Total number of rows with NAs

sum(is.na(activity$steps))

# Fill in missing values using the 5-minute interval. Create dataset that is equal to the original 
# dataset but with the missing data filled in. 

## Is a function of interval. when avg_steps_per_interval

get_avg_steps_per_interval <- function(interval) {
    avg_steps_per_interval[avg_steps_per_interval$interval==interval,]$avg_steps
}


activity_noNA <- activity
for(i in 1:nrow(activity_noNA)){
    if(is.na(activity_noNA[i,]$steps)){
        activity_noNA[i,]$steps <- get_avg_steps_per_interval(activity_noNA[i,]$interval)
    }
}
 # make a histogram of total number of steps taken each day. 

total_daily_steps_noNA <- activity_noNA |> 
    group_by(date) |> 
    summarise(daily_steps = sum(steps, na.rm = TRUE))

head(total_daily_steps_noNA)


histogram2 <- ggplot(total_daily_steps_noNA, aes(daily_steps)) +
    geom_histogram(bins = 12, fill = "orange", col = "black") +
    xlab("Total Steps Per Day") +
    ylab("Frequency")

histogram2

mean(total_daily_steps_noNA$daily_steps, na.rm = TRUE)
median(total_daily_steps_noNA$daily_steps, na.rm = TRUE)


# Are there differences in activity patterns between weekdays and weekends?

activity_noNA$day <- weekdays(activity_noNA$date)
for(i in 1:nrow(activity_noNA)) {
    if(activity_noNA[i,]$day %in% c("Saturday", "Sunday")) {
        activity_noNA[i,]$day <- "weekend"
    }
    else {
        activity_noNA[i,]$day <- "weekday"
    }
}

avg_steps_per_interval_wk <- activity_noNA |> 
    group_by(interval, day) |> 
    summarise(avg_steps_wk = mean(steps, na.rm = TRUE))
head(avg_steps_per_interval_wk)

# Make a panel plot


panel_plot <- ggplot(avg_steps_per_interval_wk, aes(x = interval, y = avg_steps_wk)) +
    geom_line(color = "blue") +
    theme_classic() +
    facet_wrap(vars(day), nrow = 2) +
    xlab("5-minute interval") +
    ylab("Average number of steps")
panel_plot
