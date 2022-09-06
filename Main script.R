## Loading and pre-processing the data

library(readr)
activity <- read_csv("activity.zip")

# What is mean total number of steps taken per day? For this part of the assignment, you can ignore the missing values in the dataset.

## 1. Make a histogram of the total number of steps taken each day

library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)

sum_steps <- activity %>% group_by(date) %>% summarize(total_steps = sum(steps, na.rm = TRUE))

sum_steps

hist <- ggplot(sum_steps, aes(total_steps)) + geom_histogram(binwidth = 2000) +
        labs(y = "Frquency", x = "Total steps per day")

hist

## 2. Calculate and report the **mean** and **median** total number of steps taken per day

mean_sum_steps <- round(as.numeric(mean(sum_steps$total_steps)))
median_sum_steps <- as.numeric(median(sum_steps$total_steps))


#####  The mean of the number of steps taken is `r mean_sum_steps` and the median is `r median_sum_steps`


# What is the average daily activity pattern?

## 1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

steps_interval <- activity %>% group_by(interval) %>% summarize(steps = mean(steps, na.rm = TRUE))


ggplot(steps_interval, aes(interval, steps)) + geom_line() +
        labs(x= "Intervals (5 minutes)", y = "Mean daily steps")

                
## 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

interval_row <- which.max(steps_interval$steps)
max_interval <- steps_interval[interval_row,1]

#### On average, across all the days, the 5-minute interval #`r max_interval` contains the maximum number of steps.


        
## Imputing missing values

## Note that there are a number of days/intervals where there are missing values (coded as `NA`). The presence of missing days may introduce
        #bias into some calculations or summaries of the data.

## 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)
        
colSums(is.na(activity))

nas <- length(which(is.na(activity$steps)))

#### The variable "steps" is the only one with NA values. They total `r nas`

## 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the 
        #mean/median for that day, or the mean for that 5-minute interval, etc.

library(tidyr)

nona <- activity %>% mutate(steps = replace_na(steps,mean(steps, na.rm = TRUE)))


## 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

sum_steps_nona <- nona %>% group_by(date) %>% summarize(daily_steps = sum(steps))


## 4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. 
        # Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of 
        # the total daily number of steps?

hist_nona <- ggplot(sum_steps_nona, aes(daily_steps)) +
        geom_histogram(binwidth = 2000) +
        labs(x = "Total steps per day")


mean_steps_nona <- mean(sum_steps_nona$daily_steps)
median_steps_nona <- median(sum_steps_nona$daily_steps)

df <- data.frame(mean_sum_steps, mean_steps_nona, median_sum_steps, median_steps_nona)

mean_diff <- mean_steps_nona - mean_sum_steps
median_diff <- median_steps_nona - median_sum_steps

#### The mean difference between the data without and with NA is `r mean_diff` and the median difference is `r median_diff` 

## Are there differences in activity patterns between weekdays and weekends? For this part the `weekdays()` function may be of some help here.
        # Use the dataset with the filled-in missing values for this part.

## 1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

plot_weekdays <- nona %>% mutate(days = weekdays(date),
                                      weekday = case_when(
                                      days == "lundi" ~ "weekday",
                                      days == "mardi" ~ "weekday",
                                      days == "mercredi" ~ "weekday",
                                      days == "jeudi" ~ "weekday",
                                      days == "vendredi" ~ "weekday",
                                      days == "samedi" ~ "weekend",
                                      days == "dimanche" ~ "weekend"),
                                      weekday = as.factor(weekday)) %>%
        group_by(weekday, interval) %>%
        summarize(mean(steps))

head(plot_weekdays)


## 2. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged
        # across all weekday days or weekend days (y-axis).

ggplot(plot_weekdays, aes(interval, plot_weekdays$`mean(steps)`)) + geom_line() + 
        facet_wrap(~weekday, nrow = 2) +
        labs(x= "Intervals (5 minutes)", y = "Mean daily steps")
        
