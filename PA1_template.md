---
title: "Reproducible Research: Peer Assessment 1"
author: "Berfeito"
date: "6 September 2022"
output:
        html_document: 
                code_folding: show
---

### This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

# First Part

Load the data  and Process/transform it (if necessary) into a format suitable for your analysis


```{r, echo = TRUE, results='hide', warning=FALSE, message=FALSE}
library(readr)
activity <- read_csv("activity.zip")
```

# Second part

For this part missing values are being ignored in order to build a histogram of the total number of steps taken each day and the calculation of mean and median total number of steps taken per day.

```{r, echo = TRUE, results='hide', warning=FALSE, message=FALSE}
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)

sum_steps <- activity %>% group_by(date) %>% summarize(total_steps = sum(steps, na.rm = TRUE))

sum_steps

hist <- ggplot(sum_steps, aes(total_steps)) + geom_histogram(binwidth = 2000) +
        labs(y = "Frquency", x = "Total steps per day")

hist

mean_sum_steps <- mean(sum_steps$total_steps)
median_sum_steps <- median(sum_steps$total_steps)
```

**After analysis, the mean of the number of steps taken is `r mean_sum_steps` and the median is `r median_sum_steps`**

For this next step, a time series plot of the 5-minute interval will help verify which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps

```{r, echo = TRUE, results='hide', warning=FALSE, message=FALSE}
steps_interval <- activity %>% group_by(interval) %>% summarize(steps = mean(steps, na.rm = TRUE))


ggplot(steps_interval, aes(interval, steps)) + geom_line() +
        labs(x= "Intervals (5 minutes)", y = "Mean daily steps")
        
interval_row <- which.max(steps_interval$steps)
max_interval <- steps_interval[interval_row,1]
```

**On average, across all the days, the 5-minute interval #`r max_interval` contains the maximum number of steps.**

# Third part

For the next part, NA values will not be exluded but replaced after their total number is calculated. The strategy employed will be to use the mean/median for the values around the NA position.

```{r, echo = TRUE, results='hide', warning=FALSE, message=FALSE}
colSums(is.na(activity))

nas <- length(which(is.na(activity$steps)))
```

**The variable "steps" is the only one with NA values. They total `r nas`**


A new dataset, equal to the original dataset but with the missing data filled in will be created and a histogram of the total number of steps taken each day prepared for vizualization.

```{r, echo = TRUE, results='hide', warning=FALSE, message=FALSE}
library(tidyr)

nona <- activity %>% mutate(steps = replace_na(steps,mean(steps, na.rm = TRUE)))

sum_steps_nona <- nona %>% group_by(date) %>% summarize(daily_steps = sum(steps))

hist_nona <- ggplot(sum_steps_nona, aes(daily_steps)) +
        geom_histogram(binwidth = 2000) +
        labs(x = "Total steps per day")
```


With this analysis available, the mean and median of the total number of steps taken per day will be calculated. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```{r, echo = TRUE, results='hide', warning=FALSE, message=FALSE}
mean_steps_nona <- mean(sum_steps_nona$daily_steps)
median_steps_nona <- median(sum_steps_nona$daily_steps)

df <- data.frame(mean_sum_steps, mean_steps_nona, median_sum_steps, median_steps_nona)

mean_diff <- mean_steps_nona - mean_sum_steps
median_diff <- median_steps_nona - median_sum_steps
```

The mean difference between the data without and with NA is `r mean_diff` and the median difference is `r median_diff`. The impact of inputting the missing data is acceptable.

# Fourth part

For this part a new factor variable identifying weekdays and weekends will be created to observe if there is a difference in activity within these days. Next, a panel containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days will be created.

```{r, echo = TRUE, results='hide', warning=FALSE, message=FALSE}
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

ggplot(plot_weekdays, aes(interval, plot_weekdays$`mean(steps)`)) + geom_line() + 
        facet_wrap(~weekday, nrow = 2) +
        labs(x= "Intervals (5 minutes)", y = "Mean daily steps")
```

**A difference in activity patterns between weekdays and weekends can indeed be observed**
