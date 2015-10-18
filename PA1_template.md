# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
activity <- read.csv("activity.csv", header = TRUE)
activitynona <- subset(activity, !is.na(steps))
```


## What is mean total number of steps taken per day?

```r
sum_day <- aggregate(steps ~ date, activity, sum)
mean_day <- mean(sum_day$steps)
mean_day
```

```
## [1] 10766.19
```

```r
median_day <- median(sum_day$steps)
median_day
```

```
## [1] 10765
```

```r
plot1 <- hist(sum_day$steps, 8)
abline(v = mean_day, untf = FALSE, col= "red" )
abline(v = median_day-40, untf = FALSE, col= "blue" )
legend("topright", c("mean_day","median_day"), lty=c(1,1), col = c("red","blue"))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 


## What is the average daily activity pattern?

```r
sum_freq <- aggregate(steps ~ interval, activity, sum)
mean_freq <- aggregate(steps ~ interval, activity, mean)
```



```
## Warning: package 'ggplot2' was built under R version 3.2.2
```



```r
p2 <- ggplot(mean_freq , aes(interval, steps))
p2 <- p2 + geom_line() +
  xlab("Interval") +
  ylab(expression("Average Steps")) +
  ggtitle('5-minute interval and the average number of steps taken across date range')
print(p2)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
x <- max(mean_freq$steps)
mean_freq[mean_freq$steps==x,]
```

```
##     interval    steps
## 104      835 206.1698
```


## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
activityisna <- subset(activity, is.na(steps))
nrow(activityisna)
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
if the value is NA, substitute the mean for that day

my strategy was to take the average for that interval and subtitute the average for the na


```r
#getting the mean
mean_freq <- aggregate(steps ~ interval, activity, mean)
```


Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
#thanks David hood for your guidance in the discussion
#rounding because steps are measured otherwise in whole numbers

combinedSet <- merge(activity, mean_freq, by="interval")
combinedSet$steps.x[is.na(combinedSet$steps.x)] <- round(combinedSet$steps.y[is.na(combinedSet$steps.x)])
```


Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
## What is mean total number of steps taken per day?

sum_day_imp <- aggregate(steps.x ~ date, combinedSet, sum)
mean_day_imp <- mean(sum_day_imp$steps)
median_day_imp <- median(sum_day_imp$steps)

plot1 <- hist(sum_day_imp$steps, 8)
abline(v = mean_day_imp, untf = FALSE, col= "red" )
abline(v = median_day_imp-30, untf = FALSE, col= "blue" )
legend("topright", c("mean_day_imp","median_day_imp"), lty=c(1,1), col = c("red","blue"))
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

```r
mean_day_imp
```

```
## [1] 10765.64
```

```r
median_day_imp
```

```
## [1] 10762
```

The impact is not significant.  The mean and median are only slightly affected.


  

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
mean_freq <- aggregate(steps ~ interval, activity, mean)
#thanks David hood for your guidance in the discussion
#rounding because whole numbers
combinedSet <- merge(activity, mean_freq, by="interval")
combinedSet$steps.x[is.na(combinedSet$steps.x)] <- round(combinedSet$steps.y[is.na(combinedSet$steps.x)])
names(combinedSet$steps.x) <- "steps"
```







```r
#Factor variable with two levels indicating a weekday or weekend.
combinedSet$day <- weekdays(as.Date(combinedSet$date))
combinedSet$daytype <- as.factor(c("weekend","weekday"))
weekenddays <- combinedSet$day == "Saturday" | combinedSet$day == "Sunday"
combinedSet$daytype[weekenddays] = "weekend"
combinedSet$daytype[!weekenddays] = "weekday"

 
mean_freqc <- aggregate(steps.x ~ interval + daytype,combinedSet, mean)
ggplot(mean_freqc, aes(interval, steps.x)) +
    geom_line(color="blue") +
    xlab("Daily 5 minute Intervals") +
    ylab("Average Number of Steps") +    
    facet_grid(. ~ daytype)
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 
