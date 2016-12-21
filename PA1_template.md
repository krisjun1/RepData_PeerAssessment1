# Reproducible Research: Course Project 1


## Loading and preprocessing the data
1. Load the data (i.e. read.csv()):

```r
if(!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")) {
        temp <- tempfile()
        download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
        unzip(temp)
        unlink(temp)
}

activity<-read.csv("activity.csv")
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

2.Process/transform the data (if necessary) into a format suitable for your analysis

It is ready to go.

## What is mean number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.
1. Calculate the total number of steps taken per day


```r
totalStepsByDay<-aggregate(steps~date, activity, sum)
```
2.If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
hist(totalStepsByDay$steps, xlab="Class of Total Number of Steps per day", ylab="Number of Days", main="Total Number of Steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3.Calculate and report the mean and median of the total number of steps taken per day


```r
mean_raw<-mean(totalStepsByDay$steps)
mean_raw
```

```
## [1] 10766.19
```
Mean number of steps taken per day is 1.0766189\times 10^{4}.


```r
median_raw<-median(totalStepsByDay$steps)
median_raw
```

```
## [1] 10765
```
Median number of steps taken per day is 10765.

## What is the average daily activity pattern?

1.Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

The average number of steps taken:

```r
averageStepsbyInterval<-aggregate(steps~interval, activity, mean)
```

Time series plot of the average number of steps taken:

```r
with(averageStepsbyInterval, plot(interval, steps, type = "l"))
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
averageStepsbyInterval[which.max(averageStepsbyInterval[,2]),1]
```

```
## [1] 835
```

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as 𝙽𝙰). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)


```r
missingIndex<-is.na(activity[,1])
```
There are 2304 missing values in the dataset. 

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I decided to fill in all of the missing values in the dataset by the mean number of steps per interval.

Finding the mean number of steps per Interval:

```r
m<-mean(averageStepsbyInterval$steps)
```

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

Imputing missing values with m = 37.3825996

```r
activity1<-activity
activity1[missingIndex,1]<-m
head(activity1)
```

```
##     steps       date interval
## 1 37.3826 2012-10-01        0
## 2 37.3826 2012-10-01        5
## 3 37.3826 2012-10-01       10
## 4 37.3826 2012-10-01       15
## 5 37.3826 2012-10-01       20
## 6 37.3826 2012-10-01       25
```
 
4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps? 

Finding the total number of steps each day after missing values are imputed and making histogram:


```r
totalStepsByDay1<-aggregate(steps~date, activity1, sum)
hist(totalStepsByDay1$steps, xlab="Class of Total Number of Steps per day", ylab="Number of Days", main="Number of Steps taken each day after missing values are imputed")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

To calculate the mean and median total number of steps per day, we first find total number of steps per day


```r
totalStepsByDay1<-aggregate(steps~date, activity1, sum)
```


```r
mean_afterImput<-mean(totalStepsByDay1$steps)
mean_afterImput
```

```
## [1] 10766.19
```

Mean number of steps taken per day is 1.0766189\times 10^{4}.


```r
median_afterImput<-median(totalStepsByDay1$steps)
median_afterImput
```

```
## [1] 10766.19
```
Median number of steps taken per day is 1.0766189\times 10^{4}.

Since I imputed the missing values by the mean number of steps per interval, there is no difference in mean before and after imputing that is not surprising. The median has changed a little bit.

## Are there differences in activity patterns between weekdays and weekends?

For this part the 𝚠𝚎𝚎𝚔𝚍𝚊𝚢𝚜() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
activity1$date<-as.Date(activity1$date)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
activity2<-activity1%>%
        mutate(dayType= ifelse(weekdays(activity1$date)=="Saturday" | weekdays(activity1$date)=="Sunday", "Weekend", "Weekday"))
head(activity2)
```

```
##     steps       date interval dayType
## 1 37.3826 2012-10-01        0 Weekday
## 2 37.3826 2012-10-01        5 Weekday
## 3 37.3826 2012-10-01       10 Weekday
## 4 37.3826 2012-10-01       15 Weekday
## 5 37.3826 2012-10-01       20 Weekday
## 6 37.3826 2012-10-01       25 Weekday
```

2.Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
averageStepByDayTypeAndInterval<-activity2 %>%
  group_by(dayType, interval) %>%
  summarize(averageStepByDay=sum(steps))

head(averageStepByDayTypeAndInterval)
```

```
## Source: local data frame [6 x 3]
## Groups: dayType [1]
## 
##   dayType interval averageStepByDay
##     <chr>    <int>            <dbl>
## 1 Weekday        0         315.2956
## 2 Weekday        5         242.2956
## 3 Weekday       10         231.2956
## 4 Weekday       15         232.2956
## 5 Weekday       20         228.2956
## 6 Weekday       25         283.2956
```


```r
library(lattice)
with(averageStepByDayTypeAndInterval, 
      xyplot(averageStepByDay ~ interval | dayType, 
      type = "l",      
      main = "Total Number of Steps within Intervals by dayType",
      xlab = "Daily Intervals",
      ylab = "Average Number of Steps"))
```

![](PA1_template_files/figure-html/unnamed-chunk-18-1.png)<!-- -->
