---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
### Read data from "activity.csv" into dataframe "activity00"

```r
setwd("C:/Users/jay/Desktop/Coursera/Course05/Week02/Assignment")
activity00 <- read.csv("activity.csv", header = TRUE, sep = ",")
```

## What is mean total number of steps taken per day?

Sum the steps per day (ignoring missing values at this stage)


```r
activitySum01 = aggregate(activity00$steps, list(activity00$date),sum)
names(activitySum01) = c("Date","Total.Steps")
```
### Make a histogram of the  total steps per day


```r
library(ggplot2)
qplot(Total.Steps, data=activitySum01,main="Total Daily Steps",xlab="Total Daily Steps",ylab="Count", binwidth=1000,na.rm=TRUE)
```

![](PA1_template_files/figure-html/make_histo_raw-1.png)<!-- -->

### Calculate and report the mean and median total steps per day


```r
## summary(activitySum01$Total.Steps)
avgTS <- mean(activitySum01$Total.Steps,na.rm=TRUE)
medTS <- median(activitySum01$Total.Steps,na.rm=TRUE)
sumTS <- sum(activitySum01$Total.Steps,na.rm=TRUE)   # will use this later
```
The Average Daily Total Steps is 10766.19.

The Median Daily Total Steps is 10765.

## What is the average daily activity pattern?

### Make a Time Series of Average Daily Steps by 5-minute Intervals


```r
activityAvebyInterval = with(activity00, aggregate(steps,list(interval),mean,na.rm=TRUE))
names(activityAvebyInterval) = c("Interval","Ave.Steps")
ggplot(data=activityAvebyInterval,aes(Interval,Ave.Steps)) + 
    geom_line()+
    ggtitle("Average Daily Steps by Interval")+
    ylab("Average Steps")
```

![](PA1_template_files/figure-html/time_series_raw-1.png)<!-- -->


### Find the Interval containing the Maximum Average Number of Steps


```r
intMax <- activityAvebyInterval[which.max(activityAvebyInterval$Ave.Steps),1 ]
stepMax <- activityAvebyInterval[which.max(activityAvebyInterval$Ave.Steps),2 ]
```

The Maximum Average Daily Steps is 206.17 which occurs in Interval 835.


## Imputing missing values

### Find the total number of rows with missing data


```r
missCount <- sum(is.na(activity00$steps))
```

The number of rows with missing data is 2304


### Perform imputation with the average daily steps at the same interval
### Replace missing data with imputed values


```r
activityMeans = activityAvebyInterval
actNew <- activity00
for (i in 1:nrow(actNew)) {
      if (is.na(actNew[i,1])) {
          actNew[i,1]<-activityMeans[which(activityMeans$Interval==actNew[i,3]),2]
      }
  }
```

### Make a histogram of the data with imputed values and compare the mean, median, and total steps with the original dataset



```r
library(ggplot2)
actNewSum = aggregate(actNew$steps, list(actNew$date),sum)
names(actNewSum) = c("Date","Total.Steps.Imputed")
##
qplot(Total.Steps.Imputed, data=actNewSum,main="Total Daily Steps with Imputation",
      xlab="Total Daily Steps with Imputation",ylab="Count", binwidth=1000)
```

![](PA1_template_files/figure-html/make_histo_impute-1.png)<!-- -->

### Calculate statistics to compare with original dataset


```r
avgImpTS <- mean(actNewSum$Total.Steps.Imputed)
medImpTS <- median(actNewSum$Total.Steps.Imputed)
sumImpTS <- sum(actNewSum$Total.Steps.Imputed)
```

After imputation with average values, the Average Daily Total Steps does not change, but the Median Daily Steps changes slightly:

Average without imputation: 10766.19

Average with imputation: 10766.19

Median without imputation: 10765

Median with imputation: 10766.19

The Total Steps change substantially due to imputation as noted by the increased height of the histogram:

Total without imputation: 570608

Total with imputation: 656737.51



## Are there differences in activity patterns between weekdays and weekends?

### Create "weekday" variable and average data by interval


```r
actNew[,2] <- as.Date(actNew[,2])
actNew$Day.Name = weekdays(actNew$date)
actNew$type =  as.factor(ifelse(actNew$Day.Name %in% c("Saturday","Sunday"),"weekend","weekday"))
actNew$type2 =factor(actNew$type,levels=c("weekend","weekday"))  # reorder for plot
actNewByInt = with(actNew, aggregate(steps,list(interval,type2),mean))
names(actNewByInt) = c("Interval","Type", "Ave.Steps")
```

### Make the weekend/weekday time series plots


```r
g7 <- ggplot(actNewByInt, aes(Interval, Ave.Steps))
g7 + labs(y="Number of Steps") + geom_line() + 
    facet_wrap(~Type,ncol=1)
```

![](PA1_template_files/figure-html/time_series_weekday-1.png)<!-- -->

It appears that the Weekday steps peak at intervals between 800 and 900 and then decrease and remain fairly consistent before dropping off at around interval 2000.

Weekend steps still peak in the same place but the peak is lower (150 vs. 225). the intervals between the peak and interval 2000 are fairly consistent but slightly higher (approximately 75 vs. 50) than the Weekday steps.
