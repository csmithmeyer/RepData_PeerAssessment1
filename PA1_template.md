---
title: "Reprducible Research Project 1"
author: "Colby Smithmeyer"
date: "April 23, 2019"
output:
  html_document: 
    toc: yes
    keep_md: true  
---


## Loading and preprocessing the data
The first step in the project is to load the data to R. 


```r
setwd("C:/Users/colby/Desktop/coursera/")

download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", "data.zip")

unzip("data.zip","activity.csv")

data<-read.csv("activity.csv",header=T)
```

We will now process the data by removing the NA values


```r
clean<-data[!is.na(data$steps),]
```


## What is mean total number of steps taken per day?

The following code will create a table that calculates the total steps taken per day, and then create a histogram of the results.  Finally it will create a summary table showing the mean and median of the results.  

```r
steps_day<-tapply(clean$steps, clean$date, sum)

hist(steps_day, main = "Histogram of Total Steps Per Day", xlab="Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
summary(steps_day)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10765   10766   13294   21194       8
```


## What is the average daily activity pattern?

The following code will calculate the average steps for each 5- minute time interval and create a plot of the results.  Finally, it will identify the 5 minute time interval with the highest average number of steps.



```r
steps_interval<-tapply(clean$steps, clean$interval, mean)

plot(as.integer(names(steps_interval)),steps_interval, type = "l", main="Average Steps Per Interval",
     xlab="Interval", ylab="Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
names(which(steps_interval == max(steps_interval)))
```

```
## [1] "835"
```

## Imputing missing values
The following code will report the number of missing values coded as NA.  It will then replace the NA values with the average steps per 5 minute time interval and create a new data set with these values. Finally, it will create a histogram of the new values, and create a summary table displaying the mean and median of the new values. 


```r
sum(is.na(data))
```

```
## [1] 2304
```

```r
nadata<-data[is.na(data$steps),]
nadata$steps<-steps_interval

newdata<-rbind(clean,nadata)

new_steps_day<-(tapply(newdata$steps,newdata$date, sum))
hist(new_steps_day, main="Histogram of Total Steps per Day with NAs removed", xlab="Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
summary(new_steps_day)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```

As you can see there are very minor differences between the original and new values.  This validates the method used to impute the missing values.

## Are there differences in activity patterns between weekdays and weekends?
The following code creates a new column that identifies whether the activity occurred on a Weekday or Weekend.  Finally the code displays two plots; 1 an average number of steps per 5 minute time interval on the weekends, and 2, the same, but for weekdays.  

```r
newdata$date<-as.POSIXct(newdata$date, format = "%Y-%m-%d")
newdata$day<-weekdays(newdata$date)
newdata$category<-ifelse(newdata$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
newdata$category<-as.factor(newdata$category)


newdata_weekday<-newdata[(newdata$category=="Weekday"),]
newdata_weekend<-newdata[(newdata$category=="Weekend"),]

steps_weekday<-tapply(newdata_weekday$steps, newdata_weekday$interval, mean)
steps_weekend<-tapply(newdata_weekend$steps, newdata_weekend$interval, mean)

par(mfrow=c(2,1))
plot(as.integer(names(steps_weekday)),steps_weekday, type = "l", main = "Average Steps Per
     Interval (Weekday)", xlab="Interval", ylab="Steps")
plot(as.integer(names(steps_weekend)),steps_weekend, type = "l", main = "Average Steps Per
     Interval (Weekend)", xlab="Interval", ylab="Steps") 
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->



