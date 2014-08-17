---
title: "Peer Assignment 1 - Reproducible Research"
output: html_document
---

First, load the necessary libraries and read in the data.

```r
suppressMessages(library(lattice))
suppressMessages(library(plyr))
suppressMessages(library(reshape))
activity<-read.csv("/Users/bwildunc/Desktop/R/activity.csv")
```

Next sum the steps in each day and plot a histogram of these totals.

```r
totals <- ddply(activity, .(date), summarize, Total=sum(steps)) ## sum of steps in each day
totals1<-na.omit(totals) ## get rid of NAs so I can plot and take median and mean
hist(totals1$Total) ## histogram 
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

Find the mean and median of steps taken per day.

```r
mean(totals1$Total) ## mean
```

```
## [1] 10766
```

```r
median(totals1$Total) ## median
```

```
## [1] 10765
```

Time series plot showing average number of steps at each interval

```r
interval_avg<-ddply(activity, .(interval), summarize, Mean=mean(steps,na.rm=T)) ## takes the average # of steps at each interval
plot(Mean~interval,data=interval_avg,type='l', main="Avg steps throughout the day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

Which interval has the highest average number of steps?

```r
interval_avg[which.max(interval_avg$Mean),] ## shows which interval has the greatest number of steps
```

```
##     interval  Mean
## 104      835 206.2
```

Counts the NAs in the dataset

```r
table(is.na(activity$steps)) ## shows how many NAs there are
```

```
## 
## FALSE  TRUE 
## 15264  2304
```

Replaces NAs with the mean value for each interval

```r
f<- function (interval){
    mean(activity$steps[activity$interval==interval],na.rm=T)}
for (i in 1:dim(activity)[1]){
    if (is.na(activity$steps[i])) 
      activity$steps[i]=f(activity$interval[i])}
```

Histogram of daily steps new dataset.

```r
totals2 <- ddply(activity, .(date), summarize, Total=sum(steps)) ## sum of steps in each day
hist(totals2$Total) ## histogram 
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

Daily mean and median of new dataset.

```r
mean(totals2$Total) ## mean
```

```
## [1] 10766
```

```r
median(totals2$Total) ## median
```

```
## [1] 10766
```

Adds in a factor variable called weekend to the dataset

```r
activity$weekend<-weekdays(as.Date(as.character(activity$date))) %in% c('Sunday','Saturday') ## create var called weekend that is either F or T
activity$weekend<-factor(activity$weekend,labels=c("weekday","weekend")) ## converting variable to factor
```

Manipulates mean steps by time interval, creating seperate data for weekdays and weekends

```r
activity_weekday<-subset(activity,weekend=="weekday")
activity_weekend<-subset(activity,weekend=="weekend")
interval_avg_WD<-ddply(activity_weekday, .(interval), summarize, Mean=mean(steps,na.rm=T))
interval_avg_WE<-ddply(activity_weekend, .(interval), summarize, Mean=mean(steps,na.rm=T))
```

Time series plot by weekday and weekend

```r
par(mfrow=c(2,1), mar=c(2,4,2,2),oma=c(2,2,2,1)) ## setting up the paramaters for creating multiple plots
plot(Mean~interval,data=interval_avg_WD,type='l', main="weekday",cex.main=.6) ##weekday plot
plot(Mean~interval,data=interval_avg_WE,type='l', main="weekend",cex.main=.6) ##weekend plot
mtext("Weekday vs. weekend", outer=T)  ## adding main title
mtext("time of day",outer=T,side=1,cex=.8)  ## adding label at the bottom of the plot
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 
