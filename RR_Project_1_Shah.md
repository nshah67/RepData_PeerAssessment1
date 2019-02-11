---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data: 
A table of total numebr of steps by day and the  final histogram of steps by date (with missing values removed) is shown below:


```r
library(ggplot2)
library(plyr)
setwd("/Users/Neha/Documents/Hopkins/Data Science Specialization/Reproducible Research")
data <- read.table(unz("repdata_data_activity.zip", "activity.csv"), header=T, sep=",")
#converting to weekdays and correct date format
data$day <- weekdays(as.Date(data$date))
data$DateTime<- as.POSIXct(data$date, format="%Y-%m-%d")
#removing NAs
clean <- data[!is.na(data$steps),]
sumTable <- aggregate(data$steps ~ data$date, FUN=sum, )
colnames(sumTable)<- c("Date", "Steps")
sumTable
```

```
##          Date Steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## 11 2012-10-13 12426
## 12 2012-10-14 15098
## 13 2012-10-15 10139
## 14 2012-10-16 15084
## 15 2012-10-17 13452
## 16 2012-10-18 10056
## 17 2012-10-19 11829
## 18 2012-10-20 10395
## 19 2012-10-21  8821
## 20 2012-10-22 13460
## 21 2012-10-23  8918
## 22 2012-10-24  8355
## 23 2012-10-25  2492
## 24 2012-10-26  6778
## 25 2012-10-27 10119
## 26 2012-10-28 11458
## 27 2012-10-29  5018
## 28 2012-10-30  9819
## 29 2012-10-31 15414
## 30 2012-11-02 10600
## 31 2012-11-03 10571
## 32 2012-11-05 10439
## 33 2012-11-06  8334
## 34 2012-11-07 12883
## 35 2012-11-08  3219
## 36 2012-11-11 12608
## 37 2012-11-12 10765
## 38 2012-11-13  7336
## 39 2012-11-15    41
## 40 2012-11-16  5441
## 41 2012-11-17 14339
## 42 2012-11-18 15110
## 43 2012-11-19  8841
## 44 2012-11-20  4472
## 45 2012-11-21 12787
## 46 2012-11-22 20427
## 47 2012-11-23 21194
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
```

```r
hist(sumTable$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day")
```

![](RR_Project_1_Shah_files/figure-html/unnamed-chunk-1-1.png)<!-- -->
## What is mean total number of steps taken per day?

```r
as.integer(mean(sumTable$Steps))
```

```
## [1] 10766
```

```r
as.integer(median(sumTable$Steps))
```

```
## [1] 10765
```
The mean number of steps taken is 10766 steps per day, and the median is 10765 steps per day.

## What is the average daily activity pattern?

The timeseries of steps across days is shown below:

```r
intervalTable <- ddply(clean, .(interval), summarize, Avg = mean(steps))
##Create line plot of average number of steps per interval
p <- ggplot(intervalTable, aes(x=interval, y=Avg), xlab = "Interval", ylab="Average Number of Steps")
p + geom_line()+xlab("Interval")+ylab("Number of Steps")+ggtitle("Average Number of Steps per 5 minute Interval")
```

![](RR_Project_1_Shah_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
maxSteps <- max(intervalTable$Avg)
intervalTable[intervalTable$Avg==maxSteps,1]
```

```
## [1] 835
```
One average, The maximum number of steps for a five minute interval is 206 steps. That interval is the 835th interval.

## Imputing missing values

The total number of missing values in the data is 2304

```r
##Number of NAs in original data set
nrow(data[is.na(data$steps),])
```

```
## [1] 2304
```
We aim to impute misisng values using average of the five minute interval based on the day of the week - weekend may have fewer or more steps (depending on the person) than weekdays.

```r
avgTable <- ddply(clean, .(interval, day), summarize, Avg = mean(steps)) #creating average
nadata<- data[is.na(data$steps),] #new dataset
newdata<-merge(nadata, avgTable, by=c("interval", "day")) #merging
newdata2<- newdata[,c(6,4,1,2,5)] #reorder to mathc
colnames(newdata2)<- c("steps", "date", "interval", "day", "DateTime") #same variables
mergeData <- rbind(clean, newdata2) #merge
sumTable2 <- aggregate(mergeData$steps ~ mergeData$date, FUN=sum, )
colnames(sumTable2)<- c("Date", "Steps") # new table
as.integer(mean(sumTable2$Steps)) #checking mean
```

```
## [1] 10821
```

```r
as.integer(median(sumTable2$Steps))
```

```
## [1] 11015
```
The mean of the imputed data is 10821 steps per day and the median is 11015 steps per day. 

The histograms comparing the two datasets is shown below:

```r
hist(sumTable2$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with Imputation", 
     col="Black") 
```

![](RR_Project_1_Shah_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
hist(sumTable$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with missing values removed",col="Grey") 
```

![](RR_Project_1_Shah_files/figure-html/unnamed-chunk-6-2.png)<!-- -->

The shapes of the distributions are similar, but the means have slightly increased, from 10766 to 10821, and the medians have increased much more, from 10765 to 11015, in the imputed data.

## Are there differences in activity patterns between weekdays and weekends?
The timeseries comparing weekdays and weekends is below:

```r
mergeData$DayCategory <- ifelse(mergeData$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
library(lattice) 
## Summarize data by interval and type of day
intervalTable2 <- ddply(mergeData, .(interval, DayCategory), summarize, Avg = mean(steps))

##Plot data in a panel plot
xyplot(Avg~interval|DayCategory, data=intervalTable2, type="l",  layout = c(1,2),
       main="Average steps per interval based on week or weekend day", 
       ylab="Average Number of Steps", xlab="Interval")
```

![](RR_Project_1_Shah_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

The user tended to spend more time walking around throughout the day on weekends; however, the same person spent much less time walking around later in the dat during the week after an intial spurt of walking activity.
