---
title: "Reproducible Research Project 1"
author: "Daisy Njeri Njuguna"
date: "April 27, 2017"
output: html_document
---


## Introduction  
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement -- a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.



## Downloading and Reading the Activity Monitoring Data

```r
setwd("E:/M/coursera/data_science_specialization/Rstudio projects/Reproducible Research")

#Downloading the Data
if(!file.exists("./course5")){dir.create("./course5")}
download.file(url='https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip', destfile='repdata.zip', method='auto')
unzip(zipfile='repdata.zip')

#Reading the data
dat <- read.csv("activity.csv", header = TRUE)
dim(dat)
```

```
## [1] 17568     3
```

```r
head(dat)
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

```r
str(dat)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(dat)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

### What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

Calculate the total number of steps taken per day
If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
Calculate and report the mean and median of the total number of steps taken per day


```r
sumStepsPerDay <- tapply(dat$steps, dat$date, sum, na.rm = T)
hist(sumStepsPerDay, breaks = 61, main = "Histogram (sum of steps per Day)", col = "lavenderblush2")
abline(v = c(mean(sumStepsPerDay),median(sumStepsPerDay)), col = c("darkmagenta","green3"), lwd = 3)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

```r
mean(sumStepsPerDay); median(sumStepsPerDay)
```

```
## [1] 9354.23
```

```
## [1] 10395
```

###What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
meanStepsPerInterval <- tapply(dat$steps, dat$interval, mean, na.rm = T)
daysTime <- as.numeric(names(meanStepsPerInterval))/length(unique(dat$date))
plot(daysTime,meanStepsPerInterval, type = "l", axes=T,
     xlab="5 mins Interval", ylab="average steps in 5-min interval",
     main="Daily Average Activity Pattern")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

### Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
Create a new dataset that is equal to the original dataset but with the missing data filled in.
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps? 


```r
sum(is.na(dat))
```

```
## [1] 2304
```

```r
library(mice)
md.pattern(dat)
```

```
##       date interval steps     
## 15264    1        1     1    0
##  2304    1        1     0    1
##          0        0  2304 2304
```

```r
imputed_Data <- mice(dat, m=3, maxit = 10, method = 'pmm', seed = 2)
```

```
## 
##  iter imp variable
##   1   1  steps
##   1   2  steps
##   1   3  steps
##   2   1  steps
##   2   2  steps
##   2   3  steps
##   3   1  steps
##   3   2  steps
##   3   3  steps
##   4   1  steps
##   4   2  steps
##   4   3  steps
##   5   1  steps
##   5   2  steps
##   5   3  steps
##   6   1  steps
##   6   2  steps
##   6   3  steps
##   7   1  steps
##   7   2  steps
##   7   3  steps
##   8   1  steps
##   8   2  steps
##   8   3  steps
##   9   1  steps
##   9   2  steps
##   9   3  steps
##   10   1  steps
##   10   2  steps
##   10   3  steps
```

```r
summary(imputed_Data)
```

```
## Multiply imputed data set
## Call:
## mice(data = dat, m = 3, method = "pmm", maxit = 10, seed = 2)
## Number of multiple imputations:  3
## Missing cells per column:
##    steps     date interval 
##     2304        0        0 
## Imputation methods:
##    steps     date interval 
##    "pmm"    "pmm"    "pmm" 
## VisitSequence:
## steps 
##     1 
## PredictorMatrix:
##          steps date interval
## steps        0    1        1
## date         0    0        0
## interval     0    0        0
## Random generator seed value:  2
```

```r
New_dat <- complete(imputed_Data,3)
mean(dat$steps, na.rm = T)
```

```
## [1] 37.3826
```

```r
mean(New_dat$steps)
```

```
## [1] 37.88616
```

```r
summary(New_dat)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.89   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 14.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##                   (Other)   :15840
```

```r
NewsumStepsPerDay <- tapply(New_dat$steps, New_dat$date, sum)

NewmeanStepsPerDay <- mean(NewsumStepsPerDay)
NewmedianStepsPerDay <- median(NewsumStepsPerDay)

hist(NewsumStepsPerDay, breaks = 61, main = "Histogram (Imputed Data)")
abline(v = c(NewmeanStepsPerDay,NewmedianStepsPerDay), col = c("red", "forestgreen"), lwd = 2)
abline(v =  c(mean(sumStepsPerDay), median(sumStepsPerDay)), col = c("magenta", "navyblue"), lwd = 2)
text(8000,3.5,"MeanStepsPerDay", col = "magenta")
text(11000,4.5,"MedianStepsPerDay", col = "navyblue")
text(13000,5.5,"NewMeanStepsPerDay", col = "red")
text(14000,6.5,"NewMedianStepsPerDay", col = "forestgreen")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)
In the histogram above it is clear that imputing missing data has an impact. The mean before imputing data is 9354.2295082 and after imputing the mean changes to 1.0911213 &times; 10<sup>4</sup>

### Are there differences in activity patterns between weekdays and weekends?


```r
New_dat$Day <- NA
New_dat$Day <- weekdays(as.Date(New_dat$date))
New_dat$Day <- ifelse(New_dat$Day == "Sunday", "Weekend",
                      ifelse(New_dat$Day == "Saturday", "Weekend","Weekday"))

New_dat$Day <- as.factor(New_dat$Day)

New_steps <- aggregate(New_dat$steps, by=list(interval=New_dat$interval, weekday=New_dat$Day), mean)

library(ggplot2)
weekPlot <- ggplot(New_steps, aes(interval/60, x))
weekPlot + geom_line() + facet_grid(weekday ~ .) +
    theme_bw() +
    labs(y="average no. of steps per 5-min interval") +
    labs(x="day time (hrs)") +
    labs(title="Daily activity pattern")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)
### 


