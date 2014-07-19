# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
setwd("~/Git/datascience/RepData_PeerAssessment1")
act <- read.csv("./activity.csv")
str(act)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(act$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##     0.0     0.0     0.0    37.4    12.0   806.0    2304
```

```r
sum(is.na(act))
```

```
## [1] 2304
```

```r
sum(is.na(act$steps))
```

```
## [1] 2304
```

```r
act.nona <- na.omit(act)
str(act.nona)
```

```
## 'data.frame':	15264 obs. of  3 variables:
##  $ steps   : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  - attr(*, "na.action")=Class 'omit'  Named int [1:2304] 1 2 3 4 5 6 7 8 9 10 ...
##   .. ..- attr(*, "names")= chr [1:2304] "1" "2" "3" "4" ...
```

```r
sum(is.na(act.nona))
```

```
## [1] 0
```

## What is mean total number of steps taken per day?


```r
act.day <- aggregate(act.nona$steps, by=list(act.nona$date),FUN=sum)
str(act.day)
```

```
## 'data.frame':	53 obs. of  2 variables:
##  $ Group.1: Factor w/ 61 levels "2012-10-01","2012-10-02",..: 2 3 4 5 6 7 9 10 11 12 ...
##  $ x      : int  126 11352 12116 13294 15420 11015 12811 9900 10304 17382 ...
```

```r
summary(act.day$x)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8840   10800   10800   13300   21200
```

```r
hist(act.day$x,xlab="Steps",main= "Number of Steps Summed by Day")
```

![plot of chunk unnamed-chunk-2](./PA1_template_files/figure-html/unnamed-chunk-2.png) 

```r
mean(act.day$x)
```

```
## [1] 10766
```

```r
median(act.day$x)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
act.avg <- aggregate(act.nona$steps, by=list(as.factor(act.nona$interval)),mean)
str(act.avg)
```

```
## 'data.frame':	288 obs. of  2 variables:
##  $ Group.1: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
##  $ x      : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
```

```r
act.avg$interval <- act.nona$interval[0:288]
par(mfrow = c(1,1))
plot(act.avg$interval,act.avg$x, type="l",xlab="Number of Steps",ylab="Average Number of Steps")
```

![plot of chunk unnamed-chunk-3](./PA1_template_files/figure-html/unnamed-chunk-3.png) 

## Imputing missing values

For each interval with a missing value a new value will be imputed by taking the 
average over all non-missing values for that interval.  This is accomplished though
the following function.

```r
imput <- function(active) {
        na.loc <- which(is.na(active$steps))
        na.mean <- c()

        for (ndx in 1:length(na.loc)){
             na.mean <- c(na.mean,
                          mean(
                                active[which(active$interval 
                                             == active$interval[na.loc[ndx]]),]$steps,
                                na.rm=T)
                        )
             active$steps[na.loc[ndx]] = na.mean[ndx]
        }
        return(active)
}
```


```r
act.imput <- imput(act)
sum(is.na(act.imput))
```

```
## [1] 0
```

```r
str(act.imput)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(act.imput$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     0.0     0.0     0.0    37.4    27.0   806.0
```

And computing the mean and median:


```r
act.imput.day <- aggregate(act.imput$steps,by=list(act.imput$date),FUN=sum)
hist(act.imput.day$x,xlab="Steps",main= "Number of Steps Summed by Day")
```

![plot of chunk unnamed-chunk-6](./PA1_template_files/figure-html/unnamed-chunk-6.png) 

The mean is:


```r
mean(act.imput.day$x)
```

```
## [1] 10766
```

And the median is:


```r
median(act.imput.day$x)
```

```
## [1] 10766
```

## Are there differences in activity patterns between weekdays and weekends?


```r
date <- strptime(act.imput$date,"%Y-%m-%d")
daysofweek <- weekdays(date)
weekends <- daysofweek == "Saturday" | daysofweek == "Sunday"
act.imput$dayofweek <- "weekday"
act.imput[weekends,]$dayofweek <- "weekend"
str(act.imput)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps    : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date     : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval : int  0 5 10 15 20 25 30 35 40 45 ...
##  $ dayofweek: chr  "weekday" "weekday" "weekday" "weekday" ...
```

```r
act.weekday <- act.imput[act.imput$dayofweek== "weekend",]
act.weekend <- act.imput[act.imput$dayofweek == "weekday",]
sum(act.imput$dayofweek == "weekday")
```

```
## [1] 12960
```

```r
sum(act.imput$dayofweek == "weekend")
```

```
## [1] 4608
```

```r
act.wkdaypat <- aggregate(act.weekday$steps, by=list(as.factor(act.weekday$interval)),
                          mean)
act.wkdaypat$interval <- act.imput$interval[0:288]
act.wkendpat <- aggregate(act.weekend$steps, by=list(as.factor(act.weekend$interval)),
                          mean)
act.wkendpat$interval <- act.imput$interval[0:288]
par(mfrow = c(2,1))
plot(act.wkendpat$interval, act.wkendpat$x,type="l", main="Weekend",xlab="Interval",ylab ="Steps")
plot(act.wkdaypat$interval, act.wkdaypat$x, type="l",main="Weekday",xlab="Interval",ylab="Steps")
```

![plot of chunk unnamed-chunk-9](./PA1_template_files/figure-html/unnamed-chunk-9.png) 
