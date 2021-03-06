# Reproducible Research: Peer Assessment 1

From the README, we include a review of the data set.   

The variables in this dataset are:  
- **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)  
- **date**: The date on which the measurement was taken in YYYY-MM-DD
    format  
- **interval**: Identifier for the 5-minute interval in which
    measurement was taken  

The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this
dataset.

## Loading and preprocessing the data

The working directory is set to that which contains the data file, and the data is read into the data frame **act**. We use the *str* command to get a brief view of the data as a whole and based on that output we use *summary* to get an overview of the **steps** data. 


```r
library(lattice)
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
We count the missing data to insure that only **steps** has missing values.

```r
sum(is.na(act))
```

```
## [1] 2304
```
We complete the premliminary processing by removing the NA values. As we will want to use the orignial data frame **act** later in the analysis, we construct a new data frame **act.nona**. We call *str* on the new data frame and check to see that sum of the number of observations in **act.nona** and the number of NAs sum to the number of observations in the **act** data frame. We also check to insure that **act.nona** does not contain any NAs.

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

Note that in **act** and **act.nona** a day's data is collected into 288 5 minute intervals which are represented by a sequence of *int's* from 0 to 2350. Each number in this sequence corresponds to an hour and a minute based on a 24 hour clock, i.e., 2355 denotes 5 minutes to midnight.

In order to compute the mean of the steps on per day basis, we use the *aggregate* function.  The *aggregate* function returns a data frame **act.day** containing two observations. One is a factor of the days in the study; the other is the sum of the steps for that day. We look at *str* and *summary* for **act.day**. 

Using the base plotting system, we construct a histogram where the number of steps appears on the x-axis and the frequency of the steps along the y-axis. 

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

![plot of chunk unnamed-chunk-4](./PA1_template_files/figure-html/unnamed-chunk-4.png) 

The mean of the total number of steps taken per day is

```r
mean(act.day$x)
```

```
## [1] 10766
```
and the median of the total number of steps taken per day is

```r
median(act.day$x)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

In order to make a time series plot of the 5 minute interval which averages of the number of steps taken in that 5 minute interval over the days in the study, we again use the *aggregate* function. In this instance we aggregate the steps on the intervals. The resulting data frame is **act.avg** and is summarized by a call to *str*. We add an interval column to aid in ploting. The base plotting system is used.

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

![plot of chunk unnamed-chunk-7](./PA1_template_files/figure-html/unnamed-chunk-7.png) 


The 5 minute interval which contains the maximum number of steps is:

```r
which(act.avg$x == max(act.avg$x))
```

```
## [1] 104
```
## Imputing missing values

For each interval with a missing value a new value will be imputed by taking the 
average over all non-missing values for that interval. For example, for the zeroth time interval, we compute the mean for the zeroth time interval for all days. Note that as we are using the original data fram **act** which does contain NAs those intervals will be excluded in computing the mean.

This is accomplished though the following function, *imput* which takes a data frame with NAs and returns a data frame where the NAs have been replaced by the imputed values.


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

We do a quick check of the imputed data frame **act.imput** to insure that all NAs have been removed and then look at the output from the calls to *str* on **act.imput** and *summary* on **steps**.


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

Again we use the *aggregate* function to compute the number of steps taken per day. The resulting data frame **act.imput.day** is used to contruct a histogram and to compute the mean and median.  The base plotting system is used.


```r
act.imput.day <- aggregate(act.imput$steps,by=list(act.imput$date),FUN=sum)
hist(act.imput.day$x,xlab="Steps",main= "Number of Steps Summed by Day")
```

![plot of chunk unnamed-chunk-11](./PA1_template_files/figure-html/unnamed-chunk-11.png) 

The mean of the total number of steps taken per day is:


```r
mean(act.imput.day$x)
```

```
## [1] 10766
```

And the median of the total number of steps taken per day is:


```r
median(act.imput.day$x)
```

```
## [1] 10766
```

## Are there differences in activity patterns between weekdays and weekends?

In order to view any differences in activity patterns between weekdays and weekends, we must separate weekday data from weekend data. We do this in the following steps using the **act.imput** data frame:  

1. call strptime on the **date** data;
2. use *weekdays* to construct a list of weekday names associated with each date;
3. construct a logical vector that is *TRUE* for a weekend and *FALSE* otherwise;
4. Add a field *dayofweek* to **act.imput** that will be used to denote whether a day is a week day or a day on the weekend (Thanks to Richard McAnay for posting this idea to the discussion list)
5. Two futher data frames are constructed,**act.weekday** and **act.weekend**;
6. The function *aggregate* is call on each of **act.weekday** and ** act.weekend** to calculate the mean for each interval.
7. An additional field is added to **act.weekday** and **act.weekend** to aid in plotting.
8. Weekend and weekday activities are then combined and plotted using the lattice plotting system.


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
act.wkdaypat$dayofweek <- "weekday"
act.wkendpat <- aggregate(act.weekend$steps, by=list(as.factor(act.weekend$interval)),
                          mean)
act.wkendpat$interval <- act.imput$interval[0:288]
act.wkendpat$dayofweek <- "weekend"
act.days <- rbind(act.wkdaypat,act.wkendpat)
tplot <- xyplot(x~interval|dayofweek,data=act.days,type="l",layout=c(1,2),ylab="steps")
print(tplot)
```

![plot of chunk unnamed-chunk-14](./PA1_template_files/figure-html/unnamed-chunk-14.png) 

As this data was collected during October and November, we see that activity is a bit lower in the afternoon on the weekends when the Seahawks are on TV.
