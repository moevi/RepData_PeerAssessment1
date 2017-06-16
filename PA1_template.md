# Reproducible Research: Peer Assessment 1
Kpakpo Moevi  
June 6, 2017  


## Loading and preprocessing the data
#1.Code for reading in the dataset and/or processing the data
Load the data 

```r
if (!file.exists("activity.csv")) {
  activity<-  unzip("activity.zip")
}
activity_moni_data <- read.csv("activity.csv")
```
Process the data

```r
 activity_moni_data$date<-as.Date(activity_moni_data$date)
 activity_moni_data$interval<-as.factor(activity_moni_data$interval)
 head(activity_moni_data$date)
```

```
## [1] "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01"
## [6] "2012-10-01"
```

```r
 head(activity_moni_data$interval)
```

```
## [1] 0  5  10 15 20 25
## 288 Levels: 0 5 10 15 20 25 30 35 40 45 50 55 100 105 110 115 120 ... 2355
```
 Load package plyr



```r
library(plyr)
number_of_steps_per_day<-ddply(activity_moni_data,.(date),summarise,sum = sum(steps,na.rm = TRUE))
head(number_of_steps_per_day)
```

```
##         date   sum
## 1 2012-10-01     0
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

```r
#2.Histogram of the total number of steps taken each day
```

```r
hist(number_of_steps_per_day$sum,col = "blue",main = "Total number of steps taken each day Oct-Nov 2012")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


## What is mean total number of steps taken per day?
#3.Mean and median number of steps taken each day


```r
TheMean = mean(number_of_steps_per_day$sum)
TheMedian = median(number_of_steps_per_day$sum)
print(TheMean)
```

```
## [1] 9354.23
```

```r
print(TheMedian)
```

```
## [1] 10395
```


## What is the average daily activity pattern?

#4.Time series plot of the average number of steps taken


```r
library(ggplot2)
number_of_steps_per_interval<-ddply(activity_moni_data,.(interval),summarise,sum = sum(steps,na.rm = TRUE))
head(number_of_steps_per_interval)
```

```
##   interval sum
## 1        0  91
## 2        5  18
## 3       10   7
## 4       15   8
## 5       20   4
## 6       25 111
```

```r
g<-ggplot(number_of_steps_per_interval,aes(x=interval,y = sum,group =1))
g+geom_line()+ 
xlab(" Interval") + 
ylab("Average Number of Steps") +
ggtitle("Time series plot of the average number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

#5.The 5-minute interval that, on average, contains the maximum number of steps

```r
number_of_steps_per_interval[which(number_of_steps_per_interval$sum==(max(number_of_steps_per_interval$sum))),]
```

```
##     interval   sum
## 104      835 10927
```

## Imputing missing values
#6.Code to describe and show a strategy for imputing missing data


Lets count the misssing values


```r
number_missing_values<-sum(is.na(activity_moni_data))
number_missing_values
```

```
## [1] 2304
```
The number of missing values in the dataset is equal to the number of missing value in the steps which is 2304.
The technique is to replace the missing values by the mean

```r
na_position<-which(is.na(activity_moni_data$steps))
mean_vec <- rep(mean(activity_moni_data$steps, na.rm=TRUE), times=length(na_position))
```

```r
activity_moni_data[na_position,"steps"]<-mean_vec
head(activity_moni_data)
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
#7.Histogram of the total number of steps taken each day after missing values are imputed


```r
Compl_activity_moni_data<-ddply(activity_moni_data,.(date),summarise,sum = sum(steps))
hist(Compl_activity_moni_data$sum,col = "black", main = "Total number of steps taken each day after missing values are imputed")
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->



## Are there differences in activity patterns between weekdays and weekends?
#8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
We need to transform the data in order to integrate the weekdays and weekends 

```r
act1<-read.csv("activity.csv")
act1$date<-as.POSIXct(act1$date,format = "%Y-%m-%d")
```
Let's compute the weekday from the date attributte

```r
act1<-data.frame(date = act1$date,
                 weekday =  tolower(weekdays(act1$date)),
                 steps = act1$steps,
                 interval= act1$interval)
head(act1)
```

```
##         date weekday steps interval
## 1 2012-10-01  monday    NA        0
## 2 2012-10-01  monday    NA        5
## 3 2012-10-01  monday    NA       10
## 4 2012-10-01  monday    NA       15
## 5 2012-10-01  monday    NA       20
## 6 2012-10-01  monday    NA       25
```
Now lets identify whether the day is weekday or weekend


```r
act1<-cbind(act1,datetype=ifelse(act1$weekday=="saturday" | act1$weekday == "sunday","weekend","weekday"))
head(act1)
```

```
##         date weekday steps interval datetype
## 1 2012-10-01  monday    NA        0  weekday
## 2 2012-10-01  monday    NA        5  weekday
## 3 2012-10-01  monday    NA       10  weekday
## 4 2012-10-01  monday    NA       15  weekday
## 5 2012-10-01  monday    NA       20  weekday
## 6 2012-10-01  monday    NA       25  weekday
```


```r
act1<-data.frame(date=act1$date,
                     weekday = act1$weekday, 
                     datetype=act1$datetype,
                     steps = act1$steps,
                     interval = act1$interval)
head(act1)
```

```
##         date weekday datetype steps interval
## 1 2012-10-01  monday  weekday    NA        0
## 2 2012-10-01  monday  weekday    NA        5
## 3 2012-10-01  monday  weekday    NA       10
## 4 2012-10-01  monday  weekday    NA       15
## 5 2012-10-01  monday  weekday    NA       20
## 6 2012-10-01  monday  weekday    NA       25
```

```r
na_position<-which(is.na(act1$steps))
mean_vec2 <- rep(mean(act1$steps, na.rm=TRUE), times=length(na_position))
act1[na_position,"steps"]<-mean_vec2
head(act1)
```

```
##         date weekday datetype   steps interval
## 1 2012-10-01  monday  weekday 37.3826        0
## 2 2012-10-01  monday  weekday 37.3826        5
## 3 2012-10-01  monday  weekday 37.3826       10
## 4 2012-10-01  monday  weekday 37.3826       15
## 5 2012-10-01  monday  weekday 37.3826       20
## 6 2012-10-01  monday  weekday 37.3826       25
```

```r
library(lattice)
mean_data<-aggregate(act1$steps, 
                       by=list(act1$datetype, 
                               act1$weekday,
                               act1$interval),FUN= mean)
names(mean_data)<-c("datetype", "weekday", "interval", "mean")
head(mean_data)
```

```
##   datetype  weekday interval     mean
## 1  weekday   friday        0 8.307244
## 2  weekday   monday        0 9.418355
## 3  weekend saturday        0 4.672825
## 4  weekend   sunday        0 4.672825
## 5  weekday thursday        0 9.375844
## 6  weekday  tuesday        0 0.000000
```

```r
library(lattice)
xyplot(mean ~ interval|datetype,mean_data,type ="l",lwd=1, xlab="Interval",ylab="Number of steps", layout=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-24-1.png)<!-- -->

