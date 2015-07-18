# Reproducible Research: Peer Assessment 1

```r
require(knitr)
```

```
## Loading required package: knitr
```

```
## Warning: package 'knitr' was built under R version 3.2.1
```

```r
require(dplyr)
```

```
## Loading required package: dplyr
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
require(ggplot2)
```

```
## Loading required package: ggplot2
```

```
## Warning: package 'ggplot2' was built under R version 3.2.1
```

```r
opts_chunk$set(echo=TRUE)
```

## Loading and preprocessing the data
First, the data needs to be read into R using the read.csv function (as a note, you'll already need to be in the directory where the activity.csv file is stored):

```r
data<-read.csv(unz("activity.zip", "activity.csv"),colClasses=c("numeric","character","numeric"))
```

Then, the date variable needs to be converted from a character vector to a Date vector.

```r
data$date<-as.Date(data$date, "%Y-%m-%d")
```

## What is mean total number of steps taken per day?
To answer this question, the data needs to first be grouped on the date variable and then the average taken of the number of steps on each day.

```r
steps<-data%>%group_by(date)%>%summarise(total_steps=sum(steps))
```
Then a histogram needs to be built, in this case with the ggplot2 system.

```r
steps.plot<-ggplot(steps,aes(total_steps))+geom_histogram()+
    labs(main="Total Steps per Day")+scale_y_continuous(breaks=seq(0,10,2))+
    xlab("Daily Step Totals")+ylab("Count")
print(steps.plot)
```

![](PA1_template_files/figure-html/stepsplot-1.png) 
Finally, we'll use the data set we created above to check the overall mean and standard deviation for the number of steps taken.

```r
head(summarize(steps,mean=mean(total_steps),sd=sd(total_steps)))
```

```
## Source: local data frame [1 x 2]
## 
##   mean  sd
## 1   NA NaN
```

## What is the average daily activity pattern?
activity<-data%>%group_by(interval)%>%
    summarize(average_steps=mean(steps,na.rm=TRUE))
activ.plot<-ggplot(activity,aes(interval,average_steps))+geom_line()+
    labs(main="Average Steps per Time Interval")+xlab("Interval")+
    ylab("Step Average")

filter(activity,average_steps==max(average_steps))


## Imputing missing values
ok<-complete.cases(data)
sum(!ok)
missing<-mutate(data, steps = replace(data$steps, is.na(data$steps), 
                                         activity$average_steps))
miss<-missing%>%group_by(date)%>%summarise(total_steps=sum(steps))
miss.plot<-ggplot(miss,aes(total_steps))+geom_histogram()+
    labs(main="Total Steps per Day")+scale_y_continuous(breaks=seq(0,10,2))+
    xlab("Daily Step Totals")+ylab("Count")
print(miss.plot)

head(summarize(miss,mean=mean(total_steps),sd=sd(total_steps)))


## Are there differences in activity patterns between weekdays and weekends?
weekend<-c("Saturday","Sunday")
data$weekday<-factor((weekdays(data$date) %in% weekend),levels=c("FALSE","TRUE"),
                     labels=c("Weekday","Weekend"))
day<-data%>%group_by(weekday,interval)%>%
    summarize(average_steps=mean(steps,na.rm=TRUE))
day.plot<-ggplot(day,aes(interval,average_steps))+geom_line()+
    labs(main="Average Steps per Time Interval")+xlab("Interval")+
    ylab("Step Average")+facet_grid(weekday~.)
print(day.plot)
