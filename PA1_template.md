---
title: "Reproducible Research: Peer Assessment 1"
author: "Ehson Ghandehari"
date: "October 7, 2015"
output: html_document
keep_md: true
---

```{r setoptions, echo=FALSE}
# Setting global options, to provide the actual code creating the output.
knitr::opts_chunk$set(echo=TRUE, cache=TRUE, result='asis')

```

```{r packgae_loading, warning=FALSE}
# loading the necessary packages
library(plyr)
library(ggplot2)
library(magrittr)
library(lattice)
```

## Loading and preprocessing the data

```{r loading}

# reading the data and saving it in "data" object.
data<-read.csv("/Users/ehson/Documents/Coursera/reproducable_research/RepData_PeerAssessment1/activity.csv")

# Change the format of the data from factor to Date.
data$date<-as.Date(data$date,"%Y-%m-%d")

```


## What is mean total number of steps taken per day?

```{r calculation}

# Calculating the total steps taken for each data and saves it into a. The "na.rm=TRUE" argument, ignores the NA values in calculation.
# the first 6 rows are presented.
a<-ddply(data,.(date),summarize,total_steps=sum(steps,na.rm=TRUE))
head(a)

# Presenting the total steps in histogram and barplot for comparison.
par(mfcol=c(1,2))
barplot(a$total_steps, xlab="Distribution of two months",ylab="Total number of steps per day",ylim=c(0,25000))
hist(a$total_steps,xlab="Total number of steps per day",ylim=c(0,20), breaks=10,main=NULL)

# Calculating mean 
mean(data$steps, na.rm=TRUE)

#Calculating the median
median(data$steps,na.rm=TRUE)

```


## What is the average daily activity pattern?

```{r activity}

# Averaging steps for each day
day_base<-ddply(data,.(interval),summarize,mean=mean(steps,na.rm=TRUE))
data<-join(data,day_base)

# taken across all days (y-axis) vs. 5-minute interval (x-axis). 
ggplot(data,aes(x=interval,y=mean))+geom_line()+theme_classic()+labs(y= "All days average steps, 5-min Intervals" , x="5-min intervals")

# finding the maximum 5-min interval
unique(data$interval[data$mean==max(data$mean,na.rm=TRUE)])

```

## Imputing missing values

```{r NA_remove}
# Finding the number of rows having NA. There are total of 2304 NA values.
summary(data$steps)

# make a replicate of the original data.
data2<-data

# Replacing the NA values with mean values.
data2$steps[is.na(data2$steps)]<-data2$mean[is.na(data2$steps)]

# Calculating mean after eliminating NA values.
mean(data2$steps)

#Calculating the median after eliminating NA values.
median(data2$steps)

# histogram of new data.
par(mfcol=c(1,2))
b<-ddply(data2,.(date),summarize,total_steps=sum(steps,na.rm=TRUE))
hist(a$total_steps,xlab="Total number of steps per day",ylim=c(0,30), breaks=10,main="Brefore imputing NAs")
hist(b$total_steps,breaks=10,xlab="Total number of steps per day",ylim=c(0,30),main="After Imputing NAs")

```

**Note:** Imputing the missing values made the data distribution more normalized.

## Are there differences in activity patterns between weekdays and weekends?

```{r split.week.weekend}

#Splitting the data into weekend and weekdays
c<-ddply(data2,.(date),summarize,type=factor(weekdays(date)))
levels(c$type)[levels(c$type) %in% c("Monday","Tuesday","Wednesday","Thursday","Friday")]<-"Weekday"
levels(c$type)[levels(c$type) %in% c("Saturday","Sunday")]<-"Weekend"
data2<-merge(data2,c)
d<-ddply(data2,.(interval,type),summarize,mean=mean(steps))

with(d,xyplot(mean~interval|type,type="l",layout=c(1,2),xlab="5-min Intervals",ylab="Number of steps"))

```
