---
        title: "Reproducible Research: Peer Assessment 1"
output: 
        html_document:
        keep_md: true
---
        
        ## Loading and preprocessing the data
        This process assumes that the zip-file is located in the working directory of R
```{r unzipandload,echo=TRUE}
data <- read.csv(unzip("activity.zip","activity.csv"))
```

## What is mean total number of steps taken per day?
To calculate this we first create a vector with number of steps totalled per day
```{r meanandmedian,echo=TRUE}
stepspd <- tapply(data$steps,data$date,FUN=sum)
```
Below is a histogram of the total number of steps taken per day
```{r histogram,fig.height=4,echo=TRUE}
par(mar=c(5,4,1,1),las=1)
hist(stepspd,main="histogram of steps per day",xlab="Steps per day")
```
```{r meanmedian,echo=TRUE}
meansteps <- as.integer(mean(stepspd,na.rm=TRUE))
mediansteps <- as.integer(mean(stepspd,na.rm=TRUE))
```

The mean of the number of steps per day is `r meansteps`
The median of the number of steps per day is `r mediansteps`

## What is the average daily activity pattern?

First we need to calculate the average nummber of steps per interval across all days and add the interval identifier, then combine them into a table and plot it.

```{r pattern,echo=TRUE}
avgstepspint <- tapply(data$steps,data$interval,FUN=mean,na.rm=TRUE)
Interv  <- unique(data$interval)
meanstepspint <- as.data.frame(cbind(avgstepspint,Interv))
par(mar=c(5,4,2,1),las=1)
plot(Interv,avgstepspint,type="l",main="Average activity pattern",xlab="Interval",ylab="Avg. nmbr of steps per interval")
```

To determine the interval with the maximum average number of steps, this code is used:
        ```{r maxi,echo-TRUE}
maxint <- which.max(avgstepspint)
interval <- Interv[maxint]
```

The interval which contains the maximum number of steps is `r interval`

## Imputing missing values
```{r missing,echo=TRUE}
incomplete <- data[!complete.cases(data),]
nbrmissing <- nrow(incomplete)
```
The number of observations with missing data is `r nbrmissing`

A new data set "data2"is created which has its missing values imputed.
Missing values for steps per interval are replaced with the mean number of steps for that particular interval across all days.

```{r impute,echo=TRUE}
data2 <- data
for(i in 1:nrow(data2)){
        if(is.na(data2$steps[i])){
                data2$steps[i] <- meanstepspint$avgstepspint[match(data2$interval[i],meanstepspint$Interv)]
        }
}
rm(i)
```

To see if there is a difference between the "raw" data and the data with imputed values for NA's, we calculate the mean, median and histogram for the imputed data set.

```{r meanandmedian2,echo=TRUE}
stepspd2 <- tapply(data2$steps,data2$date,FUN=sum)
meansteps2 <- mean(stepspd2,na.rm=TRUE)
mediansteps2 <- median(stepspd2,na.rm=TRUE)
```
Below is a histogram of the total number of steps taken per day
```{r histogram2,fig.height=4,echo=TRUE}
par(mar=c(5,4,1,1),las=1)
hist(stepspd2,main="histogram of steps per day imputed",xlab="steps per day")
```

```{r numbers,echo=TRUE}
meansteps2 <- as.integer(mean(stepspd2,na.rm=TRUE))
mediansteps2 <- as.integer(median(stepspd2,na.rm=TRUE))
```

The mean of the number of steps per day after imputation is `r meansteps2`
The median of the number of steps per day after imputation is `r mediansteps2`

The difference of mean, median and histogram of number of steps before and after imputation of NA values of "steps" can be neglected, therefore imputing data for missing values in this case does not have any impact. 

## Are there differences in activity patterns between weekdays and weekends?
To see a difference between weekdays and weekend, we must first transform the "date" column of data2 to a "date" class and then add a factor column indicating weekday or weekend per observation.

```{r weekday,echo=TRUE}
data2$date <- as.Date(data2$date)
data2$day <- as.factor(weekdays(data2$date))
data2$weekdays <- as.factor(data2$weekdays)

for(i in 1:nrow(data2)){
if(data2$day[i] =="maandag"){data2$weekdays[i] <- "weekday"}
else if (data2$day[i]=="dinsdag"){data2$weekdays[i] <- "weekday"}
else if (data2$day[i]=="woensdag"){data2$weekdays[i] <- "weekday"}
else if (data2$day[i]=="donderdag"){data2$weekdays[i] <- "weekday"}
else if (data2$day[i]=="vrijdag"){data2$weekdays[i] <- "weekday"}
else {data$weekdays[i] <- "weekend"}
}
rm(i)
```

