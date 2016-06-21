# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
if(!file.exists('activity.csv')){
        download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip")
        unzip('activeity.zip')
}
activity.data <- read.csv("activity.csv")
```

Load Libraries


```r
library(Hmisc)
```

```
## Loading required package: lattice
```

```
## Loading required package: survival
```

```
## Loading required package: Formula
```

```
## Loading required package: ggplot2
```

```
## 
## Attaching package: 'Hmisc'
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
```

```r
library(ggplot2)
```

## What is mean total number of steps taken per day?

Create daily total steps


```r
Total.Steps <- aggregate(activity.data$steps, by = list(activity.data$date), FUN = "sum", na.rm = TRUE)
```

Plot Histogram of the total steps taken each day


```r
hist(Total.Steps$x, main = "Histogram of Total Steps Per Day", xlab = "Total Steps Per Day", col = "blue")
```

![](PA1_Assignment1_files/figure-html/Hist.Steps.Per.Day-1.png)<!-- -->

Mean total steps per day


```r
mean(Total.Steps$x)
```

```
## [1] 9354.23
```

Median total steps per day


```r
median(Total.Steps$x)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

Daily Activity Pattern
  (This plot breaks when knitted)


```r
Mean.Interval.Steps <- aggregate(activity.data$steps, by = list(activity.data$interval), FUN = "mean", na.rm = TRUE)
plot(Mean.Interval.Steps$Group.1, Mean.Interval.Steps$x, type = "l", col = "blue", lwd = 2, main = "Time Series: Average Steps", xlab = "5 Minute Intervals", ylab = "Average Steps")
```

![](PA1_Assignment1_files/figure-html/Interval.Steps-1.png)<!-- -->

Interval with the maximum average of steps


```r
max.int <- which.max(Mean.Interval.Steps$x)
Mean.Interval.Steps[max.int, "Group.1"]
```

```
## [1] 835
```

## Imputing missing values

Total number of missing values


```r
sum(is.na(activity.data$steps))
```

```
## [1] 2304
```

The missing data will be imputed using the impute function from the Hmisc package


```r
Imputed.Activity.Data <- activity.data
Imputed.Activity.Data$steps <- impute(activity.data$steps, fun = mean)
```


```r
Total.Imputed.Steps <- aggregate(Imputed.Activity.Data$steps, by = list(Imputed.Activity.Data$date), FUN = "sum")
```

Plot Histogram of the total steps, with imputed steps, taken each day


```r
hist(Total.Imputed.Steps$x, main = "Histogram of Total Steps, with Imputed Steps, Per Day", xlab = "Total Steps Per Day", col = "red")
```

![](PA1_Assignment1_files/figure-html/Hist.Steps.Per.Day.Imputed-1.png)<!-- -->

Mean total steps with imputed steps, per day


```r
mean(Total.Imputed.Steps$x)
```

```
## [1] 10766.19
```

Median total steps with imputed steps, per day


```r
median(Total.Imputed.Steps$x)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?


```r
Imputed.Activity.Data$Weekday <- weekdays(as.Date(Imputed.Activity.Data$date), abbreviate = FALSE)
Imputed.Activity.Data$day.type <- ifelse(Imputed.Activity.Data$Weekday %in% c("Saturday", "Sunday"), 'Weekend', 'Weekday')
```

Daily Activity Pattern by Day Type


```r
Day.Type.Steps <- aggregate(steps ~ interval + day.type, data = Imputed.Activity.Data, mean)
qplot(interval, steps, data = Day.Type.Steps, facets = day.type ~., geom = "line", color = "red", main = "Time Series: Average Steps by Day Type") + geom_line(size = 1.5)
```

![](PA1_Assignment1_files/figure-html/Activity.By.Day.Type-1.png)<!-- -->
