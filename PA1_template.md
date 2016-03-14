---
title: "PA1_template"
author: "Ramya Balasubramaniam"
date: "13 March 2016"
output: html_document
---

### This is an R Markdown document for processing the output as described in the assignment using the Activity.zip archive file downloaded from the url "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip". This document gives a step by step description of the process of computation of the desired results.

##loading and preprocessing the data
1. Download the file from the url
2. list the files in the working directory to ensure file is downloaded
3. Unzip the file in destination folder "data_activity"
4. Load the .csv file in R object "act" (dataframe)


```r
library(knitr)
library(lattice)
download.file(url= "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",destfile= "data_activity.zip",method = "curl")
list.files(path = "/Users/ramyabalasubramaniam")
```

```
##  [1] "addPatt.R"         "Applications"      "ass1.R"           
##  [4] "data_activity"     "data_activity.zip" "Desktop"          
##  [7] "Documents"         "Downloads"         "example_plot"     
## [10] "figure"            "GitHub"            "hist_total.png"   
## [13] "impact_impact.png" "java.log.3701"     "Library"          
## [16] "Movies"            "Music"             "newhist_comp.png" 
## [19] "PA1_template.html" "PA1_template.md"   "PA1_template.Rmd" 
## [22] "Pattern_Week.png"  "Pictures"          "Public"           
## [25] "R files"           "SOFTWARE"          "TS_avgsteps.png"
```

```r
unzip(zipfile = "/Users/ramyabalasubramaniam/data_activity.zip",exdir = "data_activity")
act <- read.csv(file = "data_activity/activity.csv")
```
## Calculate the total number of steps taken per day - Mean & median of this data
1. Calculate the total number of steps covered each of the 61 days
2. Plot the histogram save it as .png file
3. Calculate the mean - 
output:[1] 9354.23
4. Calculate the median -
output:[1] 10395


```r
tot_steps <- tapply(act$steps,as.factor(act$date),sum, na.rm= T)
hist(tot_steps,main = "Total no. of Steps per day", xlab = "Step Classes", ylab = "Frequency")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

```r
mean(tot_steps)
```

```
## [1] 9354.23
```

```r
median(tot_steps)
```

```
## [1] 10395
```
## Show the average pattern across the different intervals
1. Calculate the average number of steps taken, averaged across all days for a particular 5-minute interval
2. Make a time-series plot of the same
3. Calculate the interval for which the value is maximum and what is that value 
output: index -> 104 (interval number:835), value -> 206.1698 


```r
avg_steps <- tapply(act$steps, INDEX = as.factor(act$interval),mean, na.rm =T)
plot(x = names(avg_steps),y = avg_steps,main = "average steps taken for an interval", xlab = "interval no.", ylab = "average no. of steps", type= "l", pch = 3, lwd = 2,col ="blue")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
index = which(avg_steps == max(avg_steps))
index
```

```
## 835 
## 104
```

```r
max(avg_steps)
```

```
## [1] 206.1698
```

```r
avg_steps[index]
```

```
##      835 
## 206.1698
```
## Imput the missing values
1. Create a new data vector for the 17568 value replacing NA values with average value for that 5-minute interval
2. Combine this vector to a new dataset with the other two column values same as the original dataset
3. Calculate and plot the histogram of the total numer of steps per day for the new dataset
4. Also plot the two histograms together (using old and new dataset to see the effect on the overall values
5. Calculate the mean and median of total number of steps per day using the new dataset
output: mean [1] 10766.19
median [1] 10766.19
6. Make a barplot of the same using old and new dataset to see the effect on individual values

```r
new_steps <- vector(length = nrow(act))
for (i in 1:nrow(act))
{
     if (is.na(act[i,1]))
     {
         new_steps[i] <- avg_steps[as.character(act[i,3])]
     }
     else new_steps[i] <- act[i,1]
 }
new_act <- data.frame(steps = new_steps, date = act$date, interval = act$interval)
ntot_steps <- tapply(new_act$steps,as.factor(new_act$date),sum, na.rm= T) 
par(mfrow = c(2,1))
hist(tot_steps,main = "Total no. of Steps per day(with NA)", xlab = "Old Step Classes", ylab = "Frequency")
hist(ntot_steps,main = "Total no. of Steps per day(w/o NA)", xlab = "new Step Classes", ylab = "Frequency")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
mean(ntot_steps)
```

```
## [1] 10766.19
```

```r
median(ntot_steps)
```

```
## [1] 10766.19
```

```r
par(mfrow= c(2,1))
barplot(tot_steps,main = "total steps w/o imputation", xlab = "day number",ylab ="total steps per day")
 barplot(ntot_steps,main = "total steps with imputation", xlab = "day number",ylab ="total steps per day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-2.png)

## Show the pattern for the averages calulated across the five-minute intervals differentiated for weekdays and weekends
1. Create a new column in the new dataset indicating type of day - weekday or weekend (factor type values)
2. Plot the total number of steps for the new dataset categorizing using this new factor variable- weekday or weekend


```r
Day <- weekdays(as.Date(new_act$date))
new_act <- cbind(new_act,Day)
Day_type <- vector(mode= "character",length = nrow(new_act))
for(i in 1:nrow(new_act))
{
   if(as.character(new_act[i,4])== "Monday"|as.character(new_act[i, 4])=="Tuesday"|as.character(new_act[i,4])=="Wednesday"|as.character(new_act[i,4])=="Thrusday"|as.character(new_act[i,4])=="Friday")
   { 
   	   Day_type[i] <- "Weekday"
   	}
   else Day_type[i] <- "Weekend"
}
Day_type <- as.factor(Day_type)
new_act <- cbind(new_act,Day_type)
xyplot(steps ~ interval| Day_type, data = aggregate(steps ~ Day_type + interval, data = new_act, sum), type= "l",ylab = "no of steps", xlab ="interval number", main ="pattern over weekdays & weekends",layout = c(1,2))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)
