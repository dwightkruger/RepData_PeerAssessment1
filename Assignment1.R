# Filename:    Assignment1.R
#
# Description: Answer the following questions:
#
#              1. What is mean total number of steps taken per day? 
#
#              The data is located at https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip
#
#              The variables included in this dataset are:    
#                   - steps: Number of steps taking in a 5-minute interval (missing values
#                            are coded as NA)
#
#                   - date: The date on which the measurement was taken in YYYY-MM-DD format
#
#                   - interval: Identifier for the 5-minute interval in which measurement 
#                               was taken
#
#               The dataset is stored in a comma-separated-value (CSV) file and there are
#               a total of 17,568 observations in this dataset.

loadDataSet <- function()
{
    # Unzip the data file stored in the project.
    sourceFile.zip <- "./activity.zip"    
    unzip(sourceFile.zip, overwrite = TRUE, exdir = ".")
    
    # Read the datafile into a data frame, with the column types as numeric, date, numeric
    setClass('myDate') 
    setAs("character","myDate", function(from) as.Date(from, format="%Y-%m-%d") )    
    colClasses = c("numeric", "myDate", "numeric")
    activity = read.csv("./activity.csv", stringsAsFactors = FALSE, colClasses = colClasses)
    
    # Q1: What is mean total number of steps taken per day? Make a histogram of the 
    #     total number of steps taken each day. Calculate and report the mean and 
    #     median total number of steps taken per day.
    library(data.table)
    table = data.table(activity)
    table = table[complete.cases(table), ]
    activityStepsByDate = table[,sum(steps), by=date]
    setnames(activityStepsByDate, c("date", "V1"), c("Date", "TotalSteps"))

    meanStepsPerDay = mean(activityStepsByDate$TotalSteps, na.rm = TRUE)
    medianStepsPerDay = median(activityStepsByDate$TotalSteps, na.rm = TRUE)
    
    hist(activityStepsByDate$TotalSteps, 
         xlab="Total number of steps taken each day",
         main="Histogram of the total number\nof steps taken each day")
        
    # Q2: What is the average daily activity pattern?
    #     Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and 
    #     the average number of steps taken, averaged across all days (y-axis). 
    #
    #     Which 5-minute interval, on average across all the days in the dataset, contains the 
    #     maximum number of steps?
    
    activityByTime <- table[, median(steps), by=interval]
    setnames(activityByTime, c("interval", "V1"), c("Interval", "AvgSteps"))
    plot(x = activityByTime$Interval, 
         y=activityByTime$AvgSteps, 
         type = "l",
         xlab="Time",
         ylab="Average Steps",
         main="Average Number of Steps Taken\nAveraged Across all Days")
    
    maxInterval <- activityByTime$Interval[which.max(activityByTime$AvgSteps)]
    
    # Q3: Calculate and report the total number of missing values in the dataset (i.e. 
    #     the total number of rows with NAs)
    #
    #     Devise a strategy for filling in all of the missing values in the dataset. The 
    #     strategy does not need to be sophisticated. For example, you could use the 
    #     mean/median for that day, or the mean for that 5-minute interval, etc.
    #
    #     Create a new dataset that is equal to the original dataset but with the missing 
    #     data filled in
    #
    #     Make a histogram of the total number of steps taken each day and Calculate and report 
    #     the mean and median total number of steps taken per day. Do these values differ from 
    #     the estimates from the first part of the assignment? What is the impact of imputing 
    #     missing data on the estimates of the total daily number of steps?
        
    missingValues= is.na(activity$steps)
    totalNumberMissingValues <- sum(missingValues)
    
    # Replace the missing values with the mean for that 5-minute interval
    activityPlusMissingData <- activity
    for (i in 1:dim(activityPlusMissingData)[1])
    {
        if (missingValues[i])
        {
            activityPlusMissingData$steps[i] = activityByTime[Interval==activityPlusMissingData[i,]$interval, ]$AvgSteps
        }
    }
    
    table2 = data.table(activityPlusMissingData)
    table2 = table2[complete.cases(table2), ]
    activityStepsByDateWithMissingData = table2[,sum(steps), by=date]
    setnames(activityStepsByDateWithMissingData, c("date", "V1"), c("Date", "TotalSteps"))
    
    meanStepsPerDayWithMissingData = mean(activityStepsByDateWithMissingData$TotalSteps, na.rm = TRUE)
    medianStepsPerDayWithMissingData = median(activityStepsByDateWithMissingData$TotalSteps, na.rm = TRUE)
    
    hist(activityStepsByDateWithMissingData$TotalSteps, 
         xlab="Total number of steps taken each day",
         main="Histogram of the total number\nof steps taken each day")
    
    totalNumberOfSteps = sum(activity$steps, na.rm = TRUE)
    totalNumberOfStepsWithMissingData = sum(activityPlusMissingData$steps, na.rm = TRUE)   
    
    # Q4: Are there differences in activity patterns between weekdays and weekends?
    
    # Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
    # indicating whether a given date is a weekday or weekend day.
    
    # Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval 
    # (x-axis) and the average number of steps taken, averaged across all weekday days or weekend 
    # days (y-axis) 
    
    
    weekendActivity = grepl("(Sun|Sat)", weekdays(activityPlusMissingData$date, abbreviate=TRUE))
    activityPlusMissingData$IsWeekend <- factor(weekendActivity, levels=c(TRUE, FALSE), labels=c("Weekend", "Weekday"))
    
    table3 = data.table(activityPlusMissingData)
    weekendActivitybyTime <- table3[, median(steps), by=c("interval","IsWeekend")]
    
    setnames(weekendActivitybyTime, c("interval", "IsWeekend", "V1"), c("Interval", "IsWeekend", "AvgSteps"))    
    
    library(lattice)
    xyplot(weekendActivitybyTime$AvgSteps ~ weekendActivitybyTime$Interval | weekendActivitybyTime$IsWeekend, 
           xlab="Number of steps",
           main="Activity patterns between weekdays and weekends",
           ylab="Interval",
           type="l",
           layout=c(1,2))
                
}
