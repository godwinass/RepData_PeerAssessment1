#where the file needs to be downloaded, unzipped, both or is otherwise present in the root directory where the .Rmd file is located. 

if(file.exists("./activity.csv")){
  print("Dataset already downloaded and unzipped")
  print("Loading Data...")
}else if(file.exists("./repdata-data-activity.zip")){
  print("Dataset downloaded. Now Unzipping...")
  unzip("./repdata-data-activity.zip")
  print("Completed.")
  print("Loading Data...")
}else if(!file.exists("./repdata-data-activity.zip")){
  print("Downloading and unzipping dataset...")
  download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip","./repdata-data-activity.zip")
  unzip("./repdata-data-activity.zip")
  print("Completed.")
  print("Loading Data...")
}

#First, we read the date coercing the date column to character rather than factor
#read file using read.csv
activityData<-read.csv("./activity.csv", header=TRUE, sep=",")
#convert date column from factor to Date class
activityData$date<-as.Date(activityData$date)
#provide a summary of the activityData table
summary(activityData)

#Let's check the dimensions and a few rows of our newly created data frame
dim(activityData)

head(activityData)

#Analysis

#1. What is the mean total number of steps taken per day?
library (dplyr)
AvgDay <- activityData %>% group_by(date) %>%
  summarize(total.steps = sum(steps, na.rm = T),
          mean.steps = mean(steps, na.rm = T))

#Once the summaries are calculated, we can construct the histogram of the total steps:
  
library(ggplot2)
g <- ggplot(AvgDay, aes(x=total.steps))
g + geom_histogram(binwidth = 2500) + theme(axis.text = element_text(size = 12),axis.title = element_text(size = 14)) + labs(y = "Frequency") + labs(x = "Total steps/day")

summary(AvgDay$total.steps)

summary (AvgDay$mean.steps)

#2. What is the daily activity pattern?
AvgInterval <- activityData %>% group_by(interval) %>%
summarize(mean.steps = mean(steps, na.rm = T))
g <- ggplot(AvgInterval, aes(x = interval, y = mean.steps))
g + geom_line() + theme(axis.text = element_text(size = 12), 
                        axis.title = element_text(size = 14, face = "bold")) + 
  labs(y = "Mean number of steps") + labs(x = "Interval")

#3. Imputing missing values
mean(is.na(activityData$steps))

sum(is.na(activityData$steps))

#First, we will check for missing values in the interval column within AvgInterval, where we stored the mean number of steps for each 5 min interval:
sum(is.na(AvgInterval$mean.steps))

#Since there are no missing values in this variable we will use it to fill in for NAs. Next we create a duplicate of the original data named newData and we will draw the appropriate values AvgInterval:
newData <- activityData

#In order to fill in missing values we check at each row if the column interval is NA, when the condition is true we look for the corresponding interval (index), we search for this particular interval in the AvgInterval data and extract it to a temporary variable values. Last we choose only the column of interest from values, which is the mean.steps and assign this number to the corresponding position in the newData set. We use a for loop to run through all the rows. 
#(Note: there may be a more elegant way to do this perhaps using apply but couldnt make it work)

for (i in 1:nrow(newData)) {
  if (is.na(newData$steps[i])) {
    index <- newData$interval[i]
    value <- subset(AvgInterval, interval==index)
    newData$steps[i] <- value$mean.steps
  }
}
head(newData)

#We can observe from the previous output that now there are numeric values in the first rows of the dataset.
#We use a similar method as before to group the data by date and calculate daily totals:
newAvg <- newData %>% group_by(date) %>%
summarize(total.steps = sum(steps, na.rm = T))

#And we can construct the histogram:
  
g <- ggplot(newAvg, aes(x=total.steps))
g + geom_histogram(binwidth = 2500) + theme(axis.text = element_text(size = 12),
                                            axis.title = element_text(size = 14)) + labs(y = "Frequency") + labs(x = "Total steps/day")
#For a more quantitative comparison lets review the 5 number summaries and standard deviations of the original data AvgDay vs the data with the imputed values newData
summary (AvgDay$total.steps)

sd(AvgDay$total.steps, na.rm=T)

summary (newAvg$total.steps)

sd(newAvg$total.steps, na.rm=T)
#4. Are there differences in activity patterns between weekdays and weekends?

#Different weekend vs weekday patterns are expected as people, in general, have a different set of activities on weekends.
#In order to find the specific patterns for each set of days, we will identify the weekdays from the weekend data. First, we create a new column in newData containing the values weekend or weekday:
newData$day <- ifelse(weekdays(newData$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")

#Next we create two subsets, one containing the weekend and one containing the weekday data:
wkend <- filter(newData, day == "weekend")
wkday <- filter(newData, day == "weekday")

#Then, similarly to section 2, we group by the intervals and calculate the mean number of steps for each time interval. Since the day column is lots during the grouping, we add it again to the wkend and wday dataframes. Lastly, we merge both data sets into one named newInterval
wkend <- wkend %>%
  group_by(interval) %>%
  summarize(mean.steps = mean(steps)) 
wkend$day <- "weekend"

wkday <- wkday %>%
  group_by(interval) %>%
  summarize(mean.steps = mean(steps)) 
wkday$day <- "weekday"

newInterval <- rbind(wkend, wkday)
newInterval$day <- as.factor(newInterval$day)
newInterval$day <- relevel(newInterval$day, "weekend")

#The two panel plot is now created, using the day column as a factor to spearate the weekday from the weekend timeseries.
g <- ggplot (newInterval, aes (interval, mean.steps))
g + geom_line() + facet_grid (day~.) + theme(axis.text = element_text(size = 12), 
  axis.title = element_text(size = 14)) + labs(y = "Number of Steps") + labs(x = "Interval")

library(knitr)
library(markdown)
knit("PA1_template.Rmd")    
markdownToHTML("PA1_template.md", output="PA1_template.html")
#knit2html("PA1_template.html","PA1_template.md")