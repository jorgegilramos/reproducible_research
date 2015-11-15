setwd("/home/user/reproducible_research/peer_assessment_1")

# Download data
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
data_file <- "repdata_data_activity.zip"
file_name <- "activity.csv"

# Download file if it is not in the directory 
if (!file.exists(data_file)) {
  cat("Downloading", data_file, "at", getwd(), "...\n")
  download.file(url, data_file, method = "curl")
}

# Unzip file if the file don't exist (it is packed in the zip file)
if (!file.exists(file_name)) {
  cat("Unziping file", data_file, "at", getwd(), "...\n")
  unzip(data_file, list = FALSE, overwrite = TRUE)
}

df_activity_complete <- read.csv(file_name)
df_activity <- na.omit(df_activity_complete)

library(plyr)

#steps.date <- aggregate(steps ~ date, data = df_activity, FUN=sum)
#barplot(steps.date$steps, names.arg = steps.date$date, xlab = "date", ylab = "steps")

steps_by_day <- ddply(df_activity, .(date), summarize, total_steps = sum(steps))
colnames(steps_by_day)
hist(steps_by_day$total_steps, xlab = "steps", main = "Steps by day")
barplot(steps_by_day$total_steps, names.arg = steps_by_day$date, xlab = "date", ylab = "steps")


mean(steps_by_day$total_steps)
median(steps_by_day$total_steps)

#
#df_activity[df_activity$interval==0, ]

# 1.-
steps_by_interval <- ddply(df_activity, .(interval), summarize, total_steps = sum(steps), average_steps = mean(steps))
plot(total_steps ~ interval, type = "l", data = steps_by_interval, ylab = "steps", main = "Steps by interval", col = "red")

# 2.-
steps_by_interval$interval[which.max(steps_by_interval$total_steps)]


#lines(average_steps ~ interval, col="green", data = steps_by_interval)
#plot(average_steps ~ interval, type = "l", data = steps_by_interval, ylab = "average steps", main = "Steps by interval", col = "red")

#nrow(df_activity_complete[!complete.cases(df_activity_complete),])
sum(is.na(df_activity_complete))

# Fill
df_activity_filled <- merge(df_activity_complete, steps_by_interval[c("interval", "average_steps")], by="interval")
df_activity_filled$steps <- ifelse(!is.na(df_activity_filled$steps), df_activity_filled$steps, df_activity_filled$average_steps)


steps_by_day_filled <- ddply(df_activity_filled, .(date), summarize, total_steps = sum(steps))
hist(steps_by_day_filled$total_steps, xlab = "steps", main = "Steps by day")

#mean(steps_by_day$total_steps)
#median(steps_by_day$total_steps)

mean(steps_by_day_filled$total_steps)
median(steps_by_day_filled$total_steps)

daytype <- function(date) {
  if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
    "weekend"
  } else {
    "weekday"
  }
}

# Apply factor
df_activity_filled$daytype <- as.factor(sapply(df_activity_filled$date, daytype))

#steps_by_interval_filled_weekday <- ddply(df_activity_filled[df_activity_filled$daytype=="weekday",], .(interval), summarize, total_steps = sum(steps), average_steps = mean(steps))
#steps_by_interval_filled_weekend <- ddply(df_activity_filled[df_activity_filled$daytype=="weekend",], .(interval), summarize, total_steps = sum(steps), average_steps = mean(steps))

# Aggregate by daytype
aggregate_by_daytype <- function(daytype) {
  data <- df_activity_filled[df_activity_filled$daytype==daytype,]
  data <- ddply(data, .(interval), summarize, total_steps = sum(steps))
  data[c("interval", "total_steps")]
}

steps_by_interval_filled_weekday <- aggregate_by_daytype("weekday")
steps_by_interval_filled_weekend <- aggregate_by_daytype("weekend")

par(mfrow = c(2,1))
plot(total_steps ~ interval, type = "l", data = steps_by_interval_filled_weekday, main = "Weekday", ylab = "steps", col = "red")
plot(total_steps ~ interval, type = "l", data = steps_by_interval_filled_weekend, main = "Weekend", ylab = "steps", col = "red")


par(mfrow = c(1,1))

rm(steps_by_interval_filled_weeekend)
