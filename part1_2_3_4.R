# Assumes the file "activity.csv" is in the working directory.
library(lattice)

#PART 1

df_activity <- read.csv(file = "activity.csv",
                        header = TRUE)

df_activity_complete_cases_bool <- complete.cases(df_activity)

df_activity_complete_cases <- df_activity[df_activity_complete_cases_bool, ]

s1 <- split(df_activity_complete_cases, 
           df_activity_complete_cases$date, 
           drop = TRUE)

l1 <- sapply(s1, function(x) sum(x[, c("steps")]))

hist(l1, main = "Total number of steps taken each day", 
     xlab = "Total number of steps", breaks = 8)

mean(l1)

median(l1)

#PART 2

df_activity_complete_cases$interval <-
  as.factor(df_activity_complete_cases$interval)

s2 <- split(df_activity_complete_cases, 
            df_activity_complete_cases$interval, 
            drop = TRUE)

l2 <- sapply(s2, function(x) mean(x[[1]]))

df_avg_steps <- data.frame(interval = as.numeric(names(l2)),
                           avgSteps = as.numeric(l2))

plot(df_avg_steps$"interval",
     df_avg_steps$"avgSteps",
     type = "l",
     xlab = "5-minute interval",
     ylab = "Average number of steps",
     main = "Average number of steps averaged across all days")

n2 <- round(as.numeric(df_avg_steps$"avgSteps"), digits = 4)

m <- max(n2)

i <- which(n2 == max(n2))

interval <- df_avg_steps[i, ]$"interval"

#PART 3

#Total number of rows with NAs:
naVec <- 
  is.na(df_activity$steps) | is.na(df_activity$date) | is.na(df_activity$interval)

numberRowsNA <- sum(naVec)

#New dataset with the missing data filled in:
df_activity_NA <- df_activity[naVec, ]
df_activity_NA$interval <- as.numeric(df_activity_NA$interval)

#Merge df_activity_NA and df_avg_steps on interval.
df_activity_NA_merge <- merge(x = df_activity_NA,
                              y = df_avg_steps,
                              by.x = "interval",
                              by.y = "interval")

df_activity_NA_merge$steps <- round(df_activity_NA_merge$avgSteps)

df_activity_complete_cases_new <-
  df_activity_NA_merge[, c("steps", "date", "interval")]

df_activity_complete_cases_all <- rbind(df_activity_complete_cases_new,
                                        df_activity_complete_cases)

#Order by date (column 2):
df_activity_complete_cases_all <- 
  df_activity_complete_cases_all[order(df_activity_complete_cases_all[, 2]), ]


s3 <- split(df_activity_complete_cases_all, 
            df_activity_complete_cases_all$date, 
            drop = TRUE)

l3 <- sapply(s3, function(x) sum(x[, c("steps")]))

hist(l3, main = "Total number of steps taken each day", 
     xlab = "Total number of steps", breaks = 8)

mean(l3)

median(l3)

#PART 4

## Part 4

#Get the weekday names for each of the dates:
weekdaysVec <- 
  weekdays(as.Date(as.character(df_activity_complete_cases_all$"date")))

#Create a factor dividing the weekdays into levels "weekday" and "weekend":
weekdaysDividedVec <- 
  as.factor(
    as.character(
      lapply(X = weekdaysVec,
             FUN = function(x) if (x %in% c("Monday", "Tuesday","Wednesday","Thursday","Friday"))
               "weekday"
             else
               "weekend")
      )
    )

#Add the new factor as a column:
df_activity_complete_cases_all$weekdaysDivided <- weekdaysDividedVec

#Make column "interval" a factor in order to split on it later:
df_activity_complete_cases_all$interval <-
  as.factor(as.numeric(df_activity_complete_cases_all$interval))

#Make two dataframes, one for weekday and one for weekend:
df_activity_complete_cases_all_weekday <-
  df_activity_complete_cases_all[as.character(df_activity_complete_cases_all$"weekdaysDivided") == "weekday", ]

df_activity_complete_cases_all_weekend <-
  df_activity_complete_cases_all[as.character(df_activity_complete_cases_all$"weekdaysDivided") == "weekend", ]

#Make dataframe for average steps on weekdays:
s_weekday <- split(df_activity_complete_cases_all_weekday,
                   df_activity_complete_cases_all_weekday$interval,
                   drop = TRUE)

l_weekday <- sapply(s_weekday, function(x) mean(x[[1]]))

df_avg_steps_weekday <- data.frame(interval = as.numeric(names(l_weekday)),
                                   avgSteps = as.numeric(l_weekday))

#Make dataframe for average steps on weekends:
s_weekend <- split(df_activity_complete_cases_all_weekend,
                   df_activity_complete_cases_all_weekend$interval,
                   drop = TRUE)

l_weekend <- sapply(s_weekend, function(x) mean(x[[1]]))

df_avg_steps_weekend <- data.frame(interval = as.numeric(names(l_weekend)),
                                   avgSteps = as.numeric(l_weekend))

#Combine the two dataframes:
df_avg_steps_weekday$weekdaysDivided = "weekday"
df_avg_steps_weekend$weekdaysDivided = "weekend"

df_avg_steps_all <- rbind(df_avg_steps_weekday,
                          df_avg_steps_weekend)

#Make the column "weekdaysDivided" a factor to be used by the plot
#function:
df_avg_steps_all$weekdaysDivided <-
  as.factor(df_avg_steps_all$weekdaysDivided)

#Plot:
p <- xyplot(avgSteps ~ interval | weekdaysDivided, data = df_avg_steps_all, layout = c(1, 2), type = "l")

print(p)

