##############################################################################
# The dataset used at this assignment includes Road Traffic Accidents records in 
# US from the year of 2016 to the year of 2021.
# Datasets can be found here:
# https://www.kaggle.com/datasets/sobhanmoosavi/us-accidents
# https://www.kaggle.com/datasets/sobhanmoosavi/us-accidents?select=US_Accidents_Dec21_updated.csv
# For more information and description about the dataset, Please visit the below URL:
# https://smoosavi.org/datasets/us_accidents
###############################################################################

# Loading Important Libraries
library(tidyverse)
library(caret)
library(DescTools)
library(ggcorrplot)
library(plyr)
library(dplyr)

# ----- GOAL -----
# 1- Take a deep analysis of the us accidents variables
# 2- To make a prediction model that will help us predict accidents


# ----- Import dataset -----
data = read.csv('https://www.kaggle.com/datasets/sobhanmoosavi/us-accidents?select=US_Accidents_Dec21_updated.csv')
data = read.csv("US_Accidents_Dec21_updated.csv")data
head(data)
tail(data)

# shows an organized section of data frame
glimpse(data)

# shows structure of the data frame
str(data)
ncol()
nrow()
# Shows descriptive statistics of each category,
# Should show everything you need
dim(data)
summary(data)

#Analyse Data: Severity of accidents Analysis
accidents$Severity <- as.integer(data$Severity)
count_severity <- c(sum(data$Severity==1), sum(data$Severity==2), sum(data$Severity==3), sum(data$Severity==4))
Frequency_severity <- count_severity/sum(count_severity)
Frequency_severity

data$Severity <- as.integer(data$Severity)
count_severity <- c(sum(data$Severity==1), sum(data$Severity==2), sum(data$Severity==3), sum(data$Severity==4))
rf <- count_severity/sum(count_severity)
barplot(rf, names.arg = 1:4, main = "Relative Frequencies of Accidents Severity")

data$State <- as.character(data$State)
group_by(data$Severity)

#Number of Accidents By State
M <- unique(data$State)
length(M)
M
Accidents_By_State <- count(data$State)
Accidents_By_State
nrow(data)
colnames(data)


#Analyse Data: Day and Night Accidents Analysis
count_sunrise_sunset <- count(data$Sunrise_Sunset)
Freq_sunrise_sunset <- count_sunrise_sunset$freq/sum(count_sunrise_sunset$freq)
barplot(Freq_sunrise_sunset, names.arg = count_sunrise_sunset$x, main = "Relative Frequencies of Day and Night Accidents")
barplot(Frequency_severity, names.arg = 1:4, main = "Relative Frequencies of Accident Severity")

#Analyse Data: Distance of Accidents Analysis
Distance <- data$Distance.mi.
Distance = Distance[Distance < 5]
hist(Distance, breaks=seq(0,5,0.5), xlab = "Distance (miles)", main="Histogram of Distance of Accidents")

#Analyse Data: Longitude and Latitude Analysis
sort(sapply(data, function(x) sum(is.na(x))),decreasing = TRUE)
ggplot(data, aes(Start_Lng, Start_Lat)) +
  geom_point(alpha = 0.05)
# Analyse start and end time of accidents:
interval <- interval(strptime(data$Start_Time, "%Y-%m-%d %H:%M:%S"), strptime(data$End_Time, "%Y-%m-%d %H:%M:%S"))
data$time <- time_lenght(interval, unit = "hour")
hist(data$time_in_hours, breaks=seq(0,35,0.5), xlab = "Time in Hours", main = "Histogram of Time")

#Analyse Data: Side of Accidents Analysis (Left/ Right Side)
count_side <- count(data$Side)
Frequency_side <- count_side$freq/sum(count_side$freq)
barplot(Frequency_side, names.arg = count_side$x, main = "Relative Frequencies of Accidents side (Left/ Right)")

##############################################################

#Creating Model and training tests
data2 <- data %>% mutate_if(is.character, as.factor) %>% 
  dplyr::select(Severity, State, Distance(mi), Side, Distance(mi), Visibility(mi), Wind_Direction, Weather_Condition())
ggplot(data2, aes(x=data2$Severity, fill=data2$Severity))+
    

by_state_count <- data %>% select(data$Severity, data$State) %>% group_by(State) %>% summarise(total.count=n()) %>% arrange(Severity)
by_state_count
chart1 <- ggplot(data=by_state_count, aes(x=State, y=Severity)) + geom_bar(stat="identity")
chart1


time_in_hours_outliears <- data[ which(data$time_in_hours > 10), ]
time_in_hours_outliears[c(5,51,17,19)]

Test <- accidents$Distance.mi.
Test = Test[Test < 5]
hist(Test, breaks=seq(0,5,0.5), xlab = "Distance (miles)", main="Histogram of Distance")

time_in_hours_outliears
data %>%
  group_by(data$State) %>%
  count() %>%
  ggplot()+
  geom_col(aes(Severity, n), fill = 'green')+
  ggtitle("State Analysis")+
  xlab("State")+
  ylab("Severity")
