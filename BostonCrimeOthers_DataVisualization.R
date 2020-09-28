########################################################################################################
# Boston Crime, Class Attendance and Wine Data Set 
# Data Visualization
# Author: Burcu Ozek
########################################################################################################

# Attendance Data Set-----------------------------------

# Required libraries are downloaded
library(dplyr)
library(stringr)
library(magrittr)

# Data is read from csv file
roster <- read.csv("~/Desktop/roster.csv", na.strings="",stringsAsFactors=FALSE)
View(roster)
attendance <- read.csv("~/Desktop/attendance.csv", na.strings="",stringsAsFactors=FALSE)
View(attendance)

# Classes of variables are explored
summary(roster)
summary(attendance)

# Missing data is checked, and no missing data is found
which(is.na(attendance$firstname))
which(is.na(attendance$lastname))

# New data frame is created to make similar to rostar data
# New column which contains , is created to be used later
df1<-mutate(attendance,newcol = ",")
summary(df1)
attendance_2 <-data.frame(str_c(df1$lastname, df1$newcol, df1$firstname))
class(attendance_2$str_c.df1.lastname..df1.newcol..df1.firstname.)
# This column is added to add all the attendance of a student
attendance_2<-mutate(attendance_2,newcol = 1)
colnames(attendance_2) <- c("student", "count")

# Attendance is counted, is grouped by student
attendance_3 <- attendance_2 %>%
  group_by(student,count) %>%
  summarize(total_attendance = sum(count,na.rm=TRUE))
class(attendance_3$student)
class(roster$names)
roster$names <- as.factor(roster$names)

# Two data frame is compared to find any missing student
# If a student has not attended any lectures, zero is setted to that student's attendance
missing_student <- setdiff(roster$names, attendance_3$student)
class(missing_student)
missing_student <- as.factor(missing_student)
missing_student <- data_frame(missing_student, 1 , 0)
colnames(missing_student) <- c("student", "count", "total_attendance") 
  
# Missing student and rest of the student are combined in a new data frame
attendance_4 <- bind_rows(attendance_3, missing_student)

# To get the same output with the Problem 1, some arrangements are done
attendance_4 <- attendance_4[,c(1,3)]
attendance_4$student <- as.character(attendance_4$student)
temp <- strsplit(attendance_4$student, ",")
mat  <- matrix(unlist(temp), ncol=2, byrow=TRUE)
df <- as.data.frame(mat)
colnames(df) <- c("First Name", "Last Name")

# The output can be seen with the "attendace_last" data frame
attendance_last <- cbind(df, attendance_4$total_attendance)
colnames(attendance_last) <- c("First Name", "Last Name","Count")
attendance_last


# Boston Crime Data Set-----------------------------------

# Required libraries are downloaded
library(tidyr)

# Data is read from csv file
crime <- read.csv("~/Desktop/tmpn9gdpt2q.csv", na.strings="", stringsAsFactors=FALSE)
View(crime)

# Only the required variables are stored to the "crime" data frame
crime <- data.frame(crime$OFFENSE_CODE_GROUP, crime$DISTRICT)
colnames(crime) <- c("OFFENSE_CODE_GROUP", "DISTRICT")

#trial <- group_by(crime, OFFENSE_CODE_GROUP )
#trial_2 <- group_by(crime, OFFENSE_CODE_GROUP, DISTRICT )

#summarise(group_by(crime,OFFENSE_CODE_GROUP))

# Reqired transformations are done
crime$OFFENSE_CODE_GROUP <- as.character(crime$OFFENSE_CODE_GROUP)
crime$DISTRICT <- as.character(crime$DISTRICT)
class(crime$OFFENSE_CODE_GROUP)

# New data frame is created to get the output of the Problem 2
# Rows are showing the OFFENCE_CODE_GROUP and columns are showing the DISTRICT
crime_2 <-crime %>%
  drop_na()%>%
  group_by( OFFENSE_CODE_GROUP,DISTRICT) %>%
  summarise(count=n())  %>%
  spread(DISTRICT, count)

# If there is NA in the data, they are set to 0.
crime_2[is.na(crime_2)] <- 0 

# Problem 2 asked from us a matrix, therefore required transformation is done
# "Crime_2" matrix is giving the output
crime_2 <- as.matrix(crime_2)
crime_2



# Problem 3-----------------------------------
########### errors
##### you should have removed the dublicate with using disctict however remove x first 
##### you should have done multiple filtering 20 yr, twenty, you should also do that all capital and remove . or _ 

# Part a

# Data is read from csv file
wine <- read.csv("~/Desktop/wine_data.csv", na.strings="", stringsAsFactors=FALSE)
View(wine)

# Frequency count of “variety” variable from the dataset is shown below and stored "wine_2" data frame 
wine_2 <- wine %>%
  group_by(variety)%>%
  summarise(frequency=n())
wine_2

# Top 10 variety by count is shown below and stored "wine_3" data frame
wine_a <- arrange(wine_2, desc(frequency))
head(wine_a, n = 10L)
wine_a


# Part b 

# The average points by country is the output of the following code, and stored "wine_b" data frame
wine_b <- wine %>%
  group_by(country)%>%
  summarise(average_points=mean(points))
wine_b


# Part c 

# wine_c is showing the highest average price providence
# Santa Cruz province has the highest average price
wine_c <- wine %>%
  group_by(province)%>%
  summarise(average_price=mean(price, na.rm=TRUE))%>%
  slice(which.max(average_price))
wine_c


# Part d 

# wine_4 is showing the highest average price providence in the US
# Nevada province in the US has the highest average price
# Wines are filtered as "US"
wine_d <- wine %>%
  group_by(province, country)%>%
  filter(country == "US")%>%
  summarise(average_price=mean(price, na.rm=TRUE))%>%
  ungroup()%>%
  slice(which.max(average_price))
wine_d

# Part e

# From the “designation” variable the number of 20 year old wine is calculated and stored the wine_e data frame
# All wines which include 20 Year in their name is stored to wine_e
wine_e <-wine %>%
  filter(str_detect(designation, '20 Year')) %>%
  summarise(count=n())
wine_e

