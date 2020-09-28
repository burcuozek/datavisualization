###############################################################################################
# Airlines Data Set
# Data Visualization
# Author: Burcu Ozek
###############################################################################################

# -----------------------------------

# Required libraries are downloaded
library(dplyr)
library(ggplot2)
library(magrittr)
library(gridExtra)
library(reshape2)

# Data is read
delays <- read.csv("~/Desktop/airlines_delay.csv", na.strings="", stringsAsFactors=FALSE)
View(delays)

# None of the rows is dublicated
delays[duplicated(delays)]
unique(delays)

# Data frame is group by carrier and their delay in mins is added
delays_carrier <- delays %>%
  group_by(carrier)%>%
  summarise(tot_delay_min = sum(late_aircraft_delay, na.rm=TRUE))
delays_carrier

# Data frame is arranged by their total delays
delays_carrier_10 <- arrange(delays_carrier, desc(tot_delay_min))
delays_carrier_10 <- head(delays_carrier_10, n = 10L)

# Bar plot of 10 carrier is drawn. 
ggplot(data=delays_carrier_10, aes(x=carrier, y=tot_delay_min)) +
  geom_bar(stat="identity")

# Ordered bar plot of 10 carrier is drawn.
delays_carrier_10$carrier <- as.factor(delays_carrier_10$carrier)
ggplot(delays_carrier_10, aes(x=reorder(carrier,-tot_delay_min), y=tot_delay_min)) +
  geom_bar(stat="identity")


# -----------------------------------

# Data is grouped by carrier and also year
# Top 10 carrier is chosen again
delays_carrier_year <- delays %>%
  group_by(carrier,year)%>%
  filter(carrier == delays_carrier_10$carrier)%>%
  summarise(tot_delay_min = sum(late_aircraft_delay, na.rm=TRUE))
delays_carrier_year

#Stacked Bar Chart is plotted
delays_carrier_year$year <- as.factor(delays_carrier_year$year)
ggplot(data=delays_carrier_year, aes(x=carrier, y=tot_delay_min, fill=year)) +
  geom_bar(stat="identity")

#Reordered stacked Bar Chart is plotted
ggplot(data=delays_carrier_year, aes(x=reorder(carrier,-tot_delay_min), y=tot_delay_min, fill=year)) +
  geom_bar(stat="identity")


# Problem 3-----------------------------------

# New data frame is created by using only the delay type and carrier which will be used for melt function
delay_type<-select(delays, carrier, carrier_delay, late_aircraft_delay, nas_delay, security_delay, weather_delay)
# New data frame is converted to the log of the delays in minutes
delay_type$carrier_delay<-log(delay_type$carrier_delay)
delay_type$late_aircraft_delay<-log(delay_type$late_aircraft_delay)
delay_type$nas_delay<-log(delay_type$nas_delay)
delay_type$security_delay<-log(delay_type$security_delay)
delay_type$weather_delay<-log(delay_type$weather_delay)
delay_type

# New data frame is created using melt function from reshape library
delay_type_temp<- melt(delay_type, id = "carrier")
colnames(delay_type_temp) <- c("carrier", "delay_type", "log_delay_in_mins")
delay_type_temp <- delay_type_temp[, c(2,3)]
delay_type_temp$`delay_type_a `<-as.factor(delay_type_temp$`delay_type`)
head(delay_type_temp)
class(delay_type_temp$delay_type)
class(delay_type_temp$log_delay_in_mins)

# Density plot is drawn
ggplot(delay_type_temp, aes(x = log_delay_in_mins, fill = `delay_type` ))+geom_density(alpha = .5)


# Problem 4-----------------------------------

# New data frame is created using the carrier type and delay type
delay_type_heat<-select(delays, carrier, carrier_delay, late_aircraft_delay, nas_delay, security_delay, weather_delay)

# Melt function is used to reshape the data
delay_type_heat<- melt(delay_type_heat, id = "carrier")
colnames(delay_type_heat)<- c("carrier","delay_type" ,"sum_delay")

# If there is na in the data, 0 is replaced instead of na
delay_type_heat[is.na(delay_type_heat)] <- 0 

# Summary table is created group by carrier and delay type using the sum of the delays in minutes
delay_type_heat_summary <- delay_type_heat %>%
  group_by(carrier,delay_type)%>%
  summarise(sum_delay = sum(sum_delay))
delay_type_heat_summary

# Heat map is drawn
ggplot(delay_type_heat_summary, aes(x = delay_type, y = carrier, fill= sum_delay)) + 
  geom_tile()


# Problem 5-----------------------------------

# New data stream created using year and delay type variables
delay_line <-select(delays, year, carrier_delay, late_aircraft_delay, nas_delay, security_delay, weather_delay)
delay_line_temp<- melt(delay_line, id = "year")
colnames(delay_line_temp) <- c("year", "delay_type_a", "sum_delay")
names(delay_line_temp)
class(delay_line_temp$year)
delay_line_temp$year <- as.factor(delay_line_temp$year)
class(delay_line_temp$delay_type_a)

# If there is na in the data, 0 is replaced instead of na
delay_line_temp[is.na(delay_line_temp)] <- 0 

# Summary table is created
delays_line <- delay_line_temp %>%
  group_by(year,delay_type_a)%>%
  summarise(sum_delay_a = sum(sum_delay))
delays_line

# Columns are renamed
names(delays_line)
colnames(delays_line) <- c("year","delay_type" ,"sum_delay")

# Line plot is drawn
ggplot(delays_line, aes(x=factor(year), y=sum_delay, group=delay_type, color =  delay_type)) +geom_line() + geom_point()
  

