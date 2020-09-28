###############################################################################################
# Data Visualization
# Diabetes
# Author: Burcu Ozek
###############################################################################################

library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr)
library(lubridate)

# Plot- Age
dia <- read.csv("~/Desktop/diabetes_csv.csv", na.strings="", stringsAsFactors=FALSE)

temp_dia <- dia %>%
  group_by(Age.Interval) %>%
  summarise(Diabete = sum(diabete))
  
temp_dia_no <- dia %>%
  group_by(Age.Interval) %>%  
  summarise(No_Diabete = sum(no.diabete))

dia_sample <- data.frame(temp_dia$Age.Interval, temp_dia$Diabete, temp_dia_no$No_Diabete)
colnames(dia_sample) <- c("AgeInterval", "Diabete", "NoDiabete")

dia_sum <- dia_sample$Diabete + dia_sample$NoDiabete 
dia_rate <- dia_sample$Diabete / dia_sum
no_dia_rate <- dia_sample$NoDiabete / dia_sum

dia_sample_2 <- data.frame(temp_dia$Age.Interval, dia_rate,no_dia_rate)
colnames(dia_sample) <- c("AgeInterval", "Diabete", "NoDiabete")
dia_sample_2 <- dia_sample_2[c(1:5),]

write.csv(dia_sample_2,'~/Desktop/dia_sample_age_2.csv')


# Plot - California 1
cali <- read.csv("~/Desktop/california.csv", na.strings="", stringsAsFactors=FALSE)

sex <- cali %>%
  filter(Strata == "Sex") 

sex_sample <- sex[,c(2,4,5)]

sex_sample_2 <- sex_sample%>%
  spread(Strata.Name,Percent)  

write.csv(sex_sample_2,'~/Desktop/cali_sample_sex.csv')

# Plot - California 2

race <- cali %>%
  filter(Strata == "Race-Ethnicity") 

race_sample <- race[,c(2,4,5)]

race_sample_2 <- race_sample%>%
  spread(Strata.Name,Percent)  

write.csv(race_sample_2,'~/Desktop/cali_sample_race.csv')


# Plot - California 2

income <- cali %>%
  filter(Strata == "Income") 

income_sample <- income[,c(2,4,5)]

income_sample_2 <- income_sample%>%
  spread(Strata.Name,Percent)  

write.csv(income_sample_2,'~/Desktop/cali_sample_income.csv')

