library(readr)
setwd("~/Google Drive/Data Science/GL/DSBA/Capstone project")
default <- read_csv("~/Google Drive/Data Science/GL/DSBA/Capstone project/default_data Premium Default-Dataset.csv")
View(default)
str(default)
table(default$sourcing_channel)
summary(default)
attach(default)
default$age_in_years = age_in_days/365
mean(default$default)
# 0.93741 - means that 94% of the customers renewed their policy
# If we were to simply predict the most frequently occurring value (1 in this case), 
# We will end up with an accuracy of 94%. 
# So our aim should be to build a model that has a higher accuracy
# Not only that, we should focus on sensitivity i.e. capturing as much of the defaulter as possible
sum(is.na(default))
default_data = default
colnames(default_data)
colnames(default_data)[8] <- "Marital_status"
default_data <- default_data[,-1]
default_data$Marital_status<- as.factor(default_data$Marital_status)
default_data$default<- factor(default_data$default,labels = c("Default","Renewed"))
default_data$Veh_Owned<- as.factor(default_data$Veh_Owned)
default_data$No_of_dep<- as.factor(default_data$No_of_dep)
default_data$sourcing_channel<- as.factor(default_data$sourcing_channel)
default_data$residence_area_type<- as.factor(default_data$residence_area_type)
default_data$Accomodation<- factor(default_data$Accomodation, labels = c("Rented","Owned"))
default_data$Marital_status<- factor(default_data$Marital_status, labels = c("Unmarried","Married"))
