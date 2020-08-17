#### ENVIRONMENT SET-UP & DATA IMPORT
### Set working directory to read required input data and save any output

setwd("~/Google Drive/Data Science/GL/DSBA/Capstone project")
getwd()

### Load installed package "readxl" to import .xlsx file (given) into the global environment
library(readxl)
default <- read_excel("Insurance Premium Default-Dataset.xlsx")
# Data has been imported and saved as "default"
# It is apparent that default dataset contains 79853 observations (rows) and 17 variables (columns)
View(default)

### UNDERSTANDING THE DATA

## Data structure
# Structure
str(default)

# Load DataExplorer for exploratory data analysis. 
library(DataExplorer)

# This function helps to visualize data structure in network graph format.
plot_str(default, type="d", fontSize = 25)
# It is apparent that the dataset is a dataframe and contains 5000 observations (rows) and 14 variables (columns)
# All the variables are in the number format. Some variables are to be converted as factor as required.

library(readr)
library(DataExplorer)
plot_intro(default)
default$age= default$age_in_days/365
summary(default)

########## some keys observations from summary ########
# average age of customers is 51 years, range is from 21 to 103
# average income of customers is 208847, indicates that this is the annual income
# customers on an average has made 10 premium payments, ranges from two to 60
# there is gap in median and mean of premium indicating that distribution is skewed
# 94% of the customers had renewed their policy though premium payments

####### Data manipulation (Convert data types & remove unwanted variables)

# create a copy of rdata
default_data <- default


# remove variable "ID" because it is not significant in model building
default_data  <- default_data [,-1]
# Check if the variable "id" is removed
names(default_data)

# Convert some variables as factor
default_data$Marital_status<- as.factor(default_data$Marital_status)
default_data$default<- factor(default_data$default,labels = c("Default","Renewed"))
default_data$Veh_Owned<- as.factor(default_data$Veh_Owned)
default_data$No_of_dep<- as.factor(default_data$No_of_dep)
default_data$sourcing_channel<- as.factor(default_data$sourcing_channel)
default_data$residence_area_type<- as.factor(default_data$residence_area_type)
default_data$Accomodation<- factor(default_data$Accomodation, labels = c("Rented","Owned"))
default_data$Marital_status<- factor(default_data$Marital_status, labels = c("Unmarried","Married"))

# Check the column names and make them syntactically valid

colnames(default_data)
colnames(default_data)[1] <- "perc_by_cash"
colnames(default_data)[4] <- "late_3_to_6"
colnames(default_data)[5] <- "late_6_to_12"
colnames(default_data)[6] <- "late_12_above"
colnames(default_data)[7] <- "Marital_status"
colnames(default_data)


# This function helps to visualize data structure in network graph format.
plot_str(default_data, type="d", fontSize = 25)

colnames(default_data)
corrplot(cor(default_data[,-c(13,14,17)]))

par(mfrow = c(1,1));
corrplot(cor((default_data[,-c(13,14,17)])), method = "number", type = "upper")
str(subset_default_data )
summary(subset_default_data)


summary(default_data$default)
plot(default_data$default, col="light green", main = "Barplot of Renewal")
prop.table(table(default_data$default))
mean(default$default)
# 0.93741 - means that 94% of the customers renewed their policy
# If we were to simply predict the most frequently occurring value (1 in this case), 
# We will end up with an accuracy of 94%. 
# So our aim should be to build a model that has a higher accuracy
# Not only that, we should focus on sensitivity i.e. capturing as much of the defaulter as possible


new_vars <- colnames(default_data)
# Remove Outliers
quantile(default_data$Income,c(0.01,0.02,0.03,0.1,0.2,0.3,0.4,0.50,0.6,0.7,0.8,0.9,0.95,0.99,1)) 
# There is a significant difference at 95% and 100%, outlier treatment is required for this variable

quantile(default_data$Income, c(0.95))
default_data$Income[which(default_data$Income>450050 )]<- 450050 
quantile(default_data$no_of_premiums_paid,c(0.95))
default_data$no_of_premiums_paid[which(default_data$no_of_premiums_paid>20)] <- 20
quantile(default_data$premium,c(0.95))
default_data$premium[which(default_data$premium> 28500 )] <- 28500 
quantile(default_data$age,c(0.95))
default_data$age[which(default_data$age> 76.03836 )] <- 76.03836 



### UNIVARIATE ANALYSIS
plot_histogram(subset_default_data) #Plotting the histogram for all numerical variables
plot_density(subset_default_data, geom_density_args = list(fill="cyan", alpha = 0.4)) 
plot_boxplot(default_data, by = "default", 
             geom_boxplot_args = list("outlier.color" = "blue")) 

library(ggplot2)
library(grid)
library(gridExtra)

## visualize properties of all categorical variables


# Setting up the aesthetics
unipar = theme(legend.position = "none") + 
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 11), 
        title = element_text(size = 13, face = "bold"))

# Define color brewer
col1 = "Set2"

# Plotting the bar charts
g1=ggplot(default_data, aes(x=default, fill=default)) + geom_bar()+ unipar + scale_fill_brewer(palette=col1) +
  geom_text(aes(label = scales::percent(..prop..), group = 1), stat= "count", size = 3.3, position = position_stack(0.06))+
  geom_text(aes(label = ..count.., group = 1), stat= "count", size = 3.3, position = position_stack(0.95))

g2=ggplot(default_data, aes(x=Marital_status, fill = Marital_status))+ geom_bar()+ unipar + scale_fill_brewer(palette=col1) +
  geom_text(aes(label = scales::percent(..prop..), group = 1), stat= "count", size = 3.3, position = position_stack(0.06))+
  geom_text(aes(label = ..count.., group = 1), stat= "count", size = 3.3, position = position_stack(0.95))

g3=ggplot(default_data, aes(x=Veh_Owned, fill = Veh_Owned))+ geom_bar()+ unipar + scale_fill_brewer(palette=col1) +
  geom_text(aes(label = scales::percent(..prop..), group = 1), stat= "count", size = 3.3, position = position_stack(0.06))+
  geom_text(aes(label = ..count.., group = 1), stat= "count", size = 3.3, position = position_stack(0.95))

g4=ggplot(default_data, aes(x=sourcing_channel, fill = sourcing_channel))+ geom_bar()+ unipar + scale_fill_brewer(palette=col1) +
  geom_text(aes(label = scales::percent(..prop..), group = 1), stat= "count", size = 3.3, position = position_stack(0.06))+
  geom_text(aes(label = ..count.., group = 1), stat= "count", size = 3.3, position = position_stack(0.95))

g5=ggplot(default_data, aes(x=No_of_dep, fill = No_of_dep))+ geom_bar()+ unipar + scale_fill_brewer(palette=col1) +
  geom_text(aes(label = scales::percent(..prop..), group = 1), stat= "count", size = 3.3, position = position_stack(0.06))+
  geom_text(aes(label = ..count.., group = 1), stat= "count", size = 3.3, position = position_stack(0.95))

g6=ggplot(default_data, aes(x=residence_area_type, fill = residence_area_type))+ geom_bar()+ unipar + scale_fill_brewer(palette=col1) +
  geom_text(aes(label = scales::percent(..prop..), group = 1), stat= "count", size = 3.3, position = position_stack(0.06))+
  geom_text(aes(label = ..count.., group = 1), stat= "count", size = 3.3, position = position_stack(0.95))

g7=ggplot(default_data, aes(x=Accomodation, fill = Accomodation))+ geom_bar()+ unipar + scale_fill_brewer(palette=col1) +
  geom_text(aes(label = scales::percent(..prop..), group = 1), stat= "count", size = 3.3, position = position_stack(0.06))+
  geom_text(aes(label = ..count.., group = 1), stat= "count", size = 3.3, position = position_stack(0.95))

grid.arrange(g1,g2,g3,g4,g5,g6,g7,ncol=3)


# Setting up the aesthetics
bipar2 = theme(legend.position = "top", 
               legend.direction = "horizontal", 
               legend.title = element_text(size = 10),
               legend.text = element_text(size = 8)) + 
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 11), 
        title = element_text(size = 13, face = "bold"))
# Define color brewer
col2 = "Set2"
library(dplyr)
# default status vs categorical variables
d8 <- default_data %>% group_by(Marital_status) %>% count(default) %>% mutate(ratio=scales::percent(n/sum(n)))
p8=ggplot(default_data, aes(x=Marital_status, fill=default)) + geom_bar()+ bipar2 + scale_fill_brewer(palette=col2) +
  geom_text(data=d8, aes(y=n,label=ratio),position=position_stack(vjust=0.5))

d9 <- default_data %>% group_by(Veh_Owned) %>% count(default) %>% mutate(ratio=scales::percent(n/sum(n)))
p9=ggplot(default_data, aes(x=Veh_Owned, fill=default)) + geom_bar()+ bipar2 + scale_fill_brewer(palette=col2) +
  geom_text(data=d9, aes(y=n,label=ratio),position=position_stack(vjust=0.5))

d10 <- default_data %>% group_by(No_of_dep) %>% count(default) %>% mutate(ratio=scales::percent(n/sum(n)))
p10=ggplot(default_data, aes(x=No_of_dep, fill=default)) + geom_bar()+ bipar2 + scale_fill_brewer(palette=col2) +
  geom_text(data=d10, aes(y=n,label=ratio),position=position_stack(vjust=0.5))

d11 <- default_data %>% group_by(sourcing_channel) %>% count(default)%>% mutate(ratio=scales::percent(n/sum(n)))
p11=ggplot(default_data, aes(x=sourcing_channel, fill=default)) + geom_bar()+ bipar2 + scale_fill_brewer(palette=col2) +
  geom_text(data=d11, aes(y=n,label=ratio),position=position_stack(vjust=0.5))

d12 <- default_data %>% group_by(residence_area_type) %>% count(default)%>% mutate(ratio=scales::percent(n/sum(n)))
p12=ggplot(default_data, aes(x=residence_area_type, fill=default)) + geom_bar()+ bipar2 + scale_fill_brewer(palette=col2) +
  geom_text(data=d12, aes(y=n,label=ratio),position=position_stack(vjust=0.5))

d13 <- default_data %>% group_by(Accomodation) %>% count(default) %>% mutate(ratio=scales::percent(n/sum(n)))
p13=ggplot(default_data, aes(x=Accomodation, fill=default)) + geom_bar()+ bipar2 + scale_fill_brewer(palette=col2) +
  geom_text(data=d13, aes(y=n,label=ratio),position=position_stack(vjust=0.5))

# Partitioning the boxplots
grid.arrange(p8,p9,p10,p11,p12,p13,ncol=3)

#################### key observations   #################################################

#no significant observations found

## numerical vs numerical variables

##*## numerical vs numerical variables

# Create a subset of data with only the numeric variables
subset_default_data = default_data[, c("perc_by_cash","Income","late_3_to_6","late_6_to_12", "late_12_above","risk_score","no_of_premiums_paid", "premium", "age")] 


# Plot the correlation plot of numeric variables
library(corrplot)

par(mfrow = c(1,1));
corrplot(cor(subset_default_data ), method = "number", type = "upper")



#################### key observations   #################################################

##** No strong corelation noted among the numeric variables,


# Bivariate analysis of numerical variables with strong correlation






