############################# HR Analytics Solution ###################
################################################################
# Business Understanding
# Data Understanding
# Data Preparation & EDA
# Model Building 
# Model Evaluation
################################################################
# Business Understanding
# Problem Statement : A company is facing with high attrition rate of around 15%. Due to this attrition company is loosing lot of revenue to sustain this high attrition rate.
# As we are the top rated analytics talent in the firm, this assignment has been given to us to analyse this issue and come up with the attributes that adds to increase in attrition rate.
# We need to analyse the data and come-up with a model that would help to predict the probability of attrition of an employees.

################################################################
### Data Understanding

# Install and Load the required packages
#install.packages("MASS")
#install.packages("car")
#install.packages("e1071")
#install.packages("caret", dependencies = c("Depends", "Suggests"))
#install.packages("cowplot")
#install.packages("GGally")
#install.packages("ggplot2")
#install.packages("cowplot")
#install.packages("caTools")
#install.packages("ROCR")
#install.packages("carData")
#install.packages("lattice")
#install.packages("ggplot2")
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(data.table)
library(tidyr)
library(dplyr)
library(stringr)
library(ROCR)

#setwd("C:\\Users\\Shishir\\Desktop\\PGDDA\\HR-Analytics-Case-Study-master")
# Loading 5 files
employee_survey_data <- read.csv("employee_survey_data.csv", header = TRUE, stringsAsFactors = FALSE)
employee_general_data <- read.csv("general_data.csv", header = TRUE, stringsAsFactors = FALSE)
employee_in_time <- read.csv("in_time.csv", header = TRUE, stringsAsFactors = FALSE)
manager_survey_data <- read.csv("manager_survey_data.csv", header = TRUE, stringsAsFactors = FALSE)
employee_out_time <- read.csv("out_time.csv", header = TRUE, stringsAsFactors = FALSE)

# Check the structure of each dataset to confirm all the dataset have same number of observation
str(employee_survey_data)
str(employee_general_data)
str(employee_in_time)     
str(manager_survey_data)     
str(employee_out_time)

# Check unique variable in each dataset.
sapply(employee_survey_data, function(x) length(unique(x))) # Got EmployeeID as unique with 4410 observation.
sapply(employee_general_data, function(x) length(unique(x))) # Got EmployeeID as unique with 4410 observation.
sapply(employee_in_time, function(x) length(unique(x))) # Got X (Which is employee ID) as unique with 4410 observation.
sapply(manager_survey_data, function(x) length(unique(x))) # Got EmployeeID as unique with 4410 observation.
sapply(employee_out_time, function(x) length(unique(x))) # Got X (Which is employee ID) as unique with 4410 observation.

# Convert employee_in_time in time format 
employee_in_time <- sapply(employee_in_time, function(x) as.POSIXlt(x, origin="1970-01-01","%y-%m-%d %H:%M:%S"))
employee_in_time<-as.data.frame(employee_in_time)

# Convert employee_out_time in time format 
employee_out_time <- sapply(employee_out_time, function(x) as.POSIXlt(x, origin="1970-01-01","%y-%m-%d %H:%M:%S"))
employee_out_time<-as.data.frame(employee_out_time)
employee_in_time_00 <- employee_in_time
employee_out_time_00 <- employee_out_time

# remove column X from employee_in_time_00 & employee_out_time_00 dataset.
employee_in_time_00 <- employee_in_time_00[,-1]
employee_out_time_00 <- employee_out_time_00[,-1]

# Remove columns with value as NA from employee_in_time_00

employee_in_time_00_rmv_na <- as.data.table(employee_in_time_00)
employee_in_time_00_rmv_na_00 <- employee_in_time_00_rmv_na[,which(unlist(lapply(employee_in_time_00_rmv_na, function(x)!all(is.na(x))))),with=F]
employee_in_time_01 <- employee_in_time_00_rmv_na_00

# Remove columns with value as NA from employee_out_time_00
employee_out_time_00_rmv_na <- as.data.table(employee_out_time_00)
employee_out_time_00_rmv_na_00 <- employee_out_time_00_rmv_na[,which(unlist(lapply(employee_out_time_00_rmv_na, function(x)!all(is.na(x))))),with=F]
employee_out_time_01 <- employee_out_time_00_rmv_na_00

# Find the employee time spent at office
employee_time_spent_at_office <- employee_out_time_01 - employee_in_time_01

# Convert employee_time_spent_at_office to numeric dataframe
employee_time_spent_at_office <- lapply(employee_time_spent_at_office, as.numeric)
employee_time_spent_at_office <- as.data.frame(employee_time_spent_at_office)

employee_time_spent_at_office_01 <- employee_time_spent_at_office

# Now calculate the mean of each row and stored in one column named 'mean'
employee_time_spent_at_office_01$avg_time_spent_at_office <- apply(employee_time_spent_at_office_01,1,mean,na.rm=TRUE)

# Create a dataset with only avg_time_spent_at_office column from employee_time_spent_at_office_01 dataset.
avg_time_spent_at_office <- employee_time_spent_at_office_01[,c(250)]
avg_time_spent_at_office <- data.frame(avg_time_spent_at_office)
str(avg_time_spent_at_office)

# Now create a dataframe employee_ID which stores ID from 1 to 4410
employee_ID <- data.frame(c(1:4410))

# Rename the column name to EmployeeID
colnames(employee_ID) <- "EmployeeID"

# Now combine dataset employee_id & avg_time_spent_at_office

employee_avg_time_spent_at_office <- cbind(employee_ID, avg_time_spent_at_office)

# Check identical customer_ID across dataset
setdiff(employee_survey_data$EmployeeID,manager_survey_data$EmployeeID)  # Since setdiff is 0 this shows that both the dataset has same EmplyeeID
setdiff(manager_survey_data$EmployeeID,employee_general_data$EmployeeID) # Since setdiff is 0 this shows that both the dataset has same EmplyeeID
setdiff(employee_general_data$EmployeeID,employee_avg_time_spent_at_office$EmployeeID)  # Since setdiff is 0 this shows that both the dataset has same EmplyeeID

# Now merge all the dataframe employee_survey_data, manager_survey_data, manager_survey_data, employee_general_data & employee_avg_time_spent_at_office.
employee_data<- merge(employee_survey_data,manager_survey_data, by="EmployeeID", all = T)
employee_data<- merge(employee_data,employee_general_data, by="EmployeeID", all = T)
employee_data<- merge(employee_data,employee_avg_time_spent_at_office, by="EmployeeID", all = T)

# employee_data is the master datset.
employee_data_00 <- employee_data

# Check column EmployeeCount has only 1 as value in all the rows;
length(which(employee_data_00$EmployeeCount == 1)) # Shows 4410. Hence this column contains all row values as 1. So we need to remove this column.

# Check column StandardHours has only 8 as value in all the rows;
length(which(employee_data_00$StandardHours == 8))  # Shows 4410. Hence this column contains all row values as 8. So we need to remove this column.

# Check column Over18 has only Y as value in all the rows;
length(which(employee_data_00$Over18 == "Y"))  # Shows 4410. Hence this column contains all row values as "Y". So we need to remove this column.

# Remove EmployeeCount, StandardHours, Over18  column/variable.

employee_data_00 <- subset(employee_data_00, select = -c(EmployeeCount, Over18, StandardHours))

employee_data_01 <- employee_data_00

# Convert the Education column values with matching values as mentioned in data dictionary; This is required for EDA.
employee_data_01$Education <- ifelse(employee_data_01$Education == 1 ,"Below College",ifelse(employee_data_01$Education == 2, "College",ifelse(employee_data_01$Education == 3, "Bachelor", ifelse(employee_data_01$Education == 4, "Master", ifelse(employee_data_01$Education == 5, "Doctor", employee_data_01$Education)))))

# Convert the EnvironmentSatisfaction column values with matching values as mentioned in data dictionary; This is required for EDA.

employee_data_01$EnvironmentSatisfaction <- ifelse(employee_data_01$EnvironmentSatisfaction == 1 ,"Low",ifelse(employee_data_01$EnvironmentSatisfaction == 2, "Medium",ifelse(employee_data_01$EnvironmentSatisfaction == 3, "High", ifelse(employee_data_01$EnvironmentSatisfaction == 4, "Very High", employee_data_01$EnvironmentSatisfaction))))


# Convert the employee_data_01$JobInvolvement column values with matching values as mentioned in data dictionary; This is required for EDA.
employee_data_01$JobInvolvement <- ifelse(employee_data_01$JobInvolvement == 1 ,"Low",ifelse(employee_data_01$JobInvolvement == 2, "Medium",ifelse(employee_data_01$JobInvolvement == 3, "High", ifelse(employee_data_01$JobInvolvement == 4, "Very High", employee_data_01$JobInvolvement))))

# Convert the employee_data_01$JobSatisfaction column values with matching values as mentioned in data dictionary; This is required for EDA.
employee_data_01$JobSatisfaction <- ifelse(employee_data_01$JobSatisfaction == 1 ,"Low",ifelse(employee_data_01$JobSatisfaction == 2, "Medium",ifelse(employee_data_01$JobSatisfaction == 3, "High", ifelse(employee_data_01$JobSatisfaction == 4, "Very High", employee_data_01$JobSatisfaction))))

# Convert the employee_data_01$PerformanceRating column values with matching values as mentioned in data dictionary; This is required for EDA.
employee_data_01$PerformanceRating <- ifelse(employee_data_01$PerformanceRating == 1 ,"Low",ifelse(employee_data_01$PerformanceRating == 2, "Good",ifelse(employee_data_01$PerformanceRating == 3, "Excellent", ifelse(employee_data_01$PerformanceRating == 4, "Outstanding", employee_data_01$PerformanceRating))))

# Convert the employee_data_01$WorkLifeBalance column values with matching values as mentioned in data dictionary; This is required for EDA.
employee_data_01$WorkLifeBalance <- ifelse(employee_data_01$WorkLifeBalance == 1 ,"Bad",ifelse(employee_data_01$WorkLifeBalance == 2, "Good",ifelse(employee_data_01$WorkLifeBalance == 3, "Better", ifelse(employee_data_01$WorkLifeBalance == 4, "Best", employee_data_01$WorkLifeBalance))))

#conversion of employee_data_01 to employee_data_02
employee_data_02 <- employee_data_01
# Now plot barchart for categorical data with stacked attrition. This would show the impact of attrition on categorical variables.

bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),legend.position="none")
# Plot_01
 plot_grid(ggplot(employee_data_02, aes(x=factor(EnvironmentSatisfaction),fill=Attrition))+ geom_bar(), 
          ggplot(employee_data_02, aes(x=factor(JobSatisfaction),fill=Attrition))+ geom_bar()+bar_theme1,
	  align = "h") 
 plot_grid(ggplot(employee_data_02, aes(x=factor(WorkLifeBalance),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_data_02, aes(x=factor(JobInvolvement),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h") 
# Plot_02
 plot_grid(ggplot(employee_data_02, aes(x=factor(PerformanceRating),fill=Attrition))+ geom_bar()+bar_theme1, 
          ggplot(employee_data_02, aes(x=factor(BusinessTravel),fill=Attrition))+ geom_bar()+bar_theme1,
	  align = "h")
# Plot_03          
plot_grid(ggplot(employee_data_02, aes(x=factor(Department),fill=Attrition))+ geom_bar()+bar_theme1,
          	  ggplot(employee_data_02, aes(x=factor(Education),fill=Attrition))+ geom_bar()+bar_theme1,
          	  align = "h")
# Plot_04     
plot_grid(ggplot(employee_data_02, aes(x=factor(EducationField),fill=Attrition))+ geom_bar()+bar_theme1,
          	  ggplot(employee_data_02, aes(x=factor(Gender),fill=Attrition))+ geom_bar()+bar_theme1,
          	  align = "h")
# Plot_05a
plot_grid(ggplot(employee_data_02, aes(x=factor(JobLevel),fill=Attrition))+ geom_bar() + bar_theme1,
          	  ggplot(employee_data_02, aes(x=factor(JobRole),fill=Attrition))+ geom_bar() + bar_theme1,
          	  align = "h")
# Plot_05b
ggplot(employee_data_02, aes(x=factor(MaritalStatus),fill=Attrition))+ geom_bar() + bar_theme1

# Now plot histogram & box plot for numerical/continuous variable.


box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")
# Plot_06
plot_grid(ggplot(employee_data_02, aes(Age))+ geom_histogram(binwidth = 10),
          ggplot(employee_data_02, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

# Plot_07
plot_grid(ggplot(employee_data_02, aes(DistanceFromHome))+ geom_histogram(binwidth = 10),
          ggplot(employee_data_02, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

# Plot_08
plot_grid(ggplot(employee_data_02, aes(MonthlyIncome))+ geom_histogram(binwidth = 10),
          ggplot(employee_data_02, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
# Plot_09
plot_grid(ggplot(employee_data_02, aes(NumCompaniesWorked))+ geom_histogram(binwidth = 10),
          ggplot(employee_data_02, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
          
# Plot_10
plot_grid(ggplot(employee_data_02, aes(PercentSalaryHike))+ geom_histogram(binwidth = 10),
          ggplot(employee_data_02, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
# Plot_11

plot_grid(ggplot(employee_data_02, aes(StockOptionLevel))+ geom_histogram(binwidth = 10),
          ggplot(employee_data_02, aes(x="",y=StockOptionLevel))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

# Plot_12
plot_grid(ggplot(employee_data_02, aes(TotalWorkingYears))+ geom_histogram(binwidth = 10),
          ggplot(employee_data_02, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
# Plot_13
plot_grid(ggplot(employee_data_02, aes(YearsAtCompany))+ geom_histogram(binwidth = 10),
          ggplot(employee_data_02, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

# Plot_14
plot_grid(ggplot(employee_data_02, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 10),
          ggplot(employee_data_02, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

# Plot_15
plot_grid(ggplot(employee_data_02, aes(YearsWithCurrManager))+ geom_histogram(binwidth = 10),
          ggplot(employee_data_02, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

# Plot_16
plot_grid(ggplot(employee_data_02, aes(avg_time_spent_at_office))+ geom_histogram(binwidth = 10),
          ggplot(employee_data_02, aes(x="",y=avg_time_spent_at_office))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

# Outlier was identified with following continious variables MonthlyIncome, TotalWorkingYears, YearsAtCompany, YearsSinceLastPromotion, YearsWithCurrManager, NumCompaniesWorked, StockOptionLevel.
# Outliers treatment on the identified continious variables MonthlyIncome, TotalWorkingYears, YearsAtCompany, YearsSinceLastPromotion, YearsWithCurrManager, NumCompaniesWorked, StockOptionLevel.

# Remove outliers from MonthlyIncome variable;
box <- boxplot.stats(employee_data_02$MonthlyIncome)
out <- box$out

employee_data_03 <- employee_data_02[ !employee_data_02$MonthlyIncome %in% out, ]
employee_data_02 <- employee_data_03

# Remove outliers from TotalWorkingYears variable;
box <- boxplot.stats(employee_data_02$TotalWorkingYears)
out <- box$out
employee_data_04 <- employee_data_02[ !employee_data_02$TotalWorkingYears %in% out, ]
employee_data_02 <- employee_data_04

# Remove outliers from YearsAtCompany variable;
box <- boxplot.stats(employee_data_02$YearsAtCompany)
out <- box$out
employee_data_05 <- employee_data_02[ !employee_data_02$YearsAtCompany %in% out, ]
employee_data_02 <- employee_data_05

# Remove outliers from YearsSinceLastPromotion variable;
box <- boxplot.stats(employee_data_02$YearsSinceLastPromotion)
out <- box$out
employee_data_06 <- employee_data_02[ !employee_data_02$YearsSinceLastPromotion %in% out, ]
employee_data_02 <- employee_data_06

# Remove outliers from YearsWithCurrManager variable;
box <- boxplot.stats(employee_data_02$YearsWithCurrManager)
out <- box$out
employee_data_07 <- employee_data_02[ !employee_data_02$YearsWithCurrManager %in% out, ]
employee_data_02 <- employee_data_07

# Remove outliers from NumCompaniesWorked variable;
box <- boxplot.stats(employee_data_02$NumCompaniesWorked)
out <- box$out
employee_data_08 <- employee_data_02[ !employee_data_02$NumCompaniesWorked %in% out, ]
employee_data_02 <- employee_data_08

# Remove outliers from StockOptionLevel variable;
box <- boxplot.stats(employee_data_02$StockOptionLevel)
out <- box$out
employee_data_09 <- employee_data_02[ !employee_data_02$StockOptionLevel %in% out, ]
employee_data_02 <- employee_data_09

employee_data_10 <- employee_data_02

## Now check for missing values in employee_data_10 dataset
sapply(employee_data_10, function(x) sum(is.na(x)))

# We observed that following variables from employee_data_10 dataset contains NA. They are EnvironmentSatisfaction, JobSatisfaction,WorkLifeBalance, NumCompaniesWorked & TotalWorkingYears.
# Remove NA's from identified columns/variables;

employee_data_10 <- employee_data_10[!is.na(employee_data_10$EnvironmentSatisfaction),]
employee_data_10 <- employee_data_10[!is.na(employee_data_10$JobSatisfaction),]
employee_data_10 <- employee_data_10[!is.na(employee_data_10$WorkLifeBalance),]
employee_data_10 <- employee_data_10[!is.na(employee_data_10$NumCompaniesWorked),]
employee_data_10 <- employee_data_10[!is.na(employee_data_10$TotalWorkingYears),]

# Check again for missing values in employee_data_10 dataset
sapply(employee_data_10, function(x) sum(is.na(x))) # We observed that none of the columns/variables have NA's.

employee_data_11 <- employee_data_10

# Now perform scaling on all numeric variables of employee_data_11 dataset.

ind <- sapply(employee_data_11, is.numeric)
employee_data_11[ind] <- lapply(employee_data_11[ind], scale)

# Converting target variable Attrition from No/Yes character to factor with levels 0/1 
employee_data_11$Attrition <- ifelse(employee_data_11$Attrition == "Yes",1,0)


# Checking attrition rate for employees

attrition_rate <- sum(employee_data_11$Attrition)/nrow(employee_data_11)

attrition_rate # attrition_rate is 16.9 %.

# Creating a dataframe of categorical data
employee_data_12<- employee_data_11[,-c(1,7,11,15, 18:27)]


# Converting categorical attributes to factor
employee_data_13<- data.frame(sapply(employee_data_12, function(x) factor(x)))
str(employee_data_13)

# Creating dummy variables for factor attributes
dummies<- data.frame(sapply(employee_data_13, function(x) data.frame(model.matrix(~x-1,data =employee_data_13))[,-1]))

# Therefore the final dataset be;
employee_data_final<- cbind(employee_data_11[,c(7,11,15, 18:27)],dummies)
View(employee_data_final) # observation: 2858 of 51 variables

#######################################################################################################################

# splitting the data between train and test

set.seed(100)

indices = sample.split(employee_data_final$Attrition, SplitRatio = 0.7)

train = employee_data_final[indices,]

test = employee_data_final[!(indices),]

########################################################################################################################

## Now start building model using logistic regression

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC 1341.1

# Stepwise selection
model_2<- stepAIC(model_1, direction="both")
summary(model_2) # AIC : 1312.6

# Removing multicollinearity through VIF check

# model_3 (Excluding: EducationField.xLife.Sciences)

model_3 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
    TrainingTimesLastYear + YearsWithCurrManager + avg_time_spent_at_office + 
    EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
    JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
    WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xMedium + 
    JobInvolvement.xVery.High + BusinessTravel.xTravel_Frequently + 
    Department.xSales + Education.xDoctor + 
    EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
    EducationField.xTechnical.Degree + JobRole.xLaboratory.Technician + 
    JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
    JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle, 
    family = "binomial", data = train)
summary(model_3) # AIC: 1318.3
vif(model_3)

# model_4 (Excluding: WorkLifeBalance.xGood)

model_4 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
    TrainingTimesLastYear + YearsWithCurrManager + avg_time_spent_at_office + 
    EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
    JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
    WorkLifeBalance.xBetter + JobInvolvement.xMedium + 
    JobInvolvement.xVery.High + BusinessTravel.xTravel_Frequently + 
    Department.xSales + Education.xDoctor + 
    EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
    EducationField.xTechnical.Degree + JobRole.xLaboratory.Technician + 
    JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
    JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle, 
    family = "binomial", data = train)
summary(model_4) # AIC: 1324.6
vif(model_4)

# model_5 (Excluding: MaritalStatus.xMarried)

model_5 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
    TrainingTimesLastYear + YearsWithCurrManager + avg_time_spent_at_office + 
    EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
    JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
    WorkLifeBalance.xBetter + JobInvolvement.xMedium + 
    JobInvolvement.xVery.High + BusinessTravel.xTravel_Frequently + 
    Department.xSales + Education.xDoctor + 
    EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
    EducationField.xTechnical.Degree + JobRole.xLaboratory.Technician + 
    JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
    JobRole.xSales.Executive + MaritalStatus.xSingle, 
    family = "binomial", data = train)
summary(model_5) # AIC: 1330.1
vif(model_5)

# model_6 (Excluding: WorkLifeBalance.xBest)

model_6 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
    TrainingTimesLastYear + YearsWithCurrManager + avg_time_spent_at_office + 
    EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
    JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
    WorkLifeBalance.xBetter + JobInvolvement.xMedium + 
    JobInvolvement.xVery.High + BusinessTravel.xTravel_Frequently + 
    Department.xSales + Education.xDoctor + 
    EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
    EducationField.xTechnical.Degree + JobRole.xLaboratory.Technician + 
    JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
    JobRole.xSales.Executive + MaritalStatus.xSingle, 
    family = "binomial", data = train)
summary(model_6) # AIC: 1328.1
vif(model_6)

# model_7 (Excluding: JobInvolvement.xVery.High)

model_7 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
    TrainingTimesLastYear + YearsWithCurrManager + avg_time_spent_at_office + 
    EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
    JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
    WorkLifeBalance.xBetter + JobInvolvement.xMedium + 
    BusinessTravel.xTravel_Frequently + 
    Department.xSales + Education.xDoctor + 
    EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
    EducationField.xTechnical.Degree + JobRole.xLaboratory.Technician + 
    JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
    JobRole.xSales.Executive + MaritalStatus.xSingle, 
    family = "binomial", data = train)
summary(model_7) # AIC: 1327.9
vif(model_7)

# model_8 (Excluding: JobInvolvement.xMedium)

model_8 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
    TrainingTimesLastYear + YearsWithCurrManager + avg_time_spent_at_office + 
    EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
    JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
    WorkLifeBalance.xBetter + 
    BusinessTravel.xTravel_Frequently + 
    Department.xSales + Education.xDoctor + 
    EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
    EducationField.xTechnical.Degree + JobRole.xLaboratory.Technician + 
    JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
    JobRole.xSales.Executive + MaritalStatus.xSingle, 
    family = "binomial", data = train)
summary(model_8) # AIC: 1328.5
vif(model_8)

# model_9 (Excluding: Education.xDoctor)

model_9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
    TrainingTimesLastYear + YearsWithCurrManager + avg_time_spent_at_office + 
    EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
    JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
    WorkLifeBalance.xBetter + 
    BusinessTravel.xTravel_Frequently + 
    Department.xSales +
    EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
    EducationField.xTechnical.Degree + JobRole.xLaboratory.Technician + 
    JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
    JobRole.xSales.Executive + MaritalStatus.xSingle, 
    family = "binomial", data = train)
summary(model_9) # AIC:1328.5
vif(model_9)

# model_10 (Excluding: EducationField.xMarketing)

model_10 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
    TrainingTimesLastYear + YearsWithCurrManager + avg_time_spent_at_office + 
    EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
    JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
    WorkLifeBalance.xBetter + 
    BusinessTravel.xTravel_Frequently + 
    Department.xSales +
    EducationField.xMedical + EducationField.xOther + 
    EducationField.xTechnical.Degree + JobRole.xLaboratory.Technician + 
    JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
    JobRole.xSales.Executive + MaritalStatus.xSingle, 
    family = "binomial", data = train)
summary(model_10) # AIC: 1327.2
vif(model_10)

# model_11 (Excluding: EducationField.xMedical)

model_11 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
    TrainingTimesLastYear + YearsWithCurrManager + avg_time_spent_at_office + 
    EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
    JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
    WorkLifeBalance.xBetter + 
    BusinessTravel.xTravel_Frequently + 
    Department.xSales +
    EducationField.xOther + 
    EducationField.xTechnical.Degree + JobRole.xLaboratory.Technician + 
    JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
    JobRole.xSales.Executive + MaritalStatus.xSingle, 
    family = "binomial", data = train)
summary(model_11) # AIC: 1326.7
vif(model_11)


# model_12 (Excluding: TrainingTimesLastYear)

model_12 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
    YearsWithCurrManager + avg_time_spent_at_office + 
    EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
    JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
    WorkLifeBalance.xBetter + 
    BusinessTravel.xTravel_Frequently + 
    Department.xSales +
    EducationField.xOther + 
    EducationField.xTechnical.Degree + JobRole.xLaboratory.Technician + 
    JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
    JobRole.xSales.Executive + MaritalStatus.xSingle, 
    family = "binomial", data = train)
summary(model_12) # AIC: 1330.8
vif(model_12)

# model_13 (Excluding: EducationField.xOther)

model_13 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
    YearsWithCurrManager + avg_time_spent_at_office + 
    EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
    JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
    WorkLifeBalance.xBetter + 
    BusinessTravel.xTravel_Frequently + 
    Department.xSales +
    EducationField.xTechnical.Degree + JobRole.xLaboratory.Technician + 
    JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
    JobRole.xSales.Executive + MaritalStatus.xSingle, 
    family = "binomial", data = train)
summary(model_13) # AIC: 1333.8
vif(model_13)

# model_14 (Excluding: EducationField.xTechnical.Degree)

model_14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
    YearsWithCurrManager + avg_time_spent_at_office + 
    EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
    JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
    WorkLifeBalance.xBetter + 
    BusinessTravel.xTravel_Frequently + 
    Department.xSales +
    JobRole.xLaboratory.Technician + 
    JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
    JobRole.xSales.Executive + MaritalStatus.xSingle, 
    family = "binomial", data = train)
summary(model_14) # AIC: 1335.9
vif(model_14)

# model_15 (Excluding: Department.xSales)

model_15 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
    YearsWithCurrManager + avg_time_spent_at_office + 
    EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
    JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
    WorkLifeBalance.xBetter + 
    BusinessTravel.xTravel_Frequently + 
    JobRole.xLaboratory.Technician + 
    JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
    JobRole.xSales.Executive + MaritalStatus.xSingle, 
    family = "binomial", data = train)
summary(model_15) # AIC: 1341.5
vif(model_15)

# model_16 (Excluding: JobRole.xLaboratory.Technician)

model_16 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
    YearsWithCurrManager + avg_time_spent_at_office + 
    EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
    JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
    WorkLifeBalance.xBetter + 
    BusinessTravel.xTravel_Frequently + 
    JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
    JobRole.xSales.Executive + MaritalStatus.xSingle, 
    family = "binomial", data = train)
summary(model_16) # AIC: 1348.5 
vif(model_16)

# model_17 (Excluding: JobRole.xResearch.Director)

model_17 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
    YearsWithCurrManager + avg_time_spent_at_office + 
    EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
    JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
    WorkLifeBalance.xBetter + 
    BusinessTravel.xTravel_Frequently + 
    JobRole.xResearch.Scientist + 
    JobRole.xSales.Executive + MaritalStatus.xSingle, 
    family = "binomial", data = train)
summary(model_17) # AIC: 1354.2 
vif(model_17)


# model_18 (Excluding: JobRole.xSales.Executive)

model_18 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
    YearsWithCurrManager + avg_time_spent_at_office + 
    EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
    JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
    WorkLifeBalance.xBetter + 
    BusinessTravel.xTravel_Frequently + 
    JobRole.xResearch.Scientist + 
    MaritalStatus.xSingle, 
    family = "binomial", data = train)
summary(model_18) # AIC: 1360.9 
vif(model_18)

########################################################################
# With 13 significant variables in the model

final_model<- model_18

#######################################################################

### Model Evaluation

### Test Data ####

#Predicted probabilities of Attrition 1 for test data

test_pred = predict(final_model, type = "response", newdata = test[,-27])
summary(test_pred)

test$prob <- test_pred
View(test)

# Let's use the probability cutoff of 50%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_actual_attrition,test_pred_attrition)
test_conf <- confusionMatrix(test_pred_attrition,test_actual_attrition, positive = "Yes")
test_conf
# Now lets try with probability cutoff of 40%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_actual_attrition,test_pred_attrition)
test_conf <- confusionMatrix(test_pred_attrition,test_actual_attrition, positive = "Yes")
test_conf

#Finding the Optimal Probability Cutoff

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}
# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 
dev.off()

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


# Let's choose a cutoff value of 0.1695 for final model. This is the attrition rate calculated for employee dataset.

test_cutoff_attrition<- factor(ifelse(test_pred >=0.1695, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")
conf_final
acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

#           Accuracy    : 0.769
#           Sensitivity : 0.792     
#           Specificity : 0.764

### KS -Statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)



#On testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])
ks_table_test
max(ks_table_test)



# Lift & Gain chart 


lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)
Attrition_decile

plot_grid(ggplot(Attrition_decile,aes(x=Attrition_decile$bucket,y=Attrition_decile$Gain, color=""))+geom_line()+geom_point(),
          ggplot(Attrition_decile,aes(x=Attrition_decile$bucket,y=Attrition_decile$Cumlift))+geom_line()+geom_point(), 
          align = "h",ncol = 1)




