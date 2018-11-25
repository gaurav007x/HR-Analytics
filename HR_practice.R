
#Loading libraries
library(MASS)
library(car)
library(e1071)
library(caret)
library(cowplot)
library(caTools)
library(stringr)
library(lubridate)
library(dplyr)
library(ROCR)



genD<-read.csv("general_data.csv")
esd<-read.csv("employee_survey_data.csv")
msd<-read.csv("manager_survey_data.csv")
#merge all three
setdiff(genD$EmployeeID, esd$EmployeeID)#IDs are same
all<-merge(genD, esd, by="EmployeeID")
setdiff(genD$EmployeeID, msd$EmployeeID)#IDs are same
all<-merge(all, msd, by="EmployeeID")
#check structure and summary of the dataframe
str(all)
summary(all)
#check if duplicate rows or NA values are present
sum(is.na(all))
sum(duplicated(all))
#convert numeric variables to numeric
all[,c(2, 6, 9, 14, 15, 17, 18, 19, 20, 21, 22, 23, 24)]<-sapply(all[,c(2, 6, 9, 14, 15, 17, 18, 19, 20, 21, 22, 23, 24)], as.numeric)
all[,c(3, 4, 5, 7, 8, 10, 11, 12, 13, 16, 25, 26, 27, 28, 29)]<-lapply(all[,c(3, 4, 5, 7, 8, 10, 11, 12, 13, 16, 25, 26, 27, 28, 29)], as.factor)
str(all)
#variable wise analysis
#categorical variables

#Attrition
table(all$Attrition)
sum(is.na(all$Attrition))

#BusinessTravel
table(all$BusinessTravel)
sum(is.na(all$BusinessTravel))

#Department
table(all$Department)
sum(is.na(all$Department))

#Education
table(all$Education)
sum(is.na(all$Education))

#EducationField
table(all$EducationField)
sum(is.na(all$EducationField))

#Gender
table(all$Gender)
sum(is.na(all$Gender))

#JobLevel
table(all$JobLevel)
sum(is.na(all$JobLevel))

#JobRole
table(all$JobRole)
sum(is.na(all$JobRole))

#MaritalStatus
table(all$MaritalStatus)
sum(is.na(all$MaritalStatus))

#Over18
table(all$Over18)
sum(is.na(all$Over18))
all<-all[,colnames(all)!="Over18"]#Only single value present

#EnvironmentSatisfaction
table(all$EnvironmentSatisfaction)
sum(is.na(all$EnvironmentSatisfaction))
all$EnvironmentSatisfaction[is.na(all$EnvironmentSatisfaction)]<-which.max(summary(all$EnvironmentSatisfaction))#Removing NAs with maximum frequency value

#JobSatisfaction
table(all$JobSatisfaction)
sum(is.na(all$JobSatisfaction))
all$JobSatisfaction[is.na(all$JobSatisfaction)]<-which.max(summary(all$JobSatisfaction))#Removing NAs with maximum frequency value

#WorkLifeBalance
table(all$WorkLifeBalance)
sum(is.na(all$WorkLifeBalance))
all$WorkLifeBalance[is.na(all$WorkLifeBalance)]<-which.max(summary(all$WorkLifeBalance))#Removing NAs with maximum frequency value

#JobInvolvement
table(all$JobInvolvement)
sum(is.na(all$JobInvolvement))

#PerformanceRating
table(all$PerformanceRating)
sum(is.na(all$PerformanceRating))

#working on numeric variables

#Age
boxplot(all$Age)
sum(is.na(all$Age))

#DistanceFromHome
boxplot(all$DistanceFromHome)
sum(is.na(all$DistanceFromHome))

#EmployeeCount
boxplot(all$EmployeeCount)
sum(is.na(all$EmployeeCount))
all<-all[,colnames(all)!="EmployeeCount"]#Only single value present

#MonthlyIncome
boxplot(all$MonthlyIncome)
sum(is.na(all$MonthlyIncome))
stat1<-quantile(all$MonthlyIncome, 0.92)
all$MonthlyIncome[all$MonthlyIncome>stat1]<-stat1

#NumCompaniesWorked
boxplot(all$NumCompaniesWorked)
sum(is.na(all$NumCompaniesWorked))
stat2<-quantile(all$NumCompaniesWorked, 0.95, na.rm = TRUE)
all$NumCompaniesWorked[all$NumCompaniesWorked>stat2]<-stat2
all$NumCompaniesWorked[is.na(all$NumCompaniesWorked)]<-median(all$NumCompaniesWorked, na.rm = TRUE)

#PercentSalaryHike
boxplot(all$PercentSalaryHike)
sum(is.na(all$PercentSalaryHike))

#StandardHours
boxplot(all$StandardHours)
sum(is.na(all$StandardHours))
all<-all[,colnames(all)!="StandardHours"]#Only single value present

#StockOptionLevel
boxplot(all$StockOptionLevel)
sum(is.na(all$StockOptionLevel))

#TotalWorkingYears
boxplot(all$TotalWorkingYears)
sum(is.na(all$TotalWorkingYears))
stat3<-quantile(all$TotalWorkingYears, 0.95, na.rm = TRUE)
all$TotalWorkingYears[all$TotalWorkingYears>stat3]<-stat3
all$TotalWorkingYears[is.na(all$TotalWorkingYears)]<-median(all$TotalWorkingYears, na.rm = TRUE)

#TrainingTimesLastYear
boxplot(all$TrainingTimesLastYear)
sum(is.na(all$TrainingTimesLastYear))
#Not removing the Outliers because span is very low

#YearsAtCompany
boxplot(all$YearsAtCompany)
sum(is.na(all$YearsAtCompany))
stat4<-quantile(all$YearsAtCompany, 0.92, na.rm = TRUE)
all$YearsAtCompany[all$YearsAtCompany>stat4]<-stat4

#YearsSinceLastPromotion
boxplot(all$YearsSinceLastPromotion)
sum(is.na(all$YearsSinceLastPromotion))
#not removing outliers because range is very low

#YearsWithCurrManager
boxplot(all$YearsWithCurrManager)
sum(is.na(all$YearsWithCurrManager))
stat5<-quantile(all$YearsWithCurrManager, 0.99, na.rm = TRUE)
all$YearsWithCurrManager[all$YearsWithCurrManager>stat5]<-stat5

sum(is.na(all))

#scale all the numeric variables
all[,c(2, 6, 13, 14, 15, 16, 17, 18, 19, 20, 21)]<-scale(all[,c(2, 6, 13, 14, 15, 16, 17, 18, 19, 20, 21)])

#creating dummy variables

#changing two levels of Attrition to 0 and 1
levels(all$Attrition)<-c(0,1)
all$Attrition<-as.numeric(levels(all$Attrition))[all$Attrition]

#changing two levels of Gender to 0 and 1
levels(all$Gender)<-c(0,1)
all$Gender<-as.numeric(levels(all$Gender))[all$Gender]

#changing two levels of PerformanceRating to 0 and 1
levels(all$PerformanceRating)<-c(0,1)
all$PerformanceRating<-as.numeric(levels(all$PerformanceRating))[all$PerformanceRating]

#creating dummies for all other factors

all<-cbind(all[,-c(4, 5, 7, 8, 10, 11, 12, 22, 23, 24, 25)], data.frame(sapply(all[,c(4, 5, 7, 8, 10, 11, 12, 22, 23, 24, 25)], function(x){
  model.matrix(~x, data=all[,c(4, 5, 7, 8, 10, 11, 12, 22, 23, 24, 25)])[,-1]
})))





####Loading in_time and out_time datasets####
in_time <- read.csv("in_time.csv",stringsAsFactors = FALSE)
out_time <- read.csv("out_time.csv",stringsAsFactors = FALSE)



#### Cleaning in_time and out_time####
#First Column Name
colnames(in_time)[1] <- "CustomerID"
colnames(out_time)[1] <- "CustomerID"

#Checking na values in both data set
in_na<-which(is.na(in_time))
out_na<-which(is.na(out_time))
sum(in_na!=out_na)


customer_id <- in_time$CustomerID

in_time1 <- in_time[,-1]
out_time1 <- out_time[,-1]

#converting into date-time format

for(i in 1:ncol(in_time1)){
  
  in_time1[,i] <- as.POSIXct(in_time1[,i], format = "%Y-%m-%d %H:%M:%S")
  
}

for(i in 1:ncol(out_time1)){
  
  out_time1[,i] <- as.POSIXct(out_time1[,i], format = "%Y-%m-%d %H:%M:%S")
  
}




#Creating data frame for average office time storage
ave_working_hour <- data.frame(matrix(nrow = nrow(in_time1), ncol = 12))
colnames(ave_working_hour)<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
ave_working_hour[is.na(ave_working_hour)] <- 0

#Creating difference dataframe for calculating difference in time.

difference<-(out_time1-in_time1)*60
difference[is.na(difference)] <- 0
difference<-mutate_all(difference, function(x) as.numeric(as.character(x)))

#Creating column name for 'FOR' Condition
colname<-as.Date(substring(colnames(in_time1),2),format = "%Y.%m.%d")
sum(is.na(colname))

#population of monthly avg dataset 
for (i in 1:nrow(in_time1)) {
  sum=difference[i,1]
  Days=0
  
  for (j in 2:ncol(in_time1)) {
    
    { if((month(colname[j])!=month(colname[j-1]))| j==ncol(in_time1))
    {
      ave_working_hour[i,month(colname[j-1])]=sum/Days
      
      sum=difference[i,j]
      
      Days=0
      
      
    }
      

      
      else{
        sum=sum+difference[i,j]
        Days=Days+1
      }
    }   
    
  } 
}

ave_working_hour<-cbind(customer_id,ave_working_hour)

#merging average office times with all dataset
                 
setdiff(all$EmployeeID, ave_working_hour$customer_id)#IDs are same
all_updated<-merge(x=all,y=ave_working_hour,by.x = "EmployeeID",by.y = "customer_id")
str(all_updated)



#data cleanup for new variables
boxplot(all_updated$Jan)
sum(is.na(all_updated$Jan))
stat6<-quantile(all_updated$Jan, 0.95, na.rm = TRUE)
all_updated$Jan[all_updated$Jan>stat6]<-stat6


boxplot(all_updated$Feb)
sum(is.na(all_updated$Feb))
stat7<-quantile(all_updated$Feb, 0.95, na.rm = TRUE)
all_updated$Feb[all_updated$Feb>stat7]<-stat7


boxplot(all_updated$Mar)
sum(is.na(all_updated$Mar))
stat8<-quantile(all_updated$Mar, 0.95, na.rm = TRUE)
all_updated$Mar[all_updated$Mar>stat8]<-stat8


boxplot(all_updated$Apr)
sum(is.na(all_updated$Apr))
stat9<-quantile(all_updated$Apr, 0.95, na.rm = TRUE)
all_updated$Apr[all_updated$Apr>stat9]<-stat9



boxplot(all_updated$May)
sum(is.na(all_updated$May))
stat10<-quantile(all_updated$May, 0.95, na.rm = TRUE)
all_updated$May[all_updated$May>stat10]<-stat10


boxplot(all_updated$Jun)
sum(is.na(all_updated$Jun))
stat11<-quantile(all_updated$Jun, 0.95, na.rm = TRUE)
all_updated$Jun[all_updated$Jun>stat11]<-stat11


boxplot(all_updated$Jul)
sum(is.na(all_updated$Jul))
stat12<-quantile(all_updated$Jul, 0.95, na.rm = TRUE)
all_updated$Jul[all_updated$Jul>stat12]<-stat12


boxplot(all_updated$Aug)
sum(is.na(all_updated$Aug))
stat13<-quantile(all_updated$Aug, 0.95, na.rm = TRUE)
all_updated$Aug[all_updated$Aug>stat13]<-stat13


boxplot(all_updated$Sep)
sum(is.na(all_updated$Sep))
stat14<-quantile(all_updated$Sep, 0.95, na.rm = TRUE)
all_updated$Sep[all_updated$Sep>stat14]<-stat14


boxplot(all_updated$Oct)
sum(is.na(all_updated$Oct))
stat15<-quantile(all_updated$Oct, 0.95, na.rm = TRUE)
all_updated$Oct[all_updated$Oct>stat15]<-stat15


boxplot(all_updated$Nov)
sum(is.na(all_updated$Nov))
stat16<-quantile(all_updated$Nov, 0.95, na.rm = TRUE)
all_updated$Nov[all_updated$Nov>stat16]<-stat16


boxplot(all_updated$Dec)
sum(is.na(all_updated$Dec))
stat17<-quantile(all_updated$Dec, 0.95, na.rm = TRUE)
all_updated$Dec[all_updated$Dec>stat17]<-stat17


#scaling of the new variables
all_updated$Jan<-scale(all_updated$Jan)
all_updated$Feb<-scale(all_updated$Feb)
all_updated$Mar<-scale(all_updated$Mar)
all_updated$Apr<-scale(all_updated$Apr)
all_updated$May<-scale(all_updated$May)
all_updated$Jun<-scale(all_updated$Jun)
all_updated$Jul<-scale(all_updated$Jul)
all_updated$Aug<-scale(all_updated$Aug)
all_updated$Sep<-scale(all_updated$Sep)
all_updated$Oct<-scale(all_updated$Oct)
all_updated$Nov<-scale(all_updated$Nov)
all_updated$Dec<-scale(all_updated$Dec)





#dividing the data set into training and testing data
set.seed(100)
indices=sample.split(all_updated$Attrition,SplitRatio=0.7)
train=all_updated[indices,]
test=all_updated[!(indices),]
str(all_updated)

#modelling
model_1<-glm(Attrition~.-1,data=train,family ="binomial")
summary(model_1)


#creating optimal model automatically with stepAIC
model_2<-stepAIC(model_1,direction="both")
summary(model_2)
vif(model_2)


model_3<-glm(Attrition ~ Age + DistanceFromHome + MonthlyIncome + NumCompaniesWorked + 
               StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + Education.x5 + EducationField.xMarketing + 
               EducationField.xOther + EducationField.xTechnical.Degree + 
               JobLevel.x2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
               JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
               MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3 + Jan + Feb + Jul + 
               Sep + Oct ,data=train,family = "binomial")

summary(model_3)
vif(model_3)

#Removal of stockoptionlevel
model_4<-glm(Attrition ~ Age + DistanceFromHome + MonthlyIncome + NumCompaniesWorked + 
               TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + Education.x5 + EducationField.xMarketing + 
               EducationField.xOther + EducationField.xTechnical.Degree + 
               JobLevel.x2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
               JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
               MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3 + Jan + Feb + Jul + 
               Sep + Oct ,data=train,family = "binomial")
summary(model_4)
vif(model_4)


#Removal of Educatio.x5
model_5<-glm(Attrition ~ Age + DistanceFromHome + MonthlyIncome + NumCompaniesWorked + 
               TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + EducationField.xMarketing + 
               EducationField.xOther + EducationField.xTechnical.Degree + 
               JobLevel.x2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
               JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
               MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3 + Jan + Feb + Jul + 
               Sep + Oct ,data=train,family = "binomial")
summary(model_5)
vif(model_5)

#Removal of EducationField.xMarketing
model_6<-glm(Attrition ~ Age + DistanceFromHome + MonthlyIncome + NumCompaniesWorked + 
               TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales +EducationField.xOther + EducationField.xTechnical.Degree + 
               JobLevel.x2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
               JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
               MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3 + Jan + Feb + Jul + 
               Sep + Oct ,data=train,family = "binomial")
summary(model_6)
vif(model_6)

#Removal of EducationField.xOther
model_7<-glm(Attrition ~ Age + DistanceFromHome + MonthlyIncome + NumCompaniesWorked + 
               TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales+ EducationField.xTechnical.Degree + 
               JobLevel.x2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
               JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
               MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3 + Jan + Feb + Jul + 
               Sep + Oct ,data=train,family = "binomial")
summary(model_7)
vif(model_7)

#Removal of EducationField.xTechnical.Degree
model_8<-glm(Attrition ~ Age + DistanceFromHome + MonthlyIncome + NumCompaniesWorked + 
               TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales+JobLevel.x2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
               JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
               MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3 + Jan + Feb + Jul + 
               Sep + Oct ,data=train,family = "binomial")
summary(model_8)
vif(model_8)


#Removal of Jan
model_9<-glm(Attrition ~ Age + DistanceFromHome + MonthlyIncome + NumCompaniesWorked + 
               TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales+JobLevel.x2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
               JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
               MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3 + Feb + Jul + 
               Sep + Oct ,data=train,family = "binomial")
summary(model_9)
vif(model_9)


#Removal of DistanceFromHome
model_10<-glm(Attrition ~ Age+ MonthlyIncome + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales+JobLevel.x2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + Feb + Jul + 
                Sep + Oct ,data=train,family = "binomial")
summary(model_10)
vif(model_10)

#Removal of Feb
model_11<-glm(Attrition ~ Age+ MonthlyIncome + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales+JobLevel.x2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3+ Jul + 
                Sep + Oct ,data=train,family = "binomial")
summary(model_11)
vif(model_11)


#Removal of MonthlyIncome 
model_12<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales+JobLevel.x2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3+ Jul + 
                Sep + Oct ,data=train,family = "binomial")
summary(model_12)
vif(model_12)



#Removal of JobLevel.x2 
model_13<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3+ Jul + 
                Sep + Oct ,data=train,family = "binomial")
summary(model_13)
vif(model_13)



#Removal of JobRole.xLaboratory.Technician  
model_14<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3+ Jul + 
                Sep + Oct ,data=train,family = "binomial")
summary(model_14)
vif(model_14)

#Removal of JobRole.xResearch.Scientist
model_15<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + JobRole.xResearch.Director + 
                JobRole.xSales.Executive + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3+ Jul + 
                Sep + Oct ,data=train,family = "binomial")
summary(model_15)
vif(model_15)

#Removal of JobInvolvement.x3  
model_16<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + JobRole.xResearch.Director + 
                JobRole.xSales.Executive + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + Jul +Sep + Oct ,data=train,family = "binomial")
summary(model_16)
vif(model_16)


#Removal of JobRole.xResearch.Director
model_17<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales  + 
                JobRole.xSales.Executive + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + Jul +Sep + Oct ,data=train,family = "binomial")
summary(model_17)
vif(model_17)


#Removal of JobRole.xSales.Executive   
model_18<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales+MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + Jul +Sep + Oct ,data=train,family = "binomial")
summary(model_18)
vif(model_18)

#Removal of JobSatisfaction.x3   
model_19<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales+MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + Jul +Sep + Oct ,data=train,family = "binomial")
summary(model_19)
vif(model_19)

#Removal of JobSatisfaction.x2  
model_20<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales+MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 +JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + Jul +Sep + Oct ,data=train,family = "binomial")
summary(model_20)
vif(model_20)

#Removal of WorkLifeBalance.x4   
model_21<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales+MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 +JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                Jul +Sep + Oct ,data=train,family = "binomial")
summary(model_21)
vif(model_21)

#Removal of WorkLifeBalance.x2  
model_22<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales+MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 +JobSatisfaction.x4 +WorkLifeBalance.x3 + 
                Jul +Sep + Oct ,data=train,family = "binomial")
summary(model_22)
vif(model_22)




#Removal of  Jul
model_23<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales+MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 +JobSatisfaction.x4 +WorkLifeBalance.x3 + 
                Sep + Oct ,data=train,family = "binomial")
summary(model_23)
vif(model_23)


#Removal of   Sep 
model_24<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales+MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 +JobSatisfaction.x4 +WorkLifeBalance.x3 + 
                Oct ,data=train,family = "binomial")
summary(model_24)
vif(model_24)


#Removal of  BusinessTravel.xTravel_Frequently 
model_25<-glm(Attrition ~ Age + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales+MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 +JobSatisfaction.x4 +WorkLifeBalance.x3 + 
                 Oct ,data=train,family = "binomial")
summary(model_25)
vif(model_25)


#Removal of  BusinessTravel.xTravel_Rarely 
model_26<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                 Department.xResearch...Development + 
                Department.xSales+MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 +JobSatisfaction.x4 +WorkLifeBalance.x3 + 
                Oct ,data=train,family = "binomial")
summary(model_26)
vif(model_26)


#Removal of WorkLifeBalance.x3
model_27<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Department.xResearch...Development + 
                Department.xSales+MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 +JobSatisfaction.x4 + 
                Oct ,data=train,family = "binomial")
summary(model_27)
vif(model_27)


#Removal of Department.xResearch...Development  
model_28<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Department.xSales+MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 +JobSatisfaction.x4 + 
                Oct ,data=train,family = "binomial")
summary(model_28)
vif(model_28)


#Removal of   Department.xSales 
model_29<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 +JobSatisfaction.x4 + 
                Oct ,data=train,family = "binomial")
summary(model_29)
vif(model_29)


#Removal of   TotalWorkingYears 
model_30<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TrainingTimesLastYear+YearsSinceLastPromotion + YearsWithCurrManager + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 +JobSatisfaction.x4 + 
                Oct ,data=train,family = "binomial")
summary(model_30)
vif(model_30)


#Evaluation of model

Predict_2 <- predict(model_30,type="response",test[,-1])
summary(Predict_2)
test$prob<-Predict_2
View(test)

test_pred_attrition<-factor(ifelse(Predict_2>0.158,"yes","no"))
test_actual_attrition<-factor(ifelse(test$Attrition==1,"yes","no"))

table_1<-table(test_actual_attrition,test_pred_attrition)

#calculation of confusion matrix
test_conf<-confusionMatrix(test_pred_attrition,test_actual_attrition,positive = "yes")
conf_values<-test_conf
conf_values


#calculation of KS statistic and lift and gain chart

test_pred_attr<-ifelse(test_pred_attrition=="yes",1,0)
test_actual_attr<-ifelse(test_actual_attrition=="yes",1,0)
View(test_pred_attrition)
pred_test<- prediction(test_pred_attr, test_actual_attr)

performance_test<- performance(pred_test, "tpr", "fpr")

ks_table_test <- attr(performance_test, "y.values")[[1]] - 
  (attr(performance_test, "x.values")[[1]])

max(ks_table_test)
#function for finding lift and gain
lift <- function(labels , predict,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predict)) predict <- as.integer(as.character(predict))
  helper = data.frame(cbind(labels , predict))
  helper[,"bucket"] = ntile(-helper[,"predict"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

attr_decile = lift(test_actual_attr, Predict_2, groups = 10)
View(attr_decile)


gain_plot<-ggplot(attr_decile,aes(as.factor(attr_decile$bucket),attr_decile$Gain,group=1,
        label=sprintf("%0.2f", round(Gain, digits = 2))))+ geom_line()+geom_point()+
      geom_text(aes(hjust=0, vjust=1))+xlab("Bucket") +
  ylab("Gain")+labs(title="Gain Chart")



lift_plot<-ggplot(attr_decile,aes(as.factor(attr_decile$bucket),attr_decile$Cumlift,group=1,
                        label=sprintf("%0.2f", round(attr_decile$Cumlift, digits = 2))))+ geom_line()+geom_point()+
  geom_text(aes(hjust=0, vjust=-.2))+geom_hline(yintercept = 1)+xlab("Bucket") +
  ylab("Cumlift")+labs(title="Lift Chart")
