genD<-read.csv("general_data.csv")
esd<-read.csv("employee_survey_data.csv")
msd<-read.csv("manager_survey_data.csv")
#merge all three
setdiff(genD$CustomerID, esd$CustomerID)#IDs are same
all<-merge(genD, esd, by="EmployeeID")
setdiff(genD$CustomerID, msd$CustomerID)#IDs are same
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

library(MASS)
library(car)
library(e1071)
library(caret)
library(cowplot)
library(caTools)


set.seed(100)
indices=sample.split(all$Attrition,SplitRatio=0.7)
train=all[indices,]
test=all[!(indices),]
str(all)


model_1<-glm(Attrition~.-1,data=train,family ="binomial")
summary(model_1)


model_2<-stepAIC(model_1,direction="both")
summary(model_2)
vif(model_2)


model_3<-glm(Attrition ~ Age + DistanceFromHome + MonthlyIncome + NumCompaniesWorked + 
               StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + Education.x3 + Education.x4 + Education.x5 + 
               EducationField.xLife.Sciences + EducationField.xMarketing + 
               EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
               JobLevel.x2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
               JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
               MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3 ,data=train,family = "binomial")

summary(model_3)
vif(model_3)

#Removal of stockoptionlevel
model_4<-glm(Attrition ~ Age + DistanceFromHome + MonthlyIncome + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + Education.x3 + Education.x4 + Education.x5 + 
               EducationField.xLife.Sciences + EducationField.xMarketing + 
               EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
               JobLevel.x2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
               JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
               MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3 ,data=train,family = "binomial")
summary(model_4)


#Removal of Educatio.x4
model_5<-glm(Attrition ~ Age + DistanceFromHome + MonthlyIncome + NumCompaniesWorked + 
               TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + Education.x3 + Education.x5 + 
               EducationField.xLife.Sciences + EducationField.xMarketing + 
               EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
               JobLevel.x2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
               JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
               MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3 ,data=train,family = "binomial")
summary(model_5)


#Removal of Educatio.x3
model_6<-glm(Attrition ~ Age + DistanceFromHome + MonthlyIncome + NumCompaniesWorked + 
               TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales+ Education.x5 + 
               EducationField.xLife.Sciences + EducationField.xMarketing + 
               EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
               JobLevel.x2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
               JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
               MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3 ,data=train,family = "binomial")
summary(model_6)


#Removal of Educatio.x5
model_7<-glm(Attrition ~ Age + DistanceFromHome + MonthlyIncome + NumCompaniesWorked + 
               TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + 
               EducationField.xLife.Sciences + EducationField.xMarketing + 
               EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
               JobLevel.x2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
               JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
               MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3 ,data=train,family = "binomial")
summary(model_7)


#Removal of DistanceFromHome
model_8<-glm(Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
               TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + 
               EducationField.xLife.Sciences + EducationField.xMarketing + 
               EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
               JobLevel.x2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
               JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
               MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3 ,data=train,family = "binomial")
summary(model_8)



#Removal of MonthlyIncome
model_9<-glm(Attrition ~ Age + NumCompaniesWorked + 
               TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + 
               EducationField.xLife.Sciences + EducationField.xMarketing + 
               EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
               JobLevel.x2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
               JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
               MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3 ,data=train,family = "binomial")
summary(model_9)


#Removal of EducationField.xLife.Sciences
model_10<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + EducationField.xMarketing +EducationField.xMedical + EducationField.xOther + 
                EducationField.xTechnical.Degree + 
                JobLevel.x2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 ,data=train,family = "binomial")
summary(model_10)

#Removal of EducationField.xMarketing 
model_11<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales+EducationField.xMedical + EducationField.xOther + 
                EducationField.xTechnical.Degree + 
                JobLevel.x2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 ,data=train,family = "binomial")
summary(model_11)


#Removal of EducationField.xMedical 
model_12<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales+ EducationField.xOther + 
                EducationField.xTechnical.Degree + 
                JobLevel.x2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 ,data=train,family = "binomial")
summary(model_12)


#Removal of EducationField.xTechnical.Degree
model_13<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales+ EducationField.xOther + JobLevel.x2 + JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 ,data=train,family = "binomial")
summary(model_13)

#Removal of JobLevel.x2 
model_14<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales+ EducationField.xOther + JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 ,data=train,family = "binomial")
summary(model_14)


#Removal of EducationField.xOther 
model_15<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 ,data=train,family = "binomial")
summary(model_15)


#Removal of JobRole.xLaboratory.Technician  
model_16<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales +JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive +MaritalStatus.xSingle + EnvironmentSatisfaction.x2 +
                EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 ,data=train,family = "binomial")
summary(model_16)



#Removal of JobInvolvement.x3  
model_17<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales +JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive +MaritalStatus.xSingle + EnvironmentSatisfaction.x2 +
                EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4  ,data=train,family = "binomial")
summary(model_17)



#Removal of JobRole.xResearch.Scientist   
model_18<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales +JobRole.xResearch.Director +  
                JobRole.xSales.Executive +MaritalStatus.xSingle + EnvironmentSatisfaction.x2 +
                EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4  ,data=train,family = "binomial")
summary(model_18)


#Removal of JobRole.xResearch.Director   
model_19<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales +  
                JobRole.xSales.Executive +MaritalStatus.xSingle + EnvironmentSatisfaction.x2 +
                EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4  ,data=train,family = "binomial")
summary(model_19)


#Removal of JobRole.xSales.Executive  
model_20<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + MaritalStatus.xSingle + EnvironmentSatisfaction.x2 +
                EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4  ,data=train,family = "binomial")
summary(model_20)


#Removal of JobSatisfaction.x3   
model_21<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + MaritalStatus.xSingle + EnvironmentSatisfaction.x2 +
                EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4  ,data=train,family = "binomial")
summary(model_21)


#Removal of JobSatisfaction.x2  
model_22<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + MaritalStatus.xSingle + EnvironmentSatisfaction.x2 +
                EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4,data=train,family = "binomial")
summary(model_22)
vif(model_22)




#Removal of  WorkLifeBalance.x4 
model_23<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + MaritalStatus.xSingle + EnvironmentSatisfaction.x2 +
                EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3  
                ,data=train,family = "binomial")
summary(model_23)
vif(model_23)


#Removal of   BusinessTravel.xTravel_Frequently 
model_24<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + MaritalStatus.xSingle + EnvironmentSatisfaction.x2 +
                EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3,data=train,family = "binomial")
summary(model_24)
vif(model_24)


#Removal of  BusinessTravel.xTravel_Rarely  
model_25<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                 Department.xResearch...Development + 
                Department.xSales + MaritalStatus.xSingle + EnvironmentSatisfaction.x2 +
                EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3,data=train,family = "binomial")
summary(model_25)
vif(model_25)


#Removal of  WorkLifeBalance.x2  
model_26<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Department.xResearch...Development + 
                Department.xSales + MaritalStatus.xSingle + EnvironmentSatisfaction.x2 +
                EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x4 +  WorkLifeBalance.x3,data=train,family = "binomial")
summary(model_26)
vif(model_26)


#Removal of Department.xSales  
model_27<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Department.xResearch...Development + 
                 MaritalStatus.xSingle + EnvironmentSatisfaction.x2 +
                EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x4 +  WorkLifeBalance.x3,data=train,family = "binomial")
summary(model_27)
vif(model_27)


#Removal of Department.xResearch...Development  
model_28<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.x2 +
                EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x4 +  WorkLifeBalance.x3,data=train,family = "binomial")
summary(model_28)
vif(model_28)


#Removal of   WorkLifeBalance.x3  
model_29<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.x2 +
                EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x4 ,data=train,family = "binomial")
summary(model_29)
vif(model_29)


#Removal of   TotalWorkingYears 
model_30<-glm(Attrition ~ Age + NumCompaniesWorked + 
                TrainingTimesLastYear +YearsSinceLastPromotion + YearsWithCurrManager + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.x2 +
                EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x4 ,data=train,family = "binomial")
summary(model_30)
vif(model_30)


#Evaluation of model

Predict_2 <- predict(model_30,type="response",test[,-1])
summary(Predict_2)
test$prob<-Predict_2
View(test)

test_pred_attrition<-factor(ifelse(Predict_2>0.145,"yes","no"))
test_actual_attrition<-factor(ifelse(test$Attrition==1,"yes","no"))

table_1<-table(test_actual_attrition,test_pred_attrition)

test_conf<-confusionMatrix(test_pred_attrition,test_actual_attrition,positive = "yes")
conf_values<-test_conf
conf_values

