

# remove and clean up all variable declarations
remove (list=ls())

# manually we can install the packages as shown below, but I am coding to make it automatic
#install.packages("ROCR",dependencies = TRUE)
#install.packages("caTools",dependencies = TRUE)
#install.packages("Rcpp",dependencies = TRUE)
#install.packages("e1071",dependencies = TRUE)

#install.packages("MASS",dependencies = TRUE)
#install.packages("car",dependencies = TRUE)
#install.packages("prettyR", dependencies = TRUE)
#install.packages("ggplot2", dependencies = TRUE)
#install.packages("tidyr", dependencies = TRUE)
#install.packages("stringr", dependencies = TRUE)
#install.packages("caret")
#install.packages("ggcorrplot")

## Automatic coding to install packages
#### Check for missing packages
packagesused <- c("ROCR", "caTools","Rcpp","e1071", "MASS","car","lattice", "dplyr","ggplot2","psych","magrittr","lubridate","tidyr", "stringr","cowplot","gridExtra","corrplot","caret","ggcorrplot")
packagesmissing <- packagesused[(!packagesused %in% installed.packages())]
if(length(packagesmissing) > 0) install.packages(packagesmissing, dependencies = T)

#### Load the packages in memory
#library(Rcpp)
#library(e1071)
#library(ROCR)

#library(MASS)
#library(car)
#library(lattice)
#library(dplyr)
#library(ggplot2)
#library(psych)
#library(magrittr)
#library(lubridate)
#library(tidyr)
#library(stringr)
#library(cowplot)
#library(gridExtra)
#library(corrplot)
#library(caret)
#library(ggcorrplot)

# loading the above packages with a single line
sapply(packagesused,function(x)  library(x,character.only = TRUE))

######## End of the packages installations


#### Setting up R working directory
##setwd("C:/Users/abhishek.saxena02/Documents/BI-OLAP-LABSESSION/INPUT")

######################################################################################################################
### Loading all the original ( or as it is) datasets
######################################################################################################################


employee_survey_data<-read.csv("employee_survey_data.csv",stringsAsFactors = F)
manager_survey_data<-read.csv("manager_survey_data.csv",stringsAsFactors = F)
general_data<-read.csv("general_data.csv",stringsAsFactors = F)
in_time <- read.csv("in_time.csv",stringsAsFactors = F)
out_time <- read.csv("out_time.csv",stringsAsFactors = F)

##################################################################################################
# Lets have a look at the employee survey
str(employee_survey_data)
############################################################ 
# Lets look into manager survey data
str(manager_survey_data)    
############################################################ 
# Lets look into general_data    
str(general_data)    
############################################################ 
# Lets look into in_time
str(in_time)    
############################################################ 
# Lets look into out_time
str(out_time)  
############################################################ 

### Checking missing values

sum(is.na(general_data)) # 28 missing values 
sum(is.na(employee_survey_data)) # 83 missing values 
sum(is.na(manager_survey_data)) # 0 missing values

############################################################

# omit missing values
sapply(general_data, function(x) sum(is.na(x))) # (NumCompaniesWorked:19, TotalWorkingYears:9)
sapply(employee_survey_data, function(x) sum(is.na(x))) # (worklife:38, env:25, jobs:20)


############################################################
#checking for unique Ids now
length(unique(tolower(employee_survey_data$EmployeeID)))  
length(unique(tolower(manager_survey_data$EmployeeID))) 
length(unique(tolower(general_data$EmployeeID)))
length(unique(tolower(in_time$X))) 
length(unique(tolower(out_time$X))) 


## Checking unqiue rows to merge the dataset 
#############################################################
# Changing the Column name to in_tie and ot_time
colnames(in_time)[1] <- "EmployeeID"
colnames(in_time) <- gsub(pattern="X","",colnames(in_time))

colnames(out_time)[1] <- "EmployeeID"
colnames(out_time) <- gsub(pattern="X","",colnames(out_time))

############################################################

# Validating all dates are presnt for in and out time 
which(!colnames(in_time) == colnames(out_time))

############################################################

# removing holidays 
in_time <- in_time %>% dplyr::select(-c(which(sapply(in_time,function(x) sum(is.na(x))) == nrow(in_time))))
out_time <- out_time %>% dplyr::select(-c(which(sapply(out_time,function(x) sum(is.na(x))) == nrow(out_time))))

############################################################

#working hour each day 
in_time_wh <- as.data.frame((sapply(c(2:ncol(in_time)),function(x) ymd_hms(in_time[,x]))))
colnames(in_time_wh) <- colnames(in_time)[-1]

out_time_wh <- as.data.frame((sapply(c(2:ncol(out_time)),function(x) ymd_hms(out_time[,x]))))
colnames(out_time_wh) <- colnames(out_time)[-1]

emp_wh <- as.data.frame((as.matrix(out_time_wh) - as.matrix(in_time_wh))/(60*60))
#getting employee_ids

emp_wh <- cbind(in_time$EmployeeID,emp_wh)
colnames(emp_wh)[1] <- "EmployeeID"

############################################################

#gathering the data 
emp_ofc_time <- gather(emp_wh,key=Dates,values=c(2:ncol(emp_wh)))


#posixdate convrsion
emp_ofc_time$Dates <- ymd(emp_ofc_time$Dates)
emp_ofc_time$Month <- month(emp_ofc_time$Dates)

############################################################

#Monthly working hour analysis for each employee 
emp_wh_stats <- emp_ofc_time %>% group_by(EmployeeID,Month) %>%summarize(MonthlyHours=sum(value,na.rm=TRUE),offs=sum(is.na(value)))

# The number of working days in the month are 
monthly_base <- data.frame(Month=c(1:12),WorkDays=as.vector(sapply(c(1:12),function(x) length(which(month(unique(emp_ofc_time$Dates)) == x)))))

# Join the number of working hours with emp_working_hours_stats
emp_wh_stats <- full_join(emp_wh_stats,monthly_base,by="Month")

# Calculate the percentage of working hours for each employee every month.
emp_wh_stats <- emp_wh_stats %>% mutate(percWorked=MonthlyHours/(WorkDays*8))

# spread the data month wise for all employees 
emp_whs_spread <- spread(emp_wh_stats[,c(1,2,6)],key=Month,value=percWorked)

# Add the total offs taken by employee in this data frame
emp_whs_spread$totalOffs <- (emp_wh_stats %>% group_by(EmployeeID) %>% summarize(totalOffs=sum(offs)))$totalOffs

# Add the percentage number of hours the employee worked over the complete year
# As per number of working days in the year.
emp_whs_spread$percYearlyWorking  <- (emp_wh_stats %>%group_by(EmployeeID) %>% 
                                        summarize(YearlyWorkingHours=sum(MonthlyHours),
                                                  YearlyWorkingDays=sum(WorkDays)) %>% 
                                        mutate(percYearlyWorking=YearlyWorkingHours/(YearlyWorkingDays*8)))$percYearlyWorking

############################################################
# Lets see if there is any co-relation between monthly data
emp_whcor <- cor(emp_whs_spread[,-1])
dimnames(emp_whcor) <- list(
  c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec','off','yearly'),
  c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec','off','yearly'))



corrplot(emp_whcor,method = "number", 
         title = "working Hour Cor-Matrix", 
         type = "lower", 
         order = "FPC",
         number.cex = 0.8,
         tl.cex = 0.7,
         bg="light blue",number.digits = 2)

# It is clear from the cor plot that, we can just have the yearly column and the offs column
# considering the the yearly column and the offs column for further analysis 
############################################################
emp_wh <- emp_whs_spread[,c(1,14,15)]
## View emp_wh
emp_wh

############################################################
#Joining the derived variables data frames from time analysis with remaing data for further EDA  

df_EDA <- full_join(emp_wh,general_data,by="EmployeeID")
df_EDA <- full_join(df_EDA,employee_survey_data,by="EmployeeID")
df_EDA <- full_join(df_EDA,manager_survey_data,by="EmployeeID")

# Removing columns with all unique values 
df_EDA <- df_EDA %>% dplyr::select(-c(which(sapply(df_EDA,function(x) length(unique(x)) == 1))))
# View df_EDA 
df_EDA

# Checking NA in dataset 
sapply(df_EDA, function(x) sum(is.na(x))) 

# Fixing each columns data one by one 
## Total Working year has 9 Nulls , replacing the null with mean 
round(mean((df_EDA %>% 
              filter(!is.na(TotalWorkingYears)) %>% 
              dplyr::select(EmployeeID,Age,TotalWorkingYears) %>% 
              mutate(started_working_at=(Age-TotalWorkingYears)))$started_working_at),0)

df_EDA[which(is.na(df_EDA$TotalWorkingYears)),'TotalWorkingYears'] <- (df_EDA %>% filter(is.na(TotalWorkingYears)))$Age - 26
############################################################

# Analyzing the data from Attrition dimesnion 
df_EDA %>% group_by(Attrition) %>% summarize(cnt=length(EmployeeID))


# Removing the columns with na 
df_EDA <- as.data.frame(df_EDA %>% 
                          filter(!is.na(NumCompaniesWorked), 
                                 !is.na(EnvironmentSatisfaction), 
                                 !is.na(JobSatisfaction), 
                                 !is.na(WorkLifeBalance)))

# All NA's removed from data 
# Validating that the data doesnot have any "NA"
sum(is.na(df_EDA))

############################################################
# Continuous Univariate plots 
# Common function for univariate anaysis graph generation 
fun_UnivarG<- function(yfeature, ylabel) {
  ggplot(df_EDA, aes(x = "", y = yfeature)) + geom_boxplot(fill = "light blue", outlier.colour = "red", outlier.shape = 2) +
    stat_boxplot(geom = "errorbar", width = 0.5) +
    labs( y = '', title = paste(ylabel)) 
}

############################################################
# Calling the function and storing the graph in a variable 
cup1<-fun_UnivarG(df_EDA$Age, "Employee Age")
cup2<-fun_UnivarG(df_EDA$DistanceFromHome, "DistanceFromHome")
cup3<-fun_UnivarG(df_EDA$MonthlyIncome, "MonthlyIncome")
cup4<-fun_UnivarG(df_EDA$NumCompaniesWorked, "NumCompaniesWorked")
cup5<-fun_UnivarG(df_EDA$PercentSalaryHike, "PercentSalaryHike")
cup6<-fun_UnivarG(df_EDA$TotalWorkingYears, "TotalWorkingYears")
cup7<-fun_UnivarG(df_EDA$TrainingTimesLastYear, "TrainingTimesLastYear")
cup8<-fun_UnivarG(df_EDA$YearsAtCompany, "YearsAtCompany")
cup9<-fun_UnivarG(df_EDA$YearsSinceLastPromotion, "YearsSinceLastPromotion")
cup10<-fun_UnivarG(df_EDA$YearsWithCurrManager, "YearsWithCurrManager")
#Displaying the graphs 
grid.arrange(cup1,cup2,cup3,cup4)
grid.arrange(cup5,cup6,cup7,cup8)
grid.arrange(cup9,cup10)

############################################################
# MAy outliers have been splited in the data so treating the outliers 
# function for outlier treatment 
treatoutlier <- function(x) {
  x[which(x %in% boxplot.stats(x)$out)] <- 
    boxplot.stats(x)$stats[5]
  return(x)
}
############################################################

# Removing outliers
df_EDA$MonthlyIncome  <-treatoutlier(df_EDA$MonthlyIncome)
df_EDA$MonthlyIncome <- treatoutlier(df_EDA$MonthlyIncome)
df_EDA$NumCompaniesWorked <- treatoutlier(df_EDA$NumCompaniesWorked)
df_EDA$TotalWorkingYears <- treatoutlier(df_EDA$TotalWorkingYears)
df_EDA$TrainingTimesLastYear <-   treatoutlier(df_EDA$TrainingTimesLastYear)
df_EDA$YearsAtCompany <-   treatoutlier(df_EDA$YearsAtCompany)
df_EDA$YearsSinceLastPromotion <-   treatoutlier(df_EDA$YearsSinceLastPromotion)
df_EDA$YearsWithCurrManager <-   treatoutlier(df_EDA$YearsWithCurrManager)

##Validating otlier treatment 
############################################################

cup1<-fun_UnivarG(df_EDA$Age, "Employee Age")
cup2<-fun_UnivarG(df_EDA$DistanceFromHome, "DistanceFromHome")
cup3<-fun_UnivarG(df_EDA$MonthlyIncome, "MonthlyIncome")
cup4<-fun_UnivarG(df_EDA$NumCompaniesWorked, "NumCompaniesWorked")
cup5<-fun_UnivarG(df_EDA$PercentSalaryHike, "PercentSalaryHike")
cup6<-fun_UnivarG(df_EDA$TotalWorkingYears, "TotalWorkingYears")
cup7<-fun_UnivarG(df_EDA$TrainingTimesLastYear, "TrainingTimesLastYear")
cup8<-fun_UnivarG(df_EDA$YearsAtCompany, "YearsAtCompany")
cup9<-fun_UnivarG(df_EDA$YearsSinceLastPromotion, "YearsSinceLastPromotion")
cup10<-fun_UnivarG(df_EDA$YearsWithCurrManager, "YearsWithCurrManager")

grid.arrange(cup1,cup2,cup3,cup4)
grid.arrange(cup5,cup6,cup7,cup8)
grid.arrange(cup9,cup10)
# We can see from the graphs that the otliers have been removed 
############################################################

# Bi Variate categorical analysis 
# Functin for potting bivariate categorical variables 

fun_catBivarG <-  function(xlabel, ylabel,xaxis) {
  as.data.frame(prop.table(table(xlabel, ylabel), 2)) %>%
    ggplot(aes(x = xlabel, y = Freq,  fill = ylabel)) +
    geom_col( position = "fill" ) +
    geom_text(aes(label = Freq),
              position = position_fill(vjust = .5),  
              size = 2.5) +
    labs(x = xaxis, y = "Attr", 
         title = paste(xaxis), fill = "Attrition") + 
    theme(legend.position = 'none')
}


# Impact of categorical variable in atteration 
CTBP1<-fun_catBivarG(df_EDA$BusinessTravel, df_EDA$Attrition,"BusinessTravel") + coord_flip()
CTBP2<-fun_catBivarG(df_EDA$Department, df_EDA$Attrition,"Department") + coord_flip()
CTBP3<-fun_catBivarG(df_EDA$Education, df_EDA$Attrition,"Education")
CTBP4<-fun_catBivarG(df_EDA$EducationField, df_EDA$Attrition,"EducationField") + coord_flip()
CTBP5<-fun_catBivarG(df_EDA$JobRole, df_EDA$Attrition,"JobRole") +  coord_flip()
CTBP6<-fun_catBivarG(df_EDA$Gender, df_EDA$Attrition,"Gender") 
CTBP7<-fun_catBivarG(df_EDA$JobLevel, df_EDA$Attrition,"JobLevel")
CTBP8<-fun_catBivarG(df_EDA$MaritalStatus, df_EDA$Attrition,"MaritalStatus")
CTBP9<-fun_catBivarG(df_EDA$StockOptionLevel, df_EDA$Attrition,"StockOptionLevel")
CTBP10<-fun_catBivarG(df_EDA$EnvironmentSatisfaction, df_EDA$Attrition, "EnvironmentSatisfaction")
CTBP11<-fun_catBivarG(df_EDA$JobSatisfaction, df_EDA$Attrition,"JobSatisfaction")
CTBP12<-fun_catBivarG(df_EDA$WorkLifeBalance, df_EDA$Attrition,"WorkLifeBalance")
CTBP13<-fun_catBivarG(df_EDA$JobInvolvement, df_EDA$Attrition,"JobInvolvement")
CTBP14<-fun_catBivarG(df_EDA$JobInvolvement, df_EDA$Attrition,    "JobInvolvement")

grid.arrange(CTBP1,CTBP2,CTBP3,CTBP4)
grid.arrange(CTBP5,CTBP6,CTBP7,CTBP8)
grid.arrange(CTBP5,CTBP6,CTBP7,CTBP8)
grid.arrange(CTBP9,CTBP10,CTBP11,CTBP12,CTBP13,CTBP14)

############################################################
### Summary 
# Attrition is imacted the most by below 
# 1. Frequent BusinessTravel  
# 2. Single (Marital Status)
# 3. Low Environment Satisfaction
# 4. Low Job Satisfaction
# 5. Low Work Life balance 
# 6. Low Job Involvement
# 7. Time in office : Over time
# 8. Department :  Human Resources department
# 9. Education field :  Human Resources
# 10. JobRole : Research Director 

############################################################
#impact of continuas variable on atteration 
# Functin for potting bivariate continues variables 

fun_ConBivarG <- function(xfeature, yfeature, Yaxis) {
  ggplot(df_EDA, aes(x = xfeature, y = yfeature, fill = xfeature)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 1, show.legend = F) +
    stat_boxplot(geom = "errorbar", width = 0.5) +
    labs(x = ' ', y = Yaxis, title = paste(Yaxis)) 
}

############################################################

CNBP1<-fun_ConBivarG(df_EDA$Attrition, df_EDA$percYearlyWorking, "Per Yearly Working Hr") 
CNBP2<-fun_ConBivarG(df_EDA$Attrition, df_EDA$Age,     "Age")
CNBP3<-fun_ConBivarG(df_EDA$Attrition, df_EDA$DistanceFromHome,     "DistanceFromHome")
CNBP4<-fun_ConBivarG(df_EDA$Attrition, df_EDA$MonthlyIncome,     "MonthlyIncome")
CNBP5<-fun_ConBivarG(df_EDA$Attrition, df_EDA$NumCompaniesWorked,     "NumCompaniesWorked")
CNBP6<-fun_ConBivarG(df_EDA$Attrition, df_EDA$PercentSalaryHike,     "PercentSalaryHike")
CNBP7<-fun_ConBivarG(df_EDA$Attrition, df_EDA$TotalWorkingYears,     "TotalWorkingYears")
CNBP8<-fun_ConBivarG(df_EDA$Attrition, df_EDA$TrainingTimesLastYear,     "TrainingTimesLastYear")
CNBP9<-fun_ConBivarG(df_EDA$Attrition, df_EDA$YearsAtCompany,     "YearsAtCompany")
CNBP10<-fun_ConBivarG(df_EDA$Attrition, df_EDA$YearsSinceLastPromotion,     "YearsSinceLastPromotion")
CNBP11<-fun_ConBivarG(df_EDA$Attrition, df_EDA$YearsWithCurrManager,     "YearsWithCurrManager")

grid.arrange(CNBP1,CNBP2,CNBP3,CNBP4)
grid.arrange(CNBP5,CNBP6,CNBP7,CNBP8)
grid.arrange(CNBP9,CNBP10,CNBP11)
## Summary 
# Attrition is higher for employees with 
# 1. More percentage of yearly working hours 
# 2. Lesser Age 
# 3. Less number of total working years
# 4. Less number of years at company
# 5. less number of years with current manager
# 6. Less number of companies worked
############################################################

################################
#data preparation 
###############################
v_cat <- names(Filter(is.character, df_EDA))
v_con <- (names(Filter(is.numeric, df_EDA)))

v_con<-v_con[-c(1,11)]
v_cat<-c(v_cat,'StockOptionLevel')



df_MOdel <- cbind(df_EDA$EmployeeID ,scale(df_EDA %>% dplyr::select(v_con)), df_EDA %>% dplyr::select(v_cat))
colnames(df_MOdel)[1] <- "EmployeeID"

v_cat<-v_cat[-c(1,5)]

v_cat
# The categoiacal variables are 
# "BusinessTravel"   "Department"       "EducationField"   "JobRole"          "MaritalStatus"    "StockOptionLevel"
# Creating Dummies for categorical variables 

dummies <- sapply(df_MOdel[,which(colnames(df_MOdel) %in% c(v_cat))],function(x) 
  as.data.frame(model.matrix(~x,data=df_MOdel[,which(colnames(df_MOdel) %in% c(v_cat))])))
dummies <- as.data.frame(dummies)
dummies <- dummies %>% dplyr::select(-c(BusinessTravel..Intercept.,
                                        Department..Intercept.,
                                        EducationField..Intercept.,
                                        JobRole..Intercept.,
                                        MaritalStatus..Intercept.,
                                        StockOptionLevel..Intercept.))

df_MOdel <- cbind(df_MOdel %>%
                    dplyr::select(-c(BusinessTravel,Department,EducationField,JobRole,MaritalStatus,StockOptionLevel)),
                  dummies)

# Converting "yes" to 1 and "NO" to o for Attrition
df_MOdel$Attrition <- sapply(df_MOdel$Attrition, function(x) ifelse(x == "Yes",1,0))

# coverting  Male = 1 and Female = 0 for gender 
df_MOdel$Gender <- sapply(df_MOdel$Gender, function(x) ifelse(x == "Male",1,0))

# Removing the employeeid column from the data 
df_MOdel <- df_MOdel %>% dplyr::select(-EmployeeID)

############################################################

#Breaking the data in test and train for model creation 
set.seed(100)
trainindex <- which(sample.split(df_MOdel$Attrition,SplitRatio = 0.7))
testindex <- which(!c(1:nrow(df_MOdel) %in% trainindex))

train <- df_MOdel[trainindex,]
test <- df_MOdel[testindex,]
############################################################

# Creating the model 
model1 <- glm(Attrition~., data=df_MOdel, family="binomial")
summary(model1)
############################################################

step <- stepAIC(model1,direction="both")

############################################################
model2<- glm(Attrition ~ totalOffs + percYearlyWorking + Age + JobLevel +
               NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
               JobSatisfaction + WorkLifeBalance + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + EducationField.xLife.Sciences + EducationField.xMarketing + 
               EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
               JobRole.xLaboratory.Technician + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle,data=df_MOdel,family="binomial")
summary(model2)

############################################################
# Model is created after removing the variable with the + sign
model3<- glm(Attrition ~ EducationField.xLife.Sciences + JobLevel+totalOffs
             +JobRole.xSales.Executive+EducationField.xMedical+YearsAtCompany+
               EducationField.xMarketing+Department.xSales+ Department.xResearch...Development
             +MaritalStatus.xMarried+JobRole.xManager+EducationField.xTechnical.Degree+
               EducationField.xOther+JobRole.xResearch.Director+BusinessTravel.xTravel_Rarely+
               JobRole.xManufacturing.Director+TrainingTimesLastYear+Age+WorkLifeBalance
             +TotalWorkingYears+YearsWithCurrManager+NumCompaniesWorked+BusinessTravel.xTravel_Frequently
             +YearsSinceLastPromotion+JobSatisfaction+MaritalStatus.xSingle+EnvironmentSatisfaction
             +percYearlyWorking,data=df_MOdel,family="binomial")
summary(model3)

############################################################
# Variables with the high p value will be removed since they are insignificant EducationField.xLife.Sciences 
model4<- glm(Attrition ~ JobLevel+totalOffs+JobRole.xSales.Executive+EducationField.xMedical+YearsAtCompany+
               EducationField.xMarketing+Department.xSales+ Department.xResearch...Development
             +MaritalStatus.xMarried+JobRole.xManager+EducationField.xTechnical.Degree+
               EducationField.xOther+JobRole.xResearch.Director+BusinessTravel.xTravel_Rarely+
               JobRole.xManufacturing.Director+TrainingTimesLastYear+Age+WorkLifeBalance
             +TotalWorkingYears+YearsWithCurrManager+NumCompaniesWorked+BusinessTravel.xTravel_Frequently
             +YearsSinceLastPromotion+JobSatisfaction+MaritalStatus.xSingle+EnvironmentSatisfaction
             +percYearlyWorking,data=df_MOdel,family="binomial")
summary(model4)

############################################################
# highest p value is for variable EducationField.xMedical
model5<- glm(Attrition ~ JobLevel+totalOffs+JobRole.xSales.Executive+YearsAtCompany+
               EducationField.xMarketing+Department.xSales+ Department.xResearch...Development
             +MaritalStatus.xMarried+JobRole.xManager+EducationField.xTechnical.Degree+
               EducationField.xOther+JobRole.xResearch.Director+BusinessTravel.xTravel_Rarely+
               JobRole.xManufacturing.Director+TrainingTimesLastYear+Age+WorkLifeBalance
             +TotalWorkingYears+YearsWithCurrManager+NumCompaniesWorked+BusinessTravel.xTravel_Frequently
             +YearsSinceLastPromotion+JobSatisfaction+MaritalStatus.xSingle+EnvironmentSatisfaction
             +percYearlyWorking,data=df_MOdel,family="binomial")
summary(model5)
############################################################
# highest p value is for variable EducationField.xMarketing
model6<- glm(Attrition ~ JobLevel+totalOffs+JobRole.xSales.Executive+YearsAtCompany+
               Department.xSales+ Department.xResearch...Development
             +MaritalStatus.xMarried+JobRole.xManager+EducationField.xTechnical.Degree+
               EducationField.xOther+JobRole.xResearch.Director+BusinessTravel.xTravel_Rarely+
               JobRole.xManufacturing.Director+TrainingTimesLastYear+Age+WorkLifeBalance
             +TotalWorkingYears+YearsWithCurrManager+NumCompaniesWorked+BusinessTravel.xTravel_Frequently
             +YearsSinceLastPromotion+JobSatisfaction+MaritalStatus.xSingle+EnvironmentSatisfaction
             +percYearlyWorking,data=df_MOdel,family="binomial")
summary(model6)

############################################################
# highest p value is for variable EducationField.xOther
model7<- glm(Attrition ~ JobLevel+totalOffs+JobRole.xSales.Executive+YearsAtCompany+
               Department.xSales+ Department.xResearch...Development
             +MaritalStatus.xMarried+JobRole.xManager+EducationField.xTechnical.Degree+
               JobRole.xResearch.Director+BusinessTravel.xTravel_Rarely+
               JobRole.xManufacturing.Director+TrainingTimesLastYear+Age+WorkLifeBalance
             +TotalWorkingYears+YearsWithCurrManager+NumCompaniesWorked+BusinessTravel.xTravel_Frequently
             +YearsSinceLastPromotion+JobSatisfaction+MaritalStatus.xSingle+EnvironmentSatisfaction
             +percYearlyWorking,data=df_MOdel,family="binomial")
summary(model7)
############################################################
# highest p value is for variable EducationField.xTechnical.Degree
model8<- glm(Attrition ~ JobLevel+totalOffs+JobRole.xSales.Executive+YearsAtCompany+
               Department.xSales+ Department.xResearch...Development
             +MaritalStatus.xMarried+JobRole.xManager+
               JobRole.xResearch.Director+BusinessTravel.xTravel_Rarely+
               JobRole.xManufacturing.Director+TrainingTimesLastYear+Age+WorkLifeBalance
             +TotalWorkingYears+YearsWithCurrManager+NumCompaniesWorked+BusinessTravel.xTravel_Frequently
             +YearsSinceLastPromotion+JobSatisfaction+MaritalStatus.xSingle+EnvironmentSatisfaction
             +percYearlyWorking,data=df_MOdel,family="binomial")
summary(model8)

############################################################
# highest p value is for variable JobRole.xSales.Executive 
model9<- glm(Attrition ~ JobLevel+totalOffs+YearsAtCompany+
               Department.xSales+ Department.xResearch...Development
             +MaritalStatus.xMarried+JobRole.xManager+
               JobRole.xResearch.Director+BusinessTravel.xTravel_Rarely+
               JobRole.xManufacturing.Director+TrainingTimesLastYear+Age+WorkLifeBalance
             +TotalWorkingYears+YearsWithCurrManager+NumCompaniesWorked+BusinessTravel.xTravel_Frequently
             +YearsSinceLastPromotion+JobSatisfaction+MaritalStatus.xSingle+EnvironmentSatisfaction
             +percYearlyWorking,data=df_MOdel,family="binomial")
summary(model9)

############################################################
# highest p value is for variable YearsAtCompany 
model10<- glm(Attrition ~ JobLevel+totalOffs+
                Department.xSales+ Department.xResearch...Development
              +MaritalStatus.xMarried+JobRole.xManager+
                JobRole.xResearch.Director+BusinessTravel.xTravel_Rarely+
                JobRole.xManufacturing.Director+TrainingTimesLastYear+Age+WorkLifeBalance
              +TotalWorkingYears+YearsWithCurrManager+NumCompaniesWorked+BusinessTravel.xTravel_Frequently
              +YearsSinceLastPromotion+JobSatisfaction+MaritalStatus.xSingle+EnvironmentSatisfaction
              +percYearlyWorking,data=df_MOdel,family="binomial")
summary(model10)
############################################################
# highest p value is for variable joblevel 
model11<- glm(Attrition ~ totalOffs+
                Department.xSales+ Department.xResearch...Development
              +MaritalStatus.xMarried+JobRole.xManager+
                JobRole.xResearch.Director+BusinessTravel.xTravel_Rarely+
                JobRole.xManufacturing.Director+TrainingTimesLastYear+Age+WorkLifeBalance
              +TotalWorkingYears+YearsWithCurrManager+NumCompaniesWorked+BusinessTravel.xTravel_Frequently
              +YearsSinceLastPromotion+JobSatisfaction+MaritalStatus.xSingle+EnvironmentSatisfaction
              +percYearlyWorking,data=df_MOdel,family="binomial")
summary(model11)
############################################################
# highest p value is for variable totaloffs 
model12<- glm(Attrition ~ Department.xSales+ Department.xResearch...Development
              +MaritalStatus.xMarried+JobRole.xManager+
                JobRole.xResearch.Director+BusinessTravel.xTravel_Rarely+
                JobRole.xManufacturing.Director+TrainingTimesLastYear+Age+WorkLifeBalance
              +TotalWorkingYears+YearsWithCurrManager+NumCompaniesWorked+BusinessTravel.xTravel_Frequently
              +YearsSinceLastPromotion+JobSatisfaction+MaritalStatus.xSingle+EnvironmentSatisfaction
              +percYearlyWorking,data=df_MOdel,family="binomial")
summary(model12)
############################################################
# highest p value is for variable MaritalStatus.xMarried 
model13<- glm(Attrition ~ Department.xSales+ Department.xResearch...Development
              +JobRole.xManager+JobRole.xResearch.Director+BusinessTravel.xTravel_Rarely+
                JobRole.xManufacturing.Director+TrainingTimesLastYear+Age+WorkLifeBalance
              +TotalWorkingYears+YearsWithCurrManager+NumCompaniesWorked+BusinessTravel.xTravel_Frequently
              +YearsSinceLastPromotion+JobSatisfaction+MaritalStatus.xSingle+EnvironmentSatisfaction
              +percYearlyWorking,data=df_MOdel,family="binomial")
summary(model13)
############################################################
# highest p value is for variable JobRole.xResearch.Director  
model14<- glm(Attrition ~ Department.xSales+ Department.xResearch...Development
              +JobRole.xManager+BusinessTravel.xTravel_Rarely+
                JobRole.xManufacturing.Director+TrainingTimesLastYear+Age+WorkLifeBalance
              +TotalWorkingYears+YearsWithCurrManager+NumCompaniesWorked+BusinessTravel.xTravel_Frequently
              +YearsSinceLastPromotion+JobSatisfaction+MaritalStatus.xSingle+EnvironmentSatisfaction
              +percYearlyWorking,data=df_MOdel,family="binomial")
summary(model14)
############################################################
# highest p value is for variable JobRole.xManager  
model15<- glm(Attrition ~ Department.xSales+ Department.xResearch...Development
              +BusinessTravel.xTravel_Rarely+
                JobRole.xManufacturing.Director+TrainingTimesLastYear+Age+WorkLifeBalance
              +TotalWorkingYears+YearsWithCurrManager+NumCompaniesWorked+BusinessTravel.xTravel_Frequently
              +YearsSinceLastPromotion+JobSatisfaction+MaritalStatus.xSingle+EnvironmentSatisfaction
              +percYearlyWorking,data=df_MOdel,family="binomial")
summary(model15)

############################################################

final_model<-model15

############################################################

# Model evaluation done on TEST data.
### Test Data ####

#predicted probabilities of Churn 1 for test data

test_pred = predict(final_model, type = "response", newdata = test)
test$prob <-test_pred
View(test_pred)
# Let's see the summary 

summary(test_pred)
############################################################

test$prob <- test_pred
View(test)
# Let's use the probability cutoff of 50%.

test_pred_attr <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attr <- factor(ifelse(test$Attrition==1,"Yes","No"))

min(test$prob )
max(test$prob)
#######################################################################
test_pred_attr <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

test_conf <- confusionMatrix(test_pred_attr, test_actual_attr, positive = "Yes")
test_conf
#######################################################################

#########################################################################################
# Let's Choose the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_churn <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_churn, test_actual_attr, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)
############################################################

s = seq(.001,.90,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

############################################################
# Let's choose a cutoff value of 0.16 for final model

test_cutoff_attr <- factor(ifelse(test_pred >=0.16, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attr, test_actual_attr, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

View(test)
##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attr <- ifelse(test_cutoff_attr=="Yes",1,0)
test_actual_attr <- ifelse(test_actual_attr=="Yes",1,0)


#on testing  data
pred_object_test<- prediction(test_cutoff_attr, test_actual_attr)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)


####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

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

lift(test_actual_attr, test_pred, groups = 10)
