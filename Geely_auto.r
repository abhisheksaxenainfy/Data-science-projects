#****************************************************************************************************************************
##Author :  Abhishek Saxena


library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
require(lubridate)
library(stringr)
library(reshape)
library("MASS")
library("car")
library(splitstackshape)
#getwd()
#setwd("C:/Users/abhishek.saxena02/Documents/BI-OLAP-LABSESSION/INPUT")

# Loading the dataset
geely<-read.csv("CarPrice_Assignment.csv")

str(geely)

# check columns and rows 
cat('Loan Dataframe has', dim(geely)[1], 'rows and', dim(geely)[2], 'columns.')

# Number of duplicate rows 
cat("The number of duplicated rows are", nrow(geely) - nrow(unique(geely)))

# Create function to understand the Total, Missing, Missing_Percentage, Duplicates and Distinct Values

CheckDataForMissingValues2 <- function(x) (
  
  data.frame(	"Total" = length(x),   
              "Missing" = sum(is.na(x)),  
              "Missing_Percentage" = ifelse(length(x)>0, round((sum(is.na(x))/length(x)) * 100,2),0),  
              "Duplicates"=sum(duplicated(x)),  
              "Distinct" = length(unique(x)) 
              #"colName" = colnames(x)
  ))

geely_summary_list<- sapply(geely,CheckDataForMissingValues2 )  

geely_summary_matrix <-matrix(unlist(geely_summary_list), nrow=26, byrow=T)

geely_summary_df <- data.frame(geely_summary_matrix)
colnames(geely_summary_df) <- c("Total", "Missing", "Missing_Percentage", "Duplicates", "Distinct")
colName<-  dimnames(geely_summary_list)[2]
colName <- unlist(colName)
geely_summary_df$ColName <- colName

nrow( geely_summary_df[geely_summary_df$Missing_Percentage==100,  ]) ## columns to be removed
geely_summary_df
View(geely_summary_df)

# Data issues
geely$CarName<-str_replace_all(geely$CarName, "vw", "volkswagen")
geely$CarName<-str_replace_all(geely$CarName, "toyouta", "toyota")
geely$CarName<-str_replace_all(geely$CarName, "toyouta", "toyota")
geely$CarName<-str_replace_all(geely$CarName, "porcshce", "porsche")
geely$CarName<-str_replace_all(geely$CarName, "maxda", "mazda")
geely$CarName<-str_replace_all(geely$CarName, "Nissan", "nissan")

# Introducing column with a company name
geely$Company_name <- word(geely$CarName,1)

# Dropping carname column as company name column is being used as a independent variable
geely$CarName<-NULL
geely$car_ID<-NULL


# Derived metric "Power to weight" ratio
geely$P_W_ratio<-geely$horsepower/geely$curbweight

# Identifying the outliers in car price
boxplot(geely$price,horizontal = T)$stats[c(1, 5), ] # lower  whisker is 5118 upper whisker is 28248

#Exploratory data analysis
# Univariate Analysis
# Fueltype
ggplot(geely, aes(x = fueltype))+geom_bar()
table(geely$fueltype)
# Aspiration
ggplot(geely, aes(x = aspiration))+geom_bar()
table(geely$aspiration)
# Door number
ggplot(geely, aes(x = doornumber))+geom_bar()
table(geely$doornumber)
# carbody
ggplot(geely, aes(x = carbody))+geom_bar()
table(geely$carbody)
# Drive wheel
ggplot(geely, aes(x = drivewheel))+geom_bar()
table(geely$drivewheel)
# enginelocation
ggplot(geely, aes(x = enginelocation))+geom_bar()
table(geely$enginelocation)
# enginetype
ggplot(geely, aes(x = enginetype))+geom_bar()
table(geely$enginetype)
# cylindernumber
ggplot(geely, aes(x = cylindernumber))+geom_bar()
table(geely$cylindernumber)
# fuelsystem
ggplot(geely, aes(x = fuelsystem))+geom_bar()
table(geely$fuelsystem)
# Company
ggplot(geely, aes(x= Company_name))+geom_bar()
table(geely$Company_name)
# Boreratio
boxplot(geely$boreratio)
# Stroke
boxplot(geely$stroke)
# Compressionratio
boxplot(geely$compressionratio)
# horsepower
boxplot(geely$horsepower)
# peakrpm
boxplot(geely$peakrpm)
# citympg
boxplot(geely$citympg)
# highwaympg
boxplot(geely$highwaympg)
# price
boxplot(geely$price)

# Bivariate Analysis with respect to price
#Fueltype
ggplot(geely, aes(x=price, y= fueltype))+ geom_point()+geom_smooth()
#Aspiration
ggplot(geely, aes(x=price, y= aspiration))+ geom_point()+geom_smooth()
# Door number
ggplot(geely, aes(x=price, y= doornumber))+ geom_point()+geom_smooth()
# carbody
ggplot(geely, aes(x=price, y= carbody))+ geom_point()+geom_smooth()
# drivewheel
ggplot(geely, aes(x=price, y= drivewheel))+ geom_point()+geom_smooth()
# enginelocation
ggplot(geely, aes(x=price, y= enginelocation))+ geom_point()+geom_smooth()
# enginetype
ggplot(geely, aes(x=price, y= enginetype))+ geom_point()+geom_smooth()
# cylindernumber
ggplot(geely, aes(x=price, y= cylindernumber))+ geom_point()+geom_smooth()
# fuelsystem
ggplot(geely, aes(x=price, y= fuelsystem))+ geom_point()+geom_smooth()
# company_name
ggplot(geely, aes(x=price, y= Company_name))+ geom_point()+geom_smooth()
# Boreratio
ggplot(geely, aes(x=price, y= boreratio))+ geom_point()+geom_line()
# stroke
ggplot(geely, aes(x=price, y= stroke))+ geom_point()+geom_line()
# Compressionratio
ggplot(geely, aes(x=price, y= compressionratio))+ geom_point()+geom_line()
# Horsepower 
ggplot(geely, aes(x=price, y= horsepower))+ geom_point()+geom_line()
# peakrpm
ggplot(geely, aes(x=price, y= peakrpm))+ geom_point()+geom_line()
# citympg
ggplot(geely, aes(x=price, y= citympg))+ geom_point()+geom_line()
# highwaympg
ggplot(geely, aes(x=price, y= highwaympg))+ geom_point()+geom_line()
 
# NUmerical vs Categorical 
length(names(geely)[which(sapply(geely, is.character))])
names(geely)[which(sapply(geely, is.character))]

length(names(geely)[which(sapply(geely, is.numeric))])
names(geely)[which(sapply(geely, is.numeric))]


#DUMMY VARIABLE CREATION. 
#Funtion to create dummy variable
dummy_variable <- function(x) {
  levels(x)<-c(1,0)
  as.numeric(levels(x))[x]
}

# Dummy variable for aspiration 0 indicates gas, 1 indicates diesel
summary(factor(geely$fueltype))
geely$fueltype<-dummy_variable(geely$fueltype)

# Dummy variable for "aspiration" 0 indicates Turbo and 1 indicates std
summary(factor(geely$aspiration))
geely$aspiration<-dummy_variable(geely$aspiration)

# Dummy variable for "doornumber"  0 indicates two, 1 indicates four
summary(factor(geely$doornumber))
geely$doornumber<-dummy_variable(geely$doornumber)

# Dummy variable for "Enginelocation" 0 indicates rear, 1 indicates front
summary(factor(geely$enginelocation))
geely$enginelocation<-dummy_variable(geely$enginelocation)

# Dummy variable creation across the variable with more than two variable

#Carbody
summary(factor(geely$carbody))
dummy_1 <- data.frame(model.matrix( ~carbody, data = geely))
dummy_1 <- dummy_1[,-1]
geely1<-cbind(geely[,-5], dummy_1)

# Drivewheel
summary(factor(geely$drivewheel))
dummy_2 <- data.frame(model.matrix( ~drivewheel, data = geely1))
dummy_2 <- dummy_2[,-1]
geely2<-cbind(geely1[,-5], dummy_2)

# Enginetype
summary(factor(geely$enginetype))
dummy_3 <- data.frame(model.matrix( ~enginetype, data = geely2))
dummy_3 <- dummy_3[,-1]
geely3<-cbind(geely2[,-11], dummy_3)

# Cylinder number
summary(factor(geely$cylindernumber))
dummy_4 <- data.frame(model.matrix( ~cylindernumber, data = geely3))
dummy_4 <- dummy_4[,-1]
geely4<-cbind(geely3[,-11], dummy_4)

# Fuel system
summary(factor(geely$fuelsystem))
dummy_5 <- data.frame(model.matrix( ~fuelsystem, data = geely4))
dummy_5 <- dummy_5[,-1]
geely5<-cbind(geely4[,-12], dummy_5)


# Company name
summary(factor(geely$Company_name))
dummy_6 <- data.frame(model.matrix( ~Company_name, data = geely5))
dummy_6 <- dummy_6[,-1]
geely6<-cbind(geely5[,-20], dummy_6)


# Divide into training and test data set
#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(geely6), 0.75*nrow(geely6))
# generate the train data set
train = geely6[trainindices,]
#Similarly store the rest of the observations into an object "test".
test = geely6[-trainindices,]

#Execute the first model_1 multilinear model in the training set. 
car_model_1 <-lm(formula = price~.,data=train)
alias( lm( price~.,data=train  ) )
summary(car_model_1)

# Check if the correlation matrix givessome insight.
corrs = cor(geely6)
View(corrs)

#so many iterations have been done through the stepwise command. 
# now we need to know our model equation so lets write the Step command here. 

car_step <- stepAIC(car_model_1, direction="both")
car_step

# stepAIC makes multiple calls while checking which variables to keep
# The last call that step makes, contains only the variables it considers to be important in the model. 
# some insignifican variables have been removed. 
# Now store the last model equation of stepwise method into an object called car_model_2
# You can notice that stepAIC removed variables - Company_namejaguar, wheelbase, Company_namesaab,
# Company_nameisuzu, Company_nameaudi, Company_namehonda, drivewheelfwd, enginetypedohcv, symboling,
# enginetypeohcv, carheight, doornumber, stroke, fuelsystemmfi,highwaympg, Company_namemercury,  
# carlength, Company_namevolvo, fueltype, fuelsystemidi, citympg, compressionratio
# Model is created after removing the variable with the + sign

car_model_2<- lm(formula = price ~ stroke+wheelbase+boreratio+drivewheelrwd
                 +cylindernumberfour+fuelsystemspdi+enginetypeohc 
                 +carbodyhardtop+fuelsystem2bbl+peakrpm+fuelsystemmpfi+
                   +carwidth+horsepower+carbodysedan+carbodyhatchback+P_W_ratio
                 +carbodywagon+aspiration+enginetyperotor+enginelocation
                 +enginesize+Company_namebmw ,data=train)

summary(car_model_2)
vif(car_model_2)

# Model 3 (Removing wheelbase VIF is high and p>0.05)
car_model_3<- lm(formula = price ~ stroke+boreratio+drivewheelrwd
                 +cylindernumberfour+fuelsystemspdi+enginetypeohc 
                 +carbodyhardtop+fuelsystem2bbl+peakrpm+fuelsystemmpfi+
                   +carwidth+horsepower+carbodysedan+carbodyhatchback+P_W_ratio
                 +carbodywagon+aspiration+enginetyperotor+enginelocation
                 +enginesize+Company_namebmw ,data=train)

summary(car_model_3)
vif(car_model_3)

# Model 4 (removing horsepower since VIF is very high and p>0.05)
car_model_4<- lm(formula = price ~ stroke+boreratio+drivewheelrwd
                 +cylindernumberfour+fuelsystemspdi+enginetypeohc 
                 +carbodyhardtop+fuelsystem2bbl+peakrpm+fuelsystemmpfi+
                   +carwidth+carbodysedan+carbodyhatchback+P_W_ratio
                 +carbodywagon+aspiration+enginetyperotor+enginelocation
                 +enginesize+Company_namebmw ,data=train)
summary(car_model_4)
vif(car_model_4)

# Model 5 (removing boreratio since vif>2 and p>0.05)
car_model_5<- lm(formula = price ~ stroke+drivewheelrwd
                 +cylindernumberfour+fuelsystemspdi+enginetypeohc 
                 +carbodyhardtop+fuelsystem2bbl+peakrpm+fuelsystemmpfi+
                   +carwidth+carbodysedan+carbodyhatchback+P_W_ratio
                 +carbodywagon+aspiration+enginetyperotor+enginelocation
                 +enginesize+Company_namebmw ,data=train)
summary(car_model_5)
vif(car_model_5)

# Model 6 (removing drivewheelrwd sice p value is high)
car_model_6<- lm(formula = price ~ stroke
                 +cylindernumberfour+fuelsystemspdi+enginetypeohc 
                 +carbodyhardtop+fuelsystem2bbl+peakrpm+fuelsystemmpfi+
                   +carwidth+carbodysedan+carbodyhatchback+P_W_ratio
                 +carbodywagon+aspiration+enginetyperotor+enginelocation
                 +enginesize+Company_namebmw ,data=train)
summary(car_model_6)
vif(car_model_6)

# Model 7 (removing fuelsystem2bbl due to high P value)
car_model_7<- lm(formula = price ~ stroke
                 +cylindernumberfour+fuelsystemspdi+enginetypeohc 
                 +carbodyhardtop+peakrpm+fuelsystemmpfi+
                   +carwidth+carbodysedan+carbodyhatchback+P_W_ratio
                 +carbodywagon+aspiration+enginetyperotor+enginelocation
                 +enginesize+Company_namebmw ,data=train)
summary(car_model_7)
vif(car_model_7)

# Model 8 (Removing fuelsystemmpfi due to high P Value)

car_model_8<- lm(formula = price ~ stroke
                 +cylindernumberfour+fuelsystemspdi+enginetypeohc 
                 +carbodyhardtop+peakrpm+
                   +carwidth+carbodysedan+carbodyhatchback+P_W_ratio
                 +carbodywagon+aspiration+enginetyperotor+enginelocation
                 +enginesize+Company_namebmw ,data=train)
summary(car_model_8)
vif(car_model_8)

# Model 9 ( Removing P_W_ratio due to high P value)

car_model_9<- lm(formula = price ~ stroke
                 +cylindernumberfour+fuelsystemspdi+enginetypeohc 
                 +carbodyhardtop+peakrpm+
                   +carwidth+carbodysedan+carbodyhatchback
                 +carbodywagon+aspiration+enginetyperotor+enginelocation
                 +enginesize+Company_namebmw ,data=train)
summary(car_model_9)
vif(car_model_9)

# Model 10 (Removing fuelsystemspdi due to high P value)

car_model_10<- lm(formula = price ~ stroke
                 +cylindernumberfour+enginetypeohc 
                 +carbodyhardtop+peakrpm+
                   +carwidth+carbodysedan+carbodyhatchback
                 +carbodywagon+aspiration+enginetyperotor+enginelocation
                 +enginesize+Company_namebmw ,data=train)
summary(car_model_10)
vif(car_model_10)

# Model 11 ( Removing enginetypeohc since it is having ** as significance code)

car_model_11<- lm(formula = price ~ stroke
                  +cylindernumberfour
                  +carbodyhardtop+peakrpm+
                    +carwidth+carbodysedan+carbodyhatchback
                  +carbodywagon+aspiration+enginetyperotor+enginelocation
                  +enginesize+Company_namebmw ,data=train)
summary(car_model_11)
vif(car_model_11)

# Model 12 (Removing enginetyperotor since it is having ** as significance code )

car_model_12<- lm(formula = price ~ stroke
                  +cylindernumberfour
                  +carbodyhardtop+peakrpm+
                    +carwidth+carbodysedan+carbodyhatchback
                  +carbodywagon+aspiration+enginelocation
                  +enginesize+Company_namebmw ,data=train)
summary(car_model_12)
vif(car_model_12)

# Model 13 (Removing carbodyhardtop since it is having ** as significance code)
car_model_13<- lm(formula = price ~ stroke
                  +cylindernumberfour
                  +peakrpm+
                    +carwidth+carbodysedan+carbodyhatchback
                  +carbodywagon+aspiration+enginelocation
                  +enginesize+Company_namebmw ,data=train)
summary(car_model_13)
vif(car_model_13)

# Model 14 (Removing carbodywagon due to the high p value)

car_model_14<- lm(formula = price ~ stroke
                  +cylindernumberfour
                  +peakrpm+
                    +carwidth+carbodysedan+carbodyhatchback
                  +aspiration+enginelocation
                  +enginesize+Company_namebmw ,data=train)
summary(car_model_14)
vif(car_model_14)

# Model 15 (Removing carbodysedan due to the high P value)

car_model_15<- lm(formula = price ~ stroke
                  +cylindernumberfour
                  +peakrpm+
                    +carwidth+carbodyhatchback
                  +aspiration+enginelocation
                  +enginesize+Company_namebmw ,data=train)
summary(car_model_15)
vif(car_model_15)

# Model 16 (Removing carbodyhatchback due to the high p value)

car_model_16<- lm(formula = price ~ stroke
                  +cylindernumberfour
                  +peakrpm+
                    +carwidth
                  +aspiration+enginelocation
                  +enginesize+Company_namebmw ,data=train)
summary(car_model_16)
vif(car_model_16)

# Model 16 is the final model with R-squared:  0.9188 and Adjusted R-squared:  0.9143 
# Now we should check for the accuracy of the model since model 16 is the final model

# Predict the car prices in the testing dataset
Predict_1 <- predict(car_model_16,test[,-19])
test$test_price <- Predict_1

# Accuracy of the predictions
# Calculate correlation
r <- cor(test$price,test$test_price)
# calculate R squared by squaring correlation
rsquared <- cor(test$price,test$test_price)^2
# check R-squared
rsquared

# So from the model 16 this can be interpreted that the  stroke cylindernumberfour
# peakrpm, carwidth, aspiration, enginelocation, enginesize and Company_namebmw are the
# most significant in predicting the price of the cars
# Geely auto shuold focus on these independent variables since they are the most significant.


