#****************************************************************************************************************************
install.packages("caret")
install.packages('kernlab')
install.packages("readr")
install.packages('gridExtra')
library('doParallel')
library("caret")
library("dplyr")
library("kernlab")
library("readr")
library("gridExtra")
setwd("C:/Users/abhishek.saxena02/Documents/BI-OLAP-LABSESSION/INPUT")
#Loading Data

#Train Data

mnist_train<-read.csv("mnist_train.csv", header = FALSE, stringsAsFactors = FALSE)

#Test Data

mnist_test<-read.csv("mnist_test.csv", header = FALSE, stringsAsFactors = FALSE)

#********************************Data Understanding******************************#
#Understanding Dimensions

dim(mnist_train)

dim(mnist_test)

#Structure of the dataset
str(mnist_train)

str(mnist_test)

#printing first few rows

head(mnist_train)

head(mnist_test)

#Exploring the data

summary(mnist_train)

summary(mnist_test)

#checking missing value

sapply(mnist_train, function(x) sum(is.na(x))) #NO missing values

sapply(mnist_test, function(x) sum(is.na(x)))  #No missing values

#Checking duplicate value

sum(duplicated(mnist_test)) # no duplicate rows

sum(duplicated(mnist_train)) # no duplicate rows
str(mnist_train$V1)
#*************************************Data Preperation********************************#
mnist_train$V1<-as.factor(mnist_train$V1)

mnist_test$V1<-as.factor(mnist_test$V1)

# Taking sample of the data from the main dataset, so that it takes less computational time
set.seed(123)

sample_train_indices<-sample(1:nrow(mnist_train), 6000)
sample_train = mnist_train[sample_train_indices, ]
train.indices_new = sample(1:nrow(sample_train), 0.8*nrow(sample_train))
main_train<-sample_train[train.indices_new, ]
main_test<-sample_train[-train.indices_new,]

# Exploratory data analysis
mnist_train_factors<-as.data.frame(table(mnist_train$V1))
mnist_test_factors<-as.data.frame(table(mnist_test$V1))
main_train_factors<-as.data.frame(table(main_train$V1))

# ggplot for mnist train dataset
ggplot(data=mnist_train_factors, aes(x= Var1, y = Freq)) +
  geom_bar(stat="identity",color="LightBlue")+
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
  theme_light()+labs(y="Count", x = "Labels")

#ggplot for mnist test dataset
ggplot(data=mnist_test_factors, aes(x= Var1, y = Freq)) +
  geom_bar(stat="identity",color="LightBlue")+
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
  theme_light()+labs(y="Count", x = "Labels")

#ggplot for main train dataset
ggplot(data=main_train_factors, aes(x= Var1, y = Freq)) +
  geom_bar(stat="identity",color="LightBlue")+
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
  theme_light()+labs(y="Count", x = "Labels")



#******************************Modelling********************************************#

#Using Linear Kernel
linear_model <- ksvm(V1~ ., data = main_train, scale = FALSE, kernel = "vanilladot")#C=1 for this model
print(linear_model)
linear_evaluation<- predict(linear_model, main_test ,type = "response")
#confusion matrix - Linear Kernel
confusionMatrix(linear_evaluation,main_test$V1)

# Accuracy : 0.8917  
#                     Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7
#Sensitivity           0.96522   0.9932   0.8672  0.85185  0.94915  0.81308  0.97059  0.85484
#Specificity           0.98802   0.9924   0.9879  0.97894  0.98799  0.98811  0.98998  0.99164


############   Hyperparameter tuning and Cross Validation #####################

Linear_model_grid<-expand.grid(C=c(0.01,0.1,1,10))
fit.linear_model<-train(V1~., data=main_train, method="svmLinear", metric='Accuracy', 
                        tuneGrid=Linear_model_grid, trControl=trainControl(method = "cv", number = 5))
print(fit.linear_model)
linear_eval_2<- predict(fit.linear_model, main_test)
confusionMatrix(linear_eval_2,main_test$V1)

#  C      Accuracy   Kappa    
# 0.10  0.9058355  0.8953319
# Accuracy 90.58 @ C=0.1


#*******************************Using RBF Kernel********************************#
Model_RBF1 <- ksvm(V1 ~ ., data = main_train, scaled = FALSE, kernel = "rbfdot", C = 1, kpar = "automatic")
print(Model_RBF1) 
Pred_Model_RBF1 <- predict(Model_RBF1, newdata = main_test, type = "response")
confusionMatrix(Pred_Model_RBF1, main_test$V1) 

#Accuracy : 0.9358   
#Kappa : 0.9286
#                     Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7
#Sensitivity           0.95652   0.9932  0.92188  0.90741  0.97458  0.96262  0.99020   0.9194
#Specificity           0.99539   0.9981  0.99160  0.99267  0.99261  0.99085  0.98816   0.9935
# Accuracy is better than linear model

# Model 2
Model_RBF2 <- ksvm(V1 ~ ., data = main_train, scaled = FALSE, kernel = "rbfdot",C = 1, kpar = list(sigma = 1))
print(Model_RBF2)
Pred_Radial2 <- predict(Model_RBF2, newdata = main_test, type = "response")
confusionMatrix(Pred_Radial2, main_test$V1) 

#Accuracy :  0.1217 and statistics by class is very bad
#Sigma  = 1 is chosen thus we can conclude that model is overfitting

###########   Hyperparameter tuning and Cross Validation #####################

# We will use the train function from caret package to perform Cross Validation. 

#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 2 implies Number of folds in CV.


trainControl <- trainControl(method="cv", number=2)


#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.
grid = expand.grid(C= c(0.01, 0.1, 1, 5, 10), sigma = c(0.001, 0.01, 0.1, 1, 5)) 
no_cores <- detectCores() - 1  
registerDoParallel(cores=no_cores)  
cl <- makeCluster(no_cores) 
fit.svm_rbf <- train(V1 ~ ., data = main_train, metric = "Accuracy", method = "svmRadial",tuneGrid = grid,trControl = trainControl, preProcess = NULL)
stopCluster(cl)
print(fit.svm_rbf) 
plot(fit.svm_rbf)

#Best sigma value is 0.01 where the accuracy is highest

# Now lets check for the best value of c 
# Sigma =0.01  and multiple C values 
grid1 = expand.grid(C= c(1,2, 3, 4, 5), sigma = 0.01)
no_cores <- detectCores() - 1  
registerDoParallel(cores=no_cores)  
cl <- makeCluster(no_cores) 
fit.svm_rbf1 <- train(V1 ~ ., data = main_train, metric = "Accuracy", method = "svmRadial",tuneGrid = grid1,trControl = trainControl, preProcess = NULL)
stopCluster(cl)

# printing results of cross validation
print(fit.svm_rbf1) 
plot(fit.svm_rbf)
Pred_Radial3 <- predict(fit.svm_rbf1, newdata = main_test)
confusionMatrix(Pred_Radial3, main_test$V1)

# Accuracy 94.75 is highest at c =2 and sigma = 0.01

#*****************************Using Polynomial kernal******************************#

polynomial_model<-ksvm(V1~.,data = main_train, kernel = 'polydot', scaled = FALSE, C=1 )

polynomial_evaluation<-predict(polynomial_model, main_test)

confusionMatrix(polynomial_evaluation,main_test$V1)

# Accuracy  = 0.8917
#                     Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
#Sensitivity           0.96522   0.9932   0.8672  0.85185  0.94915  0.81308  0.97059  0.85484  0.81301  0.82946
#Specificity           0.98802   0.9924   0.9879  0.97894  0.98799  0.98811  0.98998  0.99164  0.98793  0.98693

#************************************Result*****************************************************#
# RBF model has the highest accuracy among all the model.
# Highest Accuracy is ~94.75 for c = 2, sigma = 0.01



