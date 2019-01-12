
# Loan default project

################################################################################################################
##  Data understanding and preparation
################################################################################################################


#### Check for missing packages
packagesused <- c("readr","vcd","lattice", "dplyr","ggplot2","psych","magrittr","lubridate","stringr","cowplot","gridExtra","corrplot")
packagesmissing <- packagesused[(!packagesused %in% installed.packages())]
if(length(packagesmissing) > 0) install.packages(packagesmissing)

#### Load the packages in memory
library(readr) # CSV file I/O, e.g. the read_csv function
library(vcd)
library(lattice)
library(dplyr)
library(ggplot2)
library(psych) # for neater descriptive statistics
library(magrittr)
library(lubridate)
library(stringr) #string_replace_all function 
library(cowplot) #package for combining multiple plots
library(gridExtra) #package required for tablegrob
library(corrplot)


remove (list=ls())
#### Setting up R working directory
#setwd("")
##
#setwd("C:/Users/abhishek.saxena02/Documents/BI-OLAP-LABSESSION/INPUT")

loan <- read.csv("loan.csv", stringsAsFactors = FALSE, na.strings=c("NA","#DIV/0!", "","NaN"))
attach(loan)

#### Checking the summary stats ####
dim(loan)
summary(loan)
#############

# check columns and rows 
cat('Loan Dataframe has', dim(loan)[1], 'rows and', dim(loan)[2], 'columns.')

# Number of duplicate rows 
cat("The number of duplicated rows are", nrow(loan) - nrow(unique(loan)))

# Head few records 
head(loan)
summry<- summary(loan)
View(summry)
str(loan)

# colsums - NA 
barchart(colSums(is.na(loan)))
################################################################################################################################################################
### Analyze columns to be removed
################################################################################################################################################################

# Create function to understand the Total, Missing, Missing_Percentage, Duplicates and Distinct Values

CheckDataForMissingValues2 <- function(x) (
  
  data.frame(	"Total" = length(x),   
              "Missing" = sum(is.na(x)),  
              "Missing_Percentage" = ifelse(length(x)>0, round((sum(is.na(x))/length(x)) * 100,2),0),  
              "Duplicates"=sum(duplicated(x)),  
              "Distinct" = length(unique(x)) 
              #"colName" = colnames(x)
  ));

loan_summary_list <- sapply(loan,CheckDataForMissingValues2 )        

loan_summary_matrix <-matrix(unlist(loan_summary_list), nrow=111, byrow=T)  ## 111 is the total number of columns we have

loan_summary_df <- data.frame(loan_summary_matrix)
colnames(loan_summary_df) <- c("Total", "Missing", "Missing_Percentage", "Duplicates", "Distinct")
colName<-  dimnames(loan_summary_list)[2]
colName <- unlist(colName)
loan_summary_df$ColName <- colName

nrow( loan_summary_df[loan_summary_df$Missing_Percentage==100,  ]) ## columns to be removed
loan_summary_df
View(loan_summary_df)

################################################################################################
# Data Cleaning and Manipulation
################################################################################################

# remove columns with all na
loan<-loan[,colSums(is.na(loan)) <39717]
# Chek the trend in columns with NA 
barchart(colSums(is.na(loan)))




# Only 1 value 
# NUmerical vs Categorical 
length(names(loan)[which(sapply(loan, is.character))])
names(loan)[which(sapply(loan, is.character))]

length(names(loan)[which(sapply(loan, is.numeric))])
names(loan)[which(sapply(loan, is.numeric))]

# handling the date variable 
loan$issue_date <- as.Date(gsub("^", "01-", loan$issue_d), format="%d-%b-%Y")
head(loan$issue_date)

loan$last_pymnt_date <- as.Date(gsub("^", "01-", loan$last_pymnt_d), format="%d-%b-%Y")
head (loan$last_pymnt_date)

loan$next_pymnt_date <- as.Date(gsub("^", "01-", loan$next_pymnt_d), format="%d-%b-%Y")
head (loan$next_pymnt_date)

# Indentifying the outliers in the annual income
boxplot(loan$annual_inc,horizontal = T)$stats[c(1, 5), ] #lower whisker-4000, upper whisker-145008

# Finding columns with only 1 unique value and removing them 

only_oneUnique <- vector()

for (n in names(loan)){
  x<-(length(levels(as.factor(loan[[n]]))))
  #print(x)
  
  if (x==1){
    only_oneUnique<- append(only_oneUnique,n)
  }
}

View(only_oneUnique)

loan<- loan[ , !(names(loan) %in% only_oneUnique)]

# Converting the intrest rate value from string to number 
loan$int_rate <- as.numeric(gsub("%", "", loan$int_rate))

# Converintg to factor 
loan$id<- as.factor(loan$id)  
loan$member_id<- as.factor(loan$member_id) 
loan$term<-as.factor(loan$term)
loan$grade<-as.factor(loan$grade)
loan$sub_grade<-as.factor(loan$sub_grade)
loan$emp_length<-as.factor(loan$emp_length)
loan$home_ownership<-as.factor(loan$home_ownership)
loan$home_ownership<-as.factor(loan$home_ownership)
loan$verification_status<-as.factor(loan$verification_status)
loan$loan_status<-as.factor(loan$loan_status)
loan$pymnt_plan<-as.factor(loan$pymnt_plan)
loan$purpose<-as.factor(loan$purpose)
loan$addr_state<-as.factor(loan$addr_state)

dim(loan)

## For a neater view of summary, using describe from psych
describe(loan)

############################################################################################################
##  Data analysis
############################################################################################################

###########################################################################################################

# Univariate analysis 
# Loan status

############################################################################################################

levels(loan$loan_status)
table(loan$loan_status)
ggplot(loan, aes(term, fill = loan_status)) + geom_density() + facet_grid(loan_status ~ .)

#multi valued columns 
length(levels(as.factor(loan$member_id)))
length(levels(as.factor(loan$id)))
length(levels(as.factor(loan$sub_grade)))
length(levels(as.factor(loan$emp_title)))


#continues valued columns 
summary(loan$loan_amnt)
summary(loan$funded_amnt)
summary(loan$funded_amnt_inv)
summary(loan$int_rate)

# categorical values 
#Term
levels(as.factor(loan$term))
table(loan$term)
ggplot(loan, aes(x=term)) + geom_bar()
#grade
levels(as.factor(loan$grade))
table(loan$grade)
ggplot(loan, aes(x=grade)) + geom_bar()

#EMp_length
levels(as.factor(loan$emp_length))
table(loan$emp_length)
ggplot(loan, aes(x=emp_length)) + geom_bar()

#home_ownership
levels(as.factor(loan$home_ownership))
table(loan$home_ownership)
ggplot(loan, aes(x=home_ownership)) + geom_bar()

#Verification status
levels(as.factor(loan$verification_status))
table(loan$verification_status)
ggplot(loan, aes(x=verification_status)) + geom_bar()

# Creating a derived vaiable to segrigate good and bad loan 
bad_indicators <- c("Default",
                    "Charged Off")
loan$is_bad <- ifelse(loan$loan_status %in% bad_indicators, 1,
                      ifelse(loan$loan_status=="", NA,
                             0))

table(loan$is_bad)
mean(loan$is_bad)
ggplot(loan, aes(x=is_bad)) + geom_bar()



levels(as.factor(loan$purpose))
ggplot(loan, aes(x=purpose)) + geom_bar()


levels(as.factor(loan$pub_rec_bankruptcies))
table(loan$pub_rec_bankruptcies)
ggplot(loan, aes(pub_rec_bankruptcies)) + geom_bar()


###################################################################################################################
####Bivariate analysis for bad loans ###########
###################################################################################################################
# Avoid exponential notations
options(scipen=999)
options(max.print=999999) 

names(loan)
loan1<- loan
#Function to create filled bar gaph 
fn_FillBarG <- function(x_axis,y_axis,X_LABEL,Y_LABEL){
  
  p <- ggplot(data=loan, aes(x=as.factor(x_axis), y=y_axis))+geom_bar(stat="identity", 
                                                                      aes(fill=x_axis))+coord_flip()+xlab("")+
    ylab(paste0(Y_LABEL))+ggtitle(paste0(X_LABEL," VS ", Y_LABEL))+xlab(X_LABEL)
  
  print(p)
}

fn_FillBarG2<- function(x_axis,y_axis,X_LABEL,Y_LABEL){
ggplot(loan, aes(x= x_axis, fill = y_axis)) + geom_bar() +ylab(paste0(Y_LABEL))+ggtitle(paste0(X_LABEL," VS ", Y_LABEL))+xlab(X_LABEL)
}
ggplot(loan, aes(x= emp_length, fill = factor(is_bad))) + geom_bar()

# Categorical variables VS Bad Loans plots
#Purpose
table(loan$purpose, loan$is_bad)
fn_FillBarG(purpose,"is_bad",X_LABEL='Purpose',Y_LABEL='IS BAD')
fn_FillBarG2(purpose,'is_bad',X_LABEL='Purpose',Y_LABEL='IS BAD')

#Term
table(loan$term, loan$is_bad)
fn_FillBarG(term,'is_bad',X_LABEL='term',Y_LABEL='Is Bad')

#Grade
table(loan$grade, loan$is_bad)
fn_FillBarG(grade,'is_bad',X_LABEL='grade',Y_LABEL='Is Bad')

#Sub-Grade
table(loan$sub_grade, loan$is_bad)
fn_FillBarG(sub_grade,'is_bad',X_LABEL='sub_grade',Y_LABEL='Is Bad')

#Emp_length
table(loan$emp_length, loan$is_bad)
fn_FillBarG(emp_length,'is_bad',X_LABEL='emp_length',Y_LABEL='Is Bad')

#Home_ownership
table(loan$home_ownership, loan$is_bad)
fn_FillBarG(home_ownership,'is_bad',X_LABEL='home_ownership',Y_LABEL='Is Bad')

#Verification status
table(loan$verification_status, loan$is_bad)
fn_FillBarG(verification_status,'is_bad',X_LABEL='verification_status',Y_LABEL='Is Bad')

#Address State
table(loan$addr_state, loan$is_bad)
fn_FillBarG(addr_state,'is_bad',X_LABEL='addr_state',Y_LABEL='Is Bad')

#Degoratory public record 
table(loan$pub_rec, loan$is_bad)
fn_FillBarG(pub_rec,'is_bad',X_LABEL='pub_rec',Y_LABEL='Is Bad')
#drill down analysis of Degoratory public record 
select(loan,pub_rec,is_bad)->public_record_data
subset(public_record_data, is_bad!=1)%>%group_by(pub_rec)%>% summarise(count=n())->pub_rc_not_bad
subset(public_record_data, is_bad!=0)%>%group_by(pub_rec)%>% summarise(count=n())->pub_rc_is_not_bad
merge(pub_rc_not_bad, pub_rc_is_not_bad, by = "pub_rec")->total_pub_rc_dataset
names(total_pub_rc_dataset)[2]<-paste("pub_rc_not_bad")
names(total_pub_rc_dataset)[3]<-paste("pub_rc_is_bad")
total_pub_rc_dataset$total_value<-total_pub_rc_dataset$pub_rc_not_bad+total_pub_rc_dataset$pub_rc_is_bad
total_pub_rc_dataset$percent<-(total_pub_rc_dataset$pub_rc_is_bad/total_pub_rc_dataset$total_value)*100
View(total_pub_rc_dataset)

#Public bankruptcy record
table(loan$pub_rec_bankruptcies, loan$is_bad)
fn_FillBarG(pub_rec_bankruptcies,'is_bad',X_LABEL='pub_rec_bankruptcies',Y_LABEL='Is Bad')
#drill down analysis of Public bankruptcy record 
select(loan,pub_rec_bankruptcies,is_bad)->public_bankrupcy_data
subset(public_bankrupcy_data, is_bad!=1)%>%group_by(pub_rec_bankruptcies)%>% summarise(count=n())->pub_bd_not_bad
subset(public_bankrupcy_data, is_bad!=0)%>%group_by(pub_rec_bankruptcies)%>% summarise(count=n())->pub_bd_is_not_bad
merge(pub_bd_not_bad, pub_bd_is_not_bad, by = "pub_rec_bankruptcies")->total_pub_bd_dataset
names(total_pub_bd_dataset)[2]<-paste("pub_bd_not_bad")
names(total_pub_bd_dataset)[3]<-paste("pub_bd_is_bad")
total_pub_bd_dataset$total_value<-total_pub_bd_dataset$pub_bd_not_bad+total_pub_bd_dataset$pub_bd_is_bad
total_pub_bd_dataset$percent<-(total_pub_bd_dataset$pub_bd_is_bad/total_pub_bd_dataset$total_value)*100
View(total_pub_bd_dataset)

#Total no of credit account
table(loan$total_acc, loan$is_bad)
fn_FillBarG(total_acc,'is_bad',X_LABEL='total_acc',Y_LABEL='Is Bad')

#Interest rate

select(loan, int_rate, is_bad)->interest_data

interest_bin <- function(a) {
  if (a[]>=5 & a[]<=7.5){
    x<-"1st_bin(5-7.5)"
  }
  else if (a[]>7.5 & a[]<10){
    x<-"2nd_bin(7.5-10)"
  }
  else if (a[]>10 & a[]<12.5){
    x<-"3rd_bin(10-12.5)"
  }
  else if (a[]>12.5 & a[]<15){
    x<-"4th_bin(12.5-15)"
  }
  else if (a[]>15 & a[]<17.5){
    x<-"5th_bin(15-17.5)"
  }
  else if (a[]>17.5 & a[]<20){
    x<-"6th_bin(17.5-20)"
  }
  else if (a[]>20 & a[]<22.5){
    x<-"7th_bin(20-22.5)"
  }
  else if (a[]>22.5 & a[]<25){
    x<-"8th_bin(22.5-25)"
  }
  else if (a[]>25 & a[]<27.5){
    x<-"9th_bin(25-27.5)"
  }
  else if (a[]>27.5 & a[]<30){
    x<-"10th_bin(27.5-30)"
  }
}

interest_data$bin<-sapply(interest_data$int_rate,interest_bin )
interest_data$bin<-as.character(interest_data$bin)
typeof(interest_data$bin)
subset(interest_data, is_bad!=1)%>%group_by(bin)%>% summarise(count=n())->interest_data_not_bad
names(interest_data_not_bad)[2]<-paste("Count_not_bad")
subset(interest_data, is_bad!=0)%>%group_by(bin)%>% summarise(count=n())->interest_data_is_bad
names(interest_data_is_bad)[2]<-paste("Count_is_bad")
merge(interest_data_not_bad, interest_data_is_bad, by = "bin")->total_interest_dataset
total_interest_dataset$total_applications<-(total_interest_dataset$Count_not_bad+total_interest_dataset$Count_is_bad)
total_interest_dataset$is_bad_percent<-(total_interest_dataset$Count_is_bad/total_interest_dataset$total_applications)*100
View(total_interest_dataset)
ggplot(data = total_interest_dataset, aes(x=bin, y=is_bad_percent))+geom_point()


###############################################################################################################
### Correlation
# install.packages("corrplot") # updated above
# library(corrplot) # updated above
###############################################################################################################
length(colnames(loan))
glimpse(loan)
numeric_cols <- unlist(lapply(loan, is.numeric))

numeric_df <- loan[,numeric_cols]
#numeric_df <- numeric_df[,-c(1,2)] # id is 1 and member_id is 2

colnames(numeric_df)

cordf <-cor(numeric_df)
corrplot(cordf)
head(cordf)
colnames(cordf)


###############################################################################################################

