#********************************Uber project***********************************************************************
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
require(lubridate)
library(stringr)
library(reshape)
#getwd()
#set working directory
#setwd("C:/Users/abhishek.saxena02/Documents/BI-OLAP-LABSESSION/INPUT")

# Loading dataset 
uber <- read.csv("Uber Request Data.csv",stringsAsFactors = F)
summary(uber)
str(uber)

#****************************************************************************************************************************
#Data Cleaning (Suitable date format of R), converting the multiple date format into the 
#default date format and saving into the Request.timestamp, Drop.timestamp respectively
#replacing the old Request.timestamp, drop.timestamp columns with formatted dated values.
#Handling of the NA values
#****************************************************************************************************************************
Ra<-as.POSIXct(uber$Request.timestamp, format = "%d-%m-%Y %H:%M:%S", tz = "GMT")
Rb<-as.POSIXct(uber$Request.timestamp, format = "%d/%m/%Y %H:%M", tz = "GMT")
Ra[is.na(Ra)] <- Rb[!is.na(Rb)]
typeof(Ra)
uber$Request.timestamp<-Ra
dummy_Drop.timestamp<-uber$Drop.timestamp[1:2831]
Da<-as.POSIXct(dummy_Drop.timestamp, format = "%d-%m-%Y %H:%M:%S", tz = "GMT" )
Db<-as.POSIXct(dummy_Drop.timestamp, format = "%d/%m/%Y %H:%M", tz = "GMT")
Da[is.na(Da)] <- Db[!is.na(Db)]
nrow(uber)
Da[2832:6745]<-NA
uber$Drop.timestamp<-Da
View(uber)

#****************************************************************************************************************************
# Request_hour, Derived a new column to get the insights on the specific time of the day
#****************************************************************************************************************************
uber$Request_hour<-format(uber$Request.timestamp, "%H")
uber$Request_hour<-as.numeric(uber$Request_hour)

#****************************************************************************************************************************
#weekday, Derived a new column to get the insights on the day of the week
#****************************************************************************************************************************
uber$Request_weekday<-weekdays(uber$Request.timestamp)

#****************************************************************************************************************************
#Funtion to derive the new column type of request
#Type of Request, from Airport-city or City-Airport
#****************************************************************************************************************************
Request_type<- function(a) {
  if (a[]=="Airport"){
    x<- paste(a[], "City", sep="-")
  }else {x<- paste(a[], "Airport", sep="-")}
}
uber$type_of_request <-sapply(uber$Pickup.point, Request_type)
#****************************************************************************************************************************
#Function to derive the peak hour column based on the hour of the day
#Hour_of_the_day
#****************************************************************************************************************************
hour_fun <- function(a) {
  if (a[]>=5 & a[]<10)
  {
    x<-"morning_peak_hrs"
  }
  else if (a[]>=17 & a[]<=22)
  {
    x<-"Evening_peak_hrs"
  }
  
  else{
    x<-"Normal_hrs"
  }
}
uber$Hour_of_the_day<-sapply(uber$Request_hour, hour_fun)
#****************************************************************************************************************************
#Univariate analysis
#For Status column
#percentage of cancelled request
#percentage of No cars available request
#percentage of No cars available request
#pie chart representation for the request
#****************************************************************************************************************************
select(uber, Status)%>% group_by(Status)%>%summarise(count=n())->Total_Status
View(Total_Status)
cancelled_percent<-(Total_Status$count[which(Total_Status$Status== "Cancelled")]/sum(Total_Status$count))*100
No_Cars_Available_percent<-(Total_Status$count[which(Total_Status$Status== "No Cars Available")]/sum(Total_Status$count))*100
Trip_Completed_percent<-(Total_Status$count[which(Total_Status$Status== "Trip Completed")]/sum(Total_Status$count))*100
pie(Total_Status$count, labels = Total_Status$Status, main = "Pie chart for the request")

#****************************************************************************************************************************
#PLOT:1 Plot of pickup point vs frequency or count of the request and with status
#****************************************************************************************************************************
ggplot(uber,aes(x=Pickup.point) )+geom_bar()


#****************************************************************************************************************************
#PLOT:2 plot to visualise the frequency of requests that get cancelled, show 'no cars available' and Trip completed.
#****************************************************************************************************************************
ggplot(uber, aes(x=Status))+geom_bar(position = "stack")

#shows that No cars available is the major issue and as from the graph we can see that count of cancelled trip is mostly half of the no cars available

#****************************************************************************************************************************
#Bivariate Analysis
#Analysis of Pickup point and Status
#****************************************************************************************************************************
uber_summary<-aggregate(Request.id~Pickup.point+Status,uber,length)

#****************************************************************************************************************************
#PLOT:3 Plot Pickup.point vs count of request id with status
#****************************************************************************************************************************
ggplot(uber, aes(x=Pickup.point, fill=Status)) +geom_bar()
ggplot(uber, aes(x=Pickup.point, fill=Status)) +geom_bar(position = "fill")

#****************************************************************************************************************************
#PLOT:4 identify the most problematic types of requests (city to airport / airport to city etc.) 
#****************************************************************************************************************************
ggplot(uber, aes(x=Status, fill = type_of_request))+geom_bar(position = "stack") 
ggplot(uber, aes(x=Status, fill = type_of_request))+geom_bar(position = "fill")



# From Airport to city there are very less cancellations. But MAJOR PROBLEM is CARS UNAVAILABLE at Airport for pickup
# City to Airport: Lot of cancellations, But other hand trips completed to airport is high


#****************************************************************************************************************************
#PLOT:5 Plot of the weekday vs status
#****************************************************************************************************************************
ggplot(uber, aes(x=Request_weekday))+geom_bar(position = "stack")
ggplot(uber, aes(x=Request_weekday , fill=Status)) + geom_bar(position = "stack")
#****************************************************************************************************************************
#PLOT:6 Plot request_hour vs frequency, identify the most problematic types of requests over the Requested hour
#****************************************************************************************************************************
ggplot(uber, aes(x=Request_hour)) + geom_bar(position = "stack")
ggplot(uber, aes(x=Request_hour , fill=Status)) + geom_bar(position = "stack")

#In evenings the majority of the times problem is no cars available where in morning there is the increase in the number of the cancellations


#After observing the plot 6 we can conclude that the peak hour in morning are from 5 to 10am, in evening from 5 to 10pm there is huge increase in demand
# when we take plot 3 & plot 4 in consideration then it is observed that from the graphs that most of the trips got cancelled where the type of request 
# is from city to airport and from plot 4 we can see that most cancellartion of the trip is there in morning
#which implies that there are more number ofcancellation from city-Airport and more number of time situation arises that no cars are available in evenings.

#****************************************************************************************************************************
#PLOT:7 Plot hour_of_the_day vs frequency of the request with Status
#****************************************************************************************************************************
ggplot(uber, aes(x=Hour_of_the_day)) + geom_bar(position = "stack")
ggplot(uber, aes(x=Hour_of_the_day, fill= Status)) + geom_bar(position = "stack")

#****************************************************************************************************************************
#PLOT:8 Plot hour_of_the_day vs frequency of the request with type of request
#****************************************************************************************************************************
ggplot(uber, aes(x=Hour_of_the_day, fill= type_of_request))+geom_bar(position = "stack")

#****************************************************************************************************************************
# Supply demand gap analysis
#
# Part- 1 Total Supply and demand
#****************************************************************************************************************************
Supply<-Total_Status$count[which(Total_Status$Status== "Trip Completed")]
demand<-sum(Total_Status$count)
Supply_demand_gap<-demand-Supply 
SD_ratio<-Supply/demand 
#the value of the supply demand ratio is 0.42 i.e. only 42% of the trip are getting completed company is losing out the revenue on 58% of the Requested trips

#****************************************************************************************************************************
#Part 2 (Pickup point-city)
#****************************************************************************************************************************
city_pickup<-subset(uber, Pickup.point=="City")
Supply_city<-nrow(subset(city_pickup, Status=="Trip Completed"))
demand_city<-nrow(city_pickup)
Supply_demand_gap_city<-demand_city-Supply_city
SD_ratio_city<-Supply_city/demand_city

#****************************************************************************************************************************
#Part 3 (Pickup point-Airport)
#****************************************************************************************************************************
Airport_pickup<-subset(uber, Pickup.point=="Airport")
Supply_airport<-nrow(subset(city_pickup, Status=="Trip Completed"))
demand_airport<-nrow(Airport_pickup)
Supply_demand_gap_airport<-demand_airport-Supply_airport
SD_ratio_Airport<-Supply_airport/demand_airport

#****************************************************************************************************************************
# Part 4: Total supply demand gap during peak_hrs(Evening_peak_hrs, morning_peak_hrs)
#****************************************************************************************************************************
peak_hrs<-subset(uber, Hour_of_the_day!="Normal_hrs")
Supply_peak_hrs<-nrow(subset(peak_hrs, Status=="Trip Completed"))
demand_peak_hrs<-nrow(peak_hrs)
Supply_demand_gap_peak_hrs<-demand_peak_hrs-Supply_peak_hrs
SD_ratio_peak_hrs<-Supply_peak_hrs/demand_peak_hrs

#****************************************************************************************************************************
#Part 5 supply demand gap during peak_hrs(Evening_peak_hrs, morning_peak_hrs) from City
#****************************************************************************************************************************
peak_hrs_city<-subset(city_pickup,Hour_of_the_day!="Normal_hrs")
Supply_peak_hrs_city<-nrow(subset(peak_hrs_city,Status=="Trip Completed"))
demand_peak_hrs_city<-nrow(peak_hrs_city)
Supply_demand_gap_peak_hrs_city<-demand_peak_hrs_city-Supply_peak_hrs_city
SD_ratio_peak_hrs_city<-Supply_peak_hrs_city/demand_peak_hrs_city

#****************************************************************************************************************************
#Part 6 supply demand gap during peak_hrs(Evening_peak_hrs, morning_peak_hrs) from Airport
#****************************************************************************************************************************
peak_hrs_airport<-subset(Airport_pickup,Hour_of_the_day!="Normal_hrs")
Supply_peak_hrs_airport<-nrow(subset(peak_hrs_airport,Status=="Trip Completed"))
demand_peak_hrs_airport<-nrow(peak_hrs_airport)
Supply_demand_gap_peak_hrs_airport<-demand_peak_hrs_airport-Supply_peak_hrs_airport
SD_ratio_peak_hrs_airport<-Supply_peak_hrs_airport/demand_peak_hrs_airport

#****************************************************************************************************************************
#TO convert the dataset into CSV format for the use in tableau
#****************************************************************************************************************************
write.csv(uber, file="C:\\Users\\abhishek.saxena02\\Documents\\BI-OLAP-LABSESSION\\INPUT\\uber.csv")















