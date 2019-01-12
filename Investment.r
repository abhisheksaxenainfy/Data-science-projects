#***************************Investment project*****************************************************#
library(gdata)
library(tidyr)
library(dplyr)
library(stringr)
#set working directory
#setwd("C:/Users/abhishek.saxena02/Documents/BI-OLAP-LABSESSION/INPUT")

##########################################################################################################################
## Checkpoints - Part 1
## Checkpoint 1: Data Cleaning 1
## Loading the companies and rounds2 dataframes
## Companies is text file located in the CDN http://cdn.upgrad.com/UpGrad/temp/d934844e-5182-4b58-b896-4ba2a499aa57/companies.txt
## We wanted to directly download the file from the internet
## If you want use it locally then right click and download else it will open in the browser as text file
## Also if there network bandwidth issues, it is recomended that you download the file and use. In our case we use the url.
##########################################################################################################################


#Create dataframes by importng companies and rounds2
url <- 'http://cdn.upgrad.com/UpGrad/temp/d934844e-5182-4b58-b896-4ba2a499aa57/companies.txt'
companies <- read.table(url, sep = '\t',stringsAsFactors = FALSE, header = T,quote='\"', comment='')
rounds2 <- read.csv("rounds2.csv", header = TRUE,  stringsAsFactors = FALSE)


#change permalink and category to UPPER Case
companies[,c("permalink", "name", "category_list")] <- str_to_upper(c(companies$permalink, companies$name, companies$category_list) , locale = "en")
rounds2[,c("company_permalink")] <- str_to_upper(rounds2$company_permalink, locale = "en")



#Unique records in  rounds2 dataframe
n_distinct(rounds2$company_permalink)

#Unique companies in companies dataframe
n_distinct(companies$name)

#number of records not exist in companies from rounds2
which(rounds2$company_permalink %in% companies$permalink == FALSE)



#create master data frame
master_frame <- merge(x=rounds2, y=companies, by.x = "company_permalink", by.y = "permalink", all = TRUE)

##########################################################################################################################
## Checkpoints - Part 2
## Checkpoint 3: Country Analysis
## We have a standard ISO-3166-Countries-with-Regional-Codes file , we got it from Github
## https://github.com/lukes/ISO-3166-Countries-with-Regional-Codes/blob/master/all/all.csv
## After downloading this file , we copied and added the English speaking countries list in excel file "Continent_Countries_List.xlsx"
## With VLookup feature of Excel we were able get the country code for most of the countries in "English Speaking countries PDF"
## But some of them we could not match because of the string mismatch like Great Britian, United States, Swaziland etc
## By Manual Analysis and data massaging we got all the conflicts resolved.
## After resolution we saved that file as "English_Speaking_Countries_List.csv"
## Created a dataframe English_Speaking_Countries_List to identify those countries
##########################################################################################################################

if(!file.exists("English_Speaking_Countries_List.csv")){

############## Warning ###################################################################
print("'English_Speaking_Countries_List.csv' is missing in the getwd location please copy the file")
print("As an exception handling we have added the three records of USA, GBR and IND which we have found it as top3 english speaking countries")

############## End of Warning ###########################################################  
  df_eng <- data.frame(Continent=character(), Country=character(), Country_Code=character())
  de1 <- data.frame(Continent="Americas", Country="United States", Country_Code="USA")
  de2 <- data.frame(Continent="Europe", Country="United Kingdom", Country_Code="GBR")
  de3 <- data.frame(Continent="Asia", Country="India", Country_Code="IND")
  df_eng <- rbind(df_eng,de1 )
  df_eng <- rbind(df_eng,de2 )
  df_eng <- rbind(df_eng,de3 )
  English_Speaking_Countries_List <- df_eng
}else{

  English_Speaking_Countries_List <- read.csv(file="English_Speaking_Countries_List.csv",header = TRUE, stringsAsFactors = FALSE)
  #master_frame$is_english_speaking_country <-  master_frame$country_code %in% English_Speaking_Countries_List$Country_Code

}
# View the english speaking list
English_Speaking_Countries_List

#avg funding for 4 different funding types

master_frame %>% 
  group_by(funding_round_type)%>%
  summarise(avg_fund=mean(raised_amount_usd, na.rm =TRUE))%>%
  filter(funding_round_type=='angel'|
           funding_round_type=='venture'|
           funding_round_type=='private_equity'|
           funding_round_type=='seed')

                      


#funding by country, top 9 countries by total funding for venture funding type

master_frame%>%
  filter(funding_round_type=='venture')%>%
  group_by(country_code)%>%
  summarise(total_funding = sum(raised_amount_usd, na.rm =TRUE))%>%
  arrange(desc(total_funding)) %>%
  top_n(9, total_funding) -> top9

##########################################################################################################################
## Checkpoints - Part 2
## Checkpoint 3: Country Analysis
## Identify the top three English-speaking countries in the data frame top9.
##########################################################################################################################

top_english_speaking_countries<- top9[(top9$country_code %in% English_Speaking_Countries_List$Country_Code)==TRUE,]
top3_english_speaking_countries<-top_english_speaking_countries[1:3,]  
# View Result of top3_english_speaking_countries
top3_english_speaking_countries

##########################################################################################################################
## Checkpoints - Part 2
## Checkpoint 4: Sector Analysis 1
## This is the third goal of analysis - sector analysis.
##########################################################################################################################

#sector analysis
#read mapping file 
#to change the spl chr in column names use check.names = FALSE in read.csv
mapping <- read.csv("mapping.csv", header = TRUE, stringsAsFactors = FALSE)
str(mapping)
#function to replace 0 with "na" in category list 
replace_chr <- function(cl) {
  if(str_detect(cl, pattern = "[A-z]0")|str_detect(cl, pattern = "0[A-z]")) {
    str_to_upper(str_replace_all(cl, pattern = "0", "na"))
  } else str_to_upper(cl)
}

mapping[,"category_list"]<-sapply(mapping$category_list, function(x) replace_chr(x))
#change mapping to long
#mapping <- gather(mapping, sector, s_val,Automotive...Sports:Social..Finance..Analytics..Advertising)
mapping <- gather(mapping, sector, s_val,-category_list)
#remove all 0 and "BLANKS" values in category list
mapping <- mapping[!(mapping$s_val==0) & !(toupper(mapping$sector) == "BLANKS"),]
#write.csv(mapping, "mappingr.csv")

#get primary sector from master data frame
primary_category <- sapply(str_split(master_frame$category_list, pattern = "[|]"), '[[', 1)
#add primary sector to master data frame
master_frame$primary_sector <- primary_category



##########################################################################################################################
## Checkpoints - Part 2
## Checkpoint 5: Create three separate data frames D1, D2 and D3 for each of the three countries
##########################################################################################################################

#merge master and mapping data frames
master_frame <- merge(master_frame, mapping, by.x = "primary_sector", by.y = "category_list", all.x = TRUE)
#remove where s_val column from master data frame
master_frame <- subset(master_frame, select = -s_val)

#create three data frame for top three countries , FT, investment amount
D1<- filter(master_frame, country_code == "USA", funding_round_type =="venture", raised_amount_usd>=5000000, raised_amount_usd<=15000000 )
D2 <- filter(master_frame, country_code == "GBR", funding_round_type =="venture", raised_amount_usd>=5000000, raised_amount_usd<=15000000 )
D3 <- filter(master_frame, country_code == "IND", funding_round_type =="venture", raised_amount_usd>=5000000, raised_amount_usd<=15000000 )

#update three df with sector wise summary(count, total investment) 
#Function to find summary by sector for a DF.
fn_df_sector <- function(in_df) {
  
  in_df %>%
    group_by(sector) %>%
    summarise(count = n(),
              total_investment = sum(raised_amount_usd)) 
  
}
#merging D1, D2, D3 with summary(by count, total investment ) of sectors 
D1 <- merge(D1, fn_df_sector(D1), by="sector")
D2 <- merge(D2, fn_df_sector(D2), by="sector")
D3 <- merge(D3, fn_df_sector(D3), by="sector")

#function to find total investment and count for three countries
fn_ti_count_by_country <- function(in_df){
in_df %>%
  summarise(count = n(), investment = sum(raised_amount_usd))
}
# Calling function to find total investment and count for three countries
fn_ti_count_by_country(D1)
fn_ti_count_by_country(D2)
fn_ti_count_by_country(D3)
##########################################################################################################################
## Checkpoints - Part 2
## Checkpoint 5: Getting the count and investment of top three sectors for each country
##########################################################################################################################

#count and investment of top three sectors for each country 
head(arrange(fn_df_sector(D1), desc(count)), 3)
head(arrange(fn_df_sector(D2), desc(count)), 3)
head(arrange(fn_df_sector(D3), desc(count)), 3)



#Function to find TOP company for a given sector with maximum investmet for a given country
fn_top_company_by_sector<-function(in_df, in_sector) {
  in_df %>% filter(sector==in_sector) %>%
    group_by(company_permalink, name) %>% 
    summarise(
      total_investment = sum(raised_amount_usd)) %>%
    arrange(desc(total_investment)) %>% head(1)
  
}

##########################################################################################################################
## Checkpoints - Part 2
## Checkpoint 5: Getting details for USA -- United States of America
##########################################################################################################################

#Calling the function to find top company for given sector in USA 
fn_top_company_by_sector(D1, "Others")
fn_top_company_by_sector(D1, "Social..Finance..Analytics..Advertising")

##########################################################################################################################
## Checkpoints - Part 2
## Checkpoint 5: Getting details for GBR - Great Britan
##########################################################################################################################

#Calling the function to find top company for given sector in GBR

fn_top_company_by_sector(D2, "Others")
fn_top_company_by_sector(D2, "Social..Finance..Analytics..Advertising")

##########################################################################################################################
## Checkpoints - Part 2
## Checkpoint 5: Getting details for IND - India
####################################################################
#Calling the function to find top company for given sector in ind

fn_top_company_by_sector(D3, "Others")
fn_top_company_by_sector(D3, "Social..Finance..Analytics..Advertising")

##########################################################################################################################
## Checkpoints - Part 2
## Checkpoint 6: Save the master_frame to "master.csv" for further analysis in Tableau 
## Note: You have to create all the plots in Tableau.
## Pease run the tableau file "Investment_CS.twbx" uploaded along with this zip only after master.csv is created
##########################################################################################################################

#write master to csv for tableau analysis
write.csv(master_frame, "master.csv", row.names = FALSE)

#rm(list=ls())
#rm(mapping_long)

