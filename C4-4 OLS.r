# SYS 6018 Kaggle Competition 4
# Group C4-4
# Navin Kasa
# Niharika Reddy
# Mengyao Zhang

library(tidyverse)
library(MASS)
library(dplyr)
library(jsonlite)
library(readr)
library(magrittr)
library(lubridate)
library(purrr)
library(ggplot2)
library(gridExtra)
#install.packages("countrycode")
library(countrycode)
#install.packages("highcharacter")
library(highcharter)
#install.packages("ggExtra")
library(ggExtra)
library(data.table)
#install.packages("funModeling")
library(funModeling)
library(gridExtra)
#library(dplyr)
#install.packages("zoo")
library(zoo)
library(stringr)
#install.packages("chron")
library(chron)
#install.packages("splusTimeDate")
library(splusTimeDate)
#install.packages("bsts")
library(bsts)
library(chron)


#Reading in data and combining the training and testing datasets
train <- read_csv(file = "train.csv", col_names = T) %>% mutate(Data= "Training")
test <- read_csv(file = "test.csv", col_names = T) %>% mutate(Data= "Testing")
df<-rbind(train,test)

# read in the updated test data
test_new <- read_csv(file="test_v2.csv",col_names = T)
head(test_new)
test_new$totals[1:10]
test_new$hits[1:10]

# drop the hits column
test_new <- test_new[,-c(2,7)] # dop the hits and customDimensions columns

colnames(test_new)

"
================================================================================
DATA CLEANING
================================================================================"
#Reading in data and combining the training and testing datasets
train <- read_csv(file = "train.csv", col_names = T) %>% mutate(Data= "Training")
test <- read_csv(file = "test.csv", col_names = T) %>% mutate(Data= "Testing")


df<-rbind(train,test)

#Viewing the data
head(df)
#There seem to be some JSON columns

str(df)
#JSON columns are : device, geoNetwork, totals, trafficSource

#Writing function to parse JSON
ParseJSONColumn <- function(x)  {
  paste("[ ", paste(x, collapse = ",", sep=" "), " ]")  %>% 
    fromJSON(flatten = T) %>% 
    as.tibble()
}

JSONcolumn_data <- df  %>% 
  dplyr::select(trafficSource, totals, geoNetwork, device)

JSON_cols<-apply(JSONcolumn_data,2, FUN = ParseJSONColumn)
save(JSON_cols, file = "JSON_parsed.Rdata")

head(JSON_cols)
df <- cbind(df, JSON_cols) 

# dropping the old json columns
df<-df %>% dplyr::select(-device, -geoNetwork, -totals, -trafficSource)
head(df)


#Several of the columns seem to have "not available in demo dataset","(not provided) "
#setting the same to NA

# values to convert to NA
na_vals <- c("unknown.unknown", "(not set)", "not available in demo dataset", 
             "(not provided)", "(none)", "<NA>")

for(col in 1:ncol(df)){
  df[which(df[,col] %in% na_vals), col]= NA
}
glimpse(df)

#write.table(df, "cleaned_total_data.csv", row.names=F, sep=",")



#All of the columns that were converted from json are of class character. 
#For some, we will need to change this.

# character columns to convert to numeric
num_cols <- c('totals.hits', 'totals.pageviews', 'totals.bounces', 'totals.newVisits',
              'totals.transactionRevenue')
df[, num_cols] = lapply(df[, num_cols], function(x){as.numeric(x)})

glimpse(df)

#Coverting date from int to date format
df$date <- as.Date(as.character(df$date), format='%Y%m%d')

# convert visitStartTime to POSIXct
df$visitStartTime <- as_datetime(df$visitStartTime)
glimpse(df)

#imputing transaction revenue to 0 before removing na columns
df$totals.transactionRevenue[is.na(df$totals.transactionRevenue)] <- 0

# Imputing missing countries where city is captured
df$geoNetwork.city[(df$geoNetwork.country %>% is.na()) & (!df$geoNetwork.city %>% is.na())]

# [1] "Ningbo"        "New York"      "San Francisco" "Tunis"         "Nairobi"      
# [6] "New York"      "Manila"        "Osaka"         "New York"      "Kyiv"         
# [11] "Kyiv"          "Kyiv"          "Hong Kong"     "Santa Clara"   "Kyiv"         
# [16] "Moscow"        "Kyiv"          "Kyiv"          "Kyiv"          "Kyiv"         
# [21] "London"        "Dublin"        "London"        "Minneapolis"   "New York"     
# [26] "New York"      "Melbourne"     "Buenos Aires"  "London"        "Dublin"       
# [31] "Kyiv"          "London"        "Kyiv"          "Kyiv"          "Kyiv"         
# [36] "Kyiv"          "Kyiv"          "Bengaluru"  
# 

df$geoNetwork.country[df$geoNetwork.city %in% c("San Francisco", "New York","Santa Clara","Minneapolis")] <- "United States"
df$geoNetwork.country[df$geoNetwork.city %in% c("Tunis")] <- "Tunisia"
df$geoNetwork.country[df$geoNetwork.city %in% c("Nairobi")] <- "Kenya"
df$geoNetwork.country[df$geoNetwork.city %in% c("Manila")] <- "Philippines"
df$geoNetwork.country[df$geoNetwork.city %in% c("Osaka")] <- "Japan"
df$geoNetwork.country[df$geoNetwork.city %in% c("Kyiv")] <- "Ukraine"
df$geoNetwork.country[df$geoNetwork.city %in% c("Hong Kong")] <- "Hong Kong"
df$geoNetwork.country[df$geoNetwork.city %in% c("Moscow")] <- "Moscow"
df$geoNetwork.country[df$geoNetwork.city %in% c("London")] <- "United Kingdom"
df$geoNetwork.country[df$geoNetwork.city %in% c("Dublin")] <- "Ireland"
df$geoNetwork.country[df$geoNetwork.city %in% c("Melbourne")] <- "Australia"
df$geoNetwork.country[df$geoNetwork.city %in% c("Buenos Aires")] <- "Argentina"
df$geoNetwork.country[df$geoNetwork.city %in% c("Bengaluru")] <- "India"

"
============================================================================
EDA and Dimensionality Reduction
============================================================================
"

# Finding time ranges for train and test data
time_range_train <- range(train$date)
print(time_range_train)
#[1] "2016-08-01" "2017-08-01"

time_range_test <- range(test$date)
print(time_range_test)
#"2017-08-02" "2018-04-30"

#Checking the distribution of transaction revenues across time in the training data
g1 <- train[, .(n = .N), by=date] %>%
  ggplot(aes(x=date, y=n)) + 
  geom_line(color='steelblue') +
  geom_smooth(color='orange') + 
  labs(
    x='',
    y='Visits (000s)',
    title='Daily visits'
  )

g2 <- train[, .(revenue = sum(transactionRevenue, na.rm=TRUE)), by=date] %>%
  ggplot(aes(x=date, y=revenue)) + 
  geom_line(color='steelblue') +
  geom_smooth(color='orange') + 
  labs(
    x='',
    y='Revenue (unit dollars)',
    title='Daily transaction revenue'
  )

grid.arrange(g1, g2, nrow=2)
g1 <- train[, .(n = .N), by=channelGrouping] %>%
  ggplot(aes(x=reorder(channelGrouping, -n), y=n/1000)) +
  geom_bar(stat='identity', fill='steelblue') +
  labs(x='Channel Grouping',
       y='Visits (000s)',
       title='Visits by channel grouping')



#Checking for columns with missing values
options(repr.plot.height=4)
NAcol <- which(colSums(is.na(df)) > 0)
NAcount <- sort(colSums(sapply(df[NAcol], is.na)), decreasing = TRUE)

colSums(df["device.operatingSystemVersion"] %>% is.na())

NAcount


NADF <- data.frame(variable=names(NAcount), missing=NAcount)
NADF$PctMissing <- round(((NADF$missing/nrow(df))*100),1)
NADF %>%
  ggplot(aes(x=reorder(variable, PctMissing), y=PctMissing)) +
  geom_bar(stat='identity', fill='blue') + coord_flip(y=c(0,110)) +
  labs(x="", y="Percent missing") +
  geom_text(aes(label=paste0(NADF$PctMissing, "%"), hjust=-0.1))



#Dropping all columns with more than 90% missing values

df1<-df[,colSums(!is.na(df)) > 0.9*nrow(df) ]
glimpse(df1)


# Converting some of the character variables to factors

categorical_columns <- c("device.browser", "device.deviceCategory", "device.operatingSystem", "geoNetwork.continent", "geoNetwork.country", "geoNetwork.subContinent", "trafficSource.source")

df1 <- mutate_at(df1, categorical_columns, as.factor)


#Exploring no. of unique values in columns to decide which additional columns can be dropped


#trafficSource.source analysis
unique(df1$trafficSource.source)
# More than 1 unique columns hence retaining the column


unique(df1$totals.visits)
# Unique values are 1, hence dropping the column

unique(df1$channelGrouping)
unique(df1$totals.hits)

unique(df1$totals.pageviews)

unique(df1$visitNumber)
unique(df1$socialEngagementType)
#Need to drop socialEngagementType as there is only 1 unique value
df1 <- subset(df1, select = -c(totals.visits,socialEngagementType))

# As continent and subcontinent are dervived from country, dropping those columns as well
df1 <- subset(df1, select = -c(geoNetwork.continent,geoNetwork.subContinent))





####### geoNetwork.country analysis

###### Unique country list
# [1] "Turkey"                   "Australia"                "Spain"                    "Indonesia"               
# [5] "United Kingdom"           "Italy"                    "Pakistan"                 "Austria"                 
# [9] "Netherlands"              "India"                    "France"                   "Brazil"                  
# [13] "China"                    "Singapore"                "Argentina"                "Poland"                  
# [17] "Germany"                  "Canada"                   "Thailand"                 "Hungary"                 
# [21] "Malaysia"                 "Denmark"                  "Taiwan"                   "Russia"                  
# [25] "Nigeria"                  "Belgium"                  "South Korea"              "Chile"                   
# [29] "Ireland"                  "Philippines"              "Greece"                   "Mexico"                  
# [33] "Montenegro"               "United States"            "Bangladesh"               "Japan"                   
# [37] "Slovenia"                 "Czechia"                  "Sweden"                   "United Arab Emirates"    
# [41] "Switzerland"              "Portugal"                 "Peru"                     "Hong Kong"               
# [45] "Vietnam"                  "Sri Lanka"                "Serbia"                   "Norway"                  
# [49] "Romania"                  "Kenya"                    "Ukraine"                  "Israel"                  
# [53] "Slovakia"                 NA                         "Lithuania"                "Puerto Rico"             
# [57] "Bosnia & Herzegovina"     "Croatia"                  "South Africa"             "Paraguay"                
# [61] "Botswana"                 "Colombia"                 "Uruguay"                  "Algeria"                 
# [65] "Finland"                  "Guatemala"                "Egypt"                    "Malta"                   
# [69] "Bulgaria"                 "New Zealand"              "Kuwait"                   "Uzbekistan"              
# [73] "Saudi Arabia"             "Cyprus"                   "Estonia"                  "Côte d’Ivoire"           
# [77] "Morocco"                  "Tunisia"                  "Venezuela"                "Dominican Republic"      
# [81] "Senegal"                  "Cape Verde"               "Costa Rica"               "Kazakhstan"              
# [85] "Macedonia (FYROM)"        "Oman"                     "Laos"                     "Ethiopia"                
# [89] "Panama"                   "Belarus"                  "Myanmar (Burma)"          "Moldova"                 
# [93] "Zimbabwe"                 "Bahrain"                  "Mongolia"                 "Ghana"                   
# [97] "Albania"                  "Kosovo"                   "Georgia"                  "Tanzania"                
# [101] "Bolivia"                  "Cambodia"                 "Turks & Caicos Islands"   "Iraq"                    
# [105] "Jordan"                   "Lebanon"                  "Ecuador"                  "Madagascar"              
# [109] "Togo"                     "Gambia"                   "Jamaica"                  "Trinidad & Tobago"       
# [113] "Mauritius"                "Libya"                    "Mauritania"               "El Salvador"             
# [117] "Azerbaijan"               "Nicaragua"                "Palestine"                "Réunion"                 
# [121] "Iceland"                  "Greenland"                "Armenia"                  "Haiti"                   
# [125] "Uganda"                   "Qatar"                    "St. Kitts & Nevis"        "Somalia"                 
# [129] "Cameroon"                 "Namibia"                  "Latvia"                   "Congo - Kinshasa"        
# [133] "New Caledonia"            "Rwanda"                   "Kyrgyzstan"               "Honduras"                
# [137] "Nepal"                    "Benin"                    "Luxembourg"               "Guinea"                  
# [141] "Belize"                   "Guinea-Bissau"            "Sudan"                    "Yemen"                   
# [145] "Gabon"                    "Maldives"                 "Mozambique"               "French Guiana"           
# [149] "Zambia"                   "Macau"                    "Tajikistan"               "Angola"                  
# [153] "Guadeloupe"               "Martinique"               "Brunei"                   "Guyana"                  
# [157] "St. Lucia"                "Iran"                     "Monaco"                   "Swaziland"               
# [161] "Curaçao"                  "Bermuda"                  "Guernsey"                 "Afghanistan"             
# [165] "Northern Mariana Islands" "Guam"                     "Antigua & Barbuda"        "Sint Maarten"            
# [169] "Andorra"                  "St. Vincent & Grenadines" "Fiji"                     "Mali"                    
# [173] "Papua New Guinea"         "Jersey"                   "Faroe Islands"            "Cayman Islands"          
# [177] "Chad"                     "French Polynesia"         "Malawi"                   "Suriname"                
# [181] "Barbados"                 "U.S. Virgin Islands"      "Djibouti"                 "Mayotte"                 
# [185] "Aruba"                    "Lesotho"                  "Equatorial Guinea"        "Burkina Faso"            
# [189] "Grenada"                  "Norfolk Island"           "Isle of Man"              "Liechtenstein"           
# [193] "Vanuatu"                  "Sierra Leone"             "Bahamas"                  "Åland Islands"           
# [197] "St. Pierre & Miquelon"    "Gibraltar"                "British Virgin Islands"   "Burundi"                 
# [201] "Turkmenistan"             "Niger"                    "Samoa"                    "Timor-Leste"             
# [205] "Syria"                    "Comoros"                  "Liberia"                  "Bhutan"                  
# [209] "Cook Islands"             "American Samoa"           "Dominica"                 "Anguilla"                
# [213] "Caribbean Netherlands"    "Marshall Islands"         "Congo - Brazzaville"      "Seychelles"              
# [217] "San Marino"               "Central African Republic" "St. Martin"               "São Tomé & Príncipe"     
# [221] "Eritrea"                  "St. Barthélemy"           "South Sudan"              "Solomon Islands"         
# [225] "Montserrat"               "St. Helena"               "Tonga"                    "Micronesia" 




# Feature engineering using Date for holidays

us.bank.holidays <- read_csv("US Bank holidays.csv")
us.bank.holidays <- us.bank.holidays[, ! names(us.bank.holidays) %in% c("index"), drop = F]

holidays <- us.bank.holidays$date %>% as.list()


for(i in 1:11){
  buffer.dates <- holidays %>% lapply(function(d){
    
    data.frame(date=as.Date(d)-i, holiday = us.bank.holidays$holiday[us.bank.holidays$date==as.Date(d)])
  })
  
  buffer.dates <- do.call(rbind,buffer.dates)
  us.bank.holidays <- us.bank.holidays %>% rbind(buffer.dates)
}

us.bank.holidays = us.bank.holidays[!duplicated(us.bank.holidays$date),]

df2 <- left_join(df1,unique(us.bank.holidays), by=c("date"))

df2 <- df2[,!names(df2) %in% c("holiday.x"), drop=F]

names(df2)[names(df2) == 'holiday.y'] <- 'holiday'

# removing some holidays for non-US countries
us.holidays <- c("New Year Day", "Independence Day", "Labor Day", "Thanksgiving Day", "Christmas Day")
row.holidays <- c("New Year Day", "Christmas Day")
df2$holiday[(df2$geoNetwork.country =="United States") & ! (df2$holiday %in% us.holidays) ] <- NA
df2$holiday[(df2$geoNetwork.country!="United States") & ! (df2$holiday %in% row.holidays) ] <- NA

df2["is.holiday"] <- !(df2$holiday %>% is.na())

## Engineering features to check if date is during a weekend, monthend or start of month

df2["weekend"] <- df2$date %>% is.weekend()
df2["monthend"] <- df2$date %>% format("%d") %in% c('27','28','29','30','31')
df2["monthstart"] <- df2$date %>% format("%d") %in% c('1','2','3', '4', '5')



df2$holiday <-ifelse(is.na(df2$holiday),"No",df2$holiday)
df2$monthend <- ifelse(df2$monthend==FALSE,"No","Yes")
df2$monthstart <- ifelse(df2$monthstart==FALSE,"No","Yes")




#Converting character vectors to factors
categorical_columns <- c("channelGrouping", "device.isMobile", "is.holiday", "monthend", "monthstart", "weekend")

df2 <- mutate_at(df2, categorical_columns, as.factor)
glimpse(df2)

levels(df2$monthstart)
# No dates in the start of the month, hence dropping the column

df2 <- subset(df2, select = -c(monthstart, holiday))

options(repr.plot.height=4)
NAcol <- which(colSums(is.na(df2)) > 0)
NAcount <- sort(colSums(sapply(df2[NAcol], is.na)), decreasing = TRUE)

NADF <- data.frame(variable=names(NAcount), missing=NAcount)
NADF$PctMissing <- round(((NADF$missing/nrow(df2))*100),1)
NADF %>%
  ggplot(aes(x=reorder(variable, PctMissing), y=PctMissing)) +
  geom_bar(stat='identity', fill='blue') + coord_flip(y=c(0,110)) +
  labs(x="", y="Percent missing") +
  geom_text(aes(label=paste0(NADF$PctMissing, "%"), hjust=-0.1))

# Imputing missing values in device.operatingSystem and geoNetwork.country with "unknown"
df2$device.operatingSystem <-ifelse(is.na(df2$device.operatingSystem),"Unknown",df2$device.operatingSystem)
df2$geoNetwork.country <-ifelse(is.na(df2$geoNetwork.country),"Unknown",df$geoNetwork.country)

train<- df2 %>% filter(df$Data == "Training")
test<- df2 %>% filter(df$Data == "Testing")

write.csv(df2, file="df2.csv", row.names=FALSE)

write.csv(train, file="train_final.csv", row.names=FALSE)
write.csv(test, file="test_final.csv", row.names=FALSE)



"
==========================================
                   OLS
==========================================
"
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#load("train.Rdata")
#load("test. Rdata. Rdata. Rdata")
#train[1:5,1:10]
str(train)

# convert categorical variables to factors
train$geoNetwork.country <- as.factor(train$geoNetwork.country)
train$device.operatingSystem <- as.factor(train$device.operatingSystem)
train$is.holiday <- as.factor(train$is.holiday)


# split train into estimation set and validation set
set.seed(123)
est_index <- sample(1:nrow(train), size =nrow(train)/2 )
train.est <- train[est_index,]
train.val <- train[-est_index,]

# check NAs in estimation set
nas.cols <- as.vector(rep(0, ncol(train.est)))
for(i in 1:ncol(train.est)){
  nas.cols[i] <- sum(is.na(train.est[i]))
}
nas.cols
# Naming the vector colums
names(nas.cols) <- names(train.est)[1:ncol(train.est)]

# Finding columns with NAs for train.est data
with.nas <- nas.cols[nas.cols!=0]
with.nas
# trafficSource.source     totals.pageviews       device.browser 
#          32                   52                    5 

# impute NAs for trafficSource.source
Mode(train.est$trafficSource.source)
# [1] google
# 499 Levels: (direct) ... yt-go-12345.googleplex.com

train.est$trafficSource.source[which(is.na(train.est$trafficSource.source))] <- "google"

# impute NAs for totals.pageviews
train.est$totals.pageviews[which(is.na(train.est$totals.pageviews))] <- median(train.est$totals.pageviews,na.rm=TRUE)

# impute NAs for device.browser
Mode(train.est$device.browser)
# [1] Chrome
# 128 Levels: ;__CT_JOB_ID__:0a075729-93a5-43d0-9638-4cbd41d5f5a5; ...

train.est$device.browser[which(is.na(train.est$device.browser))] <- "Chrome"


# Model 1
# Excluding the following variables:
# date,fullVisitorId,sessionId,visitId,visitStartTime, Data
# trafficSource.source (get memory error if included)
# geoNetwork.country (too many levels)
# device.browser (too many levels)
lm.1 <- lm(totals.transactionRevenue ~channelGrouping+visitNumber+totals.hits+totals.pageviews
            +device.operatingSystem+device.isMobile+device.deviceCategory+is.holiday+weekend+monthend, data=train.est)
summary(lm.1)

# Model 2
# Take out channelGrouping, device.operatingSystem
lm.2 <- lm(totals.transactionRevenue ~visitNumber+totals.hits+totals.pageviews
           +device.isMobile+device.deviceCategory+is.holiday+weekend+monthend, data=train.est)
summary(lm.2)

# Model 3
# Take out totals.hits, device.isMobileTRUE,device.deviceCategory 
lm.3 <- lm(totals.transactionRevenue ~visitNumber+totals.pageviews
           +is.holiday+weekend+monthend, data=train.est)
summary(lm.3)

"
===========================
cross validate on valid set
===========================
"
# check NAs in valid set
nas.cols <- as.vector(rep(0, ncol(train.val)))
for(i in 1:ncol(train.val)){
  nas.cols[i] <- sum(is.na(train.val[i]))
}
nas.cols
# Naming the vector colums
names(nas.cols) <- names(train.val)[1:ncol(train.val)]

# Finding columns with NAs for train.val data
with.nas <- nas.cols[nas.cols!=0]
with.nas
# trafficSource.source     totals.pageviews       device.browser 
# 37                   48                    3 

# impute NAs for trafficSource.source
Mode(train.val$trafficSource.source)
# [1] google
# 499 Levels: (direct) ... yt-go-12345.googleplex.com

train.val$trafficSource.source[which(is.na(train.val$trafficSource.source))] <- "google"

# impute NAs for totals.pageviews
train.val$totals.pageviews[which(is.na(train.val$totals.pageviews))] <- median(train.val$totals.pageviews,na.rm=TRUE)

# impute NAs for device.browser
Mode(train.val$device.browser)
# [1] Chrome
# 128 Levels: ;__CT_JOB_ID__:0a075729-93a5-43d0-9638-4cbd41d5f5a5; ...

train.val$device.browser[which(is.na(train.val$device.browser))] <- "Chrome"

# predict using lm.1
pred.1 <- predict(lm.1, newdata=train.val)
# factor device.operatingSystem has new levels 12, 13, 18

# find indices in train.val with these values
index.12 <- which(train.val$device.operatingSystem == 12)
index.12
# [1]  49618 303150

index.13 <- which(train.val$device.operatingSystem == 13)
index.13
# [1] 207100

index.18 <- which(train.val$device.operatingSystem == 18)
index.18
# [1] 314667


# replace those with mode in train.est for device.operatingSystem
OS.mode <- Mode(train.est$device.operatingSystem)
OS.mode
# [1] 21

train.val$device.operatingSystem[cbind(index.12,index.13,index.18)] <- 21

# predict using lm.1 again
pred.1 <- predict(lm.1, newdata=train.val)

MSE <- mean((train.val$totals.transactionRevenue-pred.1)^2)
MSE
# [1] 3.114098e+15

# Predict using lm.2
pred.2 <- predict(lm.2, newdata=train.val)
MSE <- mean((train.val$totals.transactionRevenue-pred.2)^2)
MSE
# [1] 3.115807e+15

# Predict using lm.3
pred.3 <- predict(lm.3, newdata=train.val)
MSE <- mean((train.val$totals.transactionRevenue-pred.3)^2)
MSE
# [1] 3.116231e+15

# MODEL 1,2 have lower MSE, build models on entire train 

lm.1 <- lm(totals.transactionRevenue ~channelGrouping+visitNumber+totals.hits+totals.pageviews
           +device.operatingSystem+device.isMobile+device.deviceCategory+is.holiday+weekend+monthend, data=train)
summary(lm.1)

lm.2 <- lm(totals.transactionRevenue ~visitNumber+totals.hits+totals.pageviews
           +device.isMobile+device.deviceCategory+is.holiday+weekend+monthend, data=train)
summary(lm.2)



"
========================
PREDICT ON OLD TEST DATA
========================
"
# convert categorical variables to factors
test$geoNetwork.country <- as.factor(test$geoNetwork.country)
test$device.operatingSystem <- as.factor(test$device.operatingSystem)
test$is.holiday <- as.factor(test$is.holiday)

# predict using lm.1 
test.pred.1 <- predict(lm.1, newdata=test)
# Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels) : 
#   factor device.operatingSystem has new levels 15, 16, 19, 20

# replace those with mode in test for device.operatingSystem
OS.mode.test <- Mode(test$device.operatingSystem)
OS.mode.test
# [1] "21"

# find indices in test with these values
index.15 <- which(test$device.operatingSystem == 15)
index.15

index.16 <- which(test$device.operatingSystem == 16)
index.16

index.19 <- which(test$device.operatingSystem == 19)
index.19

index.20 <- which(test$device.operatingSystem == 20)
index.20

test$device.operatingSystem[cbind(index.15,index.16,index.19,index.20)] <- 21

# predict using lm.1 again after cleaning of test 
test.pred.1 <- predict(lm.1, newdata=test)

# bind fullVisitorId with predicted value
prediction.1 <- data.frame(cbind(test$fullVisitorId,test.pred.1))
names(prediction.1) <- c("fullVisitorId","predRevenue")
prediction.1$predRevenue <- as.numeric(prediction.1$predRevenue)

# group by fullVistorId
prediction.1.new <-group_by(prediction.1,fullVisitorId)
prediction.1.summary <-summarise(prediction.1.new, total = sum(predRevenue))
prediction.1.summary$PredictedLogRevenue <-log(prediction.1.summary$total+1)
prediction.1.summary <- prediction.1.summary[,c(1,3)]
head(prediction.1.summary)
# fullVisitorId       PredictedLogRevenue
# <fct>                             <dbl>
# 1 0000000259678714014               11.3 
# 2 0000049363351866189               11.1 
# 3 0000053049821714864                8.24
# 4 0000059488412965267                9.27
# 5 0000085840370633780                6.18
# 6 0000091131414287111                8.18
nrow(prediction.1.summary)
# replace NAs in the summary with 0
prediction.1.summary[which(is.na(prediction.1.summary$PredictedLogRevenue)),2] <- 0

# write to txt file so fullVisitorId has leading zeros
# for submission, import txt file to Excel and then save as csv 
write.table(prediction.1.summary, file = "C4-4_OLS_1.txt", sep = "\t",
            row.names = F, col.names = c("fullVisitorId", "PredictedLogRevenue"))


# predict using lm.2
test.pred.2 <- predict(lm.2, newdata=test)
prediction.2 <- data.frame(cbind(test$fullVisitorId,test.pred.2))
names(prediction.2) <- c("fullVisitorId","predRevenue")
prediction.2$predRevenue <- as.numeric(prediction.2$predRevenue)

# group by fullVistorId
prediction.2.new <-group_by(prediction.2,fullVisitorId)
prediction.2.summary <-summarise(prediction.2.new, total = sum(predRevenue))
prediction.2.summary$PredictedLogRevenue <-log(prediction.2.summary$total+1)
prediction.2.summary <- prediction.2.summary[,c(1,3)]
head(prediction.2.summary)
# fullVisitorId       PredictedLogRevenue
# <fct>                             <dbl>
# 1 0000000259678714014                7.84
# 2 0000049363351866189                7.05
# 3 0000053049821714864                6.38
# 4 0000059488412965267                7.04
# 5 0000085840370633780                6.39
# 6 0000091131414287111                6.10
nrow(prediction.2.summary)

# replace NAs in the summary with 0
prediction.2.summary[which(is.na(prediction.2.summary$PredictedLogRevenue)),2] <- 0

# write to txt file so fullVisitorId has leading zeros
# for submission, import txt file to Excel and then save as csv 
write.table(prediction.2.summary, file = "C4-4_OLS_2.txt", sep = "\t",
            row.names = F, col.names = c("fullVisitorId", "PredictedLogRevenue"))



"

The following code is trying to use the model built on old train data to predict on new test data

"
"
=====================
CLEAN NEW TEST DATA
=====================
"

str(test_new)
#JSON columns are : device, geoNetwork, totals, trafficSource

# parse JSON


JSONcolumn_data <- test_new  %>% 
  dplyr::select(trafficSource, totals, geoNetwork, device)

JSON_cols<-apply(JSONcolumn_data,2, FUN = ParseJSONColumn)
save(JSON_cols, file = "test_JSON_parsed.Rdata")

head(JSON_cols)
test_new <- cbind(test_new, JSON_cols) 

# dropping the old json columns
test_new<-test_new %>% dplyr::select(-device, -geoNetwork, -totals, -trafficSource)
head(test_new)


#Several of the columns seem to have "not available in demo dataset","(not provided) "
#setting the same to NA

# values to convert to NA
na_vals <- c("unknown.unknown", "(not set)", "not available in demo dataset", 
             "(not provided)", "(none)", "<NA>")

for(col in 1:ncol(test_new)){
  test_new[which(test_new[,col] %in% na_vals), col]= NA
}
glimpse(test_new)

#write.table(df, "cleaned_total_data.csv", row.names=F, sep=",")



#All of the columns that were converted from json are of class character. 
#For some, we will need to change this.

# character columns to convert to numeric
num_cols <- c('totals.hits', 'totals.pageviews', 'totals.bounces', 'totals.newVisits',
              'totals.transactionRevenue')
test_new[, num_cols] = lapply(test_new[, num_cols], function(x){as.numeric(x)})

glimpse(test_new)

#Coverting date from int to date format
test_new$date <- as.Date(as.character(test_new$date), format='%Y%m%d')

# convert visitStartTime to POSIXct
test_new$visitStartTime <- as_datetime(test_new$visitStartTime)
glimpse(test_new)

#imputing transaction revenue to 0 before removing na columns
test_new$totals.transactionRevenue[is.na(test_new$totals.transactionRevenue)] <- 0

# Imputing missing countries where city is captured
test_new$geoNetwork.city[(test_new$geoNetwork.country %>% is.na()) & (!test_new$geoNetwork.city %>% is.na())]

# [1] "Mexico City" "Bengaluru"   "Bengaluru"   "Santa Clara" "Austin"  


test_new$geoNetwork.country[test_new$geoNetwork.city %in% c("Santa Clara", "Austin")] <- "United States"
test_new$geoNetwork.country[test_new$geoNetwork.city %in% c("Mexico City")] <- "Mexico"
test_new$geoNetwork.country[test_new$geoNetwork.city %in% c("Bengaluru")] <- "India"


col_name_train <- colnames(train)



# Feature engineering using Date for holidays

us.bank.holidays <- read_csv("US Bank holidays.csv")
us.bank.holidays <- us.bank.holidays[, ! names(us.bank.holidays) %in% c("index"), drop = F]

holidays <- us.bank.holidays$date %>% as.list()


for(i in 1:11){
  buffer.dates <- holidays %>% lapply(function(d){
    
    data.frame(date=as.Date(d)-i, holiday = us.bank.holidays$holiday[us.bank.holidays$date==as.Date(d)])
  })
  
  buffer.dates <- do.call(rbind,buffer.dates)
  us.bank.holidays <- us.bank.holidays %>% rbind(buffer.dates)
}

us.bank.holidays = us.bank.holidays[!duplicated(us.bank.holidays$date),]

test_new_2 <- left_join(test_new,unique(us.bank.holidays), by=c("date"))

test_new_2 <- test_new_2[,!names(test_new_2) %in% c("holiday.x"), drop=F]

names(test_new_2)[names(test_new_2) == 'holiday.y'] <- 'holiday'

# removing some holidays for non-US countries
us.holidays <- c("New Year Day", "Independence Day", "Labor Day", "Thanksgiving Day", "Christmas Day")
row.holidays <- c("New Year Day", "Christmas Day")
test_new_2$holiday[(test_new_2$geoNetwork.country =="United States") & ! (test_new_2$holiday %in% us.holidays) ] <- NA
test_new_2$holiday[(test_new_2$geoNetwork.country!="United States") & ! (test_new_2$holiday %in% row.holidays) ] <- NA

test_new_2["is.holiday"] <- !(test_new_2$holiday %>% is.na())

## Engineering features to check if date is during a weekend, monthend or start of month

test_new_2["weekend"] <- test_new_2$date %>% is.weekend()
test_new_2["monthend"] <- test_new_2$date %>% format("%d") %in% c('27','28','29','30','31')
test_new_2["monthstart"] <- test_new_2$date %>% format("%d") %in% c('1','2','3', '4', '5')



test_new_2$holiday <-ifelse(is.na(test_new_2$holiday),"No",test_new_2$holiday)
test_new_2$monthend <- ifelse(test_new_2$monthend==FALSE,"No","Yes")
test_new_2$monthstart <- ifelse(test_new_2$monthstart==FALSE,"No","Yes")

# keep the same columns as train
col_name_train <- col_name_train[-c(4,8)]
test_new_2 <- test_new_2[,col_name_train]
glimpse(test_new_2)

# convert categorical variables to factors
test_new_2$geoNetwork.country <- as.factor(test_new_2$geoNetwork.country)
test_new_2$device.operatingSystem <- as.factor(test_new_2$device.operatingSystem)
test_new_2$is.holiday <- as.factor(test_new_2$is.holiday)

categorical_col <- c("channelGrouping","trafficSource.source","device.browser",
                     "device.isMobile","device.deviceCategory","is.holiday","weekend","monthend")

test_new_2 <- mutate_at(test_new_2, categorical_col, as.factor)
# write.csv(test_new_2, file="test_new_clean.csv", row.names=FALSE)
# save(test_new_2,file="test_new_2.Rdata")

"
========================
PREDICT ON NEW TEST
========================
"

# predict using lm.2
test.pred.2 <- predict(lm.2, newdata=test_new_2)
prediction <- data.frame(cbind(test_new_2$fullVisitorId,test.pred.2))
names(prediction) <- c("fullVisitorId","predRevenue")

prediction$predRevenue <- as.numeric(prediction$predRevenue)

prediction.new <-group_by(prediction,fullVisitorId)
prediction.summary <-summarise(prediction.new, total = sum(predRevenue))
prediction.summary$PredictedLogRevenue <-log(prediction.summary$total+1)
prediction.summary <- prediction.summary[,c(1,3)]
head(prediction.summary)
nrow(prediction.summary)
# replace NAs in summary with 0
prediction.summary[which(is.na(prediction.summary$PredictedLogRevenue)),2] <- 0


# write to txt file so fullVisitorId is in right format 
# for submission, import txt file to Excel and then save as csv 
write.table(prediction.summary, file = "C4-4_OLS.txt", sep = "\t",
            row.names = F, col.names = c("fullVisitorId", "PredictedLogRevenue"))
test.pred.3 <- predict(lm.3, newdata=test_new_2)




