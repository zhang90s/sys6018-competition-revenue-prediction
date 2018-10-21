#library reference for reading and writing files
library(readr)
#library reference for data handling
library(dplyr)
library(stringr)

library(jsonlite)

library(plyr)


# train <- read.csv("all/train.csv", stringsAsFactors = FALSE)
# test <- read.csv("all/test.csv", stringsAsFactors = FALSE)
# 
# train$channelGrouping <- factor(train$channelGrouping)
# train$socialEngagementType <- factor(train$socialEngagementType)
# 
# device_subcolumns <- lapply(train$device, fromJSON)
# geonetwork_subcolumns <- lapply(train$geoNetwork, fromJSON)
# totals_subcolumns <- lapply(train$totals, fromJSON)
# trafficsource_subcolumns <- lapply(train$trafficSource, fromJSON)
# 
# func_subcolumn_handler <- function(tag, subcolumn){
#   cols <- names(subcolumn)
#   lapply(cols, function(c) train[paste(tag,".",c)] <- subcolumn[c])
# }
# 
# lapply(device_subcolumns, function(x) func_subcolumn_handler("device", x))
# lapply(geonetwork_subcolumns, function(x) func_subcolumn_handler("geonetwork", x))
# lapply(totals_subcolumns, function(x) func_subcolumn_handler("totals", x))
# lapply(trafficsource_subcolumns, function(x) func_subcolumn_handler("trafficsource", x))

# device_subcolumns_df <- as.data.frame(matrix(unlist(device_subcolumns), nrow=length(device_subcolumns)))
# geonetwork_subcolumns_df <- as.data.frame(matrix(unlist(geonetwork_subcolumns), nrow=length(geonetwork_subcolumns)))
# totals_subcolumns_df <- as.data.frame(matrix(unlist(totals_subcolumns), nrow=length(totals_subcolumns)))
# trafficsource_subcolumns_df <- as.data.frame(matrix(unlist(trafficsource_subcolumns), nrow=length(trafficsource_subcolumns)))


train <- read.csv("train_new.csv", stringsAsFactors = FALSE)
test <- read.csv("test_new.csv", stringsAsFactors = FALSE)

train <- train[, !(names(train) %in% c("X"))]
test <- test[, !(names(test) %in% c("X"))]

#train["browserSize"]

train[train == "not available in demo dataset"] <- NA
train[train == "(not set)"] <- NA
train[train == "(not provided)"] <- NA
train[train$browser %>% is.na(),] <- "Chrome"

subset(train, is.na(train$browser))

test[test == "not available in demo dataset"] <- NA
test[test == "(not set)"] <- NA
test[test == "(not provided)"] <- NA
test[test$browser %>% is.na(),] <- "Chrome"

categorical_columns <- c("channelGrouping", "socialEngagementType", "browser", "deviceCategory", "operatingSystem", "continent", "country", "region", "subContinent", "medium", "source")

lapply(categorical_columns, function(c) train[c] <- factor(train[c]))
lapply(categorical_columns, function(c) test[c] <- factor(test[c]))

subset(train, is.na(train$transactionRevenue) == FALSE & train$bounces == 1)

train[train$bounces %>% is.na(),]$bounces <- 0
train[train$transactionRevenue %>% is.na() | train$transactionRevenue %>% is.nan() | train$transactionRevenue==Inf,]$transactionRevenue <- 0
train[train$transactionRevenue %>% is.na()]

train$transactionRevenue <- as.integer(train$transactionRevenue)
train[train$transactionRevenue %>% is.na() | train$transactionRevenue %>% is.nan() | train$transactionRevenue==Inf,]$transactionRevenue <- 0

test[test$bounces %>% is.na(),]$bounces <- 0

model <- lm(transactionRevenue ~ bounces+channelGrouping+socialEngagementType+browser+deviceCategory+isMobile+hits+newVisits+pageviews+visits+medium+source, data=train)
summary(model)

model2 <- lm(transactionRevenue ~ bounces+channelGrouping+socialEngagementType+browser+deviceCategory+isMobile+newVisits+pageviews+visits+medium+source, data=train)
summary(model2)

model <- lm(transactionRevenue ~ bounces+browser+deviceCategory+visits+medium+source, data=train)
summary(model3)

train.c <- colnames(train)
test.c <- colnames(test)

train.c[which(!train.c %in% test.c)]

y.hat <- predict(model2, data=test)

length(y.hat)
nrow(test)

test["transactionRevenue"] <- y.hat

test[test$channelGrouping %>% is.na(),]


