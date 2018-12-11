#### Corporacion Favorita Sales Forecasting

### Dain Thengunnal
### 09 Dec 2018

#Load packages

library(data.table)		  # for data table manipulation
library(dplyr)			  # for dataframe manipulation
library(padr)			  # for padding missing values
library(lubridate)        # for working with dates
library(xgboost)		  # for XGBoost model building
library(Matrix)			  # for matrix object
library(RcppRoll)		  # fro computing rolling/windowed calculations
library(zoo)			  # for time series functions
library(knitr)			  # for craeating R markdown
library(xtable)			  # for data table
library(ggplot2)		  # visualisation
library(Ckmeans.1d.dp)	  # for importance plot
library(caret)			  # for traing models
library(randomForest)	  # for RF modelling
																				
																
setwd("D:/Dain/Learn Data Science/Kaggle/Favorita Sales Forecasting/Data")

set.seed(123)																		# for reproducible results

#Read Data and set start and end dates

train        <- fread("train.csv")
items		 <- fread("items.csv")
stores       <- fread("stores.csv")
transactions <- fread("transactions.csv")
holidays     <- fread("holidays_events.csv")
oil          <- fread("oil.csv")
test		 <- fread("test.csv")

startDate<-'2017-07-01'																#to subset train data to latest 1 month due to memory limitations
endDate<-'2017-08-15'
validationStartDate<-'2017-08-01'

setwd("D:/Dain/Learn Data Science/Kaggle/Favorita Sales Forecasting")



### Overview of train data


str(train)
head(train)
summary(train$unit_sales)


### Overview of items data


str(items)
head(items)


### Overview of stores data


str(stores)
head(stores)



### Overview of transactions data


str(transactions)
head(transactions)


### Overview of holidays data


str(holidays)
head(holidays)

### Overview of oil data


str(oil)
head(oil)


## Data Wrangling

### train

trainoriginal <- train                    # taking backup before subsetting for exploratory analysis with oil
oiloriginal<-oil																					

                                          # train data contains returns. 
													
train <- train[train$date>=startDate & train$date<=endDate & train$unit_sales > 0,]	# subset train data to dates within the start and end date and only sales

																					# train data contains missing dates. Pad missing dates grouped by store_nbr and item_nbr.
																					# For the added dates zero will be imputed for unit_sales 

train$date <- as.Date(train$date)													# pad function requires train$date in date format


train <- pad(train, start_val = as.Date(startDate), end_val = as.Date(endDate), interval = 'day', group = c('store_nbr', 'item_nbr'), break_above = 100000000000) %>% fill_by_value(unit_sales)

																					# impute NA values for onpromotion,introduced by the previous step, with the previous or next value of onpromotion

train$onpromotion<-ifelse( is.na(train$onpromotion), 
                           ifelse( is.na(na.locf(train$onpromotion)),			 	# If the previous value is also NA then take the next value of onpromotion
								   na.locf(train$onpromotion, option = "nocb"),
								   na.locf(train$onpromotion)
								  ),
						   train$onpromotion
						  )

### holidays

																					# Split holidays dataframe into three seperate dataframes : nationalHolidays, regionalHolidays and localHolidays.


# National Holidays

nationalHolidays<-subset(holidays,locale=="National" & date>=startDate & date<=endDate,select=c("date","description"))

colnames(nationalHolidays)[2]<-c("holiday_desc")									# rename column
nationalHolidays$holiday<-TRUE														# new column holiday indicates that it is a holiday
nationalHolidays$date <- as.Date(nationalHolidays$date)

# Regional Holidays

regionalHolidays<-subset(holidays,locale=="Regional" & date>=startDate & date<=endDate,select=c("date","locale_name","description"))

colnames(regionalHolidays)[3]<-c("holiday_desc")									# rename column
regionalHolidays$date <- as.Date(regionalHolidays$date)       

# Local Holidays

localHolidays<-subset(holidays,locale=="Local" & date>=startDate & date<=endDate,select=c("date","locale_name","description"))

colnames(localHolidays)[3]<-c("holiday_desc")										# rename column
localHolidays$date <- as.Date(localHolidays$date)			  


### items

colnames(items)[2]<-c("item_family")												# rename column
colnames(items)[3]<-c("item_class")													# rename column
items$perishable<-ifelse(items$perishable==0,FALSE,TRUE)							# re-assign values for perishable column.
																					# TRUE is perishable, FALSE is non perishable,

### stores


colnames(stores)[4]<-c("store_type")												# rename column
colnames(stores)[5]<-c("store_cluster")												# rename column

### oil

																					# oil data contains missing dates and NA values for oil prices on Saturday and Sunday.
																					# pad missing dates and impute the oil_price for newly added dates with valid values.
																					# Friday's oil price will be used on Saturday and Sunday.


colnames(oil)[2]<-c("oil_price")													# rename column
oil$date <- as.Date(oil$date)

oil <- pad(oil, start_val = min(oil$date), end_val = max(oil$date), interval = 'day', break_above = 100000000000) %>% fill_by_value(oil_price)

																					# oil prices are zero on Saturday and Sunday.
																					# Friday's value will be used for Saturday and Sunday
																					
oil <- oil[, ':='(oil_price = ifelse(wday(date) == 7, lag(oil_price,1L), ifelse(wday(date) == 1, lag(oil_price,2L), oil_price)))][, oil_price := na.approx(oil_price, na.rm=FALSE)]

oil[is.na(oil$oil_price),]															# Check to see if all NA values got replaced with a  valid value


### transactions
																					# add year, month and week columns to transactions.
transactions$year<-year(as.Date(transactions$date))
transactions$month<-month(as.Date(transactions$date))
transactions$week<-week(as.Date(transactions$date))


## Exploratory Data Analysis


### train
																					# This is the most basic sales data, with a date/store/item, how many were sold, and whether the item was on promotion.
trainplotdf<-train
trainplotdf<-trainplotdf %>% group_by(store_nbr) %>% summarise(unit_sales = sum(unit_sales)) %>% arrange(store_nbr)
storenumbers<-unique(trainplotdf$store_nbr)
ggplot(trainplotdf,aes(store_nbr,unit_sales)) +
	  geom_bar(stat="identity",fill="lightblue") +
	  labs(x="Store", y="Total sales") +
	  ggtitle("Total sales per store") +
	  scale_x_continuous(breaks=storenumbers)+
	  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))

### Items
																					# A listing of each item sold in the stores with it's family, class and whether or not the item is perishable.
																					# Majority of items sold are in Grocery and Beverages family.

itemsplotdf<-items %>% group_by(item_family) %>% summarise(n()) %>% arrange(item_family)

colnames(itemsplotdf)[2]<-c("item_count")

ggplot(itemsplotdf, aes(x=item_family, y=item_count)) +
geom_bar(stat="identity", fill="lightblue") +
labs(x="Item Family", y="Number of Items") +
ggtitle("Number of items per item family") +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))

### Stores
																					# A list of stores by location, type and cluster. No detail is provided on type and cluster.
																					# State of Pichincha, where capital city Quito is located, has the most number of stores.

storesPerState<-tapply(stores$store_nbr, stores$state, length)

storesplotdf<-data.frame(state=names(storesPerState),store_count=storesPerState)

ggplot(storesplotdf, aes(x=state, y=store_count)) +
geom_bar(stat="identity", fill="lightblue") +
labs(x="State", y="Number of Stores") +
ggtitle("Number of stores by state") +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))


### Transactions
																					#It contains the count of transactions by store, by day from 2013-01-01 to 2017-08-15.
																					#Total transaction volume remains fairly constant throughout the year.
																					#There is a spike in transactions during last two weeks of the year, as expected.

																					#Total transaction by week and year from 2013-01-01 to 2017-08-15


transactionsplotdf<-transactions %>% group_by(year,week) %>% summarise(transactions = sum(transactions)) %>% arrange(year,week)

colnames(transactionsplotdf)[3]<-c("transaction_count")

transactionsplotdf<-transactionsplotdf[transactionsplotdf$week<53,]

ggplot(transactionsplotdf,aes(week,transaction_count,fill=year)) +
	  geom_bar(stat="identity") +
	  labs(x="Week", y="Transaction count") +
	  ggtitle("Transaction count by week and year")+
	  scale_x_continuous(breaks=seq(1:52))+
	  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))

### Holidays
																				#A list of all holidays, including local, regional/state and national holidays.
																				#Event and Work Day are not holidays, so they are excluded.
																				#There are more holidays in period 11 and 12, as expected. 
	 
																				# Work Day and Event are excluded																				
holidaysplotdf<- holidays[holidays$type %in% c("Holiday","Transfer","Additional","Bridge") & year(as.Date(holidays$date))== 2016,]
holidaysplotdf$period<-month(as.Date(holidaysplotdf$date))
holidaysplotdf<-holidaysplotdf %>% group_by(period,type) %>% summarise(n()) %>% arrange(period,type)
colnames(holidaysplotdf)[2]<-c("holiday_type")
colnames(holidaysplotdf)[3]<-c("holiday_count")

ggplot(holidaysplotdf,aes(period,holiday_count,fill=holiday_type)) +
	  geom_bar(stat="identity") +
	  labs(x="Period", y="No of holidays") +
	  ggtitle("No of holidays by period and holiday type")+
      scale_x_continuous(breaks=seq(1:12))


### Oil
																					#Ecuador's economy is dependent on oil prices.

																					#At the start of 2015, oil was traiding at half price compared to previous years, then reached another low in the begining of 2016.
																					#It has then been recovered since mid-2016.

ggplot(oil[oil$oil_price>0,], aes(x=date, y=oil_price)) +
	  geom_line(col="red", size=1) +
	  labs(x="Date", y="Oil price") +
	  ggtitle("World Oil Prices, Jan 2013 - August 2017")

																					# Unit sales and oil price scatter plot

trainoriginal<- trainoriginal[,c("date","unit_sales")]

unitSalesOilPriceplotdf<-merge(x = trainoriginal, y = oiloriginal, by = "date", all.x = TRUE)

unitSalesOilPriceplotdf<-na.omit(unitSalesOilPriceplotdf)
colnames(unitSalesOilPriceplotdf)[3]<-c("oil_price")

unitSalesOilPriceplotdf<- unitSalesOilPriceplotdf %>% group_by(date) %>% summarise(unit_sales = sum(unit_sales),oil_price = mean(oil_price)) %>% arrange(date)

ggplot(unitSalesOilPriceplotdf[unitSalesOilPriceplotdf$oil_price>0,], aes(x=oil_price, y=unit_sales)) +
  geom_point(alpha=0.5) +
  geom_smooth(method = 'lm')+
  labs(x="Oil price", y="Unit sales") +
  ggtitle("Unit sales and Oil Prices")

cor(unitSalesOilPriceplotdf$unit_sales,unitSalesOilPriceplotdf$oil_price)       	# correlation between oil price and unit sales 


## Feature Engineering

### Create new features for train data


train$day <- wday(train$date)														# day=1 (is Sunday from 2017-07-31)
train$day_num <- day(train$date)    												# day_num=31 from  2017-07-31
train$month <- month(train$date)													# month=7 from  2017-07-31

																					# Derive and add new statistical metrices(lag and rolling mean for last 7 days) on unit_price
train <- train[order(train$date),]													# order date before applying lag
																					# mean of last 7 days for a store and item (with dates ordered )
																					
train <- train %>% group_by(store_nbr,item_nbr) %>% mutate(lag1 = lag(unit_sales,1), rollingMean7 = lag(roll_meanr(unit_sales, 7), 1))		


### merge store data into train data
																
train<-merge(x = train, y = stores[,c("store_nbr","store_type")], by = "store_nbr", all.x = TRUE)

### merge item data into train data


train<-merge(x = train, y = items[,c("item_nbr","item_class","perishable")], by = "item_nbr", all.x = TRUE)


### merge holiday data into train data
																					# merge nationalHolidays based on date
																					
train<-merge(x = train, y = nationalHolidays[ , c("date","holiday")], by = "date", all.x = TRUE)

																					# merge regionalHolidays based on date and state
																					
tRegnHolidayindex<-which(train$date %in% c(regionalHolidays$date) & train$state %in% c(regionalHolidays$locale_name))
train$holiday[tRegnHolidayindex]<- TRUE
																					# merge localHolidays based on date and city
																					
tLocalHolidayindex<-which(train$date %in% c(localHolidays$date) & train$city %in% c(localHolidays$locale_name))
train$holiday[tLocalHolidayindex]<- TRUE

																					# for work/remaining days assign holiday as FALSE
tNoHolidayindex<-which(is.na(train$holiday))
train$holiday[tNoHolidayindex]<- FALSE

### merge oil data into train data

train<-merge(x = train, y = oil, by = "date", all.x = TRUE)



### Remove new records with NA from train


nrow(train)																			# row count in train before removing NAs
train <- train[!is.na(train$rollingMean7),]											# other features are covered by this check
nrow(train)																			# row count in train after removing NAs
																					# Check to see if any columns, excluding id, in train still contains NAs
																				
train[,-4] %>% select_if(function(x) any(is.na(x))) %>% summarise_all(funs(sum(is.na(.))))



### Feature Selection

																					# Started with all features in train dataset.
																					# Based on the importance plot the following features selected for modelling.

### Features selected 

	# continuous features : lag1, rollingMean7 and oil_price
	# categorical features: onpromotion, day, item_class and perishable
		
## Model Buliding and Prediction

	# I have tried two models XGBoost and Random Forest using the above selected features.

### Partition datasets for training and validation

validationXGB <- train[train$date>=validationStartDate,]							# validation data for XGBoost

validationRF <- train[train$date>=validationStartDate,]								# validation data for RF

train <- train[train$date<validationStartDate,]					

nrow(train)



### Model building and prediction using XGBoost

###	Prepare data in matrix format
	
trainLabel <- train$unit_sales														# create labels
previous_na_action<- options('na.action')											# returns object unchanged if there are NA values
options(na.action='na.pass')
																					# create matrix to be used by xgb.DMatrix
																					
trainMatrix <- sparse.model.matrix(unit_sales ~ day + lag1 + rollingMean7 + onpromotion + oil_price + item_class + perishable
, data = train
, contrasts.arg = c('onpromotion','day','item_class', 'perishable')
, sparse = FALSE, sci = FALSE)

options(na.action = previous_na_action$na.action)

					
### Model Training

																					# Xgboost requires the data to be in xgb.DMatrix format which accepts data as matrices
																					
trainDMatrix <- xgb.DMatrix(data = trainMatrix, label = trainLabel)

params <- list(booster = "gbtree" , objective = "reg:linear", eta=0.4, gamma=0)		# parameters for the building the model
																					# Cross-validation decreases the likelihood of over-fitting.
																					# It also determines the number of iterations to run when building the model.
																					
xgb.tab <- xgb.cv(data = trainDMatrix, param = params, maximize = FALSE, evaluation = "rmse", nrounds = 100, nthreads = 10, nfold = 2, early_stopping_round = 10)

num_iterations = xgb.tab$best_iteration												# number of rounds to train will be based on this
																					# train the model

																					
trainModelXGB <- xgb.train(data = trainDMatrix, param = params, maximize = FALSE, evaluation = 'rmse', nrounds = num_iterations)



### Feature Importance plot

																					# this helps to identify important features.

trainImportance <- xgb.importance(feature_names = colnames(trainMatrix), model = trainModelXGB)

xgb.ggplot.importance(importance_matrix = trainImportance)


### Prediction

																					# Validation Data is used for prediction as the actual unit sales is not available in the test file to compare the predicted values.

validationXGBorig<-validationXGB													# backup for comparing the actual unit sales with predicted unit sales

dates <- seq(as.Date("2017-08-01"), as.Date("2017-08-15"), by = "day")				# days to predict unit_sales for

i<-1

for (i in 1:length(dates)){
																					# subset validation data to predict only 1 day at a time

validationXGBSub <- validationXGB[validationXGB$date == dates[i],]
																		

previous_na_action<- options('na.action')
options(na.action='na.pass')
																		
validationMatrix <- sparse.model.matrix(unit_sales ~ day + lag1 + rollingMean7 + onpromotion + oil_price + item_class + perishable
, data = validationXGBSub
, contrasts.arg = c('onpromotion','day','item_class', 'perishable')
, sparse = FALSE, sci = FALSE)

options(na.action = previous_na_action$na.action)

																					# predict values for a given day
pred <- predict(trainModelXGB, validationMatrix)

																					# set negative predictions to zero
pred[pred < 0] <- 0
																					# update unit_sales column with predicted values
																					
validationXGB$unit_sales[validationXGB$date == validationXGBSub$date & validationXGB$item_nbr == validationXGBSub$item_nbr & validationXGB$store_nbr == validationXGBSub$store_nbr] <- pred

}

																					# add actual unit_sales to validationXGB as unit_sales_actual
																					
validationXGB$unit_sales_actual <- validationXGBorig$unit_sales[validationXGB$date == validationXGBorig$date & validationXGB$item_nbr == validationXGBorig$item_nbr & validationXGB$store_nbr == validationXGBorig$store_nbr]

colnames(validationXGB)[5]<-c("unit_sales_predicted")								# rename the unit_sales as unit_sales_predicted

																					# write prediction data to file
																					
write.csv(validationXGB[,c("date","store_nbr","item_nbr","onpromotion","item_class","perishable","oil_price","unit_sales_actual","unit_sales_predicted")], 'XGBprediction.csv',row.names=FALSE)



### Model Evaluation

	# NWRMSLE

																					# log1p requires input values should be >0
																					
validationXGB<-validationXGB[validationXGB$unit_sales_predicted>0 & validationXGB$unit_sales_actual>0, ]          

																					# calculate NWRMSLE
																					# 1.25 is the weight for perishable and 1 for non-perishable


sqrt(sum(ifelse(validationXGB$perishable==TRUE,1.25,1)*( log1p(validationXGB$unit_sales_predicted) - log1p(validationXGB$unit_sales_actual) )^2)/sum(ifelse(validationXGB$perishable==TRUE,1.25,1)))
						


	# RMSE
																				# calculate RMSE
																					
sqrt(sum(( validationXGB$unit_sales_predicted - validationXGB$unit_sales_actual )^2)/nrow(validationXGB))


### Plot of Actual vs Prediction

actvspredXGB1<-subset(validationXGB,select=c("date","item_nbr","store_nbr","unit_sales_predicted"))
colnames(actvspredXGB1)[4]<-c("unit_sales")
actvspredXGB1$unit_sales_type<-"predicted"			

actvspredXGB2<-subset(validationXGBorig,unit_sales>0,select=c("date","item_nbr","store_nbr","unit_sales"))
actvspredXGB2$unit_sales_type<-"actual"
				
actvspredXGB<-rbind(actvspredXGB1,actvspredXGB2)

actvspredXGBplotdf<-actvspredXGB %>% group_by(date,unit_sales_type) %>% summarise(unit_sales = sum(unit_sales)) %>% arrange(date,unit_sales_type)


ggplot(actvspredXGBplotdf, aes(x=date, y=unit_sales,col=unit_sales_type)) +
	  geom_line(size=1) +
	  labs(x="Date", y="Unit Sales") +
	  ggtitle("Actual vs Predicted Total unit sales using XGBoost")+
	  scale_x_date(breaks=as.Date(seq(as.Date(validationStartDate), as.Date(endDate), by = "day")))+
	  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
	
		
## Model building and prediction using Random Forest
							
### Model Training

																					# train three Random Forest models and combine them instead of a single lare data set
																					# it speeds up model building
rftrain<-sample_n(train, 10000)
trainModelRF1 <- randomForest(unit_sales ~ day + lag1 + rollingMean7 + onpromotion + oil_price + item_class + perishable, 			
							 data=rftrain,
							 ntree = 50,
							 mtry=7,
							 importance=TRUE
							)

rftrain<-sample_n(train, 10000)
trainModelRF2 <- randomForest(unit_sales ~ day + lag1 + rollingMean7 + onpromotion + oil_price + item_class + perishable, 			
							 data=rftrain,
							 ntree = 50,
							 mtry=7,
							 importance=TRUE
							)
							
rftrain<-sample_n(train, 10000)
trainModelRF3 <- randomForest(unit_sales ~ day + lag1 + rollingMean7 + onpromotion + oil_price + item_class + perishable, 			
							 data=rftrain,
							 ntree = 50,
							 mtry=7,
							 importance=TRUE
							)							
																	
trainModelRF<-combine(trainModelRF1,trainModelRF2,trainModelRF3)
												
							

### Feature Importance plot

importance(trainModelRF)
varImpPlot(trainModelRF)

						
### Prediction

																					# Validation Data is used for prediction as the actual unit sales is not available in the test file to compare the predicted values.


validationRForig<-validationRF														# backup for comparing the actual unit sales with predicted unit sales

dates <- seq(as.Date("2017-08-01"), as.Date("2017-08-15"), by = "day")				# days to predict unit_sales for

i=1

for (i in 1:length(dates)){
																					# subset validation data to predict only 1 day at a time
validationRFSub <- validationRF[validationRF$date == dates[i],]
																					# predict values for a given day
pred <- predict(trainModelRF,validationRFSub,type = "response")
																					# set negative predictions to zero
pred[pred < 0] <- 0
																					# update unit_sales column with predicted values
																					
validationRF$unit_sales[validationRF$date == validationRFSub$date & validationRF$item_nbr == validationRFSub$item_nbr & validationRF$store_nbr == validationRFSub$store_nbr] <- pred

}

																					# add actual unit_sales to validationRF as unit_sales_actual
																					
validationRF$unit_sales_actual <- validationRForig$unit_sales[validationRF$date == validationRForig$date & validationRF$item_nbr == validationRForig$item_nbr & validationRF$store_nbr == validationRForig$store_nbr]

colnames(validationRF)[5]<-c("unit_sales_predicted")								# rename the unit_sales as unit_sales_predicted

																					# write prediction data to file

write.csv(validationRF[,c("date","store_nbr","item_nbr","onpromotion","item_class","perishable","oil_price","unit_sales_actual","unit_sales_predicted")], 'RFprediction.csv',row.names=FALSE)

	
					
### Model Evaluation


	# NWRMSLE

																					# log1p requires input values should be >0
																					
validationRF<-validationRF[validationRF$unit_sales_predicted>0 & validationRF$unit_sales_actual>0, ]

																					# calculate NWRMSLE
																					# 1.25 is the weight for perishable and 1 for non-perishable
																					
sqrt(sum(ifelse(validationRF$perishable==TRUE,1.25,1)*( log1p(validationRF$unit_sales_predicted) - log1p(validationRF$unit_sales_actual) )^2)/sum(ifelse(validationRF$perishable==TRUE,1.25,1)))


	# RMSE
																					# calculate RMSE
																					
sqrt(sum(( validationRF$unit_sales_predicted - validationRF$unit_sales_actual )^2)/nrow(validationRF))
	

### Plot of Actual vs Prediction

actvspredRF1<-subset(validationRF,select=c("date","item_nbr","store_nbr","unit_sales_predicted"))
colnames(actvspredRF1)[4]<-c("unit_sales")
actvspredRF1$unit_sales_type<-"predicted"			

actvspredRF2<-subset(validationRForig,unit_sales>0,select=c("date","item_nbr","store_nbr","unit_sales"))
actvspredRF2$unit_sales_type<-"actual"
				
actvspredRF<-rbind(actvspredRF1,actvspredRF2)

actvspredRFplotdf<-actvspredRF %>% group_by(date,unit_sales_type) %>% summarise(unit_sales = sum(unit_sales)) %>% arrange(date,unit_sales_type)


ggplot(actvspredRFplotdf, aes(x=date, y=unit_sales,col=unit_sales_type)) +
	  geom_line(size=1) +
	  labs(x="Date", y="Total unit sales") +
	  ggtitle("Actual vs Predicted Total unit sales using Random Forest")+
	  scale_x_date(breaks=as.Date(seq(as.Date(validationStartDate), as.Date(endDate), by = "day")))+
	  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
	


