install.packages("forecast")
install.packages("zoo")
library(data.table)
library(zoo)
library(forecast)
library(ggplot2)
install.packages("data.table")
library(data.table)

###read table
store<- read.csv("store.csv",stringsAsFactors = F)
train <- read.csv("train.csv",stringsAsFactors = F)
test <-read.csv("test.csv",stringsAsFactors = F)

##Explore
train <- data.table(train)
test <- data.table(test)
is.data.table(train)

str(store)
str(train)
str(test)

#convert date to dateformat from factor
train[, Date := as.Date(Date)]
test[,Date := as.Date(Date)]
train[,SchoolHoliday := as.character(SchoolHoliday)]
test[,SchoolHoliday := as.character(SchoolHoliday)]

#summary
summary(train)
summary(test)
train <- train[order(Date)]
test <- test[order(Date)]

hist(train$Sales, 100)
hist(train$Customers, 100)

table(train$Promo) / nrow(train)
table(test$Promo)/nrow(test)
View(store)

########understand individual store
store1 <- subset(train, Store == 1)
store_store1 <- subset(store,Store ==1)
store_store1
table(store1$Promo)/nrow(store1)

table(store1$StateHoliday)/nrow(store1)
no_sales <- subset(store1,Sales ==0)
table(no_sales$DayOfWeek)/nrow(no_sales)  #7th day of the week has no sales??? sunday???

no_sales
table(no_sales$StateHoliday)/nrow(no_sales)
table(no_sales$Promo)/nrow(no_sales)
nrow(no_sales)
nrow(store1)


plot(train$DayOfWeek,train$Sales)
plot(store1$DayOfWeek,store1$Sales)


#########plot box plot and plot to understand sales by date, sales by promo, sales by holiday
setkey(train, Date)
system.time( 
    dt.salesbydate <- train[,list(sales=sum(Sales),
                                      customer=sum(Customers)
                                      ),
                                by=list(Date)
                                ] 
)
dt.salesbydate
plot(dt.salesbydate$Date,dt.salesbydate$sale)

freq=12
myts <- ts(dt.salesbydate$sales, start=c(2013, 1), end=c(2015, 07), frequency=365) 
myts
plot(myts)

head(train)
names(train) <- tolower(names(train))
names(test) <- tolower(names(test))
names(store) <- tolower(names(store))

############Derive more variables : Year, Month and Day #####################
install.packages("lubridate")
library(lubridate)

#train$Date <- as.Date(train$Date)
train$month <- as.data.frame(month(train$date))
head(train)
train$year <- as.data.frame(year(train$date))
train$day <- as.data.frame(day$date)  #this function is not working 

# class(train$date)
# train$date <- as.POSIXlt(train$date)  # not working......read the documentation for POSIXLT (HOW TO CONVERT CHAR TO DATE....)
# zz <- cbind(train, year = year(date), month = month(date), day = day(date))
# head(zz)
# 
# class(train$Date)
# 
# strptime(train$Date2, "%y/%m/%d")
# train$Date2 <- as.Date(train$Date)
# head(train)

head(store)
#Merge store data and train and test data

###############################################



