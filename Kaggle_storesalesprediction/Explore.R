##Author: Rachel Huang
##Content: Create new variables and joining dataset
library(data.table)

#Step 1:
source("Import.R")

#Step 2:

#function 0 convert to calendar week
calendar_weeks <- function(x){as.integer(format(as.POSIXct(x),"%U"))}
# test 
#calendar_weeks("2015-01-15")


#function 1 creating new date variables for train and test data
func_dates <- function(input_table_name,new_table_name){
    new_table_name <- data.table(input_table_name)
    names(new_table_name) <- tolower(names(new_table_name))
    new_table_name$date <- as.Date(new_table_name$date,'%Y-%m-%d')
 #derive more variables
    new_table_name$year = as.data.frame(as.POSIXlt(new_table_name$date)$year + 1900)    # x$year : years since 1900
    new_table_name$month = as.data.frame(as.POSIXlt(new_table_name$date)$mon+1 )
    new_table_name$day = as.data.frame(as.POSIXlt(new_table_name$date)$mday )
    new_table_name$calendar_week = mapply(calendar_weeks,new_table_name$date)
return (new_table_name)   
}
    
dt.test = func_dates(test,dt.test)
dt.train = func_dates(train,dt.train)
dt.train



#function2 
dt.store <- data.table(store)
names(dt.store) <-tolower(names(dt.store))
head(dt.train[store==2,])
head(dt.store[store==2,])

#merge store table to train table, then divide table to continues promotion stores and non continuous promotion stores
#then create new variables: month of last promotion for sales 
#then predict
#important to know: promo2 since year, 2010 - this year's promotion (2012,2013-2014 )*52 then for weeks, 13 - 20 =-7

setkey(dt.train, store)
setkey(dt.store,store)
system.time(
    dt.merge <- merge(dt.train, dt.store, by=c('store'))
)
nrow(dt.train)
nrow(dt.merge)
#  user  system elapsed 
#  0.018   0.002   0.022




head(dt.merge)
summary(dt.merge)
dt.merge[dt.merge$competitionopensinceyear==1900,]
nrow(dt.store[is.na(dt.store$competitionopensinceyear) ,])
nrow(dt.store)
354/1115

# library(car) 
# library(ggplot2)
# scatterplot(sales ~ promo2sinceyear | competitionopensinceyear, data=dt.merge, 
#             xlab="Weight of Car", ylab="Miles Per Gallon", 
#             main="Enhanced Scatter Plot", 
#             labels=row.names(mtcars))
# 
# names(dt.merge)
# 
# p <- qplot(competitiondistance, sales, data=dt.merge, 
#             main="Scatterplots of MPG vs. Horsepower",
#            xlab="Horsepower", ylab="Miles per Gallon")
# 
# # White background and black grid lines
# p + theme_bw()

#na data
summary(dt.merge)
#promo2sinceweek NA's : 
#promotion year - promo2sinceyear = 1 , if promo2sinceyear is na, then difference = 0; promoweek - promo2sinceweek
#competition open since month??? month of sales - competition since month, (year sales - competitions ince year)*12 , 
#problem: competition open dates may be missing because distance data have more lines than date data

head(dt.merge)

dt.merge$years_from_comp_open <- dt.merge$year-dt.merge$competitionopensinceyear
dt.merge$months_from_comp_open <- dt.merge$month-dt.merge$competitionopensincemonth
dt.merge$gap_months <- dt.merge$years_from_comp_open*12 + dt.merge$months_from_comp_open
#from first time the continuous promotion started:
dt.merge$promotion_weeks_interval <- (dt.merge$promo2sinceyear-dt.merge$year)*52 - (dt.merge$promo2sinceweek-dt.merge$calendar_week)


#bigger the gap is , lower the sales are, why??? set NAs to very big gap???


dt.merge2 <- dt.merge[!is.na(competitionopensinceyear),]
dt.merge3 <- dt.merge2[competitionopensinceyear>1980,]
plot(dt.merge3$gap_months,dt.merge3$sales)

new2 <- dt.merge[is.na(dt.merge$competitiondistance),]
new1 <- dt.merge[is.na(dt.merge$competitionopensincemonth),]

View(new1)
head(new2)

nrow(dt.store[is.na(dt.store$competitionopensinceyear),])


