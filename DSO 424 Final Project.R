library(rio)
library(lubridate)
library(dplyr)
library(hms)
library(xts)
library(forecast)
library(ggplot2)
library(segmented)
########### IMPORT DATA #######################################################

url = c("https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2021/04/metro-trips-2021-q1-1.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2021/10/metro-trips-2021-q3.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2021/07/metro-trips-2021-q2.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2021/01/metro-trips-2020-q4.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2020/10/metro-trips-2020-q3.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2021/03/metro-trips-2020-q2-v2.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2020/04/metro-bike-share-trips-2020-q1.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2020/01/metro-bike-share-trips-2019-q4.csv.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2019/10/metro-bike-share-trips-2019-q3-1.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2019/08/metro-bike-share-trips-2019-q2.csv.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2019/04/metro-bike-share-trips-2019-q1.csv.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2019/01/metro-bike-share-trips-2018-q4.csv.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2018/10/metro-bike-share-trips-2018-q3.csv.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2018/08/metro-bike-share-trips-2018-q2.csv.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2018/04/metro-bike-share-trips-2018-q1.csv.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2018/02/metro-bike-share-trips-2017-q4-v2.csv.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2016/10/metro-bike-share-trips-2017-q3.csv.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2017/07/la_metro_gbfs_trips_Q2_2017.csv.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2017/04/la_metro_gbfs_trips_Q1_2017.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2018/09/metro-bike-share-trips-2016-q4.csv.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2016/10/MetroBikeShare_2016_Q3_trips.zip"
)

# import the first quarter data
data = import(url[1])
str(data)
data = data[c("start_time","start_station")]
# change time format
data$start_time = mdy_hm(data$start_time)
str(data)

# save column names
FirstQuarterNames = names(data)

# import other quarters
for (i in 2:length(url)){
  print(i)
  if (i<4){
    quarter_i = import(url[i], which = 2)}
  else{
    quarter_i = import(url[i])}
  
  if (i %in% c(19,21)){
    quarter_i = quarter_i[c("start_time","start_station_id")]}
  else{
    quarter_i = quarter_i[c("start_time","start_station")]}
  
  colnames(quarter_i) = FirstQuarterNames
  
  quarter_i$start_time = mdy_hm(quarter_i$start_time)
  data = rbind(data, quarter_i)
}

# Check final table
dim(data)

# Import Station data
station_url = "https://bikeshare.metro.net/wp-content/uploads/2021/10/metro-bike-share-stations-2021-10-01.csv"
station = import(station_url)
colnames(station) = c("station_id","station_name","go_live_date", "region","status")


########### DATA PROCESSING ###################################################

# Get the station information by joining the data with station table

data = left_join(data,station,by=c("start_station"="station_id"))
data$date = date(data$start_time)
data$hour = hour(data$start_time)

# Now we try to separate the dataset based on regions

data = data[data$region %in% c("DTLA","North Hollywood","Westside"),] # We only keep the required regions
grouped_data = data %>% count(region,date,hour)

DT = grouped_data[grouped_data$region=="DTLA",]
NH = grouped_data[grouped_data$region=="North Hollywood",]
WS = grouped_data[grouped_data$region=="Westside",]
JOIN  = data %>% count(date,hour)

# finalize the table for analysis

hourly_demand = data.frame(Hour = seq(ymd_hm("2016-7-7 4:00",tz="US/Pacific"), ymd_hm("2021-9-30 23:00",tz="US/Pacific"), by = "hour"))

DT$time=hms(hours=DT$hour)
DT$Hour=as.POSIXct(paste(DT$date, DT$time), format="%Y-%m-%d %H:%M:%S", tz="US/Pacific")
DT = left_join(hourly_demand,DT,by=c("Hour"="Hour"))
DT = DT[c("Hour","n")]
DT[is.na(DT)] = 0

JOIN$time=hms(hours=JOIN$hour)
JOIN$Hour=as.POSIXct(paste(JOIN$date, JOIN$time), format="%Y-%m-%d %H:%M:%S", tz="US/Pacific")
JOIN = left_join(hourly_demand,JOIN,by=c("Hour"="Hour")) # we use the same hourly demand table for the jointly demand
JOIN = JOIN[c("Hour","n")]
JOIN[is.na(JOIN)] = 0

# we create new hourly demand table for North Hollywood and Westside individually and format the data for time analysis

hourly_demand = data.frame(Hour = seq(ymd_hm("2019-8-15 13:00",tz="US/Pacific"), ymd_hm("2021-9-30 23:00",tz="US/Pacific"), by = "hour"))
NH$time=hms(hours=NH$hour)
NH$Hour=as.POSIXct(paste(NH$date, NH$time), format="%Y-%m-%d %H:%M:%S", tz="US/Pacific")
NH = left_join(hourly_demand,NH,by=c("Hour"="Hour"))
NH = NH[c("Hour","n")]
NH[is.na(NH)] = 0

hourly_demand = data.frame(Hour = seq(ymd_hm("2017-9-1 10:00",tz="US/Pacific"), ymd_hm("2021-9-30 23:00",tz="US/Pacific"), by = "hour"))
WS$time=hms(hours=WS$hour) 
WS$Hour=as.POSIXct(paste(WS$date, WS$time), format="%Y-%m-%d %H:%M:%S", tz="US/Pacific")
WS = left_join(hourly_demand,WS,by=c("Hour"="Hour"))
WS = WS[c("Hour","n")]
WS[is.na(WS)] = 0


###### Visualization ##########################################################


# some missing values for every region!!
DT %>% ggplot(aes(x=Hour,y=n)) + geom_line(size=1.25) +  
  geom_point(aes(color="red",size=2) + theme_bw())

NH %>% ggplot(aes(x=Hour,y=n)) + geom_line(size=1.25) +  
  geom_point(aes(color="red",size=2) + theme_bw())

WS %>% ggplot(aes(x=Hour,y=n)) + geom_line(size=1.25) +  
  geom_point(aes(color="red",size=2) + theme_bw())

JOIN %>% ggplot(aes(x=Hour,y=n)) + geom_line(size=1.25) +  
  geom_point(aes(color="red",size=2) + theme_bw())

# after seeing how much data was missing for several chunks of each regional data,
# we observed that the most consistent and meaningful chunk of data is everything starting from 2020 Q1
# therefore we decided to use data starting from 2020 to build our predictive models from


###### Modeling and Forecasting ################################################


#### DT #####

DT = DT[DT$Hour>(ymd_hm("2020-1-1 0:00",tz="US/Pacific")),]

### Model0 inspired by double seasonality ###

DT0 = DT
DT0$hour = hour(DT0$Hour)
DT0$year = year(DT0$Hour)
DT0$month = month(DT0$Hour)
DT0$mday = day(DT0$Hour)
DT0$week = week(DT0$Hour)
DT0$wday = wday(DT0$Hour)
DT0$quarter = quarter(DT0$Hour)
DT0$qday = qday(DT0$Hour)
DT0$trend = 1: (dim(DT0)[1])

DT_M0 = lm(n ~ trend + year+ quarter + month + week + hour + qday + mday + wday, data=DT0)
summary(DT_M0)

DT0$M0 = predict(DT_M0,newdata=DT0)

# plot data and overplay predictions
DT0 %>% ggplot(aes(x=Hour,y=n)) +geom_line() +
  geom_point() + geom_line(aes(x=Hour,y=M0),col="blue")+theme_bw()


#### NH #####

NH = NH[NH$Hour>(ymd_hm("2020-1-1 0:00",tz="US/Pacific")),]

### Model0 inspired by double seasonality ###

NH0 = NH
NH0$hour = hour(NH0$Hour)
NH0$year = year(NH0$Hour)
NH0$month = month(NH0$Hour)
NH0$mday = day(NH0$Hour)
NH0$week = week(NH0$Hour)
NH0$wday = wday(NH0$Hour)
NH0$quarter = quarter(NH0$Hour)
NH0$qday = qday(NH0$Hour)
NH0$trend = 1: (dim(NH0)[1])

NH_M0 = lm(n ~ trend + year+ quarter + month + week + hour + qday + mday + wday, data=NH0)
summary(NH_M0)

NH0$M0 = predict(NH_M0,newdata=NH0)

# plot data and overplay predictions
NH0 %>% ggplot(aes(x=Hour,y=n)) +geom_line() +
  geom_point() + geom_line(aes(x=Hour,y=M0),col="blue")+theme_bw()








###############################################################################
#### WS #####

WS = WS[WS$Hour>(ymd_hm("2020-1-1 0:00",tz="US/Pacific")),]

### Model0 inspired by double seasonality ###

WS0 = WS
WS0$hour = hour(WS0$Hour)
WS0$year = year(WS0$Hour)
WS0$month = month(WS0$Hour)
WS0$mday = day(WS0$Hour)
WS0$week = week(WS0$Hour)
WS0$wday = wday(WS0$Hour)
WS0$quarter = quarter(WS0$Hour)
WS0$qday = qday(WS0$Hour)
WS0$trend = 1: (dim(WS0)[1])

WS_M0 = lm(n ~ trend + year+ quarter + month + week + hour + qday + mday + wday, data=WS0)
summary(WS_M0)

WS0$M0 = predict(WS_M0,newdata=WS0)

# plot data and overplay predictions
WS0 %>% ggplot(aes(x=Hour,y=n)) +geom_line() +
  geom_point() + geom_line(aes(x=Hour,y=M0),col="blue")+theme_bw()


#### JOINTLY #####

JOIN = JOIN[JOIN$Hour>(ymd_hm("2020-1-1 0:00",tz="US/Pacific")),]

### Model0 inspired by double seasonality ###

JOIN0 = JOIN
JOIN0$hour = hour(JOIN0$Hour)
JOIN0$year = year(JOIN0$Hour)
JOIN0$month = month(JOIN0$Hour)
JOIN0$mday = day(JOIN0$Hour)
JOIN0$week = week(JOIN0$Hour)
JOIN0$wday = wday(JOIN0$Hour)
JOIN0$quarter = quarter(JOIN0$Hour)
JOIN0$qday = qday(JOIN0$Hour)
JOIN0$trend = 1 : dim(JOIN0[1])
JOIN0

JOIN_M0 = lm(n ~ trend + year+ quarter + month + week + hour + qday + mday + wday, data=JOIN0)
summary(JOIN_M0)

JOIN0$M0 = predict(JOIN_M0,newdata=JOIN0)

# plot data and overplay predictions
JOIN0 %>% ggplot(aes(x=Hour,y=n)) +geom_line() +
  geom_point() + geom_line(aes(x=Hour,y=M0),col="blue")+theme_bw()




#################################################################################


###First try of nn ###

JOIN_train = JOIN[JOIN$Hour < ymd_hm("2021-4-1 0:00",tz="US/Pacific"),]
JOIN_test = JOIN[JOIN$Hour >= ymd_hm("2021-4-1 0:00",tz="US/Pacific"),]

M1 = nnetar(xts(JOIN_train$n, JOIN_train$Hour))
MF1 = forecast(M1,h=length(xts(JOIN_test$n, JOIN_test$Hour)))

accuracy(MF1,xts(JOIN_test$n, JOIN_test$Hour))


### second try of nn & arima & ets ###

DT.train = DT[DT$Hour<ymd_hm("2021-1-1 0:00",tz="US/Pacific"),]
DT.test = DT[DT$Hour>=ymd_hm("2021-1-1 0:00",tz="US/Pacific"),]

DT.train = msts(DT.train$n,seasonal.periods = c(24,24*7,365.25*24))
DT.test = msts(DT.test$n,seasonal.periods = c(24,24*7,365.25*24))

DT_time = msts(DT$n,seasonal.periods = c(24,24*7,365.25*24))
DT.train = window(DT_time,end = c(1, sum(year(DT$Hour)<=2020)))
DT.test = window(DT_time,start =c(1, sum(year(DT$Hour)<=2020)+1))

DT.train
DT_M1 = nnetar(DT.train)
DT_MF1 = forecast(DT_M1,h=length(DT.train))

accuracy(DT_MF1,DT.test)
DT.train
autoplot(DT.train)
autoplot(DT_MF1) + autolayer(DT.test)
DT_MF1

DT_M2 = auto.arima(DT.train,lambda="auto")
DT_MF2 = forecast(DT_M2,h=length(DT.test))
accuracy(DT_MF2,DT.test)
autoplot(DT_MF2) + autolayer(DT.test)

DT_M3 = ets(DT.train)
DT_MF3 = forecast(DT_M3,h=length(DT.test))
accuracy(DT_MF3,DT.test)
autoplot(DT_MF3) + autolayer(DT.test)

### MSTS NN model for North Hollywood

NH.train = NH[NH$Hour<ymd_hm("2021-1-1 0:00",tz="US/Pacific"),]
NH.test = NH[NH$Hour>=ymd_hm("2021-1-1 0:00",tz="US/Pacific"),]

NH.train = msts(NH.train$n,seasonal.periods = c(24,24*7,365.25*24))
NH.test = msts(NH.test$n,seasonal.periods = c(24,24*7,365.25*24))

NH_time = msts(NH$n,seasonal.periods = c(24,24*7,365.25*24))
NH.train = window(NH_time,end = c(1, sum(year(NH$Hour)<=2020)))
NH.test = window(NH_time,start =c(1, sum(year(NH$Hour)<=2020)+1))


NH_M1 = nnetar(NH.train)
NH_MF1 = forecast(NH_M1,h=length(NH.test))

accuracy(NH_MF1,NH.test)
NH.train
autoplot(NH.train)
autoplot(NH_MF1) + autolayer(NH.test)

### MSTS NN model for West side

WS.train = WS[WS$Hour<ymd_hm("2021-1-1 0:00",tz="US/Pacific"),]
WS.test = WS[WS$Hour>=ymd_hm("2021-1-1 0:00",tz="US/Pacific"),]

WS.train = msts(WS.train$n,seasonal.periods = c(24,24*7,365.25*24))
WS.test = msts(WS.test$n,seasonal.periods = c(24,24*7,365.25*24))

WS_time = msts(WS$n,seasonal.periods = c(24,24*7,365.25*24))
WS.train = window(WS_time,end = c(1, sum(year(WS$Hour)<=2020)))
WS.test = window(WS_time,start =c(1, sum(year(WS$Hour)<=2020)+1))


WS_M1 = nnetar(WS.train)
WS_MF1 = forecast(WS_M1,h=length(WS.test))

accuracy(WS_MF1,WS.test)
WS.train
autoplot(WS.train)
autoplot(WS_MF1) + autolayer(WS.test)


### MSTS NN model for Joint

JOIN.train = JOIN[JOIN$Hour<ymd_hm("2021-1-1 0:00",tz="US/Pacific"),]
JOIN.test = JOIN[JOIN$Hour>=ymd_hm("2021-1-1 0:00",tz="US/Pacific"),]

JOIN.train = msts(JOIN.train$n,seasonal.periods = c(24,24*7,365.25*24))
JOIN.test = msts(JOIN.test$n,seasonal.periods = c(24,24*7,365.25*24))

JOIN_time = msts(JOIN$n,seasonal.periods = c(24,24*7,365.25*24))
JOIN.train = window(JOIN_time,end = c(1, sum(year(JOIN$Hour)<=2020)))
JOIN.test = window(JOIN_time,start =c(1, sum(year(JOIN$Hour)<=2020)+1))


JOIN_M1 = nnetar(JOIN.train)
JOIN_MF1 = forecast(JOIN_M1,h=length(JOIN.test))

accuracy(JOIN_MF1,JOIN.test)
JOIN.train
autoplot(JOIN.train)
autoplot(JOIN_MF1) + autolayer(JOIN.test)





