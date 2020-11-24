library(dplyr) 
library(ggplot2)
library(seasonal)
library(forecast) 
library(fpp)
library(forecast)
library(tseries)
library(urca)
library(glmulti)
library(forecast)
#library(Metrics)

setwd("~/Desktop/Statistics Machine Learning/Assignment_2/data/") 

####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
# Loading data 

drops <- c("v10_num_traj", "v20_num_traj", "v30_num_traj", 
           "v40_num_traj", "v50_num_traj" , 
           "v60_num_traj")

df1 <- read.csv('flow_20130225-20130303.csv') 
df1 <- df1[(df1['region_from'] == 1) , ]
df1 <- df1[(df1['region_to']   == 5) , ]
df1 <- df1[ , !(names(df1) %in% drops)]  

df2 <- read.csv('flow_20130304-20130310.csv') 
df2 <- df2[(df2['region_from'] == 1) , ]
df2 <- df2[(df2['region_to']   == 5) , ]
df2 <- df2[ , !(names(df2) %in% drops)]   

df3 <- read.csv('flow_20130311-20130317.csv') 
df3 <- df3[(df3['region_from'] == 1) , ]
df3 <- df3[ , !(names(df3) %in% drops)] 

df4 <- read.csv('flow_20130318-20130324.csv') 
df4 <- df4[(df4['region_from'] == 1) , ]
df4 <- df4[(df4['region_to']   == 5) , ]
df4 <- df4[ , !(names(df4) %in% drops)]  

print( c(dim(df1) , dim(df2) , dim(df3) , dim(df4)))


# Merge the above dataframes 
df <- rbind(df1, df2, df3, df4) 
df <- df[(df['region_from'] == 1) , ]
df <- df[(df['region_to']   == 5) , ] 
dim(df)
head(df)

drops <-  c("region_from" , "region_to" , "start_time", "end_time")
df <- df[ , !(names(df) %in% drops)]
head(df)
dim(df)

# There should be 24 time ids for each date, if not it will be zero. 
# So, I am making a data frame with 24 time ids for each date 

unique_time_id = unique(df$time_id) 
unique_date = unique(df$date) 

list_time_id = list(length(unique_time_id) * length(unique_date))
list_date = list(length(unique_time_id) * length(unique_date))
i = 0
for (time_id in unique_time_id) {
  for (date in  unique_date ){
    i =  i + 1 
    list_time_id[[i]] <-  time_id
    list_date[[i]] <-  date
  }
}

df_data_time_id = data.frame(unlist(list_date))
colnames(df_data_time_id) = 'date'
df_data_time_id['time_id'] = unlist(list_time_id)

# Outer join on the data frame with the data frame created above  
# deleting region_from and region_to columns as it is constant
# deleting start_time and end_time as we have time id 
df <- merge(x = df, y = df_data_time_id, by = c("date", "time_id"), all = TRUE)
# Rename columns
colnames(df) <- c("date" , "time_id" , "flow")
# Replace Na with zeor
df[is.na(df)] <- 0 
head(df)
dim(df)





####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
# Data cleaning 
"
Note: you have a number of tricky issues to deal with, including the presence of weekends,
and the fact that the range of hours in which people take trips differs from day to day. With
respect to the latter, the main interest of Translink is in the busier periods of the day. So, if
you wish, you can use only the time periods which are present for every day in the data.  
"
# calculate the number of zeoes per time 
df_0 = df[df$flow == 0 , ] 
df_0$flow = 1
aggregate(df_0$flow , by=list(time=df_0$time_id), FUN=sum) 

# Removing time id 1, 2,3,4,5 as they have greater than 50% zeroes 
df<- subset( df, !(df$time_id %in% c(1,2,3,4,5)))

# Histogram of flow 
summary(df$flow)
histinfo <- hist(df$flow)  
histinfo

summary(df$flow)
quantile(df$flow)
median(df$flow)
mean(df$flow)


####################################################################################################################################################################################
# Additive or Multiplicative

ssacf<- function(x) sum(acf(x, na.action = na.omit, plot = FALSE)$acf^2)
compare_ssacf<-function(add,mult) ifelse(ssacf(add)< ssacf(mult), 
                                         "Additive", "Multiplicative")  

T = ts(df$flow, frequency = 19)

T_stl_additive = stl(T,"periodic") 
T_stl_additive_residual = remainder(T_stl_additive)
T_stl_multiplicative = stl(log(T + 1),"periodic") 
T_stl_multiplicative_residual = remainder(T_stl_multiplicative)

ssacf(T_stl_additive_residual)
ssacf(T_stl_multiplicative_residual)
compare_ssacf(T_stl_additive_residual, T_stl_multiplicative_residual ) 

####################################################################################################################################################################################

df$flow_actual = df$flow
df$flow = log(df$flow + 1)
# Plot the ggplot using the day
df = df[order(df$date) , ]
head(df)
df_ts_hourly <- ts(df$flow, frequency=19) 
ggseasonplot(df_ts_hourly)

# Plot the ggplot using the weekdat detail
df$weekday <- weekdays(as.Date(df$date , format = "%d/%m/%y"))
aggregate(df$flow , by=list(weekday=df$weekday), FUN=mean)  
# Group by with day and time 

df_weekday = data.frame(df %>% group_by(weekday, time_id) %>% summarize(flow = mean(flow, na.rm = TRUE))) 

head(df_weekday)

df_weekday = df_weekday[order(df_weekday$weekday) , ]
# Plot the time series 
df_ts_weekday <- ts(df_weekday$flow , frequency = 19) 
df_ts_weekday
ggseasonplot(df_ts_weekday)
head(df)
head(df_weekday)
# Much more clear and consise with the help of week 

#####################################################################################
# Q3 
"
Check for stationarity and seasonality of the time-series data. Explain how you have done
this and include relevant graphs and numerical summaries. 
"

"
A stationary time series is one whose properties do not depend on the time at which the series is observed.
Thus, time series with trends, or with seasonality, are not stationary — t
he trend and seasonality will affect the value of the time series at different times. 
On the other hand, a white noise series is stationary — it does not matter when you observe it,
it should look much the same at any point in time. 
"


df_ts <- ts(df$flow_actual)
T_daily_actual<-ts(df_ts, frequency = 19) 
 


# Seasonality 
# Date plot  
df_ts <- ts(df$flow)
T_daily<-ts(df_ts, frequency = 19) 
ts.stl <- stl(T_daily,"periodic")  # decompose the TS
1 - var(remainder(ts.stl))/ var( seasonal(ts.stl) + remainder(ts.stl) )
# Sesonality 
T_daily_sesonality  <- seasadj(ts.stl) 

# Day plot  
df_ts <- ts(df_weekday$flow)
T_weekly<-ts(df_ts, frequency = 19) 
ts.stl <- stl(T_weekly,"periodic")  # decompose the TS
1 - var(remainder(ts.stl))/ var( seasonal(ts.stl) + remainder(ts.stl) )
df_weekday$sesonality <- seasonal(ts.stl)

# Merge with df 
df_sesonality <- merge(df, df_weekday, by = c("weekday", "time_id"))
df_sesonality$flow <- df_sesonality$flow.x - df_sesonality$sesonality
df_ts <- ts(df_sesonality$flow)
T_no_sesonality<-ts(df_ts, frequency = 19) 
ts.stl <- stl(T_no_sesonality,"periodic")  # decompose the TS
1 - var(remainder(ts.stl))/ var( seasonal(ts.stl) + remainder(ts.stl) )

# Trend
trend(ts.stl)

# Stationary
# No difference needed 
# From the above curve we can see that the seasonality is weekly. 
# Test if the series is stationary 
# Null hypothesis is that the data is stationary
# and we look for evidence that, the null hypothesis is false. 
# Consequently, small p-values (e.g., less than 0.05) 
# suggest that the data is stationary 
# Since p-value is 0.01. this means that the series is stationary.
dataset <- ts(T_daily) 
adf.test(dataset) 

T_daily %>% ur.kpss() %>% summary()

# ACF
ggAcf(T_no_sesonality , 100)
# PCF
ggPacf(T_no_sesonality, 100)  

ggAcf(T_no_sesonality %>% diff(lag = 19) , 100)
# PCF
ggPacf(T_no_sesonality %>% diff(lag = 19) , 100)   


plot(decompose(T_no_sesonality))
#####################################################################################
# Q4
# Choosing a model 

###
#  ARIMA 
###

train = subset(T_no_sesonality, end=length(T_no_sesonality)- 107)
test = subset(T_no_sesonality, start=length(T_no_sesonality)- 107 + 1) 

fit_arima <- auto.arima(train, seasonal = FALSE) 
checkresiduals(fit_arima)  
print(fit_arima)
forecastfit <- forecast(fit_arima, 107)
accuracy(test , data.frame(forecastfit)$Point.Forecast) 

###
#  SARIMA
### 


fit_sarima <- auto.arima(train)
forecast(fit_sarima)
checkresiduals(fit_sarima)   
forecastfit <- forecast(fit_sarima, 107)
print(fit_sarima)
accuracy(data.frame(forecastfit)$Point.Forecast, test) 

###
#  HW
###  


fit_hw <- hw(train, seasonal="additive") 
checkresiduals(fit_hw)   
forecastfit <- forecast(fit_hw, 107 )
accuracy(data.frame(forecastfit)$Point.Forecast, test)  
print(fit_hw$model)


###
#  ETS Model
### 

fit_ets <- ets(train) 
print(fit_ets)
checkresiduals(fit_ets)   
forecastfit <- forecast(fit_ets, 107)
accuracy(data.frame(forecastfit)$Point.Forecast, test)   



#####################################################################################
# Q5 
###
#  SARIMA
### 

train = subset(T_daily, end=length(T_no_sesonality)- 7* 19 )
test = subset(T_daily, start=length(T_no_sesonality)- 7 * 19 + 1) 

fit_sarima <- auto.arima(train)
checkresiduals(fit_sarima)   
  
 

train = subset(T_no_sesonality, end=length(T_no_sesonality)- 7* 19 )
test = subset(T_no_sesonality, start=length(T_no_sesonality)- 7 * 19 + 1) 

fit_sarima <- auto.arima(train)
checkresiduals(fit_sarima)   
print(fit_sarima)
forecastfit <- forecast(fit_sarima, 7* 19)
accuracy(data.frame(forecastfit)$Point.Forecast, test)  

fit_sarima %>% forecast(h=19) %>% autoplot()
 

###
#  Snaive
###  

fit_mean <- snaive(train, 7 * 19 ) 
print(fit_mean$model)
checkresiduals(fit_mean)
forecastfir_mean <- forecast(fit_mean)
fit_mean %>% forecast(h=19) %>% autoplot()

accuracy(data.frame(forecastfir_mean)$Point.Forecast, test) 

#####################################################################################
# Q6 

##################################################################################### 
#Q7

train = subset(T_no_sesonality, end=length(T_no_sesonality)- 19 )
test = subset(T_no_sesonality, start=length(T_no_sesonality)- 19 + 1) 

fit_sarima <- auto.arima(train)
fit_sarima %>% forecast(h=19) %>% autoplot() + autolayer(test)


checkresiduals(fit_sarima)   
print(fit_sarima)
forecastfit <- forecast(fit_sarima,  19)
 

df_forecast <- data.frame(forecastfit)
df_forecast$Lo.80 <- NULL
df_forecast$Hi.80 <- NULL
head(df_forecast)
df_forecast$Point.Forecast = exp(df_forecast$Point.Forecast + df_weekday[ df_weekday$weekday == 'Sunday' , ]$sesonality)
df_forecast$Lo.95 = exp(df_forecast$Lo.95 + df_weekday[ df_weekday$weekday == 'Sunday' , ]$sesonality)
df_forecast$Hi.95  = exp(df_forecast$Hi.95 + df_weekday[ df_weekday$weekday == 'Sunday' , ]$sesonality)
df_forecast$observed_data <- subset(T_daily_actual, start=length(T_daily)- 19 + 1) 
 
# add the sunday sesonality, take inverse log 
accuracy(df_forecast$Point.Forecast, df_forecast$observed_data)  
 
write.csv(df_forecast, 'sarima.csv')

 

###
#  Snaive
###  

fit_mean <- snaive(train, 19 ) 
fit_mean %>% forecast(h=19) %>% autoplot() + autolayer(test)
 
print(fit_mean$model)
checkresiduals(fit_mean)
forecastfir_mean <- forecast(fit_mean)
fit_mean %>% forecast(h=19) %>% autoplot() + autolayer(test)


df_forecast <- data.frame(forecastfir_mean)
df_forecast$observed_data <- data.frame(test)$test 
df_forecast$Lo.80 <- NULL
df_forecast$Hi.80 <- NULL
df_forecast$Point.Forecast = exp(df_forecast$Point.Forecast + df_weekday[ df_weekday$weekday == 'Sunday' , ]$sesonality)
df_forecast$Lo.95 = exp(df_forecast$Lo.95 + df_weekday[ df_weekday$weekday == 'Sunday' , ]$sesonality)
df_forecast$Hi.95  = exp(df_forecast$Hi.95 + df_weekday[ df_weekday$weekday == 'Sunday' , ]$sesonality)
df_forecast$observed_data <- subset(T_daily_actual, start=length(T_daily)- 19 + 1) 
 
accuracy(df_forecast$Point.Forecast , df_forecast$observed_data)  
 
write.csv(df_forecast, 'sniave.csv')



##################################################################################### 
#Q8

list_i = c() 
list_sarima_test = c() 
list_snaive_test = c() 

for (i in seq(1, 19)){
  #print(i)
  list_i = append(list_i, i)
  train = subset(T_no_sesonality, end=length(T_no_sesonality)- 19 + i -1 )
  test = subset(T_no_sesonality, start=length(T_no_sesonality)- 19 + i)[1]
  # Arima
  fit_sarima <- auto.arima(train)
  forecastfit <- forecast(fit_sarima,  1)
  accuracy(data.frame(forecastfit)$Point.Forecast, test)
  list_sarima_test = append(list_sarima_test , accuracy(data.frame(forecastfit)$Point.Forecast, test)[ , 2])
  # Snaive 
  fit_snaive <- snaive(train,19)
  forecastfit_snaice <- forecast(fit_snaive,  1)
  accuracy(data.frame(forecastfit_snaice)$Point.Forecast, test)
  #print(accuracy(data.frame(forecastfit_snaice)$Point.Forecast, test)[ , 2][2])
  list_snaive_test = append(list_snaive_test , accuracy(data.frame(forecastfit_snaice)$Point.Forecast, test)[ , 2])
}



accuracy_hour = data.frame(list_i)
accuracy_hour$sarima = list_sarima_test
accuracy_hour$sniave = list_snaive_test
 
write.csv(accuracy_hour, 'accuracy_hour.csv')
 

list_i_2 = c() 
list_sarima_test_2= c() 
list_snaive_test_2 = c()  
for (i in seq(1, 19,2)){
  print(i)
  list_i_2 = append(list_i_2, i)
  train = subset(T_no_sesonality, end=length(T_no_sesonality)- 19 + i -1 )
  test = subset(T_no_sesonality, start=length(T_no_sesonality)- 19 + i)[c(1,2)]
  # Arima
  fit_sarima <- auto.arima(train)
  forecastfit <- forecast(fit_sarima,  2)
  accuracy(data.frame(forecastfit)$Point.Forecast, test) 
  #accuracy(forecastfit, test)
  list_sarima_test_2 = append(list_sarima_test_2 , accuracy(forecastfit, test)[ , 2][2])
  # Snaive 
  fit_snaive <- snaive(train)
  forecastfit_snaice <- forecast(fit_snaive,  2)
  accuracy(data.frame(forecastfit_snaice)$Point.Forecast, test)
  accuracy(forecastfit_snaice, test)
  print(accuracy(forecastfit_snaice, test)[ , 2])
  list_snaive_test_2 = append(list_snaive_test_2 , accuracy(forecastfit_snaice, test)[ , 2][2])
}


accuracy_hour2 = data.frame(list_i_2)
accuracy_hour2$sarima = list_sarima_test_2
accuracy_hour2$sniave = list_snaive_test_2
 
write.csv(accuracy_hour2, 'accuracy_hour2.csv')



##################################################################################### 
#Q9

head(df)
tail(df)

fit_sarima <- auto.arima(train)
print(fit_sarima)
forecastfit <- forecast(fit_sarima,  19)
main_forecast = data.frame(exp(data.frame(forecastfit)$Point.Forecast  + df_weekday[ df_weekday$weekday == "Monday" ,  ]$sesonality)  - 1)

write.csv(main_forecast, 'main_forecast.csv')
 


  
