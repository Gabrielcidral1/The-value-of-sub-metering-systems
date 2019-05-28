# Energy consumption task

pacman::p_load(plyr,dplyr,tidyr,readr,lubridate,ggplot2,reshape,forecast, zoo, 
               tseries, opera, forecastHybrid, formatR, padr, RMySQL, prophet, data.table, eply,  timeDate, stringr)

##### Data import #####

## Create a database connection 
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', 
                dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

# Data set INFORMATION -----
## sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy). It corresponds to the kitchen, containing mainly a dishwasher, an oven and a microwave (hot plates are not electric but gas powered). 
## sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy). It corresponds to the laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light. 
## sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy). It corresponds to an electric water-heater and an air-conditioner.

# Load data-----
j <- c("yr_2006", "yr_2007", "yr_2008", "yr_2009", "yr_2010") 
HHPC <- c()
for (i in 1:length(j)) {
  X <- dbGetQuery(con, 
                  paste("SELECT * FROM ",
                        j[i]))
  HHPC <- rbind(HHPC,X)
}
rm(X, j)

HHPC$DateTime <- paste(HHPC$Date, HHPC$Time)

HHPC$DateTime <- ymd_hms(HHPC$DateTime)

HHPC <- pad(x = HHPC, break_above = 3)

# Fill NAs with data ----
# For the ones that are less than three minutes:
for (i in 4:ncol(HHPC)){
  HHPC[ ,i] <- na.locf(HHPC[ ,i], maxgap = 3 )
  
} 
#We consider that the 3 min gap is the time the meters and submeters need for software updates.

HHPC[is.na(HHPC)] <- 0

##### Exploratory analysis #####

#####Time conversion####

# Scale conversion
HHPC$kitchen_kwh <- HHPC$Sub_metering_1/1000
HHPC$laundry_kwh <- HHPC$Sub_metering_2/1000
HHPC$waterheat_aircond_kwh <- HHPC$Sub_metering_3/1000
HHPC$Global_active_power_kwh <- HHPC$Global_active_power/60
HHPC <- HHPC[,-which(names(HHPC) %in% c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))]  # delete old columns (sub 1, 2, 3)
HHPC$Other_kwh <- HHPC$Global_active_power_kwh - HHPC$kitchen_kwh - HHPC$laundry_kwh - 
  HHPC$waterheat_aircond_kwh

##### Daylight saving time treatment######

for (i in c("2007-03-25 02:00:00", "2008-03-30 02:00:00", "2009-03-29 02:00:00", "2010-03-28 02:00:00")) {

  HHPC_dst <- HHPC %>% 
    mutate(DateTime = ifelse(between(DateTime,as_datetime(i),
                                     as_datetime("2007-10-28 01:59:00")), 
                             DateTime+3600,DateTime))
  
  HHPC_dst$DateTime <- as.POSIXct(HHPC_dst$DateTime, origin =  "1970-01-01 00:00:00", 
                                  "%d-%m-%Y %H:%M:%S")
}


HHPC_dst$Date <- as.Date(HHPC_dst$DateTime)

# New time variables
HHPC_dst$Year <- floor_date(HHPC_dst$Date, unit = "year")
HHPC_dst$Day_week <- lubridate::wday(HHPC_dst$DateTime,label = TRUE, abbr = FALSE,
                          week_start = getOption("lubridate.week.start", 7),
                          locale = "English")

HHPC_dst$month <- lubridate::month(HHPC_dst$DateTime, label =  FALSE, abbr = FALSE, 
                        locale = "English")

HHPC_dst$MonthYear <- floor_date(HHPC_dst$Date, unit = "month")


#HHPC_dst$Summertime <- dst(as.character(HHPC_dst$DateTime)) #it takes a while to run

HHPC_dst$Hour <- lubridate::hour(HHPC_dst$DateTime)

HHPC_dst$Week <- lubridate::week(HHPC_dst$DateTime)

HHPC_dst$WeekYear <- floor_date(HHPC_dst$Date, "week")

####### Data by year, month, week, day and hour #######

granularity <- list()
group <- as.list(c("Year","MonthYear","WeekYear","Date"))


for(i in group) {

granularity[[i]] <- HHPC_dst %>% group_by_at(i) %>% 
  summarise(Global_reactive_power = sum(Global_reactive_power),
            Global_active_power_kwh = sum(Global_active_power_kwh),
            kitchen_kwh = sum(kitchen_kwh), laundry_kwh = sum(laundry_kwh),
            waterheat_aircond_kwh = sum(waterheat_aircond_kwh),
            Other_kwh = sum(Other_kwh), Voltage = mean(Voltage),
            Global_intensity = mean(Global_intensity))
}

# granularity$Year <- granularity$Year[!(granularity$Year$Year == "2006"),]
# 
# granularity$Year <- melt(as.data.frame(granularity$Year),  id=c("Year","Global_reactive_power",
#                                              "Global_active_power_kwh","Voltage", 
#                                              "Global_intensity"))
# 
# names(granularity$Year)[names(granularity$Year) == 'variable'] <- 'Sub_type' # Rename
# names(granularity$Year)[names(granularity$Year) == 'value'] <- 'Active_Power_Sub'# Rename
# 

# granularity$MonthYear <- granularity$MonthYear[!(granularity$MonthYear$MonthYear == "12 - 2006"),] # excluding incomplete month
# granularity$MonthYear <- granularity$MonthYear[!(granularity$MonthYear$MonthYear == "11 - 2010"),]
# 
# granularity$WeekYear <- granularity$WeekYear[!(granularity$WeekYear$Year == "2006"),]

 
# ############ *by hour ##########
# by_hour <- HHPC_dst %>% group_by(Date, Hour, Day_week, month, Year, Summertime) %>% 
#   summarise(Global_reactive_power = sum(Global_reactive_power), 
#             Global_active_power_kwh = sum(Global_active_power_kwh),
#             Voltage = mean(Voltage),Global_intensity = mean(Global_intensity), 
#             kitchen_kwh = sum(kitchen_kwh), laundry_kwh = sum(laundry_kwh), 
#             waterheat_aircond_kwh = sum(waterheat_aircond_kwh), Other_kwh = sum(Other_kwh))
# 
# 
# by_hour$Hour <- by_hour$Hour*100
# by_hour$Hour <- substr(as.POSIXct(sprintf("%04.0f", by_hour$Hour), format='%H%M'), 12, 16)
# write.table(by_hour, "by_hour.txt", sep=";", row.names = F)
# by_hour=read_delim("C:/Users/gabri/Desktop/Ubiqum/R/Deep_Analytics_and_Visualization/Task_1/by_hour.txt", 
#                    ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d"), 
#                                                                 Hour = col_time(format = "%H:%M")), trim_ws = TRUE)
# 
# by_hour <- cbind(by_hour,paste(by_hour$Date,by_hour$Hour), stringsAsFactors=FALSE) 
# names(by_hour)[names(by_hour) == "paste(by_hour$Date, by_hour$Hour)"] <- 'DateTime'
# 
# 
###################################### Forecast #############################################################

#################### Time Series

# Month
ts_month <- ts(granularity$MonthYear$Global_active_power_kwh, frequency=12, start = c(2007, 1), 
               end = c(2010, 10))
adf.test(ts_month) # test if stationary data
plot(ts_month)
plot(decompose(ts_month))
dec_month <- decompose(ts_month)

plot(stl(ts_month, s.window = 12))

sum(abs(x = dec_month$random), na.rm = T) # check the total absolute random

# Week
ts_week <- ts(granularity$WeekYear$Global_active_power_kwh, frequency = 52, start= c(2007, 1), 
              end = c(2010,48))
adf.test(ts_week) # test for stationarity (mean and variance doesn't change over time)
plot(ts_week)

dec_week <- decompose(ts_week)

plot(stl(ts_month, s.window = 12))

sum(abs(x = dec_week$random), na.rm = T) # check the total absolute random (compare with monthly and daily)


# Day

inds <- seq(as.Date("2007-01-01"), as.Date("2010-11-26"), by = "day") # Create a daily Date object 

## Create a time series object
set.seed(25)
ts_day <- ts(granularity$Date$Global_active_power_kwh, start = c(2007, as.numeric(format(inds[1], "%j"))), # inds[1] means first day / %j means the day of the year (from 001 to 365)
           frequency = 365)

plot(ts_day)

train_day <- window(ts_day, start=c(2007,001), end=c(2009,365))

dec_day <- decompose(ts_day)

plot(stl(ts_month, s.window = 12))

sum(abs(x = dec_day$random), na.rm = T) # check the total absolute random (compare with monthly and daily)

ts_granular <- list(ts_month, ts_week, ts_day)


###################### Models #######

# ###### *Holt-winters ######
# 
# # Month
# HW_fixed <- HoltWinters(x = ts_month)
# 
# HW_fixed_pred_1 <- forecast(HW_fixed,h= 20)
# autoplot(HW_fixed_pred_1)
# 
# # Week
# HW_week <- HoltWinters(x = ts_week, seasonal = "additive")
# 
# HW_fixed_pred_week <- forecast(HW_week,h= 50)
# autoplot(HW_fixed_pred_week, ylim = c(0,400))
# 
# ######## *Arima ############
# 
# # Month
# 
# arima_month <- auto.arima(ts_month)
# 
# arima_month_pre <- forecast(arima_month, h=20)
# 
# plot(arima_month_pre)
# 
# # Week
# 
# arima_week <- auto.arima(ts_week)
# arima_week_pred <- forecast(arima_week, h=20)
# plot(arima_week_pred)
# 
# ######## naive ######
# snaive_month <- snaive(ts_month, h=20)
# plot(snaive_month)
# 
# # Train and Test sets
# 
# # train
# train <- window(ts_month,start=c(2007,10),end=c(2009,10)) # create a cut in ts
# 
# arima_month_cv <- arima(train, order = c(0,0,0), 
#                      seasonal = list(order = c(1,1,0), period = 12)) # there was an error using auto arima, so I had to input parameters by hand
# 
# arima_month_pre.cv <- forecast(arima_month_cv, h=12)
# 
# HW_fixed_cv <- HoltWinters(x = train)
# 
# HW_fixed_pred_1.cv <- forecast(HW_fixed_cv, h = 12)
# 
# snaive_fit <- snaive(train, h = 12)
# 
# autoplot(ts_month, start=c(2007,1)) +
#   autolayer(snaive_fit, series="Seasonal naïve", PI=FALSE) +
#   autolayer(arima_month_pre.cv, series="Auto arima", PI=FALSE) +
#   autolayer(HW_fixed_pred_1.cv, series="Holt Winters", PI=FALSE) +
#   xlab("Year") + ylab("kw") +
#   ggtitle("Forecasts of energy consumption") +
#   guides(colour=guide_legend(title="Forecast"))
# 
# # test
# 
# test <- window(ts_month, start=c(2009,11))
# accuracy(snaive_fit, test)
# accuracy(arima_month_pre.cv, test)
# accuracy(HW_fixed_pred_1.cv, test)
# 
# # Combine models for test
# 
# x <- cbind(HW = HW_fixed_pred_1.cv$mean,SNAIVE = snaive_fit$mean)
# 
# mix_model <- mixture(Y = test, experts = x, model = 'BOA', loss.type = 'square')
# 
# w <- mix_model$weights
# 
# summary(mix_model)
# plot(mix_model)
# 
# z <- ts(predict(mix_model, x, test, type='response'), start=c(2009,11), freq=12)
# df <- cbind(ts_month, z)
# colnames(df) <- c("Data","Combined model")
# autoplot(df) + ylab("kw")
# 

##### Prophet

df_prophet <- granularity

df_prophet$Date <- setnames(df_prophet$Date, c("Date","Global_active_power_kwh"),c("ds","y"))
df_prophet$MonthYear <- setnames(df_prophet$MonthYear, c("MonthYear","Global_active_power_kwh"),c("ds","y"))
df_prophet$Year <- setnames(df_prophet$Year, c("Year","Global_active_power_kwh"),c("ds","y"))
df_prophet$WeekYear <- setnames(df_prophet$WeekYear, c("WeekYear","Global_active_power_kwh"),c("ds","y"))

m <- lapply(df_prophet, FUN = prophet)

# Extend dataframe 100 days into the future

future <- lapply(m, function(x) make_future_dataframe(m = x, periods = 100))

# Generate forecast for next 100 days
system.time(
  forecast <- predict(m$Date, future$Date)
  )



# export tables for power bi analysis

#granularity$MonthYear <- select(granularity$MonthYear, Global_active_power_kwh) %>% mutate(ID = seq.int(nrow(granularity$MonthYear)))

write.csv(granularity$MonthYear, file = "granularity$MonthYear.csv")
          
write.csv(granularity$Date, file = "granularity$Date.csv")


