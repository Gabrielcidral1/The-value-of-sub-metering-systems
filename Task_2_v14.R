# Energy consumption task

pacman::p_load(plyr,dplyr,tidyr,readr,lubridate,ggplot2,reshape,forecast, zoo, 
               tseries, opera, forecastHybrid, formatR, padr, RMySQL)


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
j <- c("yr_2006", "yr_2007", "yr_2008", "yr_2009", "yr_2010") # Should it be only from 2007 to 2009?
HHPC <- c()
for (i in 1:length(j)) {
  X <- dbGetQuery(con, 
                  paste("SELECT * FROM ",
                        j[i]))
  HHPC <- rbind(HHPC,X)
}
rm(X, i, j)
df$id <- NULL

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

# Missing values
missing <- HHPC[is.na(HHPC$Global_active_power),]

#####Time conversion####

# Column date time

HHPC$DateTime <- paste0(HHPC$Date,HHPC$Time)
HHPC <- HHPC[,c(ncol(HHPC), 1:(ncol(HHPC)-1))]

HHPC$DateTime <- as.POSIXct(HHPC$DateTime, "%Y-%m-%d %H:%M:%S", tz = "GMT")

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

# New time variables
HHPC_dst$Year <- year(HHPC_dst$DateTime)
HHPC_dst$Day_week <- wday(HHPC_dst$DateTime,label = TRUE, abbr = FALSE,
                          week_start = getOption("lubridate.week.start", 7),
                          locale = "English")

HHPC_dst$month <- month(HHPC_dst$DateTime, label =  FALSE, abbr = FALSE, 
                        locale = "English")

HHPC_dst$MonthYear <- paste(HHPC_dst$month,"-",HHPC_dst$Year)

#HHPC_dst$Summertime <- dst(as.character(HHPC_dst$DateTime))

HHPC_dst$Hour <- hour(HHPC_dst$DateTime)

HHPC_dst$Week <- week(HHPC_dst$DateTime)

HHPC_dst$WeekYear <- paste(HHPC_dst$Week,"-",HHPC_dst$Year)


####### Data by year, month, week, day and hour #######

#### *by year####
by_year <- HHPC_dst %>% group_by(Year) %>% 
  summarise(Global_reactive_power = sum(Global_reactive_power), 
            Global_active_power_kwh = sum(Global_active_power_kwh), 
            kitchen_kwh = sum(kitchen_kwh), laundry_kwh = sum(laundry_kwh), 
            waterheat_aircond_kwh = sum(waterheat_aircond_kwh), 
            Other_kwh = sum(Other_kwh), Voltage = mean(Voltage),
            Global_intensity = mean(Global_intensity))


# for( i in c("Year")) {
#   for(j in c("by_year", "by_month")) {
#   
# by_year <- HHPC_dst %>% group_by(paste(i) %>% 
#   summarise(Global_reactive_power = sum(Global_reactive_power), 
#             Global_active_power_kwh = sum(Global_active_power_kwh), 
#             kitchen_kwh = sum(kitchen_kwh), laundry_kwh = sum(laundry_kwh), 
#             waterheat_aircond_kwh = sum(waterheat_aircond_kwh), 
#             Other_kwh = sum(Other_kwh), Voltage = mean(Voltage),
#             Global_intensity = mean(Global_intensity))
#   }
# 
#   }
  
by_year <- by_year[!(by_year$Year == "2006"),]

by_year <- melt(as.data.frame(by_year),  id=c("Year","Global_reactive_power",
                                             "Global_active_power_kwh","Voltage", 
                                             "Global_intensity"))

names(by_year)[names(by_year) == 'variable'] <- 'Sub_type' # Rename
names(by_year)[names(by_year) == 'value'] <- 'Active_Power_Sub'# Rename

ggplot(data = by_year, aes(x = Year, y = Active_Power_Sub, fill = Sub_type)) + 
  geom_bar(stat = "identity") + 
  theme(text = element_text(size=20)) + labs(y= "Global Active Power") + 
  ggtitle("Energy consumption per year")

##### *by month ####
by_month <- HHPC_dst %>% group_by(MonthYear, month, Year) %>% 
  summarise(Global_reactive_power = sum(Global_reactive_power), 
            Global_active_power_kwh = sum(Global_active_power_kwh),
            Voltage = mean(Voltage),Global_intensity = mean(Global_intensity),
            kitchen_kwh = sum(kitchen_kwh), laundry_kwh = sum(laundry_kwh), 
            waterheat_aircond_kwh = sum(waterheat_aircond_kwh), Other_kwh = sum(Other_kwh))

by_month <- by_month[!(by_month$MonthYear == "12 - 2006"),] # excluding incomplete month
by_month <- by_month[!(by_month$MonthYear == "11 - 2010"),]

by_month <- by_month[order(by_month$Year,by_month$month),] # Order for graph
by_month <- transform(by_month, MonthYear = factor(MonthYear, levels = MonthYear))

ggplot(data = by_month, aes(x = MonthYear, y = Global_active_power_kwh, 
                            group = 1, label = round(Global_active_power_kwh,0))) + 
  geom_line(stat = "identity", color = "blue", size = 2)+ 
  geom_point(size=4, color = "blue") + theme(text = element_text(size=20), 
                                             axis.text.x = element_text(angle = 50, 
                                                                        hjust = 1),
                                             panel.background = element_rect(fill='gray95', 
                                                                             colour='gray100')) + 
  labs(y= "Global Active Power") + ggtitle("Energy consumption per month - kwh") + 
  theme(axis.text.x=element_text(color=c("black","transparent","transparent"))) 


########### *by week #######
by_week <- HHPC_dst %>% group_by(WeekYear, month, Year, MonthYear) %>% 
  summarise(Global_reactive_power = sum(Global_reactive_power), 
            Global_active_power_kwh = sum(Global_active_power_kwh),
            Voltage = mean(Voltage),
            Global_intensity = mean(Global_intensity),
            kitchen_kwh = sum(kitchen_kwh), laundry_kwh = sum(laundry_kwh), 
            waterheat_aircond_kwh = sum(waterheat_aircond_kwh), Other_kwh = sum(Other_kwh))

by_week <- by_week[!(by_week$Year == "2006"),]

########## *by day ##########
HHPC_dst$Date <- as.Date(HHPC_dst$DateTime, "%d/%m/%Y",tz = "GMT")

by_day <- HHPC_dst %>% group_by(Date,Day_week, month, Year, MonthYear) %>% 
  summarise(Global_reactive_power = sum(Global_reactive_power), 
            Global_active_power_kwh = sum(Global_active_power_kwh),
            Voltage = mean(Voltage),Global_intensity = mean(Global_intensity),
            kitchen_kwh = sum(kitchen_kwh), laundry_kwh = sum(laundry_kwh), 
            waterheat_aircond_kwh = sum(waterheat_aircond_kwh), Other_kwh = sum(Other_kwh))

data_graph_day <- filter(by_day,MonthYear %in% "11 - 2010")

data_graph_day$cumsum <- cumsum(data_graph_day$Global_active_power_kwh)

ggplot(data = data_graph_day, aes(x = Date, y = cumsum, group = 1, 
                                  label = round(cumsum,0))) + geom_line(stat = "identity", 
                                                                        color = "blue", 
                                                                        size = 2)+ 
  geom_point(size=4, color = "blue") + theme(text = element_text(size=20), 
                                             axis.text.x = element_text(angle = 0, 
                                                                        hjust = 1),
                                             panel.background = element_rect(fill='gray95', 
                                                                             colour='gray100')) + 
  labs(y= "Global Active Power") + ggtitle("Daily energy consumption - kwh") + 
  geom_text(vjust = 1.5, size = 8,color = c("transparent", "black","transparent", 
                                            "black","transparent", "black","transparent", 
                                            "black","transparent", "black","transparent", 
                                            "black","transparent", "black","transparent", 
                                            "black","transparent", "black","transparent", 
                                            "black","transparent", "black","transparent", 
                                            "black","transparent", "black"))

# Pie chart Nov. 2010
data_pie <- data_graph_day %>% group_by(month) %>% 
  summarise(waterheater_airconditioner = sum(waterheat_aircond_kwh), 
            kitchen = sum(kitchen_kwh), laundry = sum(laundry_kwh), 
            Other = sum(Other_kwh))

data_pie <- melt(as.data.frame(data_pie), id=c("month"))                     
data_pie$month <- NULL
names(data_pie)[names(data_pie) == 'variable'] <- '.'

ggplot(data_pie, aes(x="", y=value, fill= .))+
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + 
  theme(text = element_text(size=25)) + theme(axis.text = element_blank(),
                                              axis.ticks = element_blank(),
                                              panel.grid  = element_blank())

# Compare last month
data_oct_2010 <- filter(by_day,MonthYear %in% "10 - 2010")
data_oct_2010 <- filter(data_oct_2010,Date != "2010-10-31" & Date != "2010-10-30" & 
                          Date != "2010-10-29"& Date != "2010-10-28"& 
                          Date != "2010-10-27")

sum(data_oct_2010$Global_active_power_kwh)

# Compare same month last year
data_nov_2009 <- filter(by_day,MonthYear %in% "11 - 2009")
data_nov_2009 <- filter(data_nov_2009,Date != "2009-11-30" & Date != "2009-11-29"& 
                          Date != "2009-11-28"& Date != "2009-11-27")

sum(data_nov_2009$Global_active_power_kwh)


# Analysis on weekly consumption behaviour 
data_by_wday <- HHPC_dst %>% group_by(Day_week) %>% 
  summarise(Global_reactive_power = sum(Global_reactive_power), 
            Global_active_power_kwh = sum(Global_active_power_kwh),
            Voltage = mean(Voltage),Global_intensity = mean(Global_intensity),
            kitchen_kwh = sum(kitchen_kwh), laundry_kwh = sum(laundry_kwh), 
            waterheat_aircond_kwh = sum(waterheat_aircond_kwh), Other_kwh = sum(Other_kwh))

ggplot(data = data_by_wday, aes(x = Day_week, y = Global_active_power_kwh)) +
  geom_bar(stat = "identity") + theme(text = element_text(size=20)) + 
  labs(y= "Global Active Power") + 
  ggtitle("Energy consumption per day of the week")


############ *by hour ##########
by_hour <- HHPC_dst %>% group_by(Date, Hour, Day_week, month, Year, Summertime) %>% 
  summarise(Global_reactive_power = sum(Global_reactive_power), 
            Global_active_power_kwh = sum(Global_active_power_kwh),
            Voltage = mean(Voltage),Global_intensity = mean(Global_intensity), 
            kitchen_kwh = sum(kitchen_kwh), laundry_kwh = sum(laundry_kwh), 
            waterheat_aircond_kwh = sum(waterheat_aircond_kwh), Other_kwh = sum(Other_kwh))


by_hour$Hour <- by_hour$Hour*100
by_hour$Hour <- substr(as.POSIXct(sprintf("%04.0f", by_hour$Hour), format='%H%M'), 12, 16)
write.table(by_hour, "by_hour.txt", sep=";", row.names = F)
by_hour=read_delim("C:/Users/gabri/Desktop/Ubiqum/R/Deep_Analytics_and_Visualization/Task_1/by_hour.txt", 
                   ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d"), 
                                                                Hour = col_time(format = "%H:%M")), locale = locale(), trim_ws = TRUE)

by_hour <- cbind(by_hour,paste(by_hour$Date,by_hour$Hour), stringsAsFactors=FALSE) 
names(by_hour)[names(by_hour) == "paste(by_hour$Date, by_hour$Hour)"] <- 'DateTime'
by_hour <- by_hour[,c(ncol(by_hour), 1:(ncol(by_hour)-1))]

#### Analysis on daily consumption behaviour #####

# Weekend 

list <- c("Saturday", "Sunday")
by_hour_cons <- filter(by_hour,Year == "2010" & Day_week %in% list) 

by_hour_cons <- by_hour_cons %>% group_by(Hour) %>% 
  summarise(Global_active_power_kwh = mean(Global_active_power_kwh), 
            kitchen_kwh = mean(kitchen_kwh), laundry_kwh = mean(laundry_kwh), 
            waterheat_aircond_kwh = mean(waterheat_aircond_kwh), 
            Other_kwh = mean(Other_kwh))

ggplot(data = by_hour_cons, aes(x = Hour, y = Global_active_power_kwh, group = 1, 
                                label = round(Global_active_power_kwh,0))) + 
  geom_line(aes(y= kitchen_kwh, colour = "kitchen_kwh", size = 2)) + 
  geom_line(aes(y= laundry_kwh, colour = "laundry_kwh", size = 2)) + 
  geom_line(aes(y= waterheat_aircond_kwh, color = "waterheat_aircond_kwh", size = 2)) +
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 0, 
                                                                 hjust = 1),panel.background = element_rect(fill='gray95', colour='gray100')) + 
  labs(y= "Global Active Power") + ggtitle("Hourly energy consumption - kwh") 

# Weekday

list_wkday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
by_hour_cons_wkday <- filter(by_hour, Year == "2010" & Day_week %in% list_wkday) 

by_hour_cons_wkday <- by_hour_cons_wkday %>% group_by(Hour) %>% 
  summarise(Global_active_power_kwh = mean(Global_active_power_kwh),
            kitchen_kwh = mean(kitchen_kwh), laundry_kwh = mean(laundry_kwh), 
            waterheat_aircond_kwh = mean(waterheat_aircond_kwh), Other_kwh = mean(Other_kwh))

ggplot(data = by_hour_cons_wkday, 
       aes(x = Hour, y = Global_active_power_kwh, 
           group = 1, label = round(Global_active_power_kwh,0))) + 
  geom_line(aes(y= kitchen_kwh, colour = "kitchen_kwh", size = 2)) + 
  geom_line(aes(y= laundry_kwh, colour = "laundry_kwh", size = 2)) + 
  geom_line(aes(y= waterheat_aircond_kwh, color = "waterheat_aircond_kwh", size = 2)) +
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 0, 
                                                                 hjust = 1),panel.background = element_rect(fill='gray95', colour='gray100')) + 
  labs(y= "Global Active Power") + ggtitle("Hourly energy consumption - kwh") 


###################################### Predictions #############################################################

#################### Time Series

# Month
ts_month <- ts(by_month$Global_active_power_kwh, frequency=12, start = c(2007, 1), 
               end = c(2010, 10))
adf.test(ts_month) # test if stationary data
plot(ts_month)
plot(decompose(ts_month))
dec_month <- decompose(ts_month)

plot(stl(ts_month, s.window = 12))

sum(abs(x = dec_month$random), na.rm = T) # check the total absolute random

# Week
ts_week <- ts(by_week$Global_active_power_kwh, frequency = 52, start= c(2007, 1), 
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
ts_day <- ts(by_day$Global_active_power_kwh, start = c(2007, as.numeric(format(inds[1], "%j"))), # inds[1] means first day / %j means the day of the year (from 001 to 365)
           frequency = 365)

plot(ts_day)

train_day <- window(ts_day, start=c(2007,001), end=c(2009,365))


dec_day <- decompose(ts_day)

plot(stl(ts_month, s.window = 12))

sum(abs(x = dec_day$random), na.rm = T) # check the total absolute random (compare with monthly and daily)

###################### Models

####### tslm
df_tslm <- data.frame(Global_active_power_kwh = ts_month, as.numeric(time(ts_month)))
names(df_tslm) <- c("Global_active_power_kwh", "time")

tslm <- tslm(Global_active_power_kwh~season + trend,df_tslm)
fc_tslm <- forecast(tslm, h=36)
autoplot(fc_tslm)

###### *Holt-winters ######

# Month
HW_fixed <- HoltWinters(x = ts_month)

HW_fixed_pred_1 <- forecast(HW_fixed,h= 20)
autoplot(HW_fixed_pred_1)

# Week
HW_week <- HoltWinters(x = ts_week, seasonal = "additive")

HW_fixed_pred_week <- forecast(HW_week,h= 50)
autoplot(HW_fixed_pred_week, ylim = c(0,400))

######## *Arima ############

# Month

arima_month <- auto.arima(ts_month)

arima_month_pre <- forecast(arima_month, h=20)

plot(arima_month_pre)

# Week

arima_week <- auto.arima(ts_week)
arima_week_pred <- forecast(arima_week, h=20)
plot(arima_week_pred)

######## naive 
snaive_month <- snaive(ts_month, h=20)
plot(snaive_month)

# Train and Test sets

# train
train <- window(ts_month,start=c(2007,10),end=c(2009,10)) # create a cut in ts

arima_month_cv <- arima(train, order = c(0,0,0), 
                     seasonal = list(order = c(1,1,0), period = 12)) # there was an error using auto arima, so I had to input parameters by hand

arima_month_pre.cv <- forecast(arima_month_cv, h=12)

HW_fixed_cv <- HoltWinters(x = train)

HW_fixed_pred_1.cv <- forecast(HW_fixed_cv, h = 12)

snaive_fit <- snaive(train, h = 12)

autoplot(ts_month, start=c(2007,1)) +
  autolayer(snaive_fit, series="Seasonal naïve", PI=FALSE) +
  autolayer(arima_month_pre.cv, series="Auto arima", PI=FALSE) +
  autolayer(HW_fixed_pred_1.cv, series="Holt Winters", PI=FALSE) +
  xlab("Year") + ylab("kw") +
  ggtitle("Forecasts of energy consumption") +
  guides(colour=guide_legend(title="Forecast"))

# test

test <- window(ts_month, start=c(2009,11))
accuracy(snaive_fit, test)
accuracy(arima_month_pre.cv, test)
accuracy(HW_fixed_pred_1.cv, test)

# Combine models for test

x <- cbind(HW = HW_fixed_pred_1.cv$mean,SNAIVE = snaive_fit$mean)

mix_model <- mixture(Y = test, experts = x, model = 'BOA', loss.type = 'square')

w <- mix_model$weights

summary(mix_model)
plot(mix_model)

z <- ts(predict(mix_model, x, test, type='response'), start=c(2009,11), freq=12)
df <- cbind(ts_month, z)
colnames(df) <- c("Data","Combined model")
autoplot(df) + ylab("kw")
summary(mix_model)

class(mix_model)

class(HW_fixed_pred_1.cv)

######### hybrid model forecast ######

mix_model <- hybridModel(ts_month, models = c("z", "e", "s"), weights = "insample") # a = auto.arima, z = snaive, e = ets, s = stml

mix_model_fore <- forecast(mix_model, h = 20)

plot(mix_model_fore)

mean(mix_model_fore$upper - mix_model_fore$lower)


## loop for forecast combinations

results <- c()
names <- c()

x <- c("a","e","n")
y <- c("s","t","z")

combination <- as.vector(outer(x, y, paste, sep=""))

for(i in combination) {
    
    mix_model <- hybridModel(ts_month, models = x, weights = "insample.errors") # a = auto.arima, z = snaive, e = ets, s = stml

mix_model_fore <- forecast(mix_model, h = 20)

plot(mix_model_fore)

mean <- mean(mix_model_fore$upper - mix_model_fore$lower)

results <- cbind(mean, results)

colnames(results) <- x

}


# export tables for power bi analysis

#by_month <- select(by_month, Global_active_power_kwh) %>% mutate(ID = seq.int(nrow(by_month)))

write.csv(by_month, file = "by_month.csv")




