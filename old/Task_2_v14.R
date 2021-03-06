# Energy consumption task

pacman::p_load(plyr,dplyr,tidyr,readr,lubridate,ggplot2,reshape,forecast, zoo, 
               tseries, opera, forecastHybrid, formatR, padr, RMySQL, prophet, 
               data.table, eply,  timeDate, stringr, tidyverse)

##### Data import #####

## Create a database connection 
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', 
                dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

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

HHPC[is.na(HHPC)] <- 0

##### Exploratory analysis #####

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

HHPC_dst$MonthYear <- floor_date(HHPC_dst$Date, unit = "month")

HHPC_dst$WeekYear <- floor_date(HHPC_dst$Date, "week")

####### Data by year, month, week, day and hour #######

granularity <- list()
group <- as.list(c("Year","MonthYear","WeekYear","Date"))

#group <- as.list(c("Year","MonthYear","WeekYear","Date", "DateTime"))

for(i in group) {

granularity[[i]] <- HHPC_dst %>% group_by_at(i) %>% 
  summarise(Global_reactive_power = sum(Global_reactive_power),
            Global_active_power_kwh = sum(Global_active_power_kwh),
            kitchen_kwh = sum(kitchen_kwh), laundry_kwh = sum(laundry_kwh),
            waterheat_aircond_kwh = sum(waterheat_aircond_kwh),
            Other_kwh = sum(Other_kwh), Voltage = mean(Voltage),
            Global_intensity = mean(Global_intensity))
}


granularity$MonthYear$Month <- lubridate::month(granularity$MonthYear$MonthYear, label =  FALSE, abbr = FALSE, 
                                   locale = "English")

granularity$Date$week <- lubridate::week(granularity$Date$Date)


#granularity$Year <- filter(granularity$Year, Year > 2007)

# granularity$MonthYear <- granularity$MonthYear[!(granularity$MonthYear$MonthYear == "12 - 2006"),] # excluding incomplete month
# granularity$MonthYear <- granularity$MonthYear[!(granularity$MonthYear$MonthYear == "11 - 2010"),]
# 
# granularity$WeekYear <- granularity$WeekYear[!(granularity$WeekYear$Year == "2006"),]

 
###################################### Forecast #############################################################

##### Prophet

df_prophet <- granularity

df_prophet$Date <- setnames(df_prophet$Date, c("Date","Global_active_power_kwh"),c("ds","y"))
df_prophet$MonthYear <- setnames(df_prophet$MonthYear, c("MonthYear","Global_active_power_kwh"),c("ds","y"))
df_prophet$Year <- setnames(df_prophet$Year, c("Year","Global_active_power_kwh"),c("ds","y"))
df_prophet$WeekYear <- setnames(df_prophet$WeekYear, c("WeekYear","Global_active_power_kwh"),c("ds","y"))

df_prophet$Date$type <- "day"
df_prophet$WeekYear$type <- "week"
df_prophet$MonthYear$type <- "month"
df_prophet$Year$type <- "year" 

#m <- lapply(df_prophet, FUN =  prophet)

m <- lapply(df_prophet, function(x) prophet(df = x, daily.seasonality = T, 
                                            yearly.seasonality = T, 
                                            weekly.seasonality = T))

# Extend dataframe 100 days into the future

future <- lapply(m,function(x) make_future_dataframe(m = x,periods = 24, freq = paste(x$history[1,"type"]))) 


# Generate forecast for next 100 days

forecast <- map2(m, future, predict) # lapply doesn't work in these cases

dyplot.prophet(x = m$MonthYear, fcst = forecast$MonthYear)
  
# export tables for power bi analysis

#granularity$MonthYear <- select(granularity$MonthYear, Global_active_power_kwh) %>% mutate(ID = seq.int(nrow(granularity$MonthYear)))

write.csv(granularity$MonthYear, file = "granularity$MonthYear.csv")
          
write.csv(granularity$Date, file = "granularity$Date.csv")


