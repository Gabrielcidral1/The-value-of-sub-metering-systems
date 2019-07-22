# ggplot(data = by_year, aes(x = Year, y = Active_Power_Sub, fill = Sub_type)) + 
#   geom_bar(stat = "identity") + 
#   theme(text = element_text(size=20)) + labs(y= "Global Active Power") + 
#   ggtitle("Energy consumption per year")



# ggplot(data = granularity$MonthYear, aes(x = MonthYear, y = Global_active_power_kwh, 
#                             group = 1, label = round(Global_active_power_kwh,0))) + 
#   geom_line(stat = "identity", color = "blue", size = 2)+ 
#   geom_point(size=4, color = "blue") + theme(text = element_text(size=20), 
#                                              axis.text.x = element_text(angle = 50, 
#                                                                         hjust = 1),
#                                              panel.background = element_rect(fill='gray95', 
#                                                                              colour='gray100')) + 
#   labs(y= "Global Active Power") + ggtitle("Energy consumption per month - kwh") + 
#   theme(axis.text.x=element_text(color=c("black","transparent","transparent"))) 
# 


# 
# data_graph_day <- filter(granularity$Date,MonthYear %in% "11 - 2010")
# 
# data_graph_day$cumsum <- cumsum(data_graph_day$Global_active_power_kwh)
# 
# ggplot(data = data_graph_day, aes(x = Date, y = cumsum, group = 1, 
#                                   label = round(cumsum,0))) + geom_line(stat = "identity", 
#                                                                         color = "blue", 
#                                                                         size = 2)+ 
#   geom_point(size=4, color = "blue") + theme(text = element_text(size=20), 
#                                              axis.text.x = element_text(angle = 0, 
#                                                                         hjust = 1),
#                                              panel.background = element_rect(fill='gray95', 
#                                                                              colour='gray100')) + 
#   labs(y= "Global Active Power") + ggtitle("Daily energy consumption - kwh") + 
#   geom_text(vjust = 1.5, size = 8,color = c("transparent", "black","transparent", 
#                                             "black","transparent", "black","transparent", 
#                                             "black","transparent", "black","transparent", 
#                                             "black","transparent", "black","transparent", 
#                                             "black","transparent", "black","transparent", 
#                                             "black","transparent", "black","transparent", 
#                                             "black","transparent", "black"))
# 
# # Pie chart Nov. 2010
# data_pie <- data_graph_day %>% group_by(month) %>% 
#   summarise(waterheater_airconditioner = sum(waterheat_aircond_kwh), 
#             kitchen = sum(kitchen_kwh), laundry = sum(laundry_kwh), 
#             Other = sum(Other_kwh))
# 
# data_pie <- melt(as.data.frame(data_pie), id=c("month"))                     
# data_pie$month <- NULL
# names(data_pie)[names(data_pie) == 'variable'] <- '.'
# 
# ggplot(data_pie, aes(x="", y=value, fill= .))+
#   geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + 
#   theme(text = element_text(size=25)) + theme(axis.text = element_blank(),
#                                               axis.ticks = element_blank(),
#                                               panel.grid  = element_blank())
# 
# # Compare last month
# data_oct_2010 <- filter(granularity$Date,MonthYear %in% "10 - 2010")
# data_oct_2010 <- filter(data_oct_2010,Date != "2010-10-31" & Date != "2010-10-30" & 
#                           Date != "2010-10-29"& Date != "2010-10-28"& 
#                           Date != "2010-10-27")
# 
# sum(data_oct_2010$Global_active_power_kwh)
# 
# # Compare same month last year
# data_nov_2009 <- filter(granularity$Date,MonthYear %in% "11 - 2009")
# data_nov_2009 <- filter(data_nov_2009,Date != "2009-11-30" & Date != "2009-11-29"& 
#                           Date != "2009-11-28"& Date != "2009-11-27")
# 
# sum(data_nov_2009$Global_active_power_kwh)
# 
# 
# # Analysis on weekly consumption behaviour 
# data_by_wday <- HHPC_dst %>% group_by(Day_week) %>% 
#   summarise(Global_reactive_power = sum(Global_reactive_power), 
#             Global_active_power_kwh = sum(Global_active_power_kwh),
#             Voltage = mean(Voltage),Global_intensity = mean(Global_intensity),
#             kitchen_kwh = sum(kitchen_kwh), laundry_kwh = sum(laundry_kwh), 
#             waterheat_aircond_kwh = sum(waterheat_aircond_kwh), Other_kwh = sum(Other_kwh))
# 
# ggplot(data = data_by_wday, aes(x = Day_week, y = Global_active_power_kwh)) +
#   geom_bar(stat = "identity") + theme(text = element_text(size=20)) + 
#   labs(y= "Global Active Power") + 
#   ggtitle("Energy consumption per day of the week")
# 


# #### Analysis on daily consumption behaviour #####
# 
# # Weekend 
# 
# list <- c("Saturday", "Sunday")
# by_hour_cons <- filter(by_hour,Year == "2010" & Day_week %in% list) 
# 
# by_hour_cons <- by_hour_cons %>% group_by(Hour) %>% 
#   summarise(Global_active_power_kwh = mean(Global_active_power_kwh), 
#             kitchen_kwh = mean(kitchen_kwh), laundry_kwh = mean(laundry_kwh), 
#             waterheat_aircond_kwh = mean(waterheat_aircond_kwh), 
#             Other_kwh = mean(Other_kwh))
# 
# ggplot(data = by_hour_cons, aes(x = Hour, y = Global_active_power_kwh, group = 1, 
#                                 label = round(Global_active_power_kwh,0))) + 
#   geom_line(aes(y= kitchen_kwh, colour = "kitchen_kwh", size = 2)) + 
#   geom_line(aes(y= laundry_kwh, colour = "laundry_kwh", size = 2)) + 
#   geom_line(aes(y= waterheat_aircond_kwh, color = "waterheat_aircond_kwh", size = 2)) +
#   theme(text = element_text(size=20), axis.text.x = element_text(angle = 0, 
#                                                                  hjust = 1),panel.background = element_rect(fill='gray95', colour='gray100')) + 
#   labs(y= "Global Active Power") + ggtitle("Hourly energy consumption - kwh") 
# 
# # Weekday
# 
# list_wkday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
# by_hour_cons_wkday <- filter(by_hour, Year == "2010" & Day_week %in% list_wkday) 
# 
# by_hour_cons_wkday <- by_hour_cons_wkday %>% group_by(Hour) %>% 
#   summarise(Global_active_power_kwh = mean(Global_active_power_kwh),
#             kitchen_kwh = mean(kitchen_kwh), laundry_kwh = mean(laundry_kwh), 
#             waterheat_aircond_kwh = mean(waterheat_aircond_kwh), Other_kwh = mean(Other_kwh))
# 
# ggplot(data = by_hour_cons_wkday, 
#        aes(x = Hour, y = Global_active_power_kwh, 
#            group = 1, label = round(Global_active_power_kwh,0))) + 
#   geom_line(aes(y= kitchen_kwh, colour = "kitchen_kwh", size = 2)) + 
#   geom_line(aes(y= laundry_kwh, colour = "laundry_kwh", size = 2)) + 
#   geom_line(aes(y= waterheat_aircond_kwh, color = "waterheat_aircond_kwh", size = 2)) +
#   theme(text = element_text(size=20), axis.text.x = element_text(angle = 0, 
#                                                                  hjust = 1),panel.background = element_rect(fill='gray95', colour='gray100')) + 
#   labs(y= "Global Active Power") + ggtitle("Hourly energy consumption - kwh") 
# 
