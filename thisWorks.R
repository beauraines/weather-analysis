require(Hmisc)
library(lubridate)


daily_temp_2021 = subset(sammamish_weather_history,Date>='2021-01-01',select=c('Station_ID','Date','Date_Time','air_temp_set_1')) %>% 
  group_by(Station_ID, Date) %>%
  summarise(min_temp=min(air_temp_set_1),max_temp=max(air_temp_set_1),mean_temp=mean(air_temp_set_1),median_temp=median(air_temp_set_1))

daily_temp_2021$DateDecimal = month(daily_temp_2021$Date) + day(daily_temp_2021$Date)/monthDays(daily_temp_2021$Date)

sammamish_weather_history$Year_Month = floor_date(sammamish_weather_history$Date_Time)
sammamish_weather_history$Date = floor_date(sammamish_weather_history$Date_Time,unit="days")
sammamish_weather_history$DateDecimal = month(sammamish_weather_history$Date) + day(sammamish_weather_history$Date)/monthDays(sammamish_weather_history$Date)

ggplot() +
  # geom_boxplot(data=subset(sammamish_weather_history,Date<'2021-01-01'),aes(x=Year_Month,y=air_temp_set_1,group=interaction(Year_Month),fill="slateblue",alpha=0.2)) +
  geom_line(data=daily_temp_2021,mapping = aes(x=DateDecimal,y=mean_temp)) +
  geom_line(data=daily_temp_2021,mapping = aes(x=DateDecimal,y=min_temp)) +
  geom_line(data=daily_temp_2021,mapping = aes(x=DateDecimal,y=max_temp))

ggplot() +
geom_boxplot(data=subset(sammamish_weather_history,Date<'2021-01-01'),aes(x=as.factor(month(Date)),y=air_temp_set_1,group=interaction(month(Date)+.5)),fill="slateblue",alpha=0.2) +
  geom_line(data=daily_temp_2021,mapping = aes(x=DateDecimal,y=mean_temp), color="yellow") +
  geom_line(data=daily_temp_2021,mapping = aes(x=DateDecimal,y=min_temp),color ="blue") +
  geom_line(data=daily_temp_2021,mapping = aes(x=DateDecimal,y=max_temp),color="red") +
  geom_line(data=daily_temp_2021,mapping = aes(x=DateDecimal,y=median_temp),color="orange")

ggplot(daily_temp_2021,aes(x=DateDecimal,y=max_temp)) +
  geom_point(aes(color=year(Date)))

ggplot(sammamish_weather_history,aes(x=DateDecimal,y=air_temp_set_1)) +
  geom_point(aes(color=year(Date)))


daily_temps = subset(sammamish_weather_history,select=c('Station_ID','Date','Date_Time','air_temp_set_1')) %>% 
  group_by(Station_ID, Date) %>%
  summarise(min_temp=min(air_temp_set_1),max_temp=max(air_temp_set_1),mean_temp=mean(air_temp_set_1),median_temp=median(air_temp_set_1))
daily_temps$DateDecimal = month(daily_temps$Date) + day(daily_temps$Date)/monthDays(daily_temps$Date)

ggplot(daily_temps,aes(x=DateDecimal,y=max_temp)) +
  geom_point(aes(color=year(Date)))+
  geom_smooth()
