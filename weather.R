library(tidyverse)
library(readr)
library(stringr)
library(stats)
library(lubridate)

JuneJuly <- read_csv("JuneJuly.csv")

as.Date.POSIXlt(str_replace(JuneJuly$Date_Time[1]," PDT",""))
JuneJuly$Date_Time =as.POSIXct(JuneJuly$Date_Time,format="%m/%d/%Y %H:%M")
JuneJuly$Date_Time = ymd_hms(JuneJuly$Date_Time,tz=Sys.timezone())
E8450_2021_05_31 <- read_csv("E8450.2021-05-31.csv",
col_types = cols(heat_index_set_1d = col_double()))
sammamish_weather_history = rbind(E8450_2021_05_31,JuneJuly)

sammamish_temps = aggregate(sammamish_weather_history$air_temp_set_1,list(sammamish_weather_history$Date),function(air_temp){
c(max = max(air_temp), min = min(air_temp),mean = mean(air_temp))
})
names(sammamish_temps[1] <- "Date")


ggplot() +
  geom_boxplot(data=subset(sammamish_weather_history,Date<'2021-01-01'),aes(x=as.factor(month(Date)),y=air_temp_set_1),fill="slateblue",alpha=0.2) +
  geom_line(subset(sammamish_weather_history,Date>='2021-01-01'),mapping = aes(x=as.factor(format(Date,'%m')) + as.numeric(format(Date,".%d"))/days_in_month(as.numeric(format(Date,'%m'))),y=air_temp_set_1))

#    stat_summary(fun.y=mean, geom="point", shape=20, size=14, color="red", fill="red") +


ggplot() +
  +   geom_boxplot(data=subset(sammamish_weather_history,Date<'2021-01-01'),aes(x=as.factor(month(Date)),y=air_temp_set_1,group=interaction(month(Date))),fill="slateblue",alpha=0.2) +
  +   geom_line(subset(sammamish_weather_history,Date>='2021-01-01'),mapping = aes(x=(yday(Date)/31)+1,y=air_temp_set_1,colour="red",alpha=0.2))



library(viridis)
ggplot(sammamish_weather_history,aes(y=Year_Month,x=air_temp_set_1)) +
geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
theme_ridges() +
theme(legend.position = "none") +
scale_fill_viridis(name = "air_temp_set_1", option = "C")

sammamish_weather_history %>%
  + ggplot(aes(x=yday(Date_Time),y=air_temp_set_1,group=year(Date_Time),color=year(Date_Time))) +
  + geom_line()



ggplot() +
  geom_boxplot(data=subset(sammamish_weather_history,Date<'2021-01-01'),aes(x=month(Date),y=air_temp_set_1,group=interaction(month(Date))),fill="slateblue",alpha=0.2) +
  geom_line(subset(sammamish_weather_history,Date>='2021-01-01'),mapping = aes(x=month(Date),y=air_temp_set_1))