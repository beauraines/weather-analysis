
library(tidyverse)

data.frame(year = 2019:2021, month= 1:12)

f = crossing(year=c(2019:2021),month=c(1:12))
year=c(2019:2021)
month=c('01','02','03','04','05','06','07','08','09','10','11','12')
basedir='/tmp/Takeout/Nest/thermostats/02AA01AC031501TB'

house_temps=tibble()

for (y in year) {
  #print(y)
  for (m in month) {
    #print(m)
    print(str_c(basedir,"/",y,"/",m,"/",y,"-",m,"-sensors.csv"))
    file=str_c(basedir,"/",y,"/",m,"/",y,"-",m,"-sensors.csv")
    try({
      input = read_csv(file)
      house_temps =rbind(house_temps,input)
      })
    
  }
}

# Clean Data

# Convert Temp to Farenheit
house_temps$temp <- (1.8 * house_temps$`avg(temp)`) + 32


# Find min,max,average, daily range

daily_house_temps = house_temps %>%
  group_by(Date) %>%
  summarise(min_temp=min(temp,na.rm=TRUE),max_temp=max(temp,na.rm=TRUE),mean_temp=mean(temp,na.rm=TRUE),median_temp=median(temp,na.rm=TRUE))

ggplot(daily_house_temps,aes(x=Date)) + 
  geom_line(aes(y=mean_temp),color="yellow") +
  geom_line(aes(y=max_temp),color="red") +
  geom_line(aes(y=min_temp),color="blue")

