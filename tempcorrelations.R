
daily_temps = subset(sammamish_weather_history,select=c('Station_ID','Date','Date_Time','air_temp_set_1')) %>% 
  group_by(Station_ID, Date) %>%
  summarise(min_temp=min(air_temp_set_1),max_temp=max(air_temp_set_1),mean_temp=mean(air_temp_set_1),median_temp=median(air_temp_set_1))
daily_temps$DateDecimal = month(daily_temps$Date) + day(daily_temps$Date)/monthDays(daily_temps$Date)


## All Season Correlation
y=subset(daily_temps,Date >='2019-02-01' & Date <='2020-01-01',select=max_temp)
x=subset(daily_house_temps,Date >='2019-02-01' & Date <='2020-01-01',select=max_temp) %>% tail(-1)
# x and y must be the same length vector for correlations
cor(x,y,use="complete.obs",method="pearson")

ggplot(tibble(house = x,outside = y),aes(x=house$max_temp,y=outside$max_temp)) + 
  geom_point()


ggplot(daily_house_temps) + 
  geom_line(aes(x=Date,y=max_temp),color="red") +
  geom_line(subset(daily_temps,Date>='2019-01-01'),mapping=aes(x=as_date(Date),y=max_temp))


# Winter time range much lower correlation between temperateure
y=subset(daily_temps,Date >='2019-10-01' & Date <='2020-03-01',select=max_temp)
x=subset(daily_house_temps,Date >='2019-10-01' & Date <='2020-03-01',select=max_temp) 
cor(x,y,use="complete.obs",method="pearson")

# Winter time range much lower correlation between temperateure
y=subset(daily_temps,Date >='2019-10-01' & Date <='2020-03-01',select=min_temp)
x=subset(daily_house_temps,Date >='2019-10-01' & Date <='2020-03-01',select=min_temp) 
cor(x,y,use="complete.obs",method="pearson")

# Summertime time range much higher correlation between temperateure
y=subset(daily_temps,Date >='2020-06-01' & Date <='2020-09-01',select=max_temp)
x=subset(daily_house_temps,Date >='2020-06-01' & Date <='2020-09-01',select=max_temp) %>% tail(-1)
cor(x,y,use="complete.obs",method="pearson")

# Summertime time range much higher correlation between temperateure
y=subset(daily_temps,Date >='2020-06-01' & Date <='2020-09-01',select=mean_temp)
x=subset(daily_house_temps,Date >='2020-06-01' & Date <='2020-09-01',select=mean_temp)
cor(x,y,use="complete.obs",method="pearson")

# Winter time range much lower correlation between temperateure
y=subset(daily_temps,Date >='2019-10-01' & Date <='2020-03-01',select=mean_temp)
x=subset(daily_house_temps,Date >='2019-10-01' & Date <='2020-03-01',select=mean_temp) 
cor(x,y,use="complete.obs",method="pearson")

## All Season Correlation
y=subset(daily_temps,Date >='2019-02-01' & Date <='2020-01-01',select=mean_temp)
x=subset(daily_house_temps,Date >='2019-02-01' & Date <='2020-01-01',select=mean_temp) 
# x and y must be the same length vector for correlations
cor(x,y,use="complete.obs",method="pearson")



