# The R file is modify a chunk of codes

# Previous code

accident_hour <- Accidents_2017 %>%
  mutate(hour = hour(Time),
         minute = minute(Time))%>%
  count(hour) 

accident_hour


dow_hour <- Accidents_2017 %>%
  mutate(hour = hour(Time),
         minute = minute(Time),
         year = year(Accidents_Date),
         month = month(Accidents_Date),
         day = day(Accidents_Date),
         date = make_date(year, month, day),
         dow = wday(date,label =TRUE))%>%
  count(dow,hour) 

dow_hour


ggplot(accident_hour,aes(hour,n))+
  geom_line()+
  geom_point()

ggplot(dow_hour,aes(hour,n,colour = dow))+
  geom_line()+
  geom_point()


ggplot(accident_hour,aes(hour,n))+
  geom_line()+
  geom_point()+
  geom_line(data = dow_hour,aes(hour,n,colour = dow))

# Modified Code

(Accidents_Hour <- Accidents_2017 %>% count(Hour))

(DoW_Hour <- Accidents_2017 %>% 
  mutate(day_of_week = wday(Accidents_Date, label = TRUE))%>%
  count(day_of_week, Hour))

ggplot(Accidents_Hour, mapping = aes(Hour, n)) +
  geom_line() +
  geom_point()

ggplot(DoW_Hour, mapping = aes(Hour, n, color = day_of_week))+
  geom_line()+
  geom_point()

ggplot(Accidents_Hour, mapping = aes(Hour,n))+
  geom_line()+
  geom_point()+
  geom_line(data = DoW_Hour, mapping = aes(Hour,n,color = day_of_week))
