# Chunks of Codes used in the Project
# All of them written in R Programming Language

# ---------------------------------------------------------------



# 1. Loading Libraries 

library(tidyverse)
library(magrittr)
library(modelr)
library(nycflights13)
library(lubridate)
library(splines)
library(forcats)
library(plotly)



#2. Data Parsing and Import

# Loading & Parsing Data

Accidents_2017 <- read_csv("Data/datagov-uk-6efe5505-941f-45bf-b576-4c1e09b579a1/2017-8.csv", 
                           col_names = c("Reference_Number","Easting","Northing","Vehicles_Num","Accident_Date","Time","Road_Class","Road_Surface",
                                         "Lightning_Cond","Weather_Cond","Vehicle_Type","Casualty_Class","Severity","Gender","Age"), skip = 1)


Accidents_Date <- Accidents_2017$Accident_Date
Accidents_Date <- parse_date(Accidents_Date, "%m/%d/%Y")

Time <- Accidents_2017$Time
Time <- parse_time(Time, "%H%M")

Accidents_2017 <- Accidents_2017 %>%
  select(1:4, 7:15) %>%
  mutate(Accidents_Date, Time) 

Accidents_2017 <- Accidents_2017[,c(1:4,14,15,5:13)]


# Correcting spelling mistakes

Lightning_Cond <- 
  str_replace(Accidents_2017$Lightning_Cond,"Darkness: Street lights present and lit and lit","Darkness: Street lights present and lit")


Road_Class <- str_replace_all(Accidents_2017$Road_Class,
                              c("A.*" = "A","B.*" = "B","M.*" = "M"))


Road_Surface <- str_replace(Accidents_2017$Road_Surface,"^F.*","Snow")


Weather_Cond <- word(Accidents_2017$Weather_Cond,1) 
Weather_Cond <- str_replace(Weather_Cond,"Fog","Other")


Vehicle_Type <- word(Accidents_2017$Vehicle_Type,1)
Vehicle_Type <- str_replace_all(Vehicle_Type,
                                c(".Private" = "Taxi",
                                  "Ca.*" = "Car","Pedal" = "Cycle"))
Vehicle_Type <- str_replace(Vehicle_Type,"TaxiTaxi", "Taxi")


# Adding the improved columns to the dataset 

Accidents_2017 <- Accidents_2017 %>% 
  select(1:6,12:15) %>%
  mutate(Lightning_Cond,Weather_Cond,Road_Class,Road_Surface,Vehicle_Type,
         Year = year(Accidents_Date),
         Month = month(Accidents_Date),
         Day = day(Accidents_Date),
         Hour = hour(Time),
         Minute = minute(Time),
         Accidents_DateTime = make_datetime(Year, Month, Day, Hour, Minute))

Accidents_2017 <- Accidents_2017[,c(1:4, 16:20, 5:6, 21, 7:15)]


# Assigning the columns to their proper classes. 

for(i in 2:length(Accidents_2017)) {
  if(is.character(Accidents_2017[[i]])) {
    Accidents_2017[[i]] <- as.factor(Accidents_2017[[i]])
  } else if (is.numeric(Accidents_2017[[i]])) {
    Accidents_2017[[i]] <- as.numeric(Accidents_2017[[i]])
  }
}


map(Accidents_2017,class)

Accidents_2017


# Checking NAs 

sum(rowSums(is.na(Accidents_2017)))

# No NA values to explore



#3. Exploring Data Analysis

EDA1 <- Accidents_2017 %>%
  group_by(Gender, Age) %>%
  mutate(Count = n() ) %>%
  ungroup() %>%
  ggplot(mapping = aes(x=Age , y = Count)) +
  geom_point(alpha = 0.6) + 
  facet_wrap(~Gender) +
  ggtitle("Number of Casualties Per Age For Males and Females") +
  ylab("Number of Casualties") 

layout_plot <- function(my_plot, x = -0.057, y = - 0.033){
  my_plot[['x']][['layout']][['annotations']][[1]][['y']] <- x
  my_plot[['x']][['layout']][['annotations']][[2]][['x']] <- y
  my_plot
}

ggplotly(EDA1) %>% layout_plot

Accidents_2017 %>%
  group_by(Gender, Age) %>%
  summarise(Count = n() )


EDA2 <- Accidents_2017 %>%
  group_by(Casualty_Class, Age) %>%
  summarise(Count = n() ) %>%
  ungroup() %>%
  ggplot(mapping = aes(x=Age , y = Count)) +
  geom_point(alpha = 0.6) + 
  geom_smooth()+
  facet_wrap(~Casualty_Class) +
  ggtitle("Count against Age based on Casualty class") +
  ylab("Number of Casualties")

layout_plot <- function(my_plot, x = -0.057, y = - 0.033){
  my_plot[['x']][['layout']][['annotations']][[1]][['y']] <- x
  my_plot[['x']][['layout']][['annotations']][[2]][['x']] <- y
  my_plot
}

ggplotly(EDA2) %>% layout_plot


Accidents_2017 %>%
  group_by(Casualty_Class,Age) %>%
  summarise(Count = n() )


Accidents_2017 %>%
  group_by(Casualty_Class) %>%
  summarise(Count = n() )



#4. Sections Answering Questions

# Why does the number of accidents increase towards the end of the year ?

daily <- Accidents_2017 %>%
  group_by(Accidents_Date) %>%
  summarise(n = n()) 

daily

ggplot(daily, mapping = aes(Accidents_Date, n)) +
  geom_line()+
  geom_smooth()+
  ggtitle("Number of accidents against Date")+
  ylab("Number of accidents")+
  xlab("Date")

mod <- lm(n ~ Accidents_Date,data = daily)

grid <- daily %>%
  data_grid(Accidents_Date) %>%
  add_predictions(mod)

ggplot(daily,aes(Accidents_Date,n))+
  geom_line()+
  geom_point(aes(y = pred),data = grid,colour = "red")+
  ggtitle("Number of accidents against Date with predictions")+
  ylab("Number of accidents")+
  xlab("Date")

daily <- daily %>%
  add_residuals(mod)

Resid_plot1 <- ggplot(daily,aes(Accidents_Date,resid))+
  geom_point()+
  geom_ref_line(h = 0) +
  geom_smooth()+
  xlab("Date")+
  ylab("Residuals")+
  ggtitle("Residuals against Date")

ggplotly(Resid_plot1) 

Accidents_by_month <- 
  Accidents_2017 %>%
  count(Month)

Accidents_by_month

ggplot(data = Accidents_by_month, mapping = aes(x = Month, y = n)) +
  geom_bar(aes(fill = Month) , show.legend = F, stat = "identity") +
  ggtitle("Number of Accidents Per Month") +
  xlab("Month") +
  ylab("Number of Accidents")

Months <- Accidents_2017%>%
  filter( Month %in% c(1,9,10,11,12))
Months

Accidents_2017%>%
  count(Lightning_Cond)

Months %>%
  count(Lightning_Cond)


Accidents_2017%>%
  count(Road_Surface)
Months %>%
  count(Road_Surface)


Accidents_2017%>%
  count(Weather_Cond)
Months %>%
  count(Weather_Cond)

Climate <- Accidents_2017 %>%
  count(Accidents_Date,Month,Road_Surface,Lightning_Cond)


mod <- lm(n ~ Road_Surface + Lightning_Cond ,data = Climate)

grid <- Climate %>%
  data_grid(Month,Road_Surface,Lightning_Cond) %>%
  add_predictions(mod)

ggplot(Climate,aes(Month,n))+
  geom_point()+
  geom_point(data = grid,aes(y = pred),colour = "red")+
  ggtitle("Number of accidents against Month with predictions")+
  ylab("Number of accidents") +
  xlab("Month")

Climate <- Climate %>%
  add_residuals(mod)

Climate

Resid_plot2 <- ggplot(Climate,aes(Month,resid)) +
  geom_hex()+
  geom_ref_line(h = 0)+
  geom_smooth()+
  ggtitle("Residuals against Month")+
  ylab("Residuals")+
  xlab("Month")

ggplotly(Resid_plot2)

Resid_plot3 <- ggplot(Climate,aes(Accidents_Date,resid))+
  geom_hex()+
  geom_ref_line(h = 0) +
  geom_smooth()+
  ggtitle("Residuals against Date")+
  ylab("Residuals")+
  xlab("Date")

ggplotly(Resid_plot3)


#  Does the day of the week affect the time at which accidents happen ?

accident_hour <- Accidents_2017 %>%
  count(Hour) 

accident_hour

ggplot(accident_hour,aes(Hour,n))+
  geom_line()+
  geom_point()+
  ggtitle("Number of accidents against Hour")+
  ylab("Number of accidents")+
  xlab("Hour")

mod1 <- lm(n ~ ns(Hour,1) ,data = accident_hour)
mod2 <- lm(n ~ ns(Hour,2) ,data = accident_hour)
mod3 <- lm(n ~ ns(Hour,3) ,data = accident_hour)
mod4 <- lm(n ~ ns(Hour,4) ,data = accident_hour)
mod5 <- lm(n ~ ns(Hour,5) ,data = accident_hour)
mod6 <- lm(n ~ ns(Hour,6) ,data = accident_hour)

grid <- accident_hour %>%
  data_grid(Hour = seq_range(Hour,n = 50,expand = 0.1)) %>%
  gather_predictions(mod1,mod2,mod3,mod4,mod5,mod6,.pred = "Y")
grid

ggplot(accident_hour,aes(Hour,n))+
  geom_point()+
  geom_smooth(data = grid,aes(y = Y),colour = "red")+
  facet_wrap(~ model)+
  ggtitle("Number of accidents against Hour with predictions")+
  ylab("Number of accidents")+
  xlab("Hour")


accident_hour <- accident_hour %>%
  gather_residuals(mod1,mod2,mod3,mod4,mod5,mod6)

Resid_plot4 <- ggplot(accident_hour,aes(Hour,resid)) +
  geom_point()+ 
  geom_ref_line(h = 0)+ 
  geom_smooth()+
  facet_wrap(~model)+
  ggtitle("Residuals against Hour")+
  ylab("Residuals")+
  xlab("Hour")


ggplotly(Resid_plot4) %>% layout_plot

dow_hour <- Accidents_2017 %>%
  mutate(dow = wday(Accidents_DateTime,label =TRUE))%>%
  count(dow,Hour) 

dow_hour


ggplot(dow_hour,aes(Hour,n,colour = dow))+
  geom_line()+
  geom_point() +
  labs(col = "Days of the Week")+
  ggtitle("Number of accidents against Hour based on day of the week")+
  ylab("Number of accidents")+
  xlab("Hour")

mod <- lm(n ~ ns(Hour,6) ,data = dow_hour)
mod1 <- lm(n ~ ns(Hour,6) + dow,data = dow_hour)
mod2 <- lm(n ~ ns(Hour,6) * dow,data = dow_hour)

grid <- dow_hour %>%
  data_grid(Hour = seq_range(Hour,n = 50,expand = 0.1),dow) %>%
  gather_predictions(hour_alone = mod,plus_dow = mod1,multi_dow = mod2,.pred = "Y")
grid

ggplot(dow_hour,aes(Hour,n))+
  geom_point()+
  geom_smooth(data = grid,aes(y = Y),colour = "red")+
  facet_wrap(~ model)+
  ggtitle("Number of accidents against Hour with prediction based on model")+
  ylab("Number of accidents")+
  xlab("Hour")

dow_hour <- dow_hour %>%
  gather_residuals(hour_alone = mod,plus_dow = mod1,multi_dow = mod2)

Resid_plot5 <- ggplot(dow_hour,aes(Hour,resid)) +
  geom_point()+ geom_ref_line(h = 0)+ geom_smooth()+
  facet_wrap(~model)+
  ggtitle("Residuals against Hour based on model")+
  ylab("Residuals")+
  xlab("Hour")

ggplotly(Resid_plot5) %>% layout_plot()

dow_hour %>%
  filter(model == "multi_dow") %>%
  ggplot(aes(Hour,resid,colour = dow))+
  geom_ref_line(h = 0) + 
  geom_point() +
  labs(col = "Days of the Week")+
  ggtitle("Residuals against Hour based on model 2")+
  ylab("Residuals")+
  xlab("Hour")
