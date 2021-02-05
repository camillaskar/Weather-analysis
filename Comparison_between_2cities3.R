library(readr)
hourlyclimate <- read_csv("C:/Users/qw21/Downloads/hourlyclimate.csv")
library(dplyr)
library(tidyverse)
library(data.table)
library(callr)
library(ggplot2)
library(isoband)
library(magrittr)
library(lubridate)
library(purrr)
library(rlang)



#creating a new data frame with only means and date

yearly_climate = group_by(hourlyclimate, origin, year, month, day) %>% summarise(temp = mean(temp), dewp = mean(dewp), humid = mean(humid), ) %>% as.data.frame()
yearly_climate %<>% mutate(date = as.Date(paste0(year, '-', month, '-', day)))



#splitting data into two - JFK and LGA & creating 2 new data frames 

JFK1 <- data.frame(hourlyclimate %>%filter(hourlyclimate$origin=='JFK'))
LGA1 <- data.frame(hourlyclimate %>%filter(hourlyclimate$origin=='LGA'))



#filtered data from selected columns

select(hourlyclimate, 3, 4, 13) %>%
  filter(month == '1' & day == '13') 


      
#summarising to find means(temp, humid and etc)

JFK_mean <- data.frame(JFK1 %>% summarise(mean_temp=mean(temp, na.rm = T),
                                  mean_dewp=mean(dewp, na.rm = T),
                                                 mean_humid=mean(humid, na.rm = T),
                                                                 mean_winddir=mean(wind_dir, na.rm = T),
                                                                                   mean_windspeed=mean(wind_speed, na.rm = T),
                                                                                                       mean_windgust=mean(wind_gust, na.rm = T)))
LGA_means <-data.frame(LGA1 %>% summarise(mean_temp1=mean(temp, na.rm = T),
                   mean_dewp1=mean(dewp, na.rm = T),
                   mean_humid1=mean(humid, na.rm = T),
                   mean_winddir1=mean(wind_dir, na.rm = T),
                   mean_windspeed1=mean(wind_speed, na.rm = T),
                   mean_windgust1=mean(wind_gust, na.rm = T)))



#grouping by origins and temperatures
yearly_climate %>% 
  group_by(origin) %>% 
  summarise(count=n(), avg=mean(temp, na.rm = T), coolest =min(temp,na.rm = T) , hottest=max(temp,na.rm = T) ) %>%
  arrange(desc(count))


# ggplot and visualization (geom_point)


yearly_climate_JFK<- data.frame(yearly_climate %>% filter(yearly_climate$origin=='JFK'))
yearly_climate_LGA<- data.frame(yearly_climate %>% filter(yearly_climate$origin=='LGA'))



a <- ggplot(yearly_climate_JFK, aes(temp, date))
a <- a + geom_point(colour = "orange")
a <- a + geom_smooth(method = lm, se = FALSE)
a <- a + theme_light() + ggtitle("Change of tempreature in JFK")
a
                                                                                                                   

b <- ggplot(yearly_climate_LGA, aes(temp, date))
b <- b + geom_point(colour = "green")
b <- b + geom_smooth(method = lm, se = FALSE)
b <- b + theme_light() + ggtitle("Change of tempreature in LGA")
b



#creating a histogram

a <- ggplot(yearly_climate_JFK, aes(dewp))
a + geom_histogram(binwidth = 1, col="red")


b <- ggplot(yearly_climate_LGA, aes(dewp))
b + geom_histogram(binwidth = 1, col="red")




#density
a + geom_density()

b+geom_density()


#geom bar
a <- ggplot(yearly_climate_JFK, aes(temp))
a + geom_bar(col="blue")

b <- ggplot(yearly_climate_LGA, aes(temp))
b + geom_bar(col="red") 



##geom_box
# subset data
hde <- hourlyclimate %>% subset(origin == "JFK" | origin == "LGA")
# plot the distributions
i <- ggplot(hde, aes(origin, wind_gust))
i + geom_boxplot()




#Comparing yearly temperature with the mean temperature

e <- ggplot(yearly_climate, aes(temp))
e <- e + geom_density()

yearly_climate %>% 
  group_by(origin) %>% 
  summarise(count=n(), avg=mean(temp, na.rm = T), coolest =min(temp,na.rm = T) , hottest=max(temp,na.rm = T) ) %>%
  arrange(desc(count))


a <- a + geom_vline(xintercept = mean_temp, lty = 2, colour = "red")
a <- a + theme_light()
a <- a + theme(axis.title = element_text(size = 9, color = "grey"),
               axis.text = element_text(size = 15))
a







#mapping colors to variables

ggplot(yearly_climate_JFK) + 
  geom_point(mapping = aes(x = temp, y = dewp, color = humid))


ggplot(yearly_climate) + 
  geom_point(mapping = aes(x = temp, y = dewp, color = origin))




#comparing temperatures throughout the year in two origins


p = ggplot(yearly_climate, aes(x = date, y = yearly_climate$temp, group = origin, color = origin))  
p + geom_line(size = 1) +  
  labs(title = 'Temperature of JFK and LGA in 2013', x = 'Date', y = 'Temperature') + 
  scale_x_date(breaks=as.Date(c("2013-01-01","2013-02-01","2013-03-01","2013-04-01", "2013-05-01","2013-06-01",
                                "2013-07-01","2013-08-01",'2013-09-01', "2013-10-01", "2013-11-01", "2013-12-01")))



#creating data frame with wind_dir

yearly_climate1 = group_by(hourlyclimate, origin, year, month, day) %>% summarise(wind_dir = mean(wind_dir), 
                                                                                  wind_speed = mean(wind_speed), 
                                                                                  wind_gust = mean(wind_gust), ) %>% as.data.frame()
yearly_climate1 %<>% mutate(date = as.Date(paste0(year, '-', month, '-', day)))

p = ggplot(yearly_climate, aes(x = date, y = yearly_climate1$wind_dir, group = origin, color = origin))  
p + geom_smooth(size = 3) +  
  labs(title = 'JFK & LGA daily wind direction - 2013', x = 'Date', y = 'Wind direction') +
  theme_dark()
  theme(axis.title = element_text(size = 9, color = "black"))
  
  
  
  
  
  
  
  
  #Combining two plots from different data.frames (EXTRA FEATURE)
  
  (plot2 <- ggplot(NULL, aes(date, temp), color=origin)+geom_point(data=yearly_climate_JFK, col = "red") + geom_step(data = yearly_climate_LGA))
  
  
  #Extra feature 2
  
  ggplot(JFK1, aes(JFK1$temp, JFK1$humid)) + geom_point(colour = "purple", pch = 3)
  
  
  #geom bar
  a <- ggplot(yearly_climate_JFK, aes(x=date, fill=yearly_climate_JFK$month))
  a + geom_bar()+
    labs(x="Temperature", y="Date")
  
  b <- ggplot(yearly_climate, aes(y=temp, fill=origin))
  b + geom_bar(col=origin)+
    labs(x="", y="Temprerature")
  
  
  
