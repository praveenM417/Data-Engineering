#Import libraries
library(readxl)
library(ggplot2)
library(lubridate)
library(dplyr)
library(sqldf)

#dataset link
#https://www.kaggle.com/conorrot/irish-weather-hourly-data

#Importing data into R#
data<-as.data.frame(read_excel("irish_weather_plot.xlsx", sheet = 2))
colnames(data)
str(data)

#Removing the null values in Temperature Column#
TempTable<-data[!(data$temp==""),]

#Creating new Column Year in the table (changed date column into years)#
TemprTable_mod<-mutate(TempTable,year=year(date))

summary(TemprTable_mod$temp)

head(TemprTable_mod)

#Fiding mean value of temperature across years #
year_temp<- sqldf("select year, avg(temp) as year_mean 
                 From TemprTable_mod 
                 Group by year")


#head(year_temp)

#Merging the data frame with the original table#
Temperature_Year<-sqldf("select * from TemprTable_mod as d join year_temp as y 
             on d.year=y.year ")

#View(Temperature_Year)
#Plotting histogram with Temperature#
qplot(data=TemprTable_mod, temp, main = "Temperature Distribution",xlab="Temperature")

#Line chart with Mean Temperature across Years#
qplot(year_temp$year,year_temp$year_mean, geom=c("point", "line"), main = "Mean temperature from 1989 to 2015" ,col = "green", xlab = "year", ylab= "Temperature")

#Scatterplot with Temperature and Humidity#
qplot(data = Temperature_Year,x=temp,y=rhum,xlab = "Temperature",ylab = "Humidity",main = "Temperature vs Humidity",geom = c("point","smooth"))

county_table<-data[!(data$county==""),]
county_temp<-sqldf("select county, avg(temp) as county_mean from county_table Group by County")

head(county_temp)

#Mean Temperature of county
barplot(county_temp$county_mean,
        main = "County vs Mean Temperature",
        xlab = "county",
        ylab = "Temperature",
        names.arg = c("Cork","Kerry"),
        col = "darkred")

#Couny and year wise annual mean temperature
county_year<-sqldf("select county,year,avg(temp) as Mean_Temperature From Temperature_Year Group by county,year order by year desc limit 20")

#Mean Temperature vs county
ggplot(county_year, aes(factor(year),Mean_Temperature, fill = county)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")



