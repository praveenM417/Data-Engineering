#library import
library(lubridate)
library(stringr)
library(dplyr)
library(ggplot2)
library(sqldf)

#Read input
historyDF <- readLines('history_database')
historyDF <- as.data.frame(historyDF) 
colnames(historyDF)<-'whole_val'

#column seperations (date column, code column)
historyDF$date_ts<-as.numeric(str_sub(historyDF$whole_val,1,10))
historyDF$code<-str_trim(str_sub(historyDF$whole_val,15))
historyDF <- historyDF %>% select (-c(1))
historyDF$code <- as.character(historyDF$code )

#Epoch value converter
historyDF$date_ts <- as_datetime(as.POSIXct(historyDF$date_ts, origin="1970-01-01"),tz='GMT')
#Mutate new columns like day, year, minth using date
historyDF_added<-mutate(historyDF,year=year(date_ts),month=month(date_ts),hour=hour(date_ts),day = weekdays(date_ts))
#filter from the day that this assignment started
historyDF_added<-filter(historyDF_added,month>=11)
#mutate new column 'commented' using column 'code'
historyDF_added<-mutate(historyDF_added,commented=ifelse(str_detect(code,'#')==TRUE,TRUE,FALSE))
#mutate new column 'library'
historyDF_added<-mutate(historyDF_added,library=str_replace(ifelse(str_detect(code,'library')==TRUE|str_detect(code,'package')==TRUE,TRUE,FALSE),'library',''))

#factor conversion
historyDF_added$year<-as.factor( historyDF_added$year)
historyDF_added$month<-as.factor( historyDF_added$month)
historyDF_added$hour<-as.factor( historyDF_added$hour)
historyDF_added$day<-as.factor( historyDF_added$day)
historyDF_added$library<-as.factor( historyDF_added$library)
hourLine<-sqldf("select hour,count(*) as total_count from historyDF_added group by hour")

#Bar chart with Mean Temperature across hours#
ggplot(hourLine, aes(factor(hour),total_count)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")

hourcommentLine<-sqldf("select hour,commented,count(*) as total_count from historyDF_added group by hour,commented")

#plot on the column category 'commented' with hour
qplot(data = historyDF_added,x=hour,fill=commented,main='frequency of hour-commented/uncommented')

#plot on the column category 'day' with frequency
qplot(data = historyDF_added,day,main='Frequency of Days')

#plot on the column category 'library' with frequency
qplot(data = historyDF_added,library,main='library codes Presence')