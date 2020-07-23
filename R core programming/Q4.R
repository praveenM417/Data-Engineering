#import libraries
library(readODS)
library(stringr)
library(dplyr)
library(sqldf)
library(ggplot2)
library(rvest)
library(selectr)
#read input
footfallDF <- read.ods('Footfall2013.ods')
sheetListFootFall<-ods_sheets('Footfall2013.ods')

#Talbot St--- North
#South King St--- South
#Dynamic funcrion to iterate over sheets and return a merged dataframe.
DataStreets <- function(entance) {
  DF_Header<-list()
  southDF <- data.frame()
  length(sheetListFootFall)
  for(a in 1: length(sheetListFootFall)){
    tempDF <- read.ods(file = 'Footfall2013.ods', sheet = sheetListFootFall[a], formulaAsFormula = FALSE)
    tempDF$week<-sheetListFootFall[a]
    cnt <- 0
    threshold<-0
    condition <- FALSE
    length(tempDF$A)
    for(i in 1: length(tempDF$A))
    {
      if(str_detect(tempDF$A[i],entance)|condition==TRUE)
      {condition = TRUE
      if(threshold <= 26)
      {
        cnt <- cnt+1;
       
        if(cnt>3)
        { southDF<-rbind(southDF, tempDF[i,])}
        if(cnt==3)
        { header<-tempDF[i,]}
       
        threshold <- threshold+1
      }
      if(threshold > 26)
        break
      }
    }}
  DF_Header[['DF']]<-southDF
  str(header)
  DF_Header[['header']]<-header
  return(DF_Header)
}
#Manipulating the column names
NorthEnreanceDF_header<-DataStreets('Talbot St')
NorthEnreanceDF<-NorthEnreanceDF_header[['DF']]
header<-NorthEnreanceDF_header[['header']]
SouthEnreanceDF<-DataStreets('South King St')[['DF']]
NorthEnreanceDF['camera']<-'North'
SouthEnreanceDF['camera']<-'South'
North_SouthDF <- merge(NorthEnreanceDF,SouthEnreanceDF,all = TRUE)
levels(NorthEnreanceDF$week)
#Header creation
counter<-0
label<-0
for( i in 2:(length(header)))
{
  counter<-counter+1
  if(counter==1)
    label<-label+1
  header[i]<-paste(label,header[i],sep = '-')
  if(counter==2)
    counter<-0
}
header$camera<-'camera'
header[length(header)-1] <- 'week'

#Reshaping the wider table into narrow table
colnames(North_SouthDF) <- header
copy <-header
header<-copy
time <- North_SouthDF$Time
head(North_SouthDF)

southDFwithout <- North_SouthDF %>% select (-c(1))
str(southDFwithout)

listDF<-list()
length(southDFwithout)
head(southDFwithout)
instance <- 0
for(i in 1:7)
{
  tempoDF <- data.frame(as.vector(southDFwithout[,instance+1]),as.vector(southDFwithout[,instance+2]))
  colnames(tempoDF)<-c('intime','outtime')
  tempoDF$day <- i
  tempoDF$Time <-time
  tempoDF$camera<-as.vector(southDFwithout$camera)
  tempoDF$week<-as.vector(southDFwithout$week)
  str(tempoDF)
  instance <- instance+2
  listDF[[paste(i,'day',sep='')]]<-tempoDF
}
#Reshaping again to bring days to row values 
finalDFFoot <-data.frame()
for(k in 1:length(listDF))
{ 
  interDF <- as.data.frame(listDF[[paste(k,'day',sep='')]])
  str(interDF)
  
  finalDFFoot <-rbind(finalDFFoot,interDF)
  
}
finalDFFoot$day<-as.factor(finalDFFoot$day)
finalDFFoot$week<-as.factor(finalDFFoot$week)
colnames(finalDFFoot)
finalDFFoot$Time<-as.factor(finalDFFoot$Time)
levels(finalDFFoot$week)
finalDFFoot$camera<-as.factor(finalDFFoot$camera)

#DF for average foot fall in and out per day
avgTimeperDay<-sqldf('select camera,week,day,time,avg(inTime) as intime, avg(outtime) as outtime from finalDFFoot group by day,time,camera order by camera,week,day,time')
#DF to bring in/out as rows from columns -- reshaping 3
DF_InOut_Reshape<-sqldf("select camera,week,day,time,'In' as type,intime as countOfPeople from avgTimeperDay union select camera,week,day,time,'Out' as type,outtime as countOfPeople from avgTimeperDay")
levels(DF_InOut_Reshape$week)

#Analysis Dataframes
GlobalCount<-sqldf("select camera,type,avg(countOfPeople) as countOfPeople from DF_InOut_Reshape group by camera,type ")
InPeople <- sqldf("select camera,week,day,time,avg(countOfPeople) as countOfPeople from DF_InOut_Reshape where type = 'In' group by camera,week,day,time ")
OutPeople <- sqldf("select camera,week,day,time,avg(countOfPeople) as countOfPeople from DF_InOut_Reshape where type = 'Out' group by camera,week,day,time")

#comparison of intime for North and south
ggplot(data=avgTimeperDay, aes(x= day, y=intime,fill=camera)) + 
  geom_bar(stat="identity",position="dodge")

#comparison of outtime for North and south
ggplot(avgTimeperDay, aes(fill=camera, y=outtime, x=day)) + 
  geom_bar(position="dodge", stat="identity")

#comparison of people who entered the street, across the hours of a day for camera at north entrance vs south entrance
ggplot(InPeople, aes(fill=camera, y=countOfPeople, x=Time)) + 
  geom_bar(position="dodge", stat="identity")

#comparison of people who left the street, across the hours of a day for camera at north entrance vs south entrance
ggplot(OutPeople, aes(fill=camera, y=countOfPeople, x=Time)) + 
  geom_bar(position="dodge", stat="identity")

#comparison of people who entered the street, across the 52 weeks for camera at north entrance vs south entrance
ggplot(InPeople, aes(fill=camera, y=countOfPeople, x=week)) + 
  geom_bar(position="dodge", stat="identity")

#comparison of people who left the street, across the 52 weeks for camera at north entrance vs south entrance
ggplot(OutPeople, aes(fill=camera, y=countOfPeople, x=week)) + 
  geom_bar(position="dodge", stat="identity")

#comparison of people who left the street/entered the street grouped by north,south cameras
ggplot(GlobalCount, aes(fill=camera, y=countOfPeople, x=type)) + 
  geom_bar(position="dodge", stat="identity")

################Part 2 #############################
#Web scrapping
readHTML<-read_html("https://www.timeanddate.com/weather/ireland/dublin/historic?month=1&year=2013")

#Getting the html element table with the particular id for specification
rows <- readHTML%>%
  html_node('body') %>%
  xml_find_all("//table[contains(@id, 'wt-his')]")

#converting into dataframe
HTMLDF<-as.data.frame(html_table(rows))

#manipulating the column names and selecting wanted columns
colnames(HTMLDF) <- HTMLDF[1,]
HTMLDF <- HTMLDF[-1,]
colnames(HTMLDF)[2]<-'dummy'
colnames(HTMLDF)[6]<-'dummy2'
HTMLDF <- HTMLDF %>%
  select(1,3,5)

#Filtering out the unwanted scrapped data to match the camera footfall dataframe
HTMLDF<-filter(HTMLDF,(str_detect(Time,':30')|str_detect(Time,'Weather'))==FALSE)

#Filtering the camera footfall dataframe so as to make the conditions equal across the scrapped data and column transformations using replace_all
DF_InOut_Reshape$Time<-as.numeric(DF_InOut_Reshape$Time)
DF1NorthFiltered <- filter(DF_InOut_Reshape,camera=='North'&week=='Week_1'&day==1&type=='In'&Time<=10)
DF1NorthFiltered$Temp<-as.numeric(gsub("[^0-9\\.]", "", HTMLDF$Temp))[1:9]
DF1NorthFiltered$Wind<-as.numeric(gsub("[^0-9\\.]", "", HTMLDF$Wind))[1:9]
DF1NorthFiltered$Time<-as.factor(as.numeric(str_sub(DF1NorthFiltered$Time,1,2)))
View(DF1NorthFiltered)
# 3 dimensional bubble chart
ggplot(DF1NorthFiltered, aes(x = Time, y = countOfPeople)) + geom_point(aes(size = Wind,colour = Temp))


