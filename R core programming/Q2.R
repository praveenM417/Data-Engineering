#import libraries
library(jsonlite)
library(sqldf)
library(lubridate)
library(stringr)
library(ggplot2)
stackBase <-data.frame()
#fetching total of 500 rows from the api as there is a restriction of fetching only 100 for a page at a time
for(i in 1:2)
{
  url<-paste("https://api.stackexchange.com/2.2/answers?key=U4DMV*8nvpm3EOpvf69Rxw((&site=stackoverflow&page=",i,"&pagesize=100&order=desc&sort=activity&filter=default",sep="")
  url<-str_replace_all(url," ","")
  jsonData <- fromJSON(url, flatten = TRUE)
  if(str_detect(paste(colnames(jsonData$items),collapse=" "),"community_owned_date")==TRUE)
  {
    jsonData$items<-select(jsonData$items,-(community_owned_date))}
  stackBase <- rbind(stackBase,jsonData$items)
  Sys.sleep(10)# to avoid throttle issue
}
#Manipulating column names
colnames(jsonData$items)
colnames(stackBase)

#Transforming Date columns - epoch conversion
stackBase$creation_date <- as_datetime(as.POSIXct(stackBase$creation_date, origin="1970-01-01"),tz='GMT')

stackBase$last_activity_date <- ymd_hms(as.POSIXct(stackBase$last_activity_date, origin="1970-01-01"),tz='GMT')

#Summarisation of the dataset variables######## Overview to an unknown person #############
MaxDate <- sqldf("select min(creation_date) as firstDate,max(creation_date) as lastDate from stackBase")

MaxDate$firstDate <- as_datetime(as.POSIXct(MaxDate$firstDate, origin="1970-01-01"),tz='GMT')
MaxDate$firstDate
MaxDate$lastDate <- as_datetime(as.POSIXct(MaxDate$lastDate, origin="1970-01-01"),tz='GMT')
MaxDate$lastDate
#firstDate            lastDate
#2009-02-01 20:18:57  2019-12-01 16:18:51
#column names manipulation
for( i in 1:length(stackBase))
{
  colnames(stackBase)[i] <- str_replace((colnames(stackBase)[i]),"\\.","_")
}
number_users_answered <- sqldf("select count(distinct owner_user_id) as owner_strength from stackBase")
number_users_answered
number_answers <- sqldf("select count(distinct answer_id) as answers_strength from stackBase") 
number_answers
number_questions <-sqldf("select count(distinct question_id) as questions_strength from stackBase")
number_questions
Question_answers <- sqldf("select question_id,count(answer_id) as more_than_single_answers
                          from stackBase group by question_id having more_than_single_answers >1
                          ")
Question_answers

stackBase$is_accepted<-as.factor(stackBase$is_accepted)

levels(stackBase$is_accepted)

count_accepted_notAccepted <- sqldf("select is_accepted,count(*) from stackBase group by is_accepted")

#is_accepted count(*)
#1       FALSE       83
#2        TRUE       17

summary(stackBase$score)

stackBase$owner_user_type<-as.factor(stackBase$owner_user_type)

levels(stackBase$owner_user_type)

summary(stackBase$owner_accept_rate)

count_user_type <- sqldf("select owner_user_type,count(*) from stackBase group by owner_user_type")

user_answered_many_ques<- sqldf("select owner_user_id,count(answer_id) as more_than_single_answers
                          from stackBase group by owner_user_id having more_than_single_answers >1
                          ")
length(user_answered_many_ques[,1])
#People who were fully satisfied
FullySatisfiedQuestioners<-sqldf("select count(answer_id) from stackBase where owner_accept_rate == 100")

#Adding Boolean derived columns - Score_factor, Satisfaction
stackBase$satisfaction<-as.factor(ifelse(stackBase$owner_accept_rate>60,'Good Satisfaction',
                                         ifelse(stackBase$owner_accept_rate>20,'poor Satisfaction',
                                                ifelse(stackBase$owner_accept_rate==NA,'unknown','very poor satisfaction')))
) 
table(stackBase$satisfaction)
stackBase$score_factor<-ifelse(stackBase$score>0,'Best Question',
                               ifelse(stackBase$score<0,'bad question',
                                      ifelse(stackBase$score==0,'Average question','very poor satisfaction')))
#Scenario problem
qplot(data = stackBase, x = is_accepted , fill = owner_user_type)+facet_grid(~score_factor)


