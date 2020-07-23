library(plotrix)
library(readODS)
library(dplyr)
library(sqldf)
library(ggplot2)
library(rvest)
library(selectr)
library(maps)
library(bigrquery)
library(sqldf)
library(reshape)
library(stringr)

###This is the account i used to run big queries, please give yes when it asks for confirmation, if the access is not granted or passsword is asked use the below
#password-p madhuvvv
bq_auth(email = 'reach2praveen001@gmail.com')

states_map <-  map_data("state")
country_map <- map_data("world")
#Visualization 1
#Web scrapping
#converting into dataframe
df_global_temp <- read_html("https://en.wikipedia.org/wiki/Instrumental_temperature_record") %>%
  html_table(header = TRUE, fill = TRUE) %>% 
  .[[2]] %>% #Extract the second table in the list
  as.data.frame()
#Data cleaning and wrangling
colnames(df_global_temp)<-c('year','temp','change')
df_global_temp$temp<-str_trim(str_sub(df_global_temp$temp,1,6))

#Removal of special characters
for(i in 1:length(df_global_temp$temp))
    {
      
     if(str_length(df_global_temp$temp[i]) >5 | str_detect(df_global_temp$temp[i],"0.02")==TRUE)
       df_global_temp$temp[i]= paste('-',str_sub(df_global_temp$temp[i],2,str_length(df_global_temp$temp[i])),sep='')
      else
        df_global_temp$temp[i]=df_global_temp$temp[i]
      
}
df_global_temp$temp<-as.numeric(df_global_temp$temp)
df_global_temp$year<-str_replace(df_global_temp$year,"(incomplete)","")
#line plot 
ggplot(data=df_global_temp, aes(x=year, y=temp, group=1)) +
  geom_line(color="red")+
  geom_point()

####################################################
  
#Visualization 2
#Web scrapping
#converting into dataframe
df_global <- read_html("https://en.wikipedia.org/wiki/List_of_countries_by_greenhouse_gas_emissions") %>%
  html_table(header = TRUE, fill = TRUE) %>% 
  .[[2]] %>% #Extract the second table in the list
  as.data.frame()

#Data cleaning, Making data suitable for plotting world map
#Adjusting column names
#Removing special characters from the country names
#Give NA to the missing columns and late filter them

for(i in 1:length(df_global$Country))
{
  if(is.na(df_global$Country[i]))
  {  df_global$Country[i] = 'NA'}
  else
    {
    a<-as.character(df_global$Country[i])
    print(a)
    
    if(str_detect(a,"\\(")==TRUE)
    {
      
      df_global$Country[i]=  str_trim(str_sub( df_global$Country[i],1,str_locate( df_global$Country[i],"[(]")-1))
      if(df_global$Country[i]=='United States')
      { df_global$Country[i]='USA'}
      
    }
    else
      df_global$Country[i]= df_global$Country[i]}
  
}
#filtering missing values
df_global<-filter(df_global,(is.na(Country)==FALSE & Country!='World'))
# MAnipulating column name
colnames(df_global)[3]<-'GlobalWarming_Contibution'

#Removing special characters again (%)
df_global$GlobalWarming_Contibution<-as.numeric(str_replace_all(df_global$GlobalWarming_Contibution,'\\%',''))
country_map$region<-tolower(country_map$region)
df_global$Country<-tolower(df_global$Country)

#World MAp Plot
ggplot(df_global, aes(map_id = Country)) +
  geom_map(aes(fill = GlobalWarming_Contibution), map = country_map)+
  expand_limits(x = country_map$long, y = country_map$lat)

#####################################################################

#Visualisation 3
#Pie chart plot to show the top 5 countries
top_5<-sqldf("select Country,GlobalWarming_Contibution from df_global order by GlobalWarming_Contibution desc limit 3")
slices <- top_5$GlobalWarming_Contibution
lbls <- as.factor(top_5$Country)
pie3D(slices,labels=lbls,explode=0.1,
      main="Top 5 Countries - global warming ")

##################################################################

#Visualisation 4
# Store the project id
projectid = "airy-torus-255613"

#######CO#######
sql_2 <- "SELECT state_name,time_local,avg(sample_measurement) as sample_measurement FROM
`bigquery-public-data.epa_historical_air_quality.co_hourly_summary`
WHERE date_local > '1990-01-01' and date_local < '2017-12-31' group by state_name,time_local"

# Run the query and store the data in a dataframe
df_hour <- query_exec(sql_2, projectid, use_legacy_sql = FALSE,page_size = 10000, max_pages = Inf)

df_hour$time_local<-as.factor(df_hour$time_local)

# Grouping by hour
df_hour_grouped <- sqldf("select time_local,avg(sample_measurement) as emission from df_hour group by time_local")
df_hour_grouped$time_local<-as.factor(df_hour_grouped$time_local)

#######no2#########

nitrogen_hr<-query_exec("SELECT state_name,time_local,avg(sample_measurement) as sample_measurement
FROM `bigquery-public-data.epa_historical_air_quality.no2_hourly_summary`
WHERE date_local > '1990-01-01' and date_local < '2017-12-31' group by state_name,time_local",projectid, use_legacy_sql = FALSE,page_size = 10000, max_pages = Inf)

# Grouping by hour
n2o_hour_grouped <- sqldf("select time_local,avg(sample_measurement) as n2o from nitrogen_hr group by time_local")

########o3##########

ozone_hr<-query_exec("SELECT state_name,time_local,avg(sample_measurement) as sample_measurement
FROM `bigquery-public-data.epa_historical_air_quality.o3_hourly_summary`
WHERE date_local > '1990-01-01' and date_local < '2017-12-31' group by state_name,time_local",projectid, use_legacy_sql = FALSE,page_size = 10000, max_pages = Inf)

# Grouping by hour
ozone_hour_grouped <- sqldf("select time_local,avg(sample_measurement) as ozone
                            from ozone_hr group by time_local")
#Ozone plot
ggplot(data=ozone_hour_grouped, aes(x=time_local, y=ozone, group=1)) +
  geom_line(color="red")+
  geom_point()

########NO##########

nano_hr<-query_exec("SELECT state_name,time_local,avg(sample_measurement) as sample_measurement
FROM `bigquery-public-data.epa_historical_air_quality.nonoxnoy_hourly_summary`
WHERE date_local > '1990-01-01' and date_local < '2017-12-31' group by state_name,time_local",projectid, use_legacy_sql = FALSE,page_size = 10000, max_pages = Inf)

#NO Group by hour
nano_hour_grouped <- sqldf("select time_local,avg(sample_measurement) as nanoxy
                            from nano_hr group by time_local")

#Nitric Oxide PLot
ggplot(data=nano_hour_grouped, aes(x=time_local, y=nanoxy, group=1)) +
  geom_line(color="red")+
  geom_point()
str(nano_hour_grouped)

#########so2###########

so2_hr<-query_exec("SELECT state_name,time_local,avg(sample_measurement) as sample_measurement
FROM `bigquery-public-data.epa_historical_air_quality.so2_hourly_summary`
WHERE date_local > '1990-01-01' and date_local < '2017-12-31' group by state_name,time_local",projectid, use_legacy_sql = FALSE,page_size = 10000, max_pages = Inf)

#so2 grouped by hour
so2_hour_grouped <- sqldf("select time_local,avg(sample_measurement) as so2
                            from so2_hr group by time_local")


qplot(so2_hour_grouped$time_local,so2_hour_grouped$so2, geom=c("point", "line"), main = "Mean temperature from 1989 to 2015" ,col = "green", xlab = "year", ylab= "Temperature")


#join all gases data

all_gases <- sqldf("select a.time_local as hour,a.emission as CarbonMonoxide,b.n2o as NitrogenDioxide,
d.nanoxy as NitricOxide,e.so2 as sulphurDioxide
                    from df_hour_grouped a left join
                   n2o_hour_grouped b on a.time_local=b.time_local left join ozone_hour_grouped c
                   on a.time_local = c.time_local left join nano_hour_grouped d on d.time_local=a.time_local 
                   left join so2_hour_grouped e on e.time_local = a.time_local")


#Data frame reshapping
reshaped_gases <- melt(all_gases, id=c("hour"))
reshaped_gases$variable<-as.factor(reshaped_gases$variable)
reshaped_gases$hour<-as.factor(str_sub(reshaped_gases$hour,1,2))

#line plot all gases
ggplot(data=reshaped_gases, aes(x=hour, y=value, group=1)) +
  geom_line(color="red")+
  geom_point()+facet_wrap(~variable,scales='free')

######################################################################

#Visualisation 4
# Set your query for CO
sql <- "select lower(state_name) as state_name,avg(observation_count) as emission from
`bigquery-public-data.epa_historical_air_quality.co_daily_summary` group by state_name"

# Run the query and store the data in a dataframe
df_group_state <- query_exec(sql, projectid, use_legacy_sql = FALSE,page_size = 10000, max_pages = Inf)

#calculate total and percent share
total_co <- sum(df_group_state$emission)
df_co<-sqldf(paste("select state_name,(emission/",total_co,")*100 as CO from df_group_state",sep=""))

# Set your query for No2
sql <- "select lower(state_name) as state_name,avg(observation_count) as emission from
`bigquery-public-data.epa_historical_air_quality.no2_daily_summary` group by state_name"

# Run the query and store the data in a dataframe
df_group_state_no2 <- query_exec(sql, projectid, use_legacy_sql = FALSE,page_size = 10000, max_pages = Inf)

#calculate total and percent share
total_no2 <- sum(df_group_state_no2$emission)
df_no2<-sqldf(paste("select state_name,(emission/",total_co,")*100 as n02 from df_group_state_no2",sep=""))

# Set your query for so2
sql <- "select lower(state_name) as state_name,avg(observation_count) as emission from
`bigquery-public-data.epa_historical_air_quality.so2_daily_summary` group by state_name"

# Run the query and store the data in a dataframe
df_group_state_so2 <- query_exec(sql, projectid, use_legacy_sql = FALSE,page_size = 10000, max_pages = Inf)

##calculate total and percent share
total_so2 <- sum(df_group_state_so2$emission)
df_so2<-sqldf(paste("select state_name,(emission/",total_co,")*100 as so2 from df_group_state_so2",sep=""))


# Set your query for NO
sql <- "select lower(state_name) as state_name,avg(observation_count) as emission from
`bigquery-public-data.epa_historical_air_quality.nonoxnoy_daily_summary` group by state_name"

# Run the query and store the data in a dataframe
df_group_state_no <- query_exec(sql, projectid, use_legacy_sql = FALSE,page_size = 10000, max_pages = Inf)

##calculate total and percent share
total_no <- sum(df_group_state_no$emission)
df_no<-sqldf(paste("select state_name,(emission/",total_co,") as no from df_group_state_no",sep=""))

#Joining the above dataframes
state_global<-sqldf("select a.state_name,a.co as co ,b.n02 as no2,c.so2 as so2,d.no as NO from 
                    df_co a join df_no2 b on a.state_name=b.state_name join 
                    df_so2 c on a.state_name=c.state_name join 
                    df_no d on a.state_name=d.state_name")

#Reshaping to narrow table
reshaped_states <- melt(state_global, id=c("state_name"))
View(top_states_gas)
reshaped_states$variable<-as.factor(reshaped_states$variable)

#Computing top emission states
top_states<-sqldf("select state_name,avg(value) as emit from reshaped_states group by state_name order by emit desc limit 5")
top_states_gas<-sqldf("select a.state_name,b.variable,b.value from top_states a left join reshaped_states b 
                      on a.state_name=b.state_name")

#ggplot for Top spot
ggplot(top_states_gas, aes(fill=variable, y=value, x=state_name)) + 
  geom_bar(position="dodge", stat="identity")

#MAp plot for USA States emission
ggplot(df_group_state, aes(map_id = state_name)) +
  geom_map(aes(fill = emission,color=state_name), map = states_map)+
  expand_limits(x = states_map$long, y = states_map$lat)



#############################################################

#Visualisation 5
temp_hr<-query_exec("SELECT state_name,county_name,date_local,time_local,parameter_name,sample_measurement
FROM `bigquery-public-data.epa_historical_air_quality.temperature_hourly_summary`
WHERE (date_local > '2017-01-01' and date_local < '2017-01-31') ",projectid, use_legacy_sql = FALSE,page_size = 10000, max_pages = Inf)

#Temperature grouped by hour
temp_hr_grouped <- sqldf("select time_local,avg(sample_measurement) as temp
                            from temp_hr where date_ group by time_local")

#Individual line plot for temperature across hours
ggplot(data=temp_hr_grouped, aes(x=time_local, y=temp, group=1)) +
  geom_line(color="red")+
  geom_point()

#emsission grouped by state and year
past_years_emission <- query_exec("SELECT state_name,EXTRACT(YEAR FROM  (date_local)) as year ,avg(arithmetic_mean) as o3_emission
FROM `bigquery-public-data.epa_historical_air_quality.no2_daily_summary` WHERE (date_local > '1990-01-01' and date_local < '2017-12-31')  group by state_name,year
 order by state_name,year",projectid, use_legacy_sql = FALSE,page_size = 10000, max_pages = Inf)

#Temperature grouped by state and year
past_years_temp<- query_exec("SELECT state_name,EXTRACT(YEAR FROM  (date_local)) as year ,avg(arithmetic_mean) as tempearature
FROM `bigquery-public-data.epa_historical_air_quality.temperature_daily_summary` WHERE (date_local > '1990-01-01' and date_local < '2017-12-31')  group by state_name,year",projectid, use_legacy_sql = FALSE,page_size = 10000, max_pages = Inf)

#joining above dataframes
past_years_emission_temp <- sqldf("select a.state_name,a.year,a.o3_emission,b.tempearature from
                                  past_years_emission a inner join past_years_temp b on
                                   a.state_name=b.state_name and a.year=b.year")
#Column transformations
past_years_emission_temp$tempearature <- ifelse(is.na(past_years_emission_temp$tempearature)==TRUE,mean(past_years_emission_temp$tempearature,na.rm = TRUE),past_years_emission_temp$tempearature)
past_years_emission_temp$tempearature<-as.numeric(past_years_emission_temp$tempearature)


#individual plotting of temperature and emission across years
past_years_temp_plot<-sqldf("select year,avg(tempearature) as temp from past_years_temp group by year")

ggplot(data=past_years_temp_plot, aes(x=year, y=temp, group=1)) +
  geom_line(color="red")+
  geom_point()

past_years_emission_plot<-sqldf("select year,avg(o3_emission) as emission from past_years_emission group by year")
ggplot(data=past_years_emission_plot, aes(x=year, y=emission, group=1)) +
  geom_line(color="red")+
  geom_point()

#Animation scatter plot
library(gganimate)
theme_set(theme_bw())
library(gapminder)
p <- ggplot(
  past_years_emission_temp,
  aes(x =o3_emission   , y=tempearature,  colour = state_name)
) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
labs(x = "emission", y = "temperature")
p
p + transition_time(year) +
  labs(title = "Year: {frame_time}")+shadow_mark(alpha = 0.3, size = 0.5)


##################################################################################################################