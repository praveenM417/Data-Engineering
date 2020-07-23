#import libraries
library(stringr)
library(sqldf)
library(dplyr)
library(lubridate)
library(reshape)

#Unzip and store file names in a vector
unzip('DublinBusGTFS.zip',exdir='dublinBus')
busFiles<-list.files('dublinBus')

#storing all the files as dataframes inside a list
listBusDF <- list()
#refV<-c('routes','trips','agency','shapes','stops_times','stops','transfers','calender','calender_dates')
for(i in 1:length(busFiles))
{
  listBusDF[[str_replace(busFiles[i],'.txt','')]] <- read.csv(paste('/dublinBus/',busFiles[i],sep=''))
  
}

#Read all the inputs and store as dataframes
routes<-read.csv('/dublinBus/routes.txt')
trips<-read.csv('/dublinBus/trips.txt')
shapes<-read.csv('/dublinBus/shapes.txt')
stops<-read.csv('/dublinBus/stops.txt')
stop_times<-read.csv('/dublinBus/stop_times.txt')
calender<-read.csv('/dublinBus/calendar.txt')
calender_dates<-read.csv('/dublinBus/calendar_dates.txt')
colnames(stops)[1]<-'stop_id'
colnames(stop_times)[1]<-'trip_id'

#Adjusting column names
colnames(shapes)<-c("shape_id","shape_pt_lat","shape_pt_lon","shape_pt_sequence","shape_dist_traveled")
colnames(routes)<-c('route_id','agency_id','route_short_name','route_long_name','route_type')
colnames(trips)<-c('route_id','service_id','trip_id','shape_id','trip_headsign','direction_id','block_id')

#combining route trips
busExpo<-sqldf("select routes.route_id as route_id,routes.route_long_name as route_long_name ,routes.route_type as route_type,
trips.trip_id as trip_id,trips.shape_id as shape_id,trips.direction_id as direction_id
,trips.service_id as service_id from 
routes left join trips on routes.route_id = trips.route_id ")


#Exploring top 10 frequently used routes
#Trip - Routes
route_trip<-sqldf("select route_id,count(trip_id) as cnt from busExpo
                  group by route_id order by cnt desc limit 10")
route_trip_last<-sqldf("select route_id,count(trip_id) as cnt from busExpo
                  group by route_id order by cnt limit 10")
distinctRoutes<-sqldf("select count(distinct route_id) from busExpo")

distinctRoutes#Distinct number of routes
#Top 10 and last 10 frequently used routes
head(route_trip_last)
head(route_trip)
#plot for top 10 frequently used routes
ggplot(route_trip, aes( y=cnt, x=route_id)) + 
  geom_bar(stat="identity")

#Exploring route and the total distance of the route
#shape - Routes
route_trip_shape<-sqldf("select busExpo.route_id,busExpo.trip_id,shapes.shape_pt_sequence,shapes.shape_dist_traveled from busExpo left join shapes
                   on busExpo.shape_id = shapes.shape_id")

route_maxDist <- sqldf("select route_id,max(shape_dist_traveled) as dist from route_trip_shape group by route_id order by dist desc limit 10")
head(route_maxDist)
route_minDist <- sqldf("select route_id,min(shape_dist_traveled) as dist from route_trip_shape where shape_dist_traveled !=0 group by route_id order by dist limit 10")
head(route_minDist)
#plots for Max/Min Distance in top 10 routes
ggplot(route_minDist, aes( y=dist, x=route_id)) + 
  geom_bar(stat="identity")
ggplot(route_maxDist, aes( y=dist, x=route_id)) + 
  geom_bar(stat="identity")

#Exploring distance variable having route_id as the category
route_trip_shape_boxplot<-sqldf("select busExpo.route_id,shapes.shape_dist_traveled from busExpo left join shapes
                   on busExpo.shape_id = shapes.shape_id")

route_trip_shape_boxplot_sorted <- sqldf("select route_trip_shape_boxplot.route_id,
                                    route_trip_shape_boxplot.shape_dist_traveled 
                                    from route_trip_shape_boxplot inner join 
                                         route_maxDist on route_maxDist.route_id = 
                                         route_trip_shape_boxplot.route_id")
distinctRoute <- sqldf("select distinct route_id from route_trip_shape_boxplot_sorted")

route_trip_shape_boxplot_sorted$route_id<-as.factor(route_trip_shape_boxplot_sorted$route_id)
levels(as.factor(route_trip_shape_boxplot_sorted$route_id))
#Boxplot distribution
qplot(data = route_trip_shape_boxplot_sorted,reorder(route_id , shape_dist_traveled) ,shape_dist_traveled,geom='boxplot')


#####part 2 #####

#My route - 0-14C-b12-1  - 7 trips
myRoute<-sqldf("select route_id,count( distinct service_id) as serviceCount
      ,count(*) as tripcount from trips group by route_id having serviceCount == 3 
               and tripcount <10 ")
myRoute
#Manipulating column names to avoid special characerts
colnames(listBusDF[['trips']])[1]<-'route_id'
colnames(listBusDF[['routes']])[1]<-'route_id'
colnames(listBusDF[['stop_times']])[1]<-'trip_id'
#View(listBusDF[['trips']])
route_details<-filter(listBusDF[['trips']],route_id=='0-14C-b12-1')%>%
  left_join(.,listBusDF[['routes']] , by='route_id')

#Route_id with stops
trip_stop<-sqldf("select r.trip_id,s.stop_id,s.arrival_time,s.departure_time,r.direction_id,
                  ss.stop_name,s.stop_sequence from 
                  route_details r inner join stop_times s
                  on r.trip_id=s.trip_id inner join stops ss 
                  on ss.stop_id = s.stop_id
                  order by r.trip_id,s.stop_sequence
                 
                 ")
#All stop sequences for thar route
trip_stop_seq<-sqldf("select r.trip_id,min(stop_sequence) as firstStop,
                  max(stop_sequence) as laststop,min(departure_time) as firsttime,max(departure_time) as lasttime from 
                  trip_stop r 
                  group by r.trip_id
                 
                 ")
#Route_id - First stop name - Last stop name
Trip_first_last<-sqldf("select x.trip_id,x.firststopname,y.laststopname,x.firsttime,y.lasttime from (select 
                          a.trip_id,b.stop_name as firststopname,b.departure_time as firsttime from 
                                                      trip_stop_seq a inner join trip_stop b on 
                                                      a.trip_id = b.trip_id and a.firstStop = b.stop_sequence) x
                                                      inner join
                                                      (select a.trip_id,b.stop_name as laststopname,b.departure_time as lasttime
                                                      from 
                                                        trip_stop_seq a inner join trip_stop b on 
                                                        a.trip_id = b.trip_id and a.laststop = b.stop_sequence) y
                                                      on x.trip_id = y.trip_id")

#reshaping calender and get the service day
colnames(calender)[1]<-'service_id'
mdata <- melt(calender, id=c("service_id","start_date","end_date"))
dayService<-filter(mdata,service_id==3 & value ==1)
Trip_first_last1<-sqldf("select a.*,b.variable from Trip_first_last a join mdata b on b.service_id=3 and b.value=1")
