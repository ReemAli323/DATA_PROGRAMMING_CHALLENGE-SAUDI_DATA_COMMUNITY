library(tidyverse)
library(nycflights13)
library(nycflights13)
library(tidyverse)
par(mfrow=c(1,1))

#1# Find the number of flights each month.
d2<-flights %>% group_by(month, origin) %>%
  summarise(FlightCount = n())
d2
plot1=ggplot(d2,aes(x = month,FlightCount)) +
  geom_bar(aes(fill=origin),position = "dodge",stat = "identity")+
  ggtitle("The number of flights each month")+
  scale_x_discrete(limits=seq(1,12,by=1))+
  theme(plot.title = element_text(lineheight=.8,hjust = 0.3,size=8))
plot1

#2#Top 10 flights from NYC to it's destination
c_data <- flights%>%
  left_join(airports, c("origin" = "faa"))%>%
  left_join(airports, c("dest" = "faa"))%>%
  semi_join(airports, c("dest" = "faa"))%>%
  group_by(origin, dest, lon.x, lat.x, lon.y, lat.y)%>%
  count()%>%
  arrange(desc(n))%>%
  head(10)
c_data
plot2=ggplot() +
  borders("state") +
  geom_point() +
  coord_quickmap() +
  geom_curve(data = c_data, aes(x=lon.x, y = lat.x, xend = lon.y, yend = lat.y))+
  ggtitle("Top 10 flights from NYC to it's destination")+
  theme(plot.title = element_text(lineheight=.8,hjust = 0.5,size=8))+
  annotate("segment", x=c_data$lon.x[1], xend=c_data$lon.y[1], y=c_data$lat.x[1], yend=c_data$lat.y[1])+
  annotate("text", x=-118, y=33, label = "LAX", color= "red", size=3)+
  annotate("segment", x=c_data$lon.x[1], xend=c_data$lon.y[1], y=c_data$lat.x[1], yend=c_data$lat.y[1])+
  annotate("text", x=-85, y=33, label = "ATL", color= "red", size=3)+
  annotate("segment", x=c_data$lon.x[1], xend=c_data$lon.y[1], y=c_data$lat.x[1], yend=c_data$lat.y[1])+
  annotate("text", x=-90, y=43, label = "ORD", color= "red", size=3)+
  annotate("segment", x=c_data$lon.x[1], xend=c_data$lon.y[1], y=c_data$lat.x[1], yend=c_data$lat.y[1])+
  annotate("text", x=-123, y=37, label = "SFO", color= "red", size=3)+
  annotate("segment", x=c_data$lon.x[1], xend=c_data$lon.y[1], y=c_data$lat.x[1], yend=c_data$lat.y[1])+
  annotate("text", x=-80, y=35, label = "CLT", color= "red", size=3)+
  annotate("segment", x=c_data$lon.x[1], xend=c_data$lon.y[1], y=c_data$lat.x[1], yend=c_data$lat.y[1])+
  annotate("text", x=-70, y=43, label = "BOS", color= "red", size=3)+
  annotate("segment", x=c_data$lon.x[1], xend=c_data$lon.y[1], y=c_data$lat.x[1], yend=c_data$lat.y[1])+
  annotate("text", x=-80, y=25, label = "MIA", color= "red", size=3)+
  annotate("segment", x=c_data$lon.x[1], xend=c_data$lon.y[1], y=c_data$lat.x[1], yend=c_data$lat.y[1])+
  annotate("text", x=-80, y=29, label = "MCO", color= "red", size=3)
plot2








#3# the average delay by destination on the airports 
(avg_delays_dest<-
    flights %>%
    group_by(dest) %>%
    summarise(delay = mean(arr_delay, na.rm = T)) %>%
    inner_join(airports, by = c(dest = "faa")))
plot3=avg_delays_dest %>%
  ggplot(aes(lon, lat, color = delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap()+
  ggtitle("The average delay by destination on the airports")+
  theme(plot.title = element_text(lineheight=.8,hjust = 0.5,size=8))+
  theme(legend.position='bottom')

plot3


theme(legend.position = c(.95, .95),
      legend.justification = c("left", "top"),
      legend.box.just = "right",
      legend.margin = margin(6, 6, 6, 6))



#4#Create weather table that looks at the avg. weather conditions for each day of the year
plot4=flights %>%
  group_by(year,month, day) %>%
  summarise(hour = sum(dep_delay, na.rm = T)+ sum(arr_delay, na.rm = T)) %>%
  mutate(hour = hour + lag(hour)) %>%
  arrange(desc(hour))%>%
  ggplot(aes(month,hour)) +
  geom_point()+
  scale_x_discrete(limits=seq(1,12,by=1))+
  ggtitle("The worst delays due to weather conditions in each month")+
  theme(plot.title = element_text(lineheight=.5,hjust = 0.5,size = 8))

plot4




##5-Look at the number of cancelled flights per day. 
#Is there a pattern? Is the proportion of cancelled flights related to the average delay? 
#Use a graph to show the relationship.
plot5=flights %>%
  group_by(day) %>%
  summarise(cancelled = mean(is.na(dep_delay)),
            mean_dep = mean(dep_delay, na.rm = TRUE),
            mean_arr = mean(arr_delay, na.rm = TRUE)) %>%
  ggplot(aes(y = cancelled)) +
  geom_point(aes(x = mean_dep), colour = "red") +
  geom_point(aes(x = mean_arr), colour = "blue") +
  labs(x = "Avg delay p day", y = "Cancelled flights p day")+
  ggtitle("The proportion of cancelled flights to the average delay")+
  theme(plot.title = element_text(lineheight=.8,hjust = 0.5,size = 8))

plot5


#6# a relationship between the travel distance and  its speed
# longest flights by distance
arrange(flights, desc(distance))
# shortest flights by distance
arrange(flights, distance)

speed<-flights$distance/(flights$air_time/60)
f.des<-cbind(flights, speed)
plot6=ggplot(f.des,aes(distance, speed)) + 
  geom_point() +
  geom_smooth(se=FALSE)+
  ggtitle("A relationship between the travel distance and  its speed")+
  theme(plot.title = element_text(lineheight=.8,hjust = 0.5,size=8))

plot6



?flights

install.packages("gridExtra")
library(gridExtra)
grid.arrange(plot1, plot2,plot6,plot4, nrow=2, ncol=2,top = "Data analysis for NYC flights in 2013")
grid.arrange(plot5, nrow=1, ncol=1,top="Data analysis for NYC flights in 2013")

flights$carrier
flights$dest



select(flights,carrier, dest,tailnum,origin)
airports$faa
