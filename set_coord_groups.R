library(readxl)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(geosphere)
#expectation
# 2 geocaches per hour
# ~5hr per session = 10 geocaches / session or 20 over the day
# speculate scouts invest 2 hr, we need 4-6 geocaches. if we get blocks of 6 waypoints they can be cut to 3 (~1hr) or added to 12 (~full morning/afternoon).
#5 gps units 2*2hr periods *2 sessions we need 40 blocks of 6 line waypoints

GPS_Tag_ <- read_excel("waypoints.xlsx", 
                                              skip = 2)

set.seed(05152012) 

#brake search into 3 groups
#g1=c("BT 26","BT 27","BT 28","BT 25","BT 24","BT 17","BT 16","BT 18","BT 03","BT 04")
#g2=c("BT 29","BT 30","BT 15","BT 22","BT 14","BT 10","BT 07","BT 06","BT 05")
#g3=c("BT 13","BT 23","BT 11","BT 09","BT 08","BT 12","BT 19","BT 20","BT 21")

#brake search into 2 groups
g1=c("BT 26","BT 27","BT 28","BT 25","BT 24","BT 17","BT 16","BT 18","BT 03","BT 04", "BT 29","BT 30","BT 15","BT 05")
g2=c("BT 13","BT 23","BT 11","BT 09","BT 08","BT 12","BT 19","BT 20","BT 21","BT 22","BT 14","BT 10","BT 07","BT 06")
g3=c("")

#euclidean distance between any two points in the df
#fix names and remove BT01/BT02 which are missing
gps_dist<-GPS_Tag_ %>% mutate(k = 1) %>% rename("long" = "Degrees...12", "lat" = "Degrees...8") %>% filter((!Point %in% c("BT 01","BT 02"))) %>% mutate(group=case_when(
  Point %in% g1 ~ 1,
  Point %in% g2 ~ 2,
  Point %in% g3 ~ 3
))

#euclidean dis calc and remove any where dist is exactly the same (poor mans paired point removal)
groupslist <-map(1:80, ~ gps_dist %>% group_by(group) %>% slice_sample(n = 1, replace = FALSE) %>%  ungroup() %>%  mutate(dist = distHaversine(cbind(long,lat))) %>% mutate (dist = sum(dist,na.rm=T))) 


groups <-as.data.frame(do.call(rbind, groupslist))

#QC graphs
#barchart of waypoint usage
ggplot(groups,aes(index))+geom_bar()

#histogram of distances
ggplot(output,aes(dist))+geom_histogram()


output_wdist<-groups %>% group_by(dist) %>%  mutate(group_no = cur_group_id()) %>% select(Point,Latitude,Longitude,Northing,Easting,group_no,dist)

output<-groups %>% group_by(dist) %>%  mutate(group_no = cur_group_id()) %>% select(Point,Latitude,Longitude,Northing,Easting,group_no)

write_tsv(output,"out.txt")
write_tsv(output_wdist,"out_dist.txt")
