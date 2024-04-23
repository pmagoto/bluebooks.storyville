#data prep
people.1900 <- read.csv("people.1900.csv") %>% #read csv locally
  mutate(full.address = paste0(street.address, ",", " ", city, ",", " ", state)) %>% #create a full address column to paste in the full address for geocoding
  mutate_geocode(full.address, output = "latlon", source = "google") #geocode the full address
people.1915 <- read.csv("people.1915.csv") %>% #read csv locally
  mutate(full.address = paste0(street.address, ",", " ", city, ",", " ", state)) %>% #create a full address column to paste in the full address for geocoding
  mutate_geocode(full.address, output = "latlon", source = "google") #geocode the full address
addresses.1900 <- read.csv("addresses.1900.csv") %>% #read csv locally
  mutate(full.address = paste0(street.address, ",", " ", city, ",", " ", state)) %>% #create a full address column to paste in the full address for geocoding
  mutate_geocode(full.address, output = "latlon", source = "google") #geocode the full address
addresses.1915 <- read.csv("addresses.1915.csv") %>% #read csv locally
  mutate(full.address = paste0(street.address, ",", " ", city, ",", " ", state)) %>% #create a full address column to paste in the full address for geocoding
  mutate_geocode(full.address, output = "latlon", source = "google") #geocode the full address
#write csv's with geocoded data back onto local file
write.csv(people.1900, file= "people.1900.geocoded.csv")
write.csv(people.1915, file= "people.1915.geocoded.csv")
write.csv(addresses.1900, file= "addresses.1900.geocoded.csv")
write.csv(addresses.1915, file= "addresses.1915.geocoded.csv")

#creating static maps
#mapstatic
#Center Coordinates <- 29.960301,-90.074093
centerofSV <- c(lon = -90.073891, lat = 29.95905) #creating the static map
staticSV <- get_map(location = centerofSV, zoom = 17, maptype = "satellite", color = "bw")
#lonlat of all corners of Stroyville Vice District
#Bottom left <- Corner of Canal St. and N. Robertson St. <- 29.958558,-90.076779
#Top left <- Corner of N. Robertson and St. Louis St. <- 29.961689,-90.073984
#Top Right <- Corner of St. Louis Street and N. basin St. <- 29.959363,-90.070555
#Bottom Right <- Corner of N. basin St and Canal St. <- 29.956301,-90.073466
longitudeSV <- c(-90.076779,-90.073984, -90.070555, -90.073466) #longitude of my polygon from above
latitudeSV <- c(29.958558, 29.961689, 29.959363,  29.956301) #latitude of my polygon from above
lonlatSV <- as.data.frame(cbind(longitudeSV,latitudeSV)) #creating a dataframe for my polygon
SVwithlabels <- ggmap(staticSV) + #load static map at zoom 17
  geom_polygon(data = lonlatSV, aes(x=longitudeSV, y=latitudeSV),alpha = 0.1, color = "red", fill = "red") + # add the polygon on top of static map
  annotate("text", x=-90.073891,y=29.95905,label="Storyville Vice District", color="white", size=5) +   #label the district 
  annotate("text", x=-90.072649,y=29.960785,label="St. Louis Street", color="white", size=3, angle = -39) + 
  annotate("text", x=-90.075394,y=29.960117,label="North Robertson Street", color="white", size=3, angle = 52) +
  annotate("text", x=-90.075277,y=29.95746,label="Canal Street", color="white", size=3, angle = -39) +
  annotate("text", x=-90.071914,y=29.957815,label="North Basin Street", color="white", size=3, angle = 52) + #annotate the street names and adjust their angle
  labs(x = "Longitude", y = "Latitude") #add X/Y axis labels
#create base leaflet map
SVleafletwithlabels <- leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(lng = lonlatSV[,1], lat = lonlatSV[,2], color = "red", weight = 2, fillColor = "red", fillOpacity = 0.2)