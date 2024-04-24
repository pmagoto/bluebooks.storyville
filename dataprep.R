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

#creating the BSV and WSV coordinates and labels
#black storyville coords: using the city ordinance 4118 to create my data.
#bottom left:29.954152,-90.080601
#top left: 29.955334,-90.079958
#top right:29.954153,-90.077019
#bottom right: 29.952984,-90.077834
#center for both 29.956423,-90.07619
longitudeBSV <- c(-90.080601, -90.079958, -90.077019, -90.077834) #longitude for the polygon
latitudeBSV <- c(29.954152,29.955334,29.954153,29.952984) #latitude for the polygon
lonlatBSV <- as.data.frame(cbind(longitudeBSV,latitudeBSV)) #create dataframe for the polygon
#white storyville coords 
#bottom left:29.959378,-90.076034
#top left: 29.961675,-90.073988
#top right:29.959434,-90.070663
#bottom right:29.957063,-90.072809
#center of WSV: 29.958675,-90.073182
longitudeWSV <- c(-90.076034, -90.073988, -90.070663,-90.072809)
latitudeWSV <- c(29.959378, 29.961675,29.959434,29.957063)
lonlatWSV <- as.data.frame(cbind(longitudeWSV,latitudeWSV))
#start creating the static map for this visualization
centerofBSVandWSV <-  c(lon = -90.07619, lat =29.956423) #find the center for a new statis map
BSVandWSVstatic <-  get_map(location = centerofBSVandWSV, zoom = 16, maptype = "satellite", color = "bw") #create static map
BSVlabeled <- ggmap(BSVandWSVstatic) + #load static map at zoom 16
  geom_polygon(data = lonlatBSV, aes(x=longitudeBSV, y=latitudeBSV),alpha = 0.1, color = "gold", fill = "gold") + # add the BSV polygon on top of static map
  annotate("text", x=-90.078700 ,y=29.954400,label="Black Storyville",colour="white",size=5) + #label the black storyville
  geom_polygon(data = lonlatWSV, aes(x=longitudeWSV, y=latitudeWSV),alpha = 0.1, color = "red", fill = "red") + #Add the SV polygon to the static map
  annotate("text", x=-90.073182,y=29.95950,label="White Storyville",colour="white",size=5) +   #label the SV district
  labs(x = "Longitude", y = "Latitude")