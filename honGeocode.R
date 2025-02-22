library(tidygeocoder) #Geocoding 
library(sf) #shapefiles for mapping
library(mapview) # Google Maps
library(readxl) #reads Excel Files into R
library(janitor) #Data Cleaning
library(tidyverse) #Data Manipulation 
library(tigris) # US. CENSUS Data
library(dplyr) #Data Maniulation
library(ggspatial) # Mapping Packages
library(leaflet.providers)
library(leaflet)



decatur <- read_excel("C:\\Users\\lanip\\OneDrive\\Documents\\decaturAddressestotal.xlsx")  #has 2000 predictions addresses between Feb-Apr 2020
 
decatur #prints the file in R so you can read it 
  
 decatur_bg <- block_groups(state = "Georgia", county = "Dekalb", year =2021) %>% #taking shapefile from US Census, with the GEOID of the census block Groups in Decatur, Dekalb, Georgia
  filter(GEOID %in% c( 130890228002, 130890228003, 
                       130890225011, 130890225012,
                       130890225013, 130890225021,
                       130890225022, 130890226011,
                       130890226012, 130890226013, 
                       130890226014, 130890226021, 
                       130890227001, 130890227002, 
                       130890227003, 130890228001)) %>%
  st_as_sf(coords = c("long","lat"),crs = 4236) 

  

#uses google API to geocode the predictions and write to csv (you can run the csv so you dont overuse api )
decatur_geocode <- decatur %>%
  geocode(address = "address")
write.csv(decatur_geocode,"decaturGeocode.csv")

decatur_geocode <- read.csv("C:\\Users\\lanip\\OneDrive\\Documents\\decaturGeocode.csv")

decatur_geocode_clean <- decatur_geocode %>% 
  filter(!is.na(long) & !is.na(lat)) #Remove Null Values


# MAPPING: Convert to spatial object and map
clean <- decatur_geocode_clean %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) 


# CSV With levels of segregation by Block Groups in Decatur 
iso_decatur <- read.csv("C:\\Users\\lanip\\OneDrive\\Documents\\RFiles\\DekalbSeg.csv")
iso_decatur$geoid <- as.character(iso_decatur$geoid)

#Joining together the spatial data and the segregation data into one table
iso_df <- decatur_bg %>%
  left_join(iso_decatur, by = c("GEOID" = "geoid"))


# shapefiles of Decatur
city_limits <- read_sf("C:/Users/lanip/Downloads/City_Limits_of_Decatur_9058414913671126946")

#map of segregation levels in Decatur
seg_colored <- ggplot() + 
  geom_sf(data =iso_df, aes (fill = LS)) + 
  scale_fill_viridis_c()+
  ggtitle("Dissimularity rates in Decautur, Georgia (2021)") +
  theme_void() 

#predictions organzized by frequency 
clean_freq <- clean %>% 
  group_by(geometry) %>% 
  summarise(freq = n(), .groups = 'drop')

#Predictions mapped over visual of Decatur,GA 
decatur_pre <- ggplot() + 
  annotation_map_tile() + 
  geom_sf(data = decatur_bg,
          fill = "transparent") +
  geom_sf(data = clean_freq)
  theme_void()
decatur_pre

#Makes The Data interactives (kinda broken RN lol)
leaflet() %>%
  addProviderTiles()

##All preditctions overlaid with map of Decatur
decatur_pre <- ggplot() + 
  geom_sf(data = decatur_bg) +
  geom_sf(data= clean_freq, aes( color = freq)) +
  scale_color_viridis_c() +
  ggtitle("Predictions in Decatur, GA, Feb-Apr 2020") +
  theme_void()
decatur_pre
##NON



ggplot() + 
  annotation_map_tile() + 
  geom_sf(data = decatur_bg,
          fill = "transparent")

seg_colored

