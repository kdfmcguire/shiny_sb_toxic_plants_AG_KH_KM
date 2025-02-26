# Map and point pattern testing

library(here)
library(tidyverse)
library(sf)
library(sp)
library(spatstat)
library(terra)
library(leaflet)

plant_obs <- read_csv(here("data","sb_obs_w_characteristics_toxins.csv"))
ca_counties_sf <- read_sf(here("data","ca_counties_shapefile", "CA_counties.shp"))

######################################################

#####  TOXIC PLANTS  #####

#drop rows with NA lat or lon, filter to only dermally toxic plants
toxic_plant_obs <- plant_obs |>
  drop_na(Latitude) |>
  drop_na(Longitude) |>
  filter(!is.na(`Toxic parts`))
  
#make dataframe into sf, assign WGS84 CRS (based on info from the source, CalFlora)          
toxic_plant_obs_sf <- st_as_sf(toxic_plant_obs, coords = c("Longitude","Latitude"), crs = "WGS84")
st_crs(toxic_plant_obs_sf) #check CRS

#transform data to projected coordinate system, NAD83 California state plane zone 5
toxic_plant_obs_sf <- toxic_plant_obs_sf |>
  st_transform(crs = 2229)
st_crs(toxic_plant_obs_sf) #check CRS

#retrieve sb county outline
sb_county_sf <- ca_counties_sf |>
  janitor::clean_names() |>
  filter(name=="Santa Barbara") |>
  st_transform(crs = 2229)


#create spatial point pattern of plant observation
toxic_plant_obs_ppp <- as.ppp(toxic_plant_obs_sf)
#create spatial observation window  of sb county
sb_county_owin <- as.owin(sb_county_sf)
#make full point pattern object
sb_county_toxic_plant_obs_ppp <- ppp(toxic_plant_obs_ppp$x, toxic_plant_obs_ppp$y, window = sb_county_owin)

#remove duplicates
sb_county_toxic_plant_obs_ppp <- unique(sb_county_toxic_plant_obs_ppp)

#create kernel density map
toxic_plant_density <- density(sb_county_toxic_plant_obs_ppp, sigma = bw.diggle)
plot(toxic_plant_density)

#create a raster
sb_county_toxic_plant_obs_raster <- rast(toxic_plant_density)
#add crs info to raster
crs(sb_county_toxic_plant_obs_raster) <- "+init=epsg:2229"


######################################################

#####  NONTOXIC PLANTS  #####

#drop rows with NA lat or lon, filter to only NON dermally toxic plants
nontoxic_plant_obs <- plant_obs |>
  drop_na(Latitude) |>
  drop_na(Longitude) |>
  filter(is.na(`Toxic parts`))

#make dataframe into sf, assign WGS84 CRS (based on info from the source, CalFlora)          
nontoxic_plant_obs_sf <- st_as_sf(nontoxic_plant_obs, coords = c("Longitude","Latitude"), crs = "WGS84")
st_crs(nontoxic_plant_obs_sf) #check CRS

#transform data to projected coordinate system, NAD83 California state plane zone 5
nontoxic_plant_obs_sf <- nontoxic_plant_obs_sf |>
  st_transform(crs = 2229)
st_crs(nontoxic_plant_obs_sf) #check CRS
#create spatial point pattern of nontoxic plant observation
nontoxic_plant_obs_ppp <- as.ppp(nontoxic_plant_obs_sf)
#make full point pattern object
sb_county_nontoxic_plant_obs_ppp <- ppp(nontoxic_plant_obs_ppp$x, nontoxic_plant_obs_ppp$y, window = sb_county_owin)

#remove duplicates
sb_county_nontoxic_plant_obs_ppp <- unique(sb_county_nontoxic_plant_obs_ppp)

#create kernel density map
nontoxic_plant_density <- density(sb_county_nontoxic_plant_obs_ppp, sigma = bw.diggle)
plot(nontoxic_plant_density)

#create a raster
sb_county_nontoxic_plant_obs_raster <- rast(nontoxic_plant_density)
#add crs info to raster
crs(sb_county_nontoxic_plant_obs_raster) <- "+init=epsg:2229"

######################################################

#####  INTENSITY RATIO  #####

# Process from Spatial Statistics for Data Science: Theory and Practice with R,
#Chapter 21: Intensity estimation
# https://www.paulamoraga.com/book-spatial/intensity-estimation.html#intensity-ratio

#find common bandwidth- mean of default bandwidths obtained when using density()
#to estimate the intensity of toxic and nontoxic plants separately.
bw_toxic <- attr(density(sb_county_toxic_plant_obs_ppp), "sigma")
bw_nontoxic <- attr(density(sb_county_nontoxic_plant_obs_ppp), "sigma")
bw <- (bw_toxic + bw_nontoxic)/2

#use selected bandwidth to compute the smoothed intensity estimates
int_toxic<- density(sb_county_toxic_plant_obs_ppp, sigma = bw)
int_nontoxic <- density(sb_county_nontoxic_plant_obs_ppp, sigma = bw)

#estimate α as the ratio of number of toxic observations to the number of nontoxic observations
#to account for there being far mor nontoxic plant observations
alpha <- sb_county_toxic_plant_obs_ppp$n/sb_county_nontoxic_plant_obs_ppp$n

#create intensity ratio raster
int_ratio <- int_toxic$v/(alpha * int_nontoxic$v)
int_ratio_raster <- rast(int_ratio)

#transpose the image values returned by density(), since they are stored in transposed form
#save as raster and plot
int_ratio_raster_flip <- flip(int_ratio_raster, direction="vertical")
plot(int_ratio_raster_flip)

#set raster's spatial extent to be the same as the individual kernel density rasters
#and add crs info to raster so it can be mapped
ext(int_ratio_raster_flip) <- ext(sb_county_sf)
crs(int_ratio_raster_flip) <- "+init=epsg:2229"

######################################################

#####   MAP   #####

#create color palette for raster
map_pal <- colorNumeric(palette="YlOrRd", domain=values(int_ratio_raster_flip))

leaflet() |>
  addTiles() |>
  addRasterImage(int_ratio_raster_flip, colors="YlOrRd", opacity = 0.7) |> #add opacity slider?
  setView(lng = -120.2, lat = 34.5, zoom = 8) 
