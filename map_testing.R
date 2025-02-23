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

#drop rows with NA lat or lon, filter to only dermaly toxic plants
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
sb_county_plant_obs_ppp <- ppp(toxic_plant_obs_ppp$x, toxic_plant_obs_ppp$y, window = sb_county_owin)

#remove duplicates
sb_county_plant_obs_ppp <- unique(sb_county_plant_obs_ppp)

plot(sb_county_plant_obs_ppp)

#create kernel density map
plant_density <- density(sb_county_plant_obs_ppp, sigma = bw.diggle)
plot(plant_density)

#create a raster
sb_county_plant_obs_raster <- rast(plant_density)
#add crs info to raster
crs(sb_county_plant_obs_raster) <- "+init=epsg:2229"

#create color palette for raster
map_pal <- colorNumeric(palette="YlOrRd", domain=values(sb_county_plant_obs_raster))

leaflet() |>
  addTiles() |>
  addRasterImage(sb_county_plant_obs_raster, colors="YlOrRd", opacity = 0.7) |> #add opacity slider?
  setView(lng = -120.2, lat = 34.5, zoom = 8)  |>
  addLegend(pal = map_pal, values = values(sb_county_plant_obs_raster, na.rm=T))
