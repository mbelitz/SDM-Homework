library(rgbif)
library(dplyr)
library(rnaturalearth)
library(raster)
library(sf)
library(ggplot2)

# You are asked by a conservation agency to help refine the potential distribution of Chaetodipus californicus, 
# the California pocket mouse. This is mouse found in California, Baja and Northern Mexico. 
# This species is nocturnal, and solitary, aggressive and can go into torpor. 
# It is a granivore and can be found at a range of elevations (>2000m). 
# You can see the point occurrences available on GBIF, and you can search for this species and 
# see a range map on Map of Life or IUCN website. You can even download a range map if you desire. 
# Your goal is to clean all the point presences and develop the environmental layers 
# (including defining the accessible areas) needed for an ecological niche model.
# 
# 1) Download the point records. We'd suggest doing that on GBIF

mouse_occ <- occ_search(scientificName = "Chaetodipus californicus",
          fields = c("gbifID", "basisOfRecord", "catalogNumber", "collectionCode", 
                     "collectionID", "recordedBy", "continent", "countryCode", 
                     "stateProvince", "county", "locality", "year", "month", "day", 
                     "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters", 
                     "establishmentMeans", "scientificName", "class", "order", 
                     "family", "genus", "specificEpithet", "infraspecificEpithet", 
                     "taxonomicStatus"), limit = 500000)

mouse_occ_df <- mouse_occ$data

# 2) Clean the records using tools in R. Your approach can be your own but show your code.

# remove occurrence points with NA values

mouse_occ_df_clean <- mouse_occ_df %>% 
  filter(!is.na(decimalLatitude)) %>% 
  filter(!is.na(decimalLongitude))

# Visualize where the points fall on a map
na <- rnaturalearth::ne_countries(continent = "North America", returnclass = "sf")
plot(mouse_occ_df_clean$decimalLongitude, mouse_occ_df_clean$decimalLatitude)
plot(na, add = TRUE)

# read in range map
rm <- shapefile("redlist_species_data_a4f9b948-1974-4534-bb9f-936c1b3cdede/data_0.shp")

# make everything a simple feature

rm_sf <- st_as_sf(rm)

ggplot() + 
  geom_sf(na, mapping = aes()) +
  geom_point(mouse_occ_df_clean, mapping = aes(x = decimalLongitude, y = decimalLatitude)) +
  geom_sf(rm_sf, mapping = aes(), color = "blue", alpha = 0.2) 

# I'm going to clip occurrence points outside of 1.5 latitudinal degrees of the range map

rm_sf_buffer <- st_buffer(rm_sf, dist = 1.5)
occ_sf <- st_as_sf(x = mouse_occ_df_clean, coords = c("decimalLongitude", "decimalLatitude"))
st_crs(occ_sf) <- "+proj=longlat +datum=WGS84 +no_defs"

ggplot() + 
  geom_sf(na, mapping = aes()) +
  geom_sf(occ_sf, mapping = aes()) +
  geom_sf(rm_sf_buffer, mapping = aes(), color = "blue", alpha = 0.2) 

# keep points contained by range map buffer

occ_df_intersect <- st_intersection(occ_sf, rm_sf_buffer)

ggplot() + 
  geom_sf(na, mapping = aes()) +
  geom_sf(occ_df_intersect, mapping = aes()) +
  geom_sf(rm_sf_buffer, mapping = aes(), color = "blue", alpha = 0.2) 

# remove points in the ocean

occ_df_intersect <- st_intersection(occ_df_intersect, na)

ggplot() + 
  geom_sf(na, mapping = aes()) +
  geom_sf(occ_df_intersect, mapping = aes()) +
  geom_sf(rm_sf_buffer, mapping = aes(), color = "blue", alpha = 0.2) 

# 3) Decide on the accessible area of the species, using whatever criteria you think make sense based on the species biology. 
# Write your explanation for accessible area.

# first I'm going to get Lat and Long coordinates out of the sf geometry
occ_df_intersect <- occ_df_intersect %>% 
  mutate(decimalLongitude = st_coordinates(occ_df_intersect )[,1]) %>% 
  mutate(decimalLatitude = st_coordinates(occ_df_intersect )[,2])

# create convex hull
occ_convex_hull <- chull(occ_df_intersect$decimalLongitude, occ_df_intersect$decimalLatitude) 

## generate the end points of polygon. 
CoordsPoly = occ_df_intersect[c(occ_convex_hull, occ_convex_hull[1]), ] %>% 
  as.data.frame() %>% 
  dplyr::select(decimalLongitude, decimalLatitude)# closed polygon
## convert this polygon coordinate matix to SpatialPolygon, so that buffering cound be done. 
SpPoly = SpatialPolygons(list(Polygons(list(Polygon(CoordsPoly)), ID=1)))
## let's add a buffer to the convex hull. Because the mouse appears to have limited
## dispersal ability, I'm going to keep the accessible area relatively small ~10 km 
Buff_SpPoly = buffer(SpPoly, width = 0.1, dissolve = F)

## See whether the convex hull is generated correctly or not
Buff_SpPoly_sf <- st_as_sf(Buff_SpPoly)
st_crs(Buff_SpPoly_sf) <- "+proj=longlat +datum=WGS84 +no_defs"

## See whether the convex hull is generated correctly or not
ggplot() + 
  geom_sf(na, mapping = aes()) +
  geom_sf(occ_df_intersect, mapping = aes()) +
  geom_sf(Buff_SpPoly_sf, mapping = aes(), color = "blue", alpha = 0.2) +
  coord_sf(xlim = c(-125, -110), ylim = c(29, 40))

# crop buffer to terrestrial land
buff_crop <- st_intersection(x = Buff_SpPoly_sf, y = na)

ggplot() + 
  geom_sf(na, mapping = aes()) +
  geom_sf(occ_df_intersect, mapping = aes()) +
  geom_sf(buff_crop, mapping = aes(), color = "blue", alpha = 0.2) +
  coord_sf(xlim = c(-125, -110), ylim = c(29, 40))


# 4) Download relevant layers for your model - We'd suggest a bioclimatic model for this case so likely worldclim layers

# Downloaded Worldclim layers

# 5) Explain your choice for layers in your model.

# # I plan to use Bioclimatic variables that are relevant to Chaetodipus californicus. C. californicus seems to 
# be sensitive to both elevation and to be strongly associated with chapparral. Chaparral is found in regions with hot, 
# dry summers, and mild, wet winters. I want the bioclim layers in my model to reflect this plant community. Additionally,
# I may be including layers that as of now are correlated, but I would remove correlated variables using model selection techniques
# before running my final ENM. 

#' I intend to include the followign 8 variables:
#' 
#' 1 & 2) BIO1 and BIO12 - The annual mean temperature and Annual precipitation, as these variables may broadly explain the niche of 
#' C. californicus.
#' 
#' 3 & 4) BIO4 and BIO 15 - Temperature Seasonality & Precipitation Seasonality - These variables may be useful in describing the 
#' variance in temperature and seasonality that are characteristic of the chaparral ecosystem. 
#' 
#' 5 & 6) BIO5 and BIO6 - Max Temperature of Warmest Month and Min Temperature of Coldest month - C. californicus can initiate torpor
#' if necessary, but avoids high elevation (>2400m), so Min temperature of Coldest month could help determine the species niche, 
#' while max temperature of warmest months could be found in true dessert areas outside of the chaparral ecosystem.
#' 
#' 7 & 8) BIO16 and BIO17, Precipitation of wettest quarter and precipitation of driest quarter. Using quarter would get at the 
#' seasonality we would expect with wet winters and dry summers. Non-chapparal ecosystems could experience different precipitation
#' patterns. 
#' 
#' Again, I would check for correlated variables before choosing what final variables to include in the model. 

# 6) Clip your layers to the accessible area you choose in #3.

# I'm going to clip all layers

# read in list of files
filelist <- list.files("Layers/", full.names = TRUE)

for(i in 1:length(filelist)){
  # read in raster
  r <- raster(filelist[i])
  
  # change buffer from sf object to spatial
  buff_crop_sp <- as_Spatial(buff_crop)
  
  #crop file to extent of the buffer
  r_crop <- crop(r, buff_crop_sp)
  
  # mask the cropped file 
  r_mask <- mask(r_crop, mask = buff_crop_sp)
  
  # save files
  r_names <- names(r)
  FileName = paste("Clipped_Layers", "/", r_names, ".asc", sep = "")
  
  writeRaster(r_mask, FileName, "GTiff")
  
  #plot and print to check loop
  plot(r_mask)
  print(i)
}

