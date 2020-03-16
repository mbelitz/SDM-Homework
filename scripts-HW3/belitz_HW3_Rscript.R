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

occ_df_inseterct <- st_intersection(occ_sf, rm_sf_buffer)

ggplot() + 
  geom_sf(na, mapping = aes()) +
  geom_sf(occ_df_inseterct, mapping = aes()) +
  geom_sf(rm_sf_buffer, mapping = aes(), color = "blue", alpha = 0.2) 

# remove points in the ocean

occ_df_inseterct <- st_intersection(occ_df_inseterct, na)

ggplot() + 
  geom_sf(na, mapping = aes()) +
  geom_sf(occ_df_inseterct, mapping = aes()) +
  geom_sf(rm_sf_buffer, mapping = aes(), color = "blue", alpha = 0.2) 

# 3) Decide on the accessible area of the species, using whatever criteria you think make sense based on the species biology. 
# Write your explanation for accessible area.

# crop buffer to terrestrial land

buff_crop <- st_intersection(x = rm_sf_buffer, y = na)

ggplot() + 
  geom_sf(na, mapping = aes()) +
  geom_sf(occ_df_inseterct, mapping = aes()) +
  geom_sf(buff_crop, mapping = aes(), color = "blue", alpha = 0.2) 


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
#' 5 & 6) BIO5 and BIO5 - Max Temperature of Warmest Month and Min Temperature of Coldest month - C. californicus can initiate torpor
#' if necessary, but avoids high elevation (>2400m), so Min temperature of Coldest month could help determine the species niche, 
#' while max temperature of warmest months could be found in true dessert areas outside of the chaparral ecosystem.
#' 
#' 7 & 8) BIO16 and BIO17, Precipitation of wettest quarter and precipitation of driest quarter. Using quarter would get at the 
#' seasonality we would expect with wet winters and dry summers. Non-chapparal ecosystems could experience different precipitation
#' patterns. 
#' 
#' Again, I would check for correlated variables before choosing what final variables to include in the model. 

# 6) Clip your layers to the accessible area you choose in #3.

bio1 <- raster("WorldClim/wc2.0_bio_10m_01.tif")

buff_crop_sp <- as_Spatial(buff_crop)

bio1_crop <- crop(x = bio1, buff_crop_sp)

plot(bio1_crop)

bio1_mask <- mask(bio1_crop, mask = buff_crop_sp)

plot(bio1_mask)

# 
# You can change up this order of operations etc - the order is just to help give you an idea of what you need to do.

