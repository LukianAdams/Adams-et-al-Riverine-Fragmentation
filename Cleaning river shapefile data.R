#Loading relevant packages
library(riverdist)
library(tidyverse)

crs <- "+proj=merc +datum=WGS84 +ellps=WGS84 +no_defs" #Setting crs to 2020 Geocentric Datum of Australia

#Loading in a shapefile for each river - Cleanup and dissolve functions used to remove disconnected river segments and convert rivers into a single linestring
mur_shape <- line2network(path = ".", layer = "Murrumbidgee Shape", reproject = crs) %>%
  cleanup() %>%
  dissolve() #No segments removed, dissolved into a single string

lac_shape <- line2network(path = ".", layer = "Lachlan Shape", reproject = crs) %>%
  cleanup() %>%
  dissolve() #No segments removed, dissolved into a single string

mac_shape <- line2network(path = ".", layer = "Macquarie Shape", reproject = crs) %>%
  cleanup() %>% #Segments removed - 2, 18, 14, 11, 10
  dissolve() %>%
  cleanup() #No segments removed second time around, dissolved into a single string

nam_shape <- line2network(path = ".", layer = "Namoi Shape", reproject = crs) %>%
  cleanup() %>% #Segments removed - 36, 37, 32, 29, 26, 23, 20, 18, 15, 11, 8, 5, 2
  dissolve() %>%
  cleanup() #No segments removed second time around, dissolved into a single string

gwy_shape <- line2network(path = ".", layer = "Gwydir Shape", reproject = crs) %>%
  cleanup() %>% #Segments removed - 1,1
  dissolve() %>%
  cleanup() #No segments removed second time around, dissolved into a single string

dum_shape <- line2network(path = ".", layer = "Dumaresq Mole Shape", reproject = crs) %>%
  cleanup() %>%
  dissolve() #No segments removed, dissolved into a single string

#Visual check of cleaned river shapefiles
plot(mur_shape)
plot(lac_shape)
plot(mac_shape)
plot(nam_shape)
plot(gwy_shape)
plot(dum_shape)

#Saving cleaned river shapefiles
saveRDS(mur_shape, file = "mur_shape_clean.rds")
saveRDS(lac_shape, file = "lac_shape_clean.rds")
saveRDS(mac_shape, file = "mac_shape_clean.rds")
saveRDS(nam_shape, file = "nam_shape_clean.rds")
saveRDS(gwy_shape, file = "gwy_shape_clean.rds")
saveRDS(dum_shape, file = "dum_shape_clean.rds")
