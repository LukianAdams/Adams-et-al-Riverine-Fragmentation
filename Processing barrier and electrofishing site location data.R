#Loading relevant packages
library(tidyverse)
library(riverdist)

crs <- "+proj=merc +datum=WGS84 +ellps=WGS84 +no_defs" #Setting crs to 2020 Geocentric Datum of Australia

#Reading in locations of barriers, after being snapped to river shapefiles in QGIS
bars_on_mur <- pointshp2segvert(path = ".", layer = "Murrumbidgee Barriers Snapped", rivers = mur_shape) %>% 
  arrange(`Location I`)

#Reading in locations of electrofishing sites, after being snapped to river shapefiles in QGIS
sites_on_mur <- pointshp2segvert(path = ".", layer = "Murrumbidgee Sites Snapped", rivers = mur_shape) %>%
  mutate(SiteName_Latitude = paste(SiteName, SampleLati, sep = "_"))

#Visual check of position of barriers and electrofishing sites along rivers
plot(mur_shape)
riverpoints(seg=sites_on_mur$seg, vert=sites_on_mur$vert, rivers=mur_shape, pch=15, col="blue")
riverpoints(seg=bars_on_mur$seg, vert=bars_on_mur$vert, rivers=mur_shape, pch=15, col="red")

#Repeating for all rivers
bars_on_lac <- pointshp2segvert(path = ".", layer = "Lachlan Barriers Snapped", rivers = lac_shape) %>%
  arrange(`Location I`)
sites_on_lac <- pointshp2segvert(path = ".", layer = "Lachlan Sites Snapped", rivers = lac_shape) %>%
  mutate(SiteName_Latitude = paste(SiteName, SampleLati, sep = "_"))
plot(lac_shape)
riverpoints(seg=sites_on_lac$seg, vert=sites_on_lac$vert, rivers=lac_shape, pch=15, col="blue")
riverpoints(seg=bars_on_lac$seg, vert=bars_on_lac$vert, rivers=lac_shape, pch=15, col="red")

bars_on_mac <- pointshp2segvert(path = ".", layer = "Macquarie Barriers Snapped", rivers = mac_shape) %>%
  arrange(`Location I`)
sites_on_mac <- pointshp2segvert(path = ".", layer = "Macquarie Sites Snapped", rivers = mac_shape) %>%
  mutate(SiteName_Latitude = paste(SiteName, SampleLati, sep = "_"))
plot(mac_shape)
riverpoints(seg=sites_on_mac$seg, vert=sites_on_mac$vert, rivers=mac_shape, pch=15, col="blue")
riverpoints(seg=bars_on_mac$seg, vert=bars_on_mac$vert, rivers=mac_shape, pch=15, col="red")

bars_on_nam <- pointshp2segvert(path = ".", layer = "Namoi Barriers Snapped", rivers = nam_shape) %>%
  arrange(`Location I`)
sites_on_nam <- pointshp2segvert(path = ".", layer = "Namoi Sites Snapped", rivers = nam_shape) %>%
  mutate(SiteName_Latitude = paste(SiteName, SampleLati, sep = "_"))
plot(nam_shape)
riverpoints(seg=sites_on_nam$seg, vert=sites_on_nam$vert, rivers=nam_shape, pch=15, col="blue")
riverpoints(seg=bars_on_nam$seg, vert=bars_on_nam$vert, rivers=nam_shape, pch=15, col="red")

bars_on_gwy <- pointshp2segvert(path = ".", layer = "Gwydir Barriers Snapped", rivers = gwy_shape) %>%
  arrange(`Location I`)
sites_on_gwy <- pointshp2segvert(path = ".", layer = "Gwydir Sites Snapped", rivers = gwy_shape) %>%
  mutate(SiteName_Latitude = paste(SiteName, SampleLati, sep = "_"))
plot(gwy_shape)
riverpoints(seg=sites_on_gwy$seg, vert=sites_on_gwy$vert, rivers=gwy_shape, pch=15, col="blue")
riverpoints(seg=bars_on_gwy$seg, vert=bars_on_gwy$vert, rivers=gwy_shape, pch=15, col="red")

bars_on_dum <- pointshp2segvert(path = ".", layer = "Dumaresq Mole Barriers Snapped", rivers = dum_shape) %>%
  arrange(`Location I`)
sites_on_dum <- pointshp2segvert(path = ".", layer = "Dumaresq Mole Sites Snapped", rivers = dum_shape) %>%
  mutate(SiteName_Latitude = paste(SiteName, SampleLati, sep = "_"))
plot(dum_shape)
riverpoints(seg=sites_on_dum$seg, vert=sites_on_dum$vert, rivers=dum_shape, pch=15, col="blue")
riverpoints(seg=bars_on_dum$seg, vert=bars_on_dum$vert, rivers=dum_shape, pch=15, col="red")

#Saving barrier shapefiles
saveRDS(bars_on_mur, file = "bars_on_mur.rds")
saveRDS(bars_on_lac, file = "bars_on_lac.rds")
saveRDS(bars_on_mac, file = "bars_on_mac.rds")
saveRDS(bars_on_nam, file = "bars_on_nam.rds")
saveRDS(bars_on_gwy, file = "bars_on_gwy.rds")
saveRDS(bars_on_dum, file = "bars_on_dum.rds")

#Saving electrofishing site shapefiles
saveRDS(sites_on_mur, file = "sites_on_mur.rds")
saveRDS(sites_on_lac, file = "sites_on_lac.rds")
saveRDS(sites_on_mac, file = "sites_on_mac.rds")
saveRDS(sites_on_nam, file = "sites_on_nam.rds")
saveRDS(sites_on_gwy, file = "sites_on_gwy.rds")
saveRDS(sites_on_dum, file = "sites_on_dum.rds")
