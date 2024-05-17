#Loading relevant packages
library(tidyverse)
library(ozmaps)

#Defining GDA2020 CRS
gda <- crs_gda2020()

#Getting map of Australia
australia <- ozmap_data()
st_crs(australia) <- st_crs(gda)
australia <- st_transform(australia, gda) #Setting CRS
nsw <- australia %>% #Filtering to only include NSW and ACT
  filter(NAME %in% c("New South Wales", "Australian Capital Territory"))
plot(australia)

#Loading in a shapefile of rivers
rivers_shape <- readOGR("Data/", layer = "MDB Rivers")
rivers_shape <- st_as_sf(rivers_shape, crs = gda)
rivers_shape <- st_transform(rivers_shape, gda) %>% #Renaming rivers
  filter(!(fullname %in% c("CONDAMINE RIVER"))) %>%
  rename(WaterbodyName = fullname) %>%
  mutate(WaterbodyName = case_when(
    WaterbodyName == "DUMARESQ RIVER" ~ "Dumaresq/Mole",
    WaterbodyName == "BALLONE CREEK" ~ "Ballone Creek",
    WaterbodyName == "BARWON RIVER" ~ "Barwon",
    WaterbodyName == "DARLING RIVER" ~ "Darling",
    WaterbodyName == "GWYDIR RIVER" ~ "Gwydir",
    WaterbodyName == "LACHLAN RIVER" ~ "Lachlan",
    WaterbodyName == "MACINTYRE RIVER" ~ "Macintyre",
    WaterbodyName == "MACQUARIE RIVER" ~ "Macquarie",
    WaterbodyName == "MURRAY RIVER" ~ "Murray",
    WaterbodyName == "MURRUMBIDGEE RIVER" ~ "Murrumbidgee",
    WaterbodyName == "NAMOI RIVER" ~ "Namoi",
    WaterbodyName == "WARREGO RIVER" ~ "Warrego",
    TRUE ~ WaterbodyName
  ))

#Loading in MDB boundary
mdb_shape <- readOGR("Data/RMaps/", layer = "mdb_boundary_GDA1994")
mdb_shape <- st_as_sf(mdb_shape, crs = gda)
mdb_shape <- st_transform(mdb_shape, gda)

#Preparing electrofishing data for plotting
data_map_df <- do.call(rbind, lapply(df_list, function(df) df[, c(1:12, ncol(df)-17)])) %>%
  group_by(SiteID) %>%
  summarise(TotalObservations = n(), #Finding total number of electrofishing samples at each site
            across(everything(), first))

data_map_sf <- st_as_sf(data_map_df, coords = c("SampleLongitude", "SampleLatitude"), crs = gda)
data_map_sf <- st_transform(data_map_sf, gda)

#Preparing barrier data for plotting
bar_sf <- st_as_sf(bars_df, coords = c("Long", "Lat"))
st_crs(bar_sf) <- st_crs(gda)
bar_sf <- st_transform(bar_sf, gda)
max_distances <- data_map_df %>%
  group_by(WaterbodyName) %>%
  summarise(max_distance = max(Distance, na.rm = TRUE))
bar_sf <- bar_sf %>%
  left_join(max_distances, by = "WaterbodyName") %>%
  filter(is.na(max_distance) | Distance <= max_distance)
bar_sf$X <- st_coordinates(bar_sf)[,1]
bar_sf$Y <- st_coordinates(bar_sf)[,2]
bar_df <- as.data.frame(bar_sf) %>% 
  mutate(
    X_offset = case_when( #Offsetting site locations from the river to reduce clutter
      Name == "Road Crossing Off Johnstones Road" ~ X + 0.18/sqrt(2),   
      Name == "Gibraltar Station Road Crossing" ~ X + 0.18/sqrt(2),
      Name == "Mole River Road Crossing" ~ X,   
      Name == "Bonshaw Weir" ~ X - 0.18,
      Name == "Texas Weir" ~ X + 0.18/sqrt(2),   
      Name == "Cunningham Weir" ~ X + 0.18/sqrt(2),
      Name == "Glenarbon Weir" ~ X + 0.18/sqrt(2),   
      Name == "Yarrowyck Road Crossing" ~ X,
      Name == "Road Crossing Off Torryburn Road 1" ~ X,
      Name == "Road Crossing Off Torryburn Road 2" ~ X,
      Name == "Road Crossing off Gwydir Park Road" ~ X - 0.18,
      Name == "Road Crossing Bundarra Lions Park" ~ X -0.18,
      Name == "Copeton Dam" ~ X,
      Name == "Tareelaroi Weir" ~ X,
      Name == "Boolooroo Weir" ~ X,
      Name == "Tyreel Regulator" ~ X,
      Name == "Rock Weir" ~ X,
      Name == "Road Crossing Moree Plains 1" ~ X,
      Name == "Road Crossing Moree Plains 2" ~ X,
      Name == "Cooma Weir" ~ X,
      Name == "Cudgildool Rock Weir" ~ X,
      Name == "Private Rock Weir" ~ X,
      Name == "Wandoona Dropboard" ~ X,
      Name == "Adjustable Crest" ~ X,
      Name == "Barwon River Road Weir" ~ X,
      Name == "WGS1" ~ X,
      Name == "Manilla Weir 2" ~ X,
      Name == "Manilla Weir 1" ~ X,
      Name == "Keepit Dam" ~ X-0.18/sqrt(2),
      Name == "Bibbla Creek Rock Bar" ~ X + 0.18/sqrt(2),
      Name == "Mollee Weir" ~ X + 0.18/sqrt(2),
      Name == "Gunidgera Weir" ~ X,
      Name == "Weeta Weir" ~ X,
      Name == "WGS4" ~ X,
      Name == "Stony Crossing" ~ X,
      Name == "Walgett Weir No. 10" ~ X,
      Name == "Weir Near Walgett" ~ X,
      Name == "The Forge" ~ X + 0.18,
      Name == "Long Point Road Crossing" ~ X - 0.18,
      Name == "Burrendong Dam" ~ X + 0.18/sqrt(2),
      Name == "Wellington Falls/Wellington" ~ X + 0.18/sqrt(2),
      Name == "Dubbo City Council Water Supply Dam" ~ X - 0.18/sqrt(2),
      Name == "Dubbo Weir" ~ X + 0.18/sqrt(2),
      Name == "Narromine Weir" ~ X - 0.18/sqrt(2),
      Name == "Gin Gin Weir" ~ X - 0.18/sqrt(2),
      Name == "Warren Weir" ~ X + 0.18/sqrt(2),
      Name == "Warren Shire Council Weir" ~ X - 0.18/sqrt(2),
      Name == "Marebone Weir" ~ X + 0.18,
      Name == "Buttabone Weir" ~ X - 0.18,
      Name == "Warren-Carinda Road Crossing" ~ X + 0.18,
      Name == "Macquarie Marsh Barrier 1" ~ X - 0.18,
      Name == "Macquarie Marsh Barrier 2" ~ X + 0.18,
      Name == "Blockbank Near Noumani" ~ X + 0.18,
      Name == "Blockbank Near Willewa North" ~ X - 0.18,
      Name == "Road Crossing Near Glenacre" ~ X + 0.18/sqrt(2),
      Name == "Fenceline Crossing 2" ~ X - 0.18/sqrt(2),
      Name == "Brewon Road Culvert" ~ X + 0.18/sqrt(2),
      Name == "Bevendale Creek Road Crossing" ~ X - 0.18,
      Name == "Fish River Road Crossing" ~ X - 0.18,
      Name == "Wyangala Dam" ~ X,
      Name == "Apex Park Weir" ~ X,
      Name == "Cottons Weir" ~ X,
      Name == "Jemalong Weir" ~ X,
      Name == "Jemalong Station Weir 2" ~ X,
      Name == "Jemalong Station Weir 1" ~ X,
      Name == "Weir Near Bedgerabong" ~ X,
      Name == "Borambil Park Weir 2" ~ X,
      Name == "Borambil Park Weir 1" ~ X,
      Name == "Warroo Stud Farm Weir" ~ X,
      Name == "Lachlan River Blockbank" ~ X,
      Name == "Lachlan River Weir 2" ~ X,
      Name == "Lachlan River Weir 1" ~ X,
      Name == "Condobolin Weir" ~ X,
      Name == "Condobolin West Weir" ~ X,
      Name == "Micabil Weir" ~ X,
      Name == "Kiacatoo Weir" ~ X,
      Name == "Booberoi Weir And Regulator" ~ X,
      Name == "Lake Cargelligo Diversion Weir" ~ X,
      Name == "Lake Brewster Diversion Weir" ~ X,
      Name == "Willandra Weir" ~ X,
      Name == "Gonowlia Weir" ~ X - 0.18/sqrt(2),
      Name == "Hillston Weir" ~ X - 0.18/sqrt(2),
      Name == "Tallawanta Weir" ~ X - 0.18/sqrt(2),
      Name == "Booligal Weir" ~ X - 0.18/sqrt(2),
      Name == "Brassbutt Weir" ~ X,
      Name == "Fixed Crest Weir Near Lake Ita" ~ X,
      Name == "Four Mile Weir" ~ X,
      Name == "Oxley Dam" ~ X,
      Name == "Cooma Weir" ~ X,
      Name == "Gillens Road Crossing" ~ X + 0.18,
      Name == "Bumbalong Road Crossing" ~ X + 0.18,
      Name == "Angle Road Crossing" ~ X + 0.18,
      Name == "Cotter Reserve Weir 1" ~ X - 0.18,
      Name == "Cotter Reserve Weir 2" ~ X + 0.18,
      Name == "Burrinjuck Dam" ~ X,
      Name == "Berembed Weir" ~ X,
      Name == "Yanco Regulator" ~ X,
      Name == "Gogeldrie Weir" ~ X,
      Name == "Hay Weir" ~ X,
      Name == "Maude Weir" ~ X,
      Name == "Waugorah Regulator" ~ X - 0.18/sqrt(2),
      Name == "Yanga Regulator" ~ X + 0.18/sqrt(2),
      Name == "Redbank Weir" ~ X - 0.18/sqrt(2),
      Name == "Munkugerie Regulator" ~ X + 0.18/sqrt(2),
      Name == "Breer Regulator" ~ X - 0.18/sqrt(2),
      Name == "Paika Escape" ~ X + 0.18/sqrt(2),
      Name == "Glen Avon Escape" ~ X - 0.18/sqrt(2),
      Name == "Balranald Weir" ~ X,
      TRUE ~ X + 0                   
    ),
    Y_offset = case_when(
      Name == "Road Crossing Off Johnstones Road" ~ Y + 0.18/sqrt(2),   
      Name == "Gibraltar Station Road Crossing" ~ Y + 0.18/sqrt(2),
      Name == "Mole River Road Crossing" ~ Y + 0.18,   
      Name == "Bonshaw Weir" ~ Y,
      Name == "Texas Weir" ~ Y + 0.18/sqrt(2),   
      Name == "Cunningham Weir" ~ Y + 0.18/sqrt(2),
      Name == "Glenarbon Weir" ~ Y + 0.18/sqrt(2),   
      Name == "Yarrowyck Road Crossing" ~ Y - 0.18,
      Name == "Road Crossing Off Torryburn Road 1" ~ Y + 0.18,   
      Name == "Road Crossing Off Torryburn Road 2" ~ Y - 0.18,
      Name == "Road Crossing off Gwydir Park Road" ~ Y,   
      Name == "Road Crossing Bundarra Lions Park" ~ Y,
      Name == "Copeton Dam" ~ Y + 0.18,
      Name == "Tareelaroi Weir" ~ Y + 0.18,
      Name == "Boolooroo Weir" ~ Y + 0.18,
      Name == "Tyreel Regulator" ~ Y - 0.18,
      Name == "Rock Weir" ~ Y + 0.18,
      Name == "Road Crossing Moree Plains 1" ~ Y - 0.18,
      Name == "Road Crossing Moree Plains 2" ~ Y + 0.18,
      Name == "Cooma Weir" ~ Y - 0.18,
      Name == "Cudgildool Rock Weir" ~ Y + 0.18,
      Name == "Private Rock Weir" ~ Y - 0.18,
      Name == "Wandoona Dropboard" ~ Y + 0.18,
      Name == "Adjustable Crest" ~ Y - 0.18,
      Name == "Barwon River Road Weir" ~ Y - 0.18,
      Name == "WGS1" ~ Y + 0.18,
      Name == "Manilla Weir 2" ~ Y-0.18,
      Name == "Manilla Weir 1" ~ Y+0.18,
      Name == "Keepit Dam" ~ Y + 0.18/sqrt(2),
      Name == "Bibbla Creek Rock Bar" ~ Y + 0.18/sqrt(2),
      Name == "Mollee Weir" ~ Y + 0.18/sqrt(2),
      Name == "Gunidgera Weir" ~ Y + 0.18,
      Name == "Weeta Weir" ~ Y - 0.18,
      Name == "WGS4" ~ Y + 0.18,
      Name == "Stony Crossing" ~ Y + 0.18,
      Name == "Walgett Weir No. 10" ~ Y - 0.18,
      Name == "Weir Near Walgett" ~ Y + 0.18,
      Name == "The Forge" ~ Y,
      Name == "Long Point Road Crossing" ~ Y,
      Name == "Burrendong Dam" ~ Y + 0.18/sqrt(2),
      Name == "Wellington Falls/Wellington" ~ Y + 0.18/sqrt(2),
      Name == "Dubbo City Council Water Supply Dam" ~ Y - 0.18/sqrt(2),
      Name == "Dubbo Weir" ~ Y + 0.18/sqrt(2),
      Name == "Narromine Weir" ~ Y - 0.18/sqrt(2),
      Name == "Gin Gin Weir" ~ Y - 0.18/sqrt(2),
      Name == "Warren Weir" ~ Y + 0.18/sqrt(2),
      Name == "Warren Shire Council Weir" ~ Y - 0.18/sqrt(2),
      Name == "Marebone Weir" ~ Y,
      Name == "Buttabone Weir" ~ Y,
      Name == "Warren-Carinda Road Crossing" ~ Y,
      Name == "Macquarie Marsh Barrier 1" ~ Y,
      Name == "Macquarie Marsh Barrier 2" ~ Y,
      Name == "Blockbank Near Noumani" ~ Y,
      Name == "Blockbank Near Willewa North" ~ Y,
      Name == "Road Crossing Near Glenacre" ~ Y + 0.18/sqrt(2),
      Name == "Fenceline Crossing 2" ~ Y - 0.18/sqrt(2),
      Name == "Brewon Road Culvert" ~ Y + 0.18/sqrt(2),
      Name == "Brewon Road Culvert" ~ Y,
      Name == "Fish River Road Crossing" ~ Y,
      Name == "Wyangala Dam" ~ Y + 0.18,
      Name == "Apex Park Weir" ~ Y + 0.18,
      Name == "Cottons Weir" ~ Y - 0.18,
      Name == "Jemalong Weir" ~ Y + 0.18,
      Name == "Jemalong Station Weir 2" ~ Y - 0.18,
      Name == "Jemalong Station Weir 1" ~ Y + 0.18,
      Name == "Weir Near Bedgerabong" ~ Y - 0.18,
      Name == "Borambil Park Weir 2" ~ Y + 0.18,
      Name == "Borambil Park Weir 1" ~ Y - 0.18,
      Name == "Warroo Stud Farm Weir" ~ Y + 0.18,
      Name == "Lachlan River Blockbank" ~ Y - 0.18,
      Name == "Lachlan River Weir 2" ~ Y + 0.18,
      Name == "Lachlan River Weir 1" ~ Y - 0.18,
      Name == "Condobolin Weir" ~ Y + 0.18,
      Name == "Condobolin West Weir" ~ Y - 0.18,
      Name == "Micabil Weir" ~ Y + 0.18,
      Name == "Kiacatoo Weir" ~ Y - 0.18,
      Name == "Booberoi Weir And Regulator" ~ Y + 0.18,
      Name == "Lake Cargelligo Diversion Weir" ~ Y - 0.18,
      Name == "Lake Brewster Diversion Weir" ~ Y + 0.18,
      Name == "Willandra Weir" ~ Y + 0.18,
      Name == "Gonowlia Weir" ~ Y + 0.18/sqrt(2),
      Name == "Hillston Weir" ~ Y + 0.18/sqrt(2),
      Name == "Tallawanta Weir" ~ Y + 0.18/sqrt(2),
      Name == "Booligal Weir" ~ Y + 0.18/sqrt(2),
      Name == "Brassbutt Weir" ~ Y - 0.18,
      Name == "Fixed Crest Weir Near Lake Ita" ~ Y + 0.18,
      Name == "Four Mile Weir" ~ Y - 0.18,
      Name == "Oxley Dam" ~ Y + 0.18,
      Name == "Cooma Weir" ~ Y - 0.18,
      Name == "Gillens Road Crossing" ~ Y,
      Name == "Bumbalong Road Crossing" ~ Y,
      Name == "Angle Road Crossing" ~ Y,
      Name == "Cotter Reserve Weir 1" ~ Y,
      Name == "Cotter Reserve Weir 2" ~ Y,
      Name == "Burrinjuck Dam" ~ Y - 0.18,
      Name == "Berembed Weir" ~ Y + 0.18,
      Name == "Yanco Regulator" ~ Y - 0.18,
      Name == "Gogeldrie Weir" ~ Y + 0.18,
      Name == "Hay Weir" ~ Y - 0.18,
      Name == "Maude Weir" ~ Y - 0.18,
      Name == "Waugorah Regulator" ~ Y + 0.18/sqrt(2),
      Name == "Yanga Regulator" ~ Y - 0.18/sqrt(2),
      Name == "Redbank Weir" ~ Y + 0.18/sqrt(2),
      Name == "Munkugerie Regulator" ~ Y - 0.18/sqrt(2),
      Name == "Breer Regulator" ~ Y + 0.18/sqrt(2),
      Name == "Paika Escape" ~ Y - 0.18/sqrt(2),
      Name == "Glen Avon Escape" ~ Y + 0.18/sqrt(2),
      Name == "Balranald Weir" ~ Y - 0.18,
      TRUE ~ Y + 0
    )
  )

#Australia map
data_map_australia <- ggplot() +
  geom_sf(data = australia, fill = "blanchedalmond", alpha = 0.6) +
  geom_sf(data = mdb_shape, fill = "forestgreen", alpha = 0.1) +
  geom_sf(data = rivers_shape, linetype = "solid", linewidth = 1.1, aes(colour = WaterbodyName, alpha = WaterbodyName)) +
  scale_colour_manual(values = c("Murrumbidgee" = "red2", "Lachlan" = "blue", "Macquarie" = "goldenrod1", "Namoi" = "chocolate4", "Gwydir" = "green4", "Dumaresq/Mole" = "darkorange"), name = "River", breaks = c("Dumaresq/Mole", "Gwydir", "Namoi", "Macquarie", "Lachlan", "Murrumbidgee")) +
  scale_alpha_manual(values = c("Barwon" = 0.6, "Ballone Creek" = 0.6, "Darling" = 0.6, "Condamine" = 0.6, "Murray" = 0.6, "Warrego" = 0.6, "Macinytre" = 0.6)) +
  scale_y_continuous(limits = c(-45, -10), expand = c(0, 0)) +
  scale_x_continuous(limits = c(111, 155), expand = c(0, 0)) +
  geom_rect(aes(xmin = 146.6, xmax = 152.5, ymin = -33.8, ymax = -28.2), colour = "lightslateblue", fill = NA, linewidth = 1, alpha = 0.5) +
  geom_rect(aes(xmin = 142.8, xmax = 149.9, ymin = -36.5, ymax = -32.7), colour = "tan2", fill = NA, linewidth = 1, alpha = 0.5) +
  theme_nothing() +
  annotation_north_arrow(location = "tl", which_north = "true") +
  #draw_label(label = "Northern Basin", x = 149.55, y = -27.7, size = 12) +
  #draw_label(label = "Southern Basin", x = 146.35, y = -36.8, size = 12) +
  theme(legend.position = "none")
data_map_australia

#MDB northern basin map
data_map_north <- ggplot() +
  geom_sf(data = australia, fill = "blanchedalmond", alpha = 0.6, inherit.aes = FALSE) +
  geom_sf(data = mdb_shape, fill = "forestgreen", alpha = 0.1, inherit.aes = FALSE) +
  geom_sf(data = rivers_shape, linetype = "solid", linewidth = 1.1, aes(colour = WaterbodyName, alpha = WaterbodyName), inherit.aes = FALSE) +
  scale_colour_manual(values = c("Murrumbidgee" = "red2", "Lachlan" = "blue", "Macquarie" = "goldenrod1", "Namoi" = "chocolate4", "Gwydir" = "green4", "Dumaresq/Mole" = "darkorange"), name = "River", breaks = c("Dumaresq/Mole", "Gwydir", "Namoi", "Macquarie", "Lachlan", "Murrumbidgee")) +
  geom_sf(data = data_map_sf, size = 4, stroke = 0.5, shape = 21, aes(fill = TotalObservations, alpha = WaterbodyName)) +
  geom_point(data = bar_df, aes(x = X_offset, y = Y_offset, alpha = WaterbodyName), size = 2, stroke = 1.7, shape = 4) +
  geom_segment(data = bar_df, aes(x = X, y = Y, xend = X_offset, yend = Y_offset, alpha = WaterbodyName), color = "black", linewidth = 0.6) +
  #geom_sf(data = bar_sf, size = 4, stroke = 1.2, shape = 17, aes(alpha = WaterbodyName)) +
  scale_alpha_manual(values = c("Lachlan" = 0.3, "Lachlan River" = 0, "Barwon" = 0.5, "Ballone Creek" = 0.5, "Darling" = 0.5, "Macintyre" = 0.5), guide = "none") +
  scale_fill_viridis(trans = "log10", option = "plasma", breaks = c(3, 30, 300), name = "Samples") +
  scale_y_continuous(limits = c(-33.8, -28.2), expand = c(0, 0)) +
  scale_x_continuous(limits = c(146.6, 152.5), expand = c(0, 0)) +
  coord_sf(label_graticule = "NE") +
  annotation_scale(location = "br", text_cex = 1.2) +
  theme_minimal_grid(line_size = 0.0000001) +
  theme(panel.border = element_rect(colour = "lightslateblue", fill = NA, linewidth = 2), 
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position = "none")
data_map_north

#MDB southern basin map
data_map_south <- ggplot() +
  geom_sf(data = australia, fill = "blanchedalmond", alpha = 0.6) +
  geom_sf(data = mdb_shape, fill = "forestgreen", alpha = 0.1) +
  geom_sf(data = rivers_shape, linetype = "solid", linewidth = 1.1, aes(colour = WaterbodyName, alpha = WaterbodyName)) +
  scale_colour_manual(values = c("Murrumbidgee" = "red2", "Lachlan" = "blue", "Macquarie" = "goldenrod1", "Namoi" = "chocolate4", "Gwydir" = "green4", "Dumaresq/Mole" = "darkorange"), name = "River", breaks = c("Dumaresq/Mole", "Gwydir", "Namoi", "Macquarie", "Lachlan", "Murrumbidgee")) +
  geom_sf(data = data_map_sf, size = 4, stroke = 0.5, shape = 21, aes(fill = TotalObservations, alpha = WaterbodyName)) +
  geom_point(data = bar_df, aes(x = X_offset, y = Y_offset, alpha = WaterbodyName), size = 2, stroke = 1.7, shape = 4) +
  geom_segment(data = bar_df, aes(x = X, y = Y, xend = X_offset, yend = Y_offset, alpha = WaterbodyName), color = "black", linewidth = 0.6) +
  scale_alpha_manual(values = c("Macquarie" = 0.3, "Macquarie River" = 0, "Barwon" = 0.5, "Ballone Creek" = 0.5, "Murray" = 0.5), guide = "none") +
  scale_fill_viridis(trans = "log10", option = "plasma", breaks = c(4, 40, 400), name = "Number of samples") +
  guides(colour = guide_legend(override.aes = list(linewidth = 2, size = 8),nrow = 6)) +
  scale_y_continuous(limits = c(-36.5, -32.7), expand = c(0, 0), breaks = c(-33, -34, -35, -36)) +
  scale_x_continuous(limits = c(142.8, 149.9), expand = c(0, 0)) +
  coord_sf(label_graticule = "SE") +
  annotation_scale(location = "bl", text_cex = 1.2) +
  theme_minimal_grid(line_size = 0.0000001) +
  theme(panel.border = element_rect(colour = "tan2", fill = NA, linewidth = 2), 
        axis.title.x = element_blank(), axis.title.y = element_blank())
data_map_legend <- get_legend(data_map_south)
data_map_south <- data_map_south +
  theme(legend.position = "none")
data_map_south

#Combining map plots
first_row_data_map <- plot_grid(data_map_australia, data_map_legend,
                                nrow = 1)
first_row_data_map

ggsave(filename = "Output/Australia Map.png", plot = first_row_data_map, width = 300, height = 200, units = "mm", device = "png", dpi = 300)

second_row_data_map <- data_map_north + data_map_south + plot_layout(widths = c(1, (7.1/5.9-0.05)))
second_row_data_map

ggsave(filename = "Output/Data Map.png", plot = second_row_data_map, width = 400, height = 300, units = "mm", device = "png", dpi = 300)

data_map_plot_full <- plot_grid(first_row_data_map, second_row_data_map,
                                nrow = 2,
                                rel_widths = c(1, 1.5))
data_map_plot_full

#Saving map in high quality
ggsave(filename = "Output/Data Map.png", plot = data_map_plot_full, width = 400, height = 400, units = "mm", device='png', dpi=300)

#Saving map dataframe
write_csv(data_map_df, file = "Data/data_map_df.csv")
