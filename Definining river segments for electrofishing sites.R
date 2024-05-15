#Loading relevant packages
library(tidyverse)

#Reading in data
sites_on_mur <- readRDS("Data/sites_on_mur.rds")
sites_on_lac <- readRDS("Data/sites_on_lac.rds")
sites_on_mac <- readRDS("Data/sites_on_mac.rds")
sites_on_nam <- readRDS("Data/sites_on_nam.rds")
sites_on_gwy <- readRDS("Data/sites_on_gwy.rds")
sites_on_dum <- readRDS("Data/sites_on_dum.rds")

bars_on_mur <- readRDS("Data/bars_on_mur.rds")
bars_on_lac <- readRDS("Data/bars_on_lac.rds")
bars_on_mac <- readRDS("Data/bars_on_mac.rds")
bars_on_nam <- readRDS("Data/bars_on_nam.rds")
bars_on_gwy <- readRDS("Data/bars_on_gwy.rds")
bars_on_dum <- readRDS("Data/bars_on_dum.rds")

mur_shape <- readRDS("Data/mur_shape.rds")
lac_shape <- readRDS("Data/lac_shape.rds")
mac_shape <- readRDS("Data/mac_shape.rds")
nam_shape <- readRDS("Data/nam_shape.rds")
gwy_shape <- readRDS("Data/gwy_shape.rds")
dum_shape <- readRDS("Data/dum_shape.rds")

electro_data <- read_csv("Data/electro_data.csv")

#Combining sites from all rivers
sites_on_rivers <- rbind(sites_on_mur, sites_on_lac, sites_on_mac, sites_on_nam, sites_on_gwy, sites_on_dum) %>%
  unique(LonLat) #Removing duplicates

#Removing bad sites from electrofishing dataset
electro_data_clean <- subset(electro_data, LonLat %in% sites_on_rivers)

#Creating a function to find the longstream distance of each electrofishing from the downstream end of the river
find_distance <- function(sites, shape) {
  site_dist <- mouthdistbysurvey(unique = sites$OperationI, survey = sites$LonLat, 
                                     seg = sites$seg, vert = sites$vert, rivers = shape)
  site_value_df <- data.frame(LonLat = colnames(site_dist), 
                              Value = apply(site_dist, 2, function(x) unique(na.omit(x))))
  rownames(site_value_df) <- NULL
  return(site_value_df = site_value_df)
}

#Creating lists of river shapefiles and electrofishing site locations for application of find_distance function
shape_list <- list(mur_shape, lac_shape, mac_shape, nam_shape, gwy_shape, dum_shape)
names(shape_list) <- c("mur", "lac", "mac", "nam", "gwy", "dum")
sites_list <- list(sites_on_mur, sites_on_lac, sites_on_mac, sites_on_nam, sites_on_gwy, sites_on_dum)

#Creating dataframe of distances and adding to main dataframe
distance_list <- Map(find_distance, sites_list, shape_list)
distance_df <- do.call(rbind, distance_list)
df_formatted <- merge(electro_data_clean, distance_df, by = "LonLat", all.x = TRUE) %>%
  rename(Distance = Value) %>%
  mutate_at("WaterbodyName", as.factor)

#Creating a function to find the longstream distance of each barrier from the downstream end of the river
find_distance_bars <- function(bars, shape) {
  bar_dist <- mouthdistbysurvey(unique = bars$Location.I, survey = bars$Dummy, 
                                     seg = bars$seg, vert = bars$vert, rivers = shape)
}

#Creating lists of barriers locations for application of find_distance_bars function
bar_df_list <- list(bars_on_mur, bars_on_lac, bars_on_mac, bars_on_nam, bars_on_gwy, bars_on_dum)
names(bar_df_list) <- c("Murrumbidgee River", "Lachlan River", "Macquarie River", "Namoi River", "Gwydir River", "Dumaresq River/Mole River")

#Adding a dummy column, necessary for mouthdistbysurvey function
add_dummy_column <- function(df) {
  df <- transform(df, Dummy = "D")
  return(df)
}

bar_df_list <- lapply(bar_df_list, add_dummy_column)

bar_distance_list <- Map(find_distance_bars, bar_df_list, shape_list)

names(bar_distance_list) <- c("Murrumbidgee River", "Lachlan River", "Macquarie River", "Namoi River", "Gwydir River", "Dumaresq/Mole River")

#Adding distances to barrier dataframes
for (i in seq_along(bar_df_list)) {
  bar_df_list[[i]]$Distance <- bar_distance_list[[i]]
  colnames(bar_df_list[[i]])[ncol(bar_df_list[[i]])] <- "Distance"
}

#Finding the size of each river fragment segment confined by barriers. 
#Most river fragments are confined by a barrier at each end, but some are confined by a barrier at one end and the end of the river at another. For those, a dummy barrier was manually added at the end of the river in QGIS
for (i in 1:length(bar_df_list)) {
  df <- bar_df_list[[i]]
  
  for (j in 1:nrow(df)) {
    current_distance <- df$Distance[j]
    lower_distances <- df$Distance[df$Distance < current_distance]
    
    if (length(lower_distances) > 0) {
      closest_lower_distance <- max(lower_distances)
      FragmentLength <- current_distance - closest_lower_distance
    } else {
      FragmentLength <- NA
    }
    
    df$FragmentLength[j] <- FragmentLength
  }
  
  bar_df_list[[i]] <- df
}

#Making a dataframe with data for barriers from all rivers
bars_df <- do.call(rbind, bar_df_list) %>%
  
  #Fixing column names which were broken in QGIS
  setnames(old = c("Structur_1", "Location.I", "Structure", "Fish.Barri", "Position.C", "Waterway.N", "Primary.St", "Height..m.", "Width..m.", "Headloss..", "MER.Zone", "Fish.Statu", "Riparian.C", "Remediat_1", "Year.Remed", "Fish.Passa", "Barrier.Im", "Velocity.B", "Flow.Depth"), 
           new = c("Name", "LocationID", "StructureType", "FishBarrier", "PositionConfirmed", "WaterbodyName", "BarrierType", "Height", "Width", "Headloss", "Zone", "FishStatus", "RiparianCondition", "RemediationType", "YearRemediated", "FishPassage", "BarrierImpact", "VelocityBarrier", "FlowDepthBarrier"))
unique(bars_df$WaterbodyName)

bars_df$WaterbodyName <- gsub("Mole River|Dumaersq River|Dumaresq River", "Dumaresq/Mole River", bars_df$WaterbodyName)
bars_df$WaterbodyName <- as.factor(bars_df$WaterbodyName)

#Removing dummy barriers
bars_df <- bars_df %>%
  filter(LocationID > 10)

#Saving bars_df so more formatting can be done in Excel
write_csv(bars_df, file = "bars_df.csv")

#Formatting electrofishing dataframe with fragment size and distance upstream information
rivers <- list("Murrumbidgee River", "Lachlan River", "Macquarie River", "Namoi River", "Gwydir River","Dumaresq/Mole River")
df_list <- list()

for (i in seq_along(rivers)) {
  river <- rivers[[i]]
  #Creating a dataframe of operations for each river
  subset_df <- df_formatted[df_formatted$WaterbodyName == rivers[i], ] %>%
    mutate(
      closest_upstream = sapply(Distance, function(x) {
        closest_higher_distance <- bars_df[bars_df$WaterbodyName == rivers[i], ] %>%
          filter(Distance > x) %>%
          arrange(Distance) %>%
          slice(1)
        if (nrow(closest_higher_distance) > 0) {
          return(closest_higher_distance$Distance)
        } else {
          return(NA)
        }
      }),
      up_bar = sapply(closest_upstream, function(x) {
        if (!is.na(x)) {
          closest_observation <- bars_df %>%
            filter(Distance == x)
          return(closest_observation$Name[1])
        } else {
          return("No Upstream Barrier")
        }
      })
    ) %>%
    #Making relevant columns factors
    select(-closest_upstream) %>%
    mutate_at(c("SiteID", "SiteName", "SampleID", "OperationID", "up_bar", "Method"), as.factor) %>%
    group_by(up_bar) %>%
     filter(n() >= 75) %>%
     ungroup() %>%
     select_if(~ is.numeric(.) && (sum(. > 0) / length(.) >= 0.005 || sum(. < 0) / length(.) >= 0.005) || !is.numeric(.)) %>%
     droplevels()
  
  subset_df$Date <- as.Date(subset_df$Date, format = "%d/%m/%Y")
  
  ordered_bars <- subset_df %>%
    arrange(Distance) %>%
    distinct(up_bar) %>%
    pull(up_bar)
  
  subset_df$up_bar_ord <- factor(subset_df$up_bar, ordered = TRUE, levels = ordered_bars)
  
  start_index <- 13
  end_index <- ncol(subset_df) - 5
  
  species_col_names <- colnames(subset_df)[start_index:end_index]
  species_col_names_fixed <- gsub("[[:space:]_-]", "", species_col_names)
  names(subset_df)[start_index:end_index] <- species_col_names_fixed
  species <- colnames(subset_df)[start_index:end_index]
  species_list_name <- paste(tolower(substr(river, 1, 3)), "_species", sep = "")
  assign(species_list_name, species)
  
  df_list[[i]] <- subset_df
  
}
names(df_list) <- c("mur_df", "lac_df", "mac_df", "nam_df", "gwy_df", "dum_df")

#Saving list of cleaned dataframes
saveRDS(df_list, file = "Data/df_list.rds")
