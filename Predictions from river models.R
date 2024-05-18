library(tidyverse)
library(dplyr)
library(coda)
library(brms)
library(readr)
library(emmeans)

fitted_mods_path <- "/Data/Fitted Mods"
fitted_mods_names <- list.files(path = fitted_mods_path, pattern = "\\.rds$", full.names = TRUE)

fitted_mods_bars_water <- list()
for (file in fitted_mods_names) {
  mod_name <- tools::file_path_sans_ext(basename(file))
  if(grepl("overbank", mod_name)) {
  mod <- readRDS(file)
  fitted_mods_bars_water[[mod_name]] <- mod
  }
}

mods_bars_water_order <- c("mur_overbank_fit", "lac_overbank_fit", "mac_overbank_fit", "nam_overbank_fit", "gwy_overbank_fit", "dum_overbank_fit")
fitted_mods_bars_water <- fitted_mods_bars_water[mods_bars_water_order]

i = as.numeric(Sys.getenv('PBS_ARRAY_INDEX'))

if (i == 1){
pred_list <- list()
  mod_name <- as.character(names(fitted_mods_bars_water[i]))
  river <- substr(mod_name, 1, 3)
  mod <- fitted_mods_bars_water[[i]]
  #Finding name of the river
  #Defining each species present
  species_list <- mod$formula$responses
  #Defining river segments
  up_bar_levels <- unique(mod$data$up_bar)
  #For each species
  for(species in species_list){
    #In each river segment
    for (up_bar in up_bar_levels){
      species <- species
      up_bar <- up_bar
      max_days <- max(mod$data$DaysSinceOverbank[mod$data$up_bar == up_bar])
      min_days <- round(min(mod$data$DaysSinceOverbank[mod$data$up_bar == up_bar])/180)*180
      #Predict the abundance at 12 intervals, starting at 0 and ending at maximum days since overbank
      emm <- emmeans(mod, ~up_bar, epred = TRUE, by = "DaysSinceOverbank", type = "response", resp = paste0(species),
                at = list(up_bar = c(paste0(up_bar)), DaysSinceOverbank = c(seq(min_days, max_days, by = 180))))
      #Summarise means and standard deviations
      emm_summary <- summary(as.mcmc(emm))
      #Turn into a dataframe 
      emm_df <- as.data.frame(emm_summary$statistics) %>%
        #Add various features
        mutate(
          WaterbodyName = river,
          Species = species,
          Distance = mean(mod$data$Distance[mod$data$up_bar == up_bar]),
          DaysSinceOverbank = seq(min_days, max_days, by = 180),
          n = nrow(mod$data[mod$data$up_bar == up_bar,])
        )
      df_name <- paste(species, up_bar)
      pred_list[[df_name]] <- emm_df
    }
  }
  saveRDS(pred_list, file = "pred_list_mur.rds")
  
} else if (i == 2){
pred_list <- list()
  mod_name <- as.character(names(fitted_mods_bars_water[i]))
  river <- substr(mod_name, 1, 3)
  mod <- fitted_mods_bars_water[[i]]
  #Finding name of the river
  #Defining each species present
  species_list <- mod$formula$responses
  #Defining river segments
  up_bar_levels <- unique(mod$data$up_bar)
  #For each species
  for(species in species_list){
    #In each river segment
    for (up_bar in up_bar_levels){
      species <- species
      up_bar <- up_bar
      max_days <- max(mod$data$DaysSinceOverbank[mod$data$up_bar == up_bar])
      min_days <- round(min(mod$data$DaysSinceOverbank[mod$data$up_bar == up_bar])/180)*180
      #Predict the abundance at 12 intervals, starting at 0 and ending at maximum days since overbank
      emm <- emmeans(mod, ~up_bar, epred = TRUE, by = "DaysSinceOverbank", type = "response", resp = paste0(species),
                at = list(up_bar = c(paste0(up_bar)), DaysSinceOverbank = c(seq(min_days, max_days, by = 180))))
      #Summarise means and standard deviations
      emm_summary <- summary(as.mcmc(emm))
      #Turn into a dataframe 
      emm_df <- as.data.frame(emm_summary$statistics) %>%
        #Add various features
        mutate(
          WaterbodyName = river,
          Species = species,
          Distance = mean(mod$data$Distance[mod$data$up_bar == up_bar]),
          DaysSinceOverbank = seq(min_days, max_days, by = 180),
          n = nrow(mod$data[mod$data$up_bar == up_bar,])
        )
      df_name <- paste(species, up_bar)
      pred_list[[df_name]] <- emm_df
    }
  }
  saveRDS(pred_list, file = "pred_list_lac.rds")
  
} else if (i == 3){
pred_list <- list()
  mod_name <- as.character(names(fitted_mods_bars_water[i]))
  river <- substr(mod_name, 1, 3)
  mod <- fitted_mods_bars_water[[i]]
  #Finding name of the river
  #Defining each species present
  species_list <- mod$formula$responses
  #Defining river segments
  up_bar_levels <- unique(mod$data$up_bar)
  #For each species
  for(species in species_list){
    #In each river segment
    for (up_bar in up_bar_levels){
      species <- species
      up_bar <- up_bar
      max_days <- max(mod$data$DaysSinceOverbank[mod$data$up_bar == up_bar])
      min_days <- round(min(mod$data$DaysSinceOverbank[mod$data$up_bar == up_bar])/180)*180
      #Predict the abundance at 12 intervals, starting at 0 and ending at maximum days since overbank
      emm <- emmeans(mod, ~up_bar, epred = TRUE, by = "DaysSinceOverbank", type = "response", resp = paste0(species),
                at = list(up_bar = c(paste0(up_bar)), DaysSinceOverbank = c(seq(min_days, max_days, by = 180))))
      #Summarise means and standard deviations
      emm_summary <- summary(as.mcmc(emm))
      #Turn into a dataframe 
      emm_df <- as.data.frame(emm_summary$statistics) %>%
        #Add various features
        mutate(
          WaterbodyName = river,
          Species = species,
          Distance = mean(mod$data$Distance[mod$data$up_bar == up_bar]),
          DaysSinceOverbank = seq(min_days, max_days, by = 180),
          n = nrow(mod$data[mod$data$up_bar == up_bar,])
        )
      df_name <- paste(species, up_bar)
      pred_list[[df_name]] <- emm_df
    }
  }
  
  saveRDS(pred_list, file = "pred_list_mac.rds")
  
} else if (i == 4){
pred_list <- list()
  mod_name <- as.character(names(fitted_mods_bars_water[i]))
  river <- substr(mod_name, 1, 3)
  mod <- fitted_mods_bars_water[[i]]
  #Finding name of the river
  #Defining each species present
  species_list <- mod$formula$responses
  #Defining river segments
  up_bar_levels <- unique(mod$data$up_bar)
  #For each species
  for(species in species_list){
    #In each river segment
    for (up_bar in up_bar_levels){
      species <- species
      up_bar <- up_bar
      max_days <- max(mod$data$DaysSinceOverbank[mod$data$up_bar == up_bar])
      min_days <- round(min(mod$data$DaysSinceOverbank[mod$data$up_bar == up_bar])/180)*180
      #Predict the abundance at 12 intervals, starting at 0 and ending at maximum days since overbank
      emm <- emmeans(mod, ~up_bar, epred = TRUE, by = "DaysSinceOverbank", type = "response", resp = paste0(species),
                at = list(up_bar = c(paste0(up_bar)), DaysSinceOverbank = c(seq(min_days, max_days, by = 180))))
      #Summarise means and standard deviations
      emm_summary <- summary(as.mcmc(emm))
      #Turn into a dataframe 
      emm_df <- as.data.frame(emm_summary$statistics) %>%
        #Add various features
        mutate(
          WaterbodyName = river,
          Species = species,
          Distance = mean(mod$data$Distance[mod$data$up_bar == up_bar]),
          DaysSinceOverbank = seq(min_days, max_days, by = 180),
          n = nrow(mod$data[mod$data$up_bar == up_bar,])
        )
      df_name <- paste(species, up_bar)
      pred_list[[df_name]] <- emm_df
    }
  }
  saveRDS(pred_list, file = "pred_list_nam.rds")
  
} else if (i == 5){
pred_list <- list()
  mod_name <- as.character(names(fitted_mods_bars_water[i]))
  river <- substr(mod_name, 1, 3)
  mod <- fitted_mods_bars_water[[i]]
  #Finding name of the river
  #Defining each species present
  species_list <- mod$formula$responses
  #Defining river segments
  up_bar_levels <- unique(mod$data$up_bar)
  #For each species
  for(species in species_list){
    #In each river segment
    for (up_bar in up_bar_levels){
      species <- species
      up_bar <- up_bar
      max_days <- max(mod$data$DaysSinceOverbank[mod$data$up_bar == up_bar])
      min_days <- round(min(mod$data$DaysSinceOverbank[mod$data$up_bar == up_bar])/180)*180
      #Predict the abundance at 12 intervals, starting at 0 and ending at maximum days since overbank
      emm <- emmeans(mod, ~up_bar, epred = TRUE, by = "DaysSinceOverbank", type = "response", resp = paste0(species),
                at = list(up_bar = c(paste0(up_bar)), DaysSinceOverbank = c(seq(min_days, max_days, by = 180))))
      #Summarise means and standard deviations
      emm_summary <- summary(as.mcmc(emm))
      #Turn into a dataframe 
      emm_df <- as.data.frame(emm_summary$statistics) %>%
        #Add various features
        mutate(
          WaterbodyName = river,
          Species = species,
          Distance = mean(mod$data$Distance[mod$data$up_bar == up_bar]),
          DaysSinceOverbank = seq(min_days, max_days, by = 180),
          n = nrow(mod$data[mod$data$up_bar == up_bar,])
        )
      df_name <- paste(species, up_bar)
      pred_list[[df_name]] <- emm_df
    }
  }
  
  saveRDS(pred_list, file = "pred_list_gwy.rds")
} else if (i == 6){
pred_list <- list()
  mod_name <- as.character(names(fitted_mods_bars_water[i]))
  river <- substr(mod_name, 1, 3)
  mod <- fitted_mods_bars_water[[i]]
  #Finding name of the river
  #Defining each species present
  species_list <- mod$formula$responses
  #Defining river segments
  up_bar_levels <- unique(mod$data$up_bar)
  #For each species
  for(species in species_list){
    #In each river segment
    for (up_bar in up_bar_levels){
      species <- species
      up_bar <- up_bar
      max_days <- max(mod$data$DaysSinceOverbank[mod$data$up_bar == up_bar])
      min_days <- round(min(mod$data$DaysSinceOverbank[mod$data$up_bar == up_bar])/180)*180
      #Predict the abundance at 12 intervals, starting at 0 and ending at maximum days since overbank
      emm <- emmeans(mod, ~up_bar, epred = TRUE, by = "DaysSinceOverbank", type = "response", resp = paste0(species),
                at = list(up_bar = c(paste0(up_bar)), DaysSinceOverbank = c(seq(min_days, max_days, by = 180))))
      #Summarise means and standard deviations
      emm_summary <- summary(as.mcmc(emm))
      #Turn into a dataframe 
      emm_df <- as.data.frame(emm_summary$statistics) %>%
        #Add various features
        mutate(
          WaterbodyName = river,
          Species = species,
          Distance = mean(mod$data$Distance[mod$data$up_bar == up_bar]),
          DaysSinceOverbank = seq(min_days, max_days, by = 180),
          n = nrow(mod$data[mod$data$up_bar == up_bar,])
        )
      df_name <- paste(species, up_bar)
      pred_list[[df_name]] <- emm_df
    }
  }
  
  saveRDS(pred_list, file = "pred_list_dum.rds")
}

pred_list <- list(pred_list_mur <- readRDS("pred_list_mur_old.rds"),
                  pred_list_lac <- readRDS("pred_list_lac_old.rds"),
                  pred_list_mac <- readRDS("pred_list_mac_old.rds"),
                  pred_list_nam <- readRDS("pred_list_nam_old.rds"),
                  pred_list_gwy <- readRDS("pred_list_gwy_old.rds"),
                  pred_list_dum <- readRDS("pred_list_dum_old.rds"))

pred_list_filtered <- list()
for (i in seq_along(pred_list)) {
  pred_df <- do.call(rbind, pred_list[[i]]) %>%
    rownames_to_column(var = "RowNames") %>%
    mutate(up_bar = gsub("^[^ ]+ (.+?)\\..*", "\\1", RowNames)) %>%
    dplyr::select(-RowNames) %>%
    mutate(Rank = dense_rank(Distance)) %>%
    group_by(up_bar)
  
  river <- first(pred_df$WaterbodyName)
  
  unique_days <- unique(pred_df$DaysSinceOverbank)
  
  pred_list_temp <- list()
  
  for (day in unique_days) {
    # Filter the dataframe for the current DaysSinceOverbank value
    subset_df <- pred_df[pred_df$DaysSinceOverbank == day, ]
    
    # Check if there is more than one unique value of up_bar
    if (length(unique(subset_df$up_bar)) > 1) {
      pred_list_temp[[paste0(day, river)]] <- subset_df
    }
  }
  
  pred_list_filtered[[i]] <- pred_list_temp
}

pred_list_filtered <- unlist(pred_list_filtered, recursive = FALSE)

pairwise_df <- data.frame()

# Iterate over data frames in the list
for(df in pred_list_filtered) {
  
  # Iterate over unique values of "Rank"
  for(rank_value in unique(df$Rank)) {
  
    # Get the rows with current rank and next rank
    current_rank_rows <- df[df$Rank == rank_value, ]
    next_rank_rows <- df[df$Rank == (rank_value + 1), ]
  
    # Iterate over unique species within current rank
    for(species_value in unique(current_rank_rows$Species)) {
    
      # Filter rows for the current species
      current_rows <- current_rank_rows[current_rank_rows$Species == species_value, ]
      next_rows <- next_rank_rows[next_rank_rows$Species == species_value, ]
    
      # Check if there are rows in both current and next rank for the species
      if(nrow(current_rows) > 0 & nrow(next_rows) > 0) {
      
        # Calculate log-response ratio difference and extract up_bar values
        LRR <- abs(log(current_rows$Mean / next_rows$Mean))
        LRR_SE <- sqrt((current_rows$SD/(current_rows$n*current_rows$Mean)) + (next_rows$SD/(next_rows$n*next_rows$Mean)))
        Level1 <- current_rows$up_bar
        Level2 <- next_rows$up_bar
        Level1Abund <- current_rows$Mean
        Level2Abund <- next_rows$Mean
        Level1SD <- current_rows$SD
        Level2SD <- next_rows$SD
        Level1SE <- current_rows$SD/sqrt(current_rows$n)
        Level2SE <- next_rows$SD/sqrt(next_rows$n)
        Level1n <- current_rows$n
        Level2n <- next_rows$n
        River <- current_rows$WaterbodyName
        Species <- current_rows$Species
        DaysSinceOverbank <- current_rows$DaysSinceOverbank
      
        # Create a temporary dataframe for the current iteration
        temp_df <- data.frame(LRR = LRR,
                              LRR_SE = LRR_SE,
                              Level1 = Level1,
                              Level2 = Level2,
                              Level1Abund = Level1Abund,
                              Level2Abund = Level2Abund,
                              Level1SD = Level1SD,
                              Level2SD = Level2SD,
                              Level1n = Level1n,
                              Level2n = Level2n,
                              Level1SE = Level1SE,
                              Level2SE = Level2SE,
                              River = River,
                              Species = Species,
                              DaysSinceOverbank = DaysSinceOverbank)
      
        # Append the temporary dataframe to the pairwise_df dataframe
        pairwise_df <- rbind(pairwise_df, temp_df)
      }
    }
  }
}

levels(as.factor(pairwise_df$Species))

meta_df <- pairwise_df %>%
  mutate_at(c("Level1", "Level2", "Species", "River"), as.factor) %>%
  mutate(WaterbodyName = River)
meta_df$River <- recode_factor(meta_df$River,
                `mur` = "Murrumbidgee",
                `lac` = "Lachlan",
                `mac` = "Macquarie",
                `nam` = "Namoi", 
                `gwy` = "Gwydir",
                `dum` = "Dumaresq/Mole")
meta_df$WaterbodyName <- recode_factor(meta_df$WaterbodyName,
                `mur` = "Murrumbidgee River",
                `lac` = "Lachlan River",
                `mac` = "Macquarie River",
                `nam` = "Namoi River", 
                `gwy` = "Gwydir River",
                `dum` = "Dumaresq/Mole River")

#comparison_df <- unique(meta_df_global[, c("Level1", "Level2", "River")])
#write_csv(comparison_df, file = "comparison_df.csv")

#Reading in data about what comparisons represent and merging with meta_df_conditional
bars_df_formatted <- read_csv("bars_formatted.csv")
comps <- read_csv("comparison_df_formatted.csv") %>%
  rowwise() %>%
  mutate(Barriers = list(c(Barrier1, Barrier2, Barrier3, Barrier4, Barrier5, Barrier6, Barrier7) %>%
                          na.omit())) %>%
  left_join(bars_df_formatted[,c("Name", "FragmentLength", "WaterbodyName")], by = c("Level1" = "Name", "WaterbodyName")) %>%
  rename(Level1FragmentLength = "FragmentLength") %>%
  left_join(bars_df_formatted[,c("Name", "FragmentLength", "WaterbodyName")], by = c("Level2" = "Name", "WaterbodyName")) %>%
  rename(Level2FragmentLength = "FragmentLength") %>%
  mutate(FragmentLengthDifference = abs(Level1FragmentLength - Level2FragmentLength))

#Finding number of barriers for each river
comps %>%
  group_by(WaterbodyName) %>%
  summarize(Sum_BarrierCount = sum(BarrierCount))

#Formatting
comps_formatted <- comps %>%  
  tidyr::unnest(Barriers) %>%
  left_join(bars_df_formatted, by = c("Barriers" = "Name", "WaterbodyName")) %>%
  mutate_at(c("Height", "Width", "Headloss", "PriorityNum"), as.numeric) %>%
  group_by(Level1, Level2, Barrier) %>%
  summarise(BarrierCount = first(BarrierCount),
            MaxHeight = ifelse(all(is.na(Height)), 0, max(Height, na.rm = TRUE)),
            MaxWidth = ifelse(all(is.na(Width)), 0, max(Width, na.rm = TRUE)),
            HeightSum = sum(Height, na.rm = TRUE),
            HeadlossSum = sum(Headloss, na.rm = TRUE),
            FishwayCount = sum(Fishway.Pr, na.rm = TRUE),
            FishwayProp = sum(Fishway.Pr, na.rm = TRUE)/BarrierCount,
            RemediatedCount = sum(Remediated, na.rm = TRUE),
            RemediatedProp = sum(Remediated, na.rm = TRUE)/BarrierCount,
            PrioritySum = sum(PriorityNum, na.rm = TRUE),
            BarrierType = ifelse(BarrierCount == 1, first(BarrierType), "Multiple Barriers"),
            StructureType = ifelse(BarrierCount == 1, first(StructureType), "Multiple Barriers"),
            FragmentLengthDifference = first(FragmentLengthDifference))

meta_df <- merge(meta_df, comps_formatted, by = c("Level1", "Level2"), all = TRUE)

#Loading trait data and adding to meta_df_conditional

traits <- read_csv("Data/traits.csv")

meta_df <- merge(meta_df, traits, by = c("Species"))

#Renaming species

meta_df$SpeciesNames <- recode_factor(meta_df$Species,
                                      Carpgudgeonspeciescomplex = "Carp gudgeon",
                                      Commoncarp = "Common carp",
                                      Unspeckedhardyhead = "Unspecked hardyhead",
                                      EasternGambusia = "Eastern mosquitofish",
                                      Goldenperch = "Golden perch",
                                      Murraycod = "Murray cod",
                                      Australiansmelt = "Australian smelt",
                                      Goldfish = "Goldfish & hybrids",
                                      Bonyherring = "Bony herring",
                                      Redfin = "Redfin perch",
                                      Rainbowtrout = "Rainbow trout",
                                      Macquarieperch = "Macquarie perch",
                                      Silverperch = "Silver perch",
                                      Troutcod = "Trout cod & hybrids",
                                      NorthernRiverBlackfish = "River blackfish",
                                      Rainbowfishspeciescomplex = "Rainbowfish",
                                      Flatheadgudgeonspeciescomplex = "Flathead gudgeon",
                                      Freshwatercatfish = "Freshwater catfish",
                                      Spangledperch = "Spangled perch",
                                      Southernpurplespottedgudgeon = "Southern purple spotted gudgeon",
                                      Oliveperchlet = "Olive perchlet")

traits$SpeciesNames <- recode_factor(traits$Species,
                                           Carpgudgeonspeciescomplex = "Carp gudgeon",
                                      Commoncarp = "Common carp",
                                      Unspeckedhardyhead = "Unspecked hardyhead",
                                      EasternGambusia = "Eastern mosquitofish",
                                      Goldenperch = "Golden perch",
                                      Murraycod = "Murray cod",
                                      Australiansmelt = "Australian smelt",
                                      Goldfish = "Goldfish & hybrids",
                                      Bonyherring = "Bony herring",
                                      Redfin = "Redfin perch",
                                      Rainbowtrout = "Rainbow trout",
                                      Macquarieperch = "Macquarie perch",
                                      Silverperch = "Silver perch",
                                      Troutcod = "Trout cod & hybrids",
                                      NorthernRiverBlackfish = "River blackfish",
                                      Rainbowfishspeciescomplex = "Rainbowfish",
                                      Flatheadgudgeonspeciescomplex = "Flathead gudgeon",
                                      Freshwatercatfish = "Freshwater catfish",
                                      Spangledperch = "Spangled perch",
                                      Southernpurplespottedgudgeon = "Southern purple spotted gudgeon",
                                      Oliveperchlet = "Olive perchlet")

#Setting order of factors

meta_df <- meta_df %>%
  mutate(MovementDistance = factor(MovementDistance, levels = c("Short", "Medium", "Long"), ordered = TRUE),
    Longevity = factor(Longevity, levels = c("Short", "Medium", "Long"), ordered = TRUE),
    RemediatedProp = factor(case_when(
        RemediatedProp == 0 ~ "None",
        RemediatedProp == 0.5 ~ "Half",
        RemediatedProp == 1 ~ "Full"),
      levels = c("None","Half", "Full"), ordered = TRUE))

#Adding missing fragment length to meta_df

meta_df <- meta_df %>%
  mutate(FragmentLengthDifference = ifelse(Barrier == "Bevendale Creek Road Crossing", 38094.84517, FragmentLengthDifference))

levels(meta_df$SpeciesNames)

#Water stuff

modelled_water <- read_csv("modelled_water.csv") %>%
  distinct() %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
  filter(DateNum >= 30498)
modelled_water <- split(modelled_water, modelled_water$OverbankGauge)

#Finding days since overbank and days since bankfull for each day

for(i in seq_along(modelled_water)){
  df <- modelled_water[[i]]
  gauge <- first(df$OverbankGauge)
  print(paste0("Processing gauge: ", gauge))

  overbank <- lookup_table_overbank$Overbank[lookup_table_overbank$OverbankGauge == gauge]

  if (!is.na(overbank) && length(overbank) > 0) {
    df$DaysSinceOverbank <- 0
    for (j in 2:nrow(df)) {
      if (!is.na(df$Discharge[j]) && df$Discharge[j] >= overbank[1]) {
        df$DaysSinceOverbank[j] <- 0
      } else {
        df$DaysSinceOverbank[j] <- df$DaysSinceOverbank[j-1] + 1
      }
    }
  } else {
    df$DaysSinceOverbank <- NA
  }
  
  modelled_water[[i]] <- df
}

modelled_water <- do.call(rbind, modelled_water)

#Truncating water dataset to align with modelled water

unique_gauges <- modelled_water %>%
  distinct(OverbankGauge)

filtered_gauge_dfs <- list()
for (level in unique_gauges$OverbankGauge) {
  available_dates <- modelled_water %>%
    filter(OverbankGauge == level) %>%
    distinct(DateNum) %>%
    pull(DateNum)
  
  filtered_df <- water %>%
    filter(OverbankGauge == level, DateNum %in% available_dates)
  
  filtered_gauge_dfs[[as.character(level)]] <- filtered_df
  
}

truncated_water <- do.call(rbind, filtered_gauge_dfs)

#Calculating summary statistics for water gauges

water_summary <- water %>%
  filter(DateNum >= "34335") %>%
  group_by(OverbankGauge) %>%
  summarise(MaxDaysSinceOverbank = max(DaysSinceOverbank),
            MeanDaysSinceOverbank = mean(DaysSinceOverbank),
            TotalOverbankDays = sum(DaysSinceOverbank == 0, na.rm = TRUE)) %>%
  merge(water_gauges, by = "OverbankGauge") %>%
  mutate(Level1 = up_bar)

modelled_water_summary <- modelled_water %>%
  filter(DateNum >= "34335") %>%
  group_by(OverbankGauge) %>%
  summarise(MaxDaysSinceOverbank = max(DaysSinceOverbank),
            MeanDaysSinceOverbank = mean(DaysSinceOverbank),
            TotalOverbankDays = sum(DaysSinceOverbank == 0, na.rm = TRUE)) %>%
  merge(water_gauges, by = "OverbankGauge") %>%
  mutate(Level1 = up_bar) %>%
  left_join(truncated_water_summary, by = c("OverbankGauge", "WaterbodyName", "up_bar", "PlanningUnit", "OverbankGaugeID", "Level1")) %>%
  mutate(
    MaxDaysSinceOverbankDiff = MaxDaysSinceOverbank.x - MaxDaysSinceOverbank.y,
    MeanDaysSinceOverbankDiff = MeanDaysSinceOverbank.x - MeanDaysSinceOverbank.y,
    TotalOverbankDaysDiff = TotalOverbankDays.x - TotalOverbankDays.y
  ) %>%
  select(
    -MaxDaysSinceOverbank.y, -MeanDaysSinceOverbank.y, -TotalOverbankDays.y,
    -ends_with(".x")
  )

meta_df <- merge(meta_df, water_summary, by = c("Level1", "WaterbodyName"))
meta_df <- merge(meta_df, modelled_water_summary[,c("Level1", "WaterbodyName", "MaxDaysSinceOverbankDiff", "MeanDaysSinceOverbankDiff", "TotalOverbankDaysDiff")], by = c("Level1", "WaterbodyName"), all.x = TRUE) 

#Applying Box-Cox transformation to LRR
bc <- boxcox(meta_df$LRR ~ 1)
lambda <- bc$x[which.max(bc$y)]
lambda

meta_df$LRR_trans <- (meta_df$LRR^lambda-1)/lambda
hist(meta_df$LRR_trans)

#Writing csv
write_csv(meta_df, file = "meta_df.csv")
