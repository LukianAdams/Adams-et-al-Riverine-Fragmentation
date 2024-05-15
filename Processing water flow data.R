#Loading necessary packages
library(tidyverse)

#Reading electrofishing data
df_list <- readRDS("Data/df_list.rds")

#Reading data on position of water gauges
water_gauges <- read_csv("Data/water_gauges.csv")

#Reading water flow data
water <- read_csv("Data/water.csv")
water$Date <- as.Date(water$Date, format = "%d/%m/%Y")
water <- split(water, water$WaterGauge)

#Finding days since overbank and days since bankfull for each day
lookup_table_overbank <- water_gauges %>%
  group_by(OverbankGauge) %>%
  summarise(Overbank = first(Overbank)) %>%
  ungroup()

for(i in seq_along(water)){
  df <- water[[i]]
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
  
  water[[i]] <- df
}


lookup_table_bankfull <- water_gauges %>%
  group_by(BankfullGauge) %>%
  summarise(Bankfull = first(Bankfull)) %>%
  ungroup()

for(i in seq_along(water)){
  df <- water[[i]]
  gauge <- first(df$BankfullGauge)

  bankfull <- lookup_table_bankfull$Bankfull[lookup_table_bankfull$BankfullGauge == gauge]

  if (!is.na(bankfull) && length(bankfull) > 0) {
     df$DaysSinceBankfull <- ifelse(!is.na(df$Discharge) & df$Discharge >= bankfull[1], 
                                   0, 
                                   NA)
   } else {
     df$DaysSinceBankfull <- NA
   }
   
   df$DaysSinceBankfull <- ave(df$DaysSinceBankfull, cumsum(!is.na(df$DaysSinceBankfull)), FUN = function(x) seq_along(x) - 1)
   df$DaysSinceBankfull_Scaled <- scale(df$DaysSinceBankfull)[,1]
   
   water[[i]] <- df
}

lookup_table_overbank <- water_gauges %>%
  group_by(OverbankGauge) %>%
  summarise(Overbank = first(Overbank)) %>%
  ungroup()

for(i in seq_along(water)){
  df <- water[[i]]
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
  
  water[[i]] <- df
}

water <- do.call(rbind, water)

#Matching gauge to site
add_WaterGauge <- function(df) {
  merged_df <- merge(df, water_gauges, by = c("up_bar", "WaterbodyName"), all.x = TRUE)
  return(merged_df)
}

df_list <- lapply(df_list, add_WaterGauge)

#Adding overbank and bankfull times to electrofishing dataframe
add_water_data <- function(df) {
  df <- left_join(df, water %>% select(DateNum, WaterbodyName, OverbankGauge, DaysSinceOverbank), 
                  by = c("DateNum", "WaterbodyName", "OverbankGauge"))
  
  df <- left_join(df, water %>% select(DateNum, WaterbodyName, BankfullGauge, DaysSinceBankfull), 
                  by = c("DateNum", "WaterbodyName", "BankfullGauge"))
  return(df)
}

df_list <- lapply(df_list, add_water_data)

#Adding ENSO metrics to electrofishing dataframes
enso <- read_csv("Data/ENSO.csv")

merge_enso <- function(df) {
  merge_columns <- "DateNum"
  new_columns <- c("ONI", "Phase", "SOI", "NPGO")
  merged_df <- merge(df, enso[c(merge_columns, new_columns)], by = merge_columns, all.x = TRUE)
  return(merged_df)
}

df_list2 <- lapply(df_list, merge_enso)

#Saving updated list of electrofishing dataframes
saveRDS(df_list2, file = "Data/df_list2.rds")
