###R Script 1
###This code is used to import and clean electrofishing data

library(tidyverse) #Loading relevant packages

electro_data <- rbind(read_csv("Data/electrofishing_data.csv"), read_csv("Data/electrofishing_data2.csv")) %>% #Importing data
  distinct() %>% #Removing duplicated observations found in both csv files
  filter(CommonName != "Zero catch at DRY site" & CommonName != "Yabby" & CommonName != "Freshwater prawn" & CommonName != "Shrimp" & CommonName != "Murray short-necked turtle" & CommonName != "Murray crayfish" & CommonName != "Eastern long-necked tortoise" & CommonName != "Platypus" & CommonName != "Unknown Shrimp" & CommonName != "Unidentified Euastacus" & ElectrofishingDuration != "NA" & ElectrofishingDuration >= 10 & ElectrofishingDuration <= 1000 & WaterbodyName != "Macintyre River") %>% #Removing dry sites (no electrofishing occured), catches of non-fish taxa, observations without an appropriate sampling effort, and observations from Macintyre River (too few observations)
  droplevels() %>%
  group_by(Method, SiteName, SiteID, Date, DateNum, SampleID, OperationID, CommonName, SampleLongitude, SampleLatitude, WaterbodyName, ElectrofishingDuration) %>% #Grouping by relevant variables, and summing catches from individual electrofishing shots
  summarise(Caught = sum(Caught)) %>%
  pivot_wider(names_from = CommonName, values_from = Caught, values_fill = 0) %>%
  dplyr::select(-"No catch") %>% #Removing no-catch fish species, while retaining observations with no fish catches
  mutate(`Goldfish` = `Goldfish` + `Common carp - Goldfish hybrid`) %>% #Pooling observations from hybrids to the main species, and between closely related rare taxa
  dplyr::select(-`Common carp - Goldfish hybrid`) %>%
  mutate(`Carp-gudgeon species complex` = `Carp-gudgeon species complex` + `Western carp-gudgeon` + `Lake's carp-gudgeon`) %>%
  dplyr::select(-`Western carp-gudgeon`) %>%
  dplyr::select(-`Lake's carp-gudgeon`) %>%
  mutate(`Murray cod` = `Murray cod` + `Unidentified Maccullochella cod`) %>%
  dplyr::select(-`Unidentified Maccullochella cod`) %>%
  mutate(`Trout cod` = `Trout cod` + `Trout Cod / Murray Cod Hybrid`) %>%
  dplyr::select(-`Trout Cod / Murray Cod Hybrid`) %>%
  mutate(`Rainbowfish species complex` = `Murray-Darling rainbowfish` + `Crimsonspotted  rainbowfish`) %>%
  dplyr::select(-`Crimsonspotted  rainbowfish`) %>%
  dplyr::select(-`Murray-Darling rainbowfish`) %>%
  mutate(`Flathead gudgeon species complex` = `Flathead gudgeon` + `Dwarf flathead gudgeon`) %>%
  dplyr::select(-`Flathead gudgeon`) %>%
  dplyr::select(-`Dwarf flathead gudgeon`) %>%
  mutate(Season = case_when(month(Date) %in% 3:5 ~ "Autumn", #Creating season, month, and coordinates columns
                            month(Date) %in% 6:8 ~ "Winter",
                            month(Date) %in% 9:11 ~ "Spring",
                            TRUE ~ "Summer")) %>%
  mutate(Month = month(Date)) %>%
  mutate(LonLat = paste(SampleLongitude, SampleLatitude, sep = "_"))

electro_data <- subset(electro_data, !(Method == "BPE" & WaterbodyName == "Murrumbidgee River")) #Removing backpack electrofishing for the Murrumbidgee (too few observations)

df_sites <- electro_data %>% #Making a version of fish abundance dataset with one observation per site
  group_by(LonLat) %>%
  summarize(across(everything(), first))

write_csv(electro_data, file = "electo_data_clean.csv") #Writing cleaned electrofishing data
write_csv(df_sites, file = "df_sites.csv") #Writing electrofishing sites to csv to be imported into QGIS and used for spatial processing
