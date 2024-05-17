###Note - This code is computationally intensive, and is set up to run as a batch job on a high performance computing cluster with 32 cores per job. Expect a wait time ranging from 1 hour - 2 weeks per job depending on the number of observations and the number of fish taxa in your data

#Loading relevant packages
library(brms)
library(coda)
library(tidyverse)
library(readr)
library(cmdstanr)

#Loading data
mur_df <- read_csv("mur_df.csv") %>%
  mutate_at(c("SampleID", "OperationID", "up_bar", "Method"), as.factor)
lac_df <- read_csv("lac_df.csv") %>%
  mutate_at(c("SampleID", "OperationID", "up_bar", "Method"), as.factor)
mac_df <- read_csv("mac_df.csv") %>%
  mutate_at(c("SampleID", "OperationID", "up_bar", "Method"), as.factor)
gwy_df <- read_csv("gwy_df.csv") %>%
  mutate_at(c("SampleID", "OperationID", "up_bar", "Method"), as.factor)
nam_df <- read_csv("nam_df.csv") %>%
  mutate_at(c("SampleID", "OperationID", "up_bar", "Method"), as.factor)
dum_df <- read_csv("dum_df.csv") %>%
  mutate_at(c("SampleID", "OperationID", "up_bar", "Method"), as.factor)

#Setting priors for 'full' and 'no overbank' models
#Beta parameters (fixed effects) - Students T distribution with 1.5 degrees of freedom, mean of 0 and standard deviation of 0.5
#Negbinomial shape parameters - Gamma distribution with k = 0.05 and θ = 0.05
#Standard deviation parameters - Students T distribution with 3 degrees of freedom, mean of 0 and standard deviation of 1
#Default priors for other parameters
mur_priors <- c(set_prior("student_t(1.5,0,0.5)", class = "b", resp = c("Carpgudgeonspeciescomplex", "Commoncarp", "Unspeckedhardyhead", "EasternGambusia", "Goldenperch", "Murraycod", "Australiansmelt", "Goldfish", "Bonyherring", "Redfin", "Rainbowtrout", "Macquarieperch", "Silverperch", "Troutcod", "NorthernRiverBlackfish", "Rainbowfishspeciescomplex", "Flatheadgudgeonspeciescomplex")),
                set_prior("gamma(0.05,0.05)", class = "shape", resp = c("Carpgudgeonspeciescomplex", "Commoncarp", "Unspeckedhardyhead", "EasternGambusia", "Goldenperch", "Murraycod", "Australiansmelt", "Goldfish", "Bonyherring", "Redfin", "Rainbowtrout", "Macquarieperch", "Silverperch", "Troutcod", "NorthernRiverBlackfish", "Rainbowfishspeciescomplex", "Flatheadgudgeonspeciescomplex")),
                set_prior("student_t(3,0,1)", class = "sd", resp = c("Carpgudgeonspeciescomplex", "Commoncarp", "Unspeckedhardyhead", "EasternGambusia", "Goldenperch", "Murraycod", "Australiansmelt", "Goldfish", "Bonyherring", "Redfin", "Rainbowtrout", "Macquarieperch", "Silverperch", "Troutcod", "NorthernRiverBlackfish", "Rainbowfishspeciescomplex", "Flatheadgudgeonspeciescomplex")))

lac_priors <- c(set_prior("student_t(1.5,0,0.5)", class = "b", resp = c("Carpgudgeonspeciescomplex", "Commoncarp", "Unspeckedhardyhead", "EasternGambusia", "Goldenperch", "Murraycod", "Australiansmelt", "Goldfish", "Bonyherring", "Redfin", "Flatheadgudgeonspeciescomplex")),
                set_prior("gamma(0.05,0.05)", class = "shape", resp = c("Carpgudgeonspeciescomplex", "Commoncarp", "Unspeckedhardyhead", "EasternGambusia", "Goldenperch", "Murraycod", "Australiansmelt", "Goldfish", "Bonyherring", "Redfin", "Flatheadgudgeonspeciescomplex")),
                set_prior("student_t(3,0,1)", class = "sd", resp = c("Carpgudgeonspeciescomplex", "Commoncarp", "Unspeckedhardyhead", "EasternGambusia", "Goldenperch", "Murraycod", "Australiansmelt", "Goldfish", "Bonyherring", "Redfin", "Flatheadgudgeonspeciescomplex")))

mac_priors <- c(set_prior("student_t(1.5,0,0.5)", class = "b", resp = c("Carpgudgeonspeciescomplex", "Commoncarp", "Unspeckedhardyhead", "Freshwatercatfish", "EasternGambusia", "Goldenperch", "Murraycod", "Australiansmelt", "Goldfish", "Spangledperch", "Bonyherring", "Redfin", "Rainbowfishspeciescomplex", "Flatheadgudgeonspeciescomplex")),
                set_prior("gamma(0.05,0.05)", class = "shape", resp = c("Carpgudgeonspeciescomplex", "Commoncarp", "Unspeckedhardyhead", "Freshwatercatfish", "EasternGambusia", "Goldenperch", "Murraycod", "Australiansmelt", "Goldfish", "Spangledperch", "Bonyherring", "Redfin", "Rainbowfishspeciescomplex", "Flatheadgudgeonspeciescomplex")),
                set_prior("student_t(3,0,1)", class = "sd", resp = c("Carpgudgeonspeciescomplex", "Commoncarp", "Unspeckedhardyhead", "Freshwatercatfish", "EasternGambusia", "Goldenperch", "Murraycod", "Australiansmelt", "Goldfish", "Spangledperch", "Bonyherring", "Redfin", "Rainbowfishspeciescomplex", "Flatheadgudgeonspeciescomplex")))

gwy_priors <- c(set_prior("student_t(1.5,0,0.5)", class = "b", resp = c("Carpgudgeonspeciescomplex", "Commoncarp", "Unspeckedhardyhead", "Freshwatercatfish", "EasternGambusia", "Goldenperch", "Murraycod", "Australiansmelt", "Goldfish", "Spangledperch", "Bonyherring", "Redfin", "Rainbowfishspeciescomplex")),
                set_prior("student_t(3,0,1)", class = "shape", resp = c("Carpgudgeonspeciescomplex", "Commoncarp", "Unspeckedhardyhead", "Freshwatercatfish", "EasternGambusia", "Goldenperch", "Murraycod", "Australiansmelt", "Goldfish", "Spangledperch", "Bonyherring", "Redfin", "Rainbowfishspeciescomplex")),
                set_prior("student_t(3,0,1)", class = "sd", resp = c("Carpgudgeonspeciescomplex", "Commoncarp", "Unspeckedhardyhead", "Freshwatercatfish", "EasternGambusia", "Goldenperch", "Murraycod", "Australiansmelt", "Goldfish", "Spangledperch", "Bonyherring", "Redfin", "Rainbowfishspeciescomplex")))

nam_priors <- c(set_prior("student_t(1.5,0,0.5)", class = "b", resp = c("Carpgudgeonspeciescomplex", "Commoncarp", "Unspeckedhardyhead", "Freshwatercatfish", "EasternGambusia", "Goldenperch", "Murraycod", "Australiansmelt", "Goldfish", "Spangledperch", "Bonyherring", "Silverperch", "Rainbowfishspeciescomplex")),
                set_prior("gamma(0.05,0.05)", class = "shape", resp = c("Carpgudgeonspeciescomplex", "Commoncarp", "Unspeckedhardyhead", "Freshwatercatfish", "EasternGambusia", "Goldenperch", "Murraycod", "Australiansmelt", "Goldfish", "Spangledperch", "Bonyherring", "Silverperch", "Rainbowfishspeciescomplex")),
                set_prior("student_t(3,0,1)", class = "sd", resp = c("Carpgudgeonspeciescomplex", "Commoncarp", "Unspeckedhardyhead", "Freshwatercatfish", "EasternGambusia", "Goldenperch", "Murraycod", "Australiansmelt", "Goldfish", "Spangledperch", "Bonyherring", "Silverperch", "Rainbowfishspeciescomplex")))

dum_priors <- c(set_prior("student_t(1.5,0,0.5)", class = "b", resp = c("Carpgudgeonspeciescomplex", "Commoncarp", "Unspeckedhardyhead", "Freshwatercatfish", "EasternGambusia", "Goldenperch", "Murraycod", "Southernpurplespottedgudgeon", "Australiansmelt", "Goldfish", "Spangledperch", "Bonyherring", "Oliveperchlet", "Rainbowfishspeciescomplex")),
                set_prior("gamma(0.05,0.05)", class = "shape", resp = c("Carpgudgeonspeciescomplex", "Commoncarp", "Unspeckedhardyhead", "Freshwatercatfish", "EasternGambusia", "Goldenperch", "Murraycod", "Southernpurplespottedgudgeon", "Australiansmelt", "Goldfish", "Spangledperch", "Bonyherring", "Oliveperchlet", "Rainbowfishspeciescomplex")),
                set_prior("student_t(3,0,1)", class = "sd", resp = c("Carpgudgeonspeciescomplex", "Commoncarp", "Unspeckedhardyhead", "Freshwatercatfish", "EasternGambusia", "Goldenperch", "Murraycod", "Southernpurplespottedgudgeon", "Australiansmelt", "Goldfish", "Spangledperch", "Bonyherring", "Oliveperchlet", "Rainbowfishspeciescomplex")))

#Setting priors for 'no barrier' models
#Negbinomial shape parameters - Gamma distribution with k = 0.05 and θ = 0.05
#Standard deviation parameters - Students T distribution with 3 degrees of freedom, mean of 0 and standard deviation of 1
#Default priors for other parameters
mur_priors_null <- c(set_prior("gamma(0.05,0.05)", class = "shape", resp = c("Carpgudgeonspeciescomplex", "Commoncarp", "Unspeckedhardyhead", "EasternGambusia", "Goldenperch", "Murraycod", "Australiansmelt", "Goldfish", "Bonyherring", "Redfin", "Rainbowtrout", "Macquarieperch", "Silverperch", "Troutcod", "NorthernRiverBlackfish", "Rainbowfishspeciescomplex", "Flatheadgudgeonspeciescomplex")),
                     set_prior("student_t(3,0,1)", class = "sd", resp = c("Carpgudgeonspeciescomplex", "Commoncarp", "Unspeckedhardyhead", "EasternGambusia", "Goldenperch", "Murraycod", "Australiansmelt", "Goldfish", "Bonyherring", "Redfin", "Rainbowtrout", "Macquarieperch", "Silverperch", "Troutcod", "NorthernRiverBlackfish", "Rainbowfishspeciescomplex", "Flatheadgudgeonspeciescomplex")))

lac_priors_null <- c(set_prior("gamma(0.05,0.05)", class = "shape", resp = c("Carpgudgeonspeciescomplex", "Commoncarp", "Unspeckedhardyhead", "EasternGambusia", "Goldenperch", "Murraycod", "Australiansmelt", "Goldfish", "Bonyherring", "Redfin", "Flatheadgudgeonspeciescomplex")),
                     set_prior("student_t(3,0,1)", class = "sd", resp = c("Carpgudgeonspeciescomplex", "Commoncarp", "Unspeckedhardyhead", "EasternGambusia", "Goldenperch", "Murraycod", "Australiansmelt", "Goldfish", "Bonyherring", "Redfin", "Flatheadgudgeonspeciescomplex")))

mac_priors_null <- c(set_prior("gamma(0.05,0.05)", class = "shape", resp = c("Carpgudgeonspeciescomplex", "Commoncarp", "Unspeckedhardyhead", "Freshwatercatfish", "EasternGambusia", "Goldenperch", "Murraycod", "Australiansmelt", "Goldfish", "Spangledperch", "Bonyherring", "Redfin", "Rainbowfishspeciescomplex", "Flatheadgudgeonspeciescomplex")),
                     set_prior("student_t(3,0,1)", class = "sd", resp = c("Carpgudgeonspeciescomplex", "Commoncarp", "Unspeckedhardyhead", "Freshwatercatfish", "EasternGambusia", "Goldenperch", "Murraycod", "Australiansmelt", "Goldfish", "Spangledperch", "Bonyherring", "Redfin", "Rainbowfishspeciescomplex", "Flatheadgudgeonspeciescomplex")))

gwy_priors_null <- c(set_prior("student_t(3,0,1)", class = "shape", resp = c("Carpgudgeonspeciescomplex", "Commoncarp", "Unspeckedhardyhead", "Freshwatercatfish", "EasternGambusia", "Goldenperch", "Murraycod", "Australiansmelt", "Goldfish", "Spangledperch", "Bonyherring", "Redfin", "Rainbowfishspeciescomplex")),
                     set_prior("student_t(3,0,1)", class = "sd", resp = c("Carpgudgeonspeciescomplex", "Commoncarp", "Unspeckedhardyhead", "Freshwatercatfish", "EasternGambusia", "Goldenperch", "Murraycod", "Australiansmelt", "Goldfish", "Spangledperch", "Bonyherring", "Redfin", "Rainbowfishspeciescomplex")))

nam_priors_null <- c(set_prior("gamma(0.05,0.05)", class = "shape", resp = c("Carpgudgeonspeciescomplex", "Commoncarp", "Unspeckedhardyhead", "Freshwatercatfish", "EasternGambusia", "Goldenperch", "Murraycod", "Australiansmelt", "Goldfish", "Spangledperch", "Bonyherring", "Silverperch", "Rainbowfishspeciescomplex")),
                     set_prior("student_t(3,0,1)", class = "sd", resp = c("Carpgudgeonspeciescomplex", "Commoncarp", "Unspeckedhardyhead", "Freshwatercatfish", "EasternGambusia", "Goldenperch", "Murraycod", "Australiansmelt", "Goldfish", "Spangledperch", "Bonyherring", "Silverperch", "Rainbowfishspeciescomplex")))

dum_priors_null <- c(set_prior("gamma(0.05,0.05)", class = "shape", resp = c("Carpgudgeonspeciescomplex", "Commoncarp", "Unspeckedhardyhead", "Freshwatercatfish", "EasternGambusia", "Goldenperch", "Murraycod", "Southernpurplespottedgudgeon", "Australiansmelt", "Goldfish", "Spangledperch", "Bonyherring", "Oliveperchlet", "Rainbowfishspeciescomplex")),
                     set_prior("student_t(3,0,1)", class = "sd", resp = c("Carpgudgeonspeciescomplex", "Commoncarp", "Unspeckedhardyhead", "Freshwatercatfish", "EasternGambusia", "Goldenperch", "Murraycod", "Southernpurplespottedgudgeon", "Australiansmelt", "Goldfish", "Spangledperch", "Bonyherring", "Oliveperchlet", "Rainbowfishspeciescomplex")))

#Setting job ID as i to run a batch job on Katana HPC
i = as.numeric(Sys.getenv('PBS_ARRAY_INDEX'))

#Running river models

if (i == 1){
  
  mur_bars_water_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, EasternGambusia, Goldenperch, Murraycod, Australiansmelt, Goldfish, Bonyherring, Redfin, Silverperch, Troutcod, NorthernRiverBlackfish, Rainbowfishspeciescomplex) 
                            ~ t2(scale(DateNum), scale(Distance), bs = c("ts", "ts"), k = 20, full = TRUE)  + up_bar + up_bar:scale(DaysSinceOverbank, center = FALSE) + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
  
  mur_bars_water_fit <- brm(mur_bars_water_bform, data = mur_df_filtered,
                      family = zero_inflated_negbinomial,
                      chains = 4, cores = 4,
                      iter = 5000, warmup = 2000, init = 0,
                      backend = "cmdstanr", threads = threading(8), seed = 123,
                      prior = mur_priors,  control = list(adapt_delta = 0.99, max_treedepth = 12),
                      save_pars = save_pars(all = TRUE))
  
  saveRDS(mur_bars_water_fit, file = "Output/Fitted Models/mur_bars_water_fit.rds")
    
} else if (i == 2){
  
  lac_bars_water_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, Unspeckedhardyhead, EasternGambusia, Goldenperch, Murraycod, Australiansmelt, Goldfish, Bonyherring, Redfin, Flatheadgudgeonspeciescomplex) 
                            ~ t2(scale(DateNum), scale(Distance), bs = c("ts", "ts"), k = 20, full = TRUE)  + up_bar + up_bar:scale(DaysSinceOverbank, center = FALSE) + (1|Method) + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
  
  lac_bars_water_fit <- brm(lac_bars_water_bform, data = lac_df_filtered,
                      family = zero_inflated_negbinomial,
                      chains = 4, cores = 4,
                      iter = 5000, warmup = 2000, init = 0,
                      backend = "cmdstanr", threads = threading(8), seed = 123,
                      prior = lac_priors,  control = list(adapt_delta = 0.99, max_treedepth = 12),
                      save_pars = save_pars(all = TRUE))
  
  saveRDS(lac_bars_water_fit, file = "Output/Fitted Models/lac_bars_water_fit.rds")
  
} else if (i == 3){
  
  mac_bars_water_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, Unspeckedhardyhead, Freshwatercatfish, EasternGambusia, Goldenperch, Murraycod, Australiansmelt, Goldfish, Spangledperch, Bonyherring, Redfin, Rainbowfishspeciescomplex, Flatheadgudgeonspeciescomplex) 
                            ~ t2(scale(DateNum), scale(Distance), bs = c("ts", "ts"), k = 20, full = TRUE)  + up_bar + up_bar:scale(DaysSinceOverbank, center = FALSE) + (1|Method) + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
  
  mac_bars_water_fit <- brm(mac_bars_water_bform, data = mac_df_filtered,
                      family = zero_inflated_negbinomial,
                      chains = 4, cores = 4,
                      iter = 5000, warmup = 2000, init = 0,
                      backend = "cmdstanr", threads = threading(8), seed = 123,
                      prior = mac_priors,  control = list(adapt_delta = 0.99, max_treedepth = 12),
                      save_pars = save_pars(all = TRUE))
  
  saveRDS(mac_bars_water_fit, file = "Output/Fitted Models/mac_bars_water_fit.rds")
  
  
} else if (i == 4){
  
  gwy_bars_water_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, Unspeckedhardyhead, Freshwatercatfish, EasternGambusia, Goldenperch, Murraycod, Australiansmelt, Goldfish, Spangledperch, Bonyherring, Redfin, Rainbowfishspeciescomplex) 
                            ~ t2(scale(DateNum), scale(Distance), bs = c("ts", "ts"), k = 20, full = TRUE)  + up_bar + up_bar:scale(DaysSinceOverbank, center = FALSE) + (1|Method) + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
  
  gwy_bars_water_fit <- brm(gwy_bars_water_bform, data = gwy_df_filtered,
                      family = zero_inflated_negbinomial,
                      chains = 4, cores = 4,
                      iter = 5000, warmup = 2000, init = 0,
                      backend = "cmdstanr", threads = threading(8), seed = 123,
                      prior = gwy_priors,  control = list(adapt_delta = 0.99, max_treedepth = 12),
                      save_pars = save_pars(all = TRUE))
  
  saveRDS(gwy_bars_water_fit, file = "Output/Fitted Models/gwy_bars_water_fit.rds")
  
} else if (i == 5){
  
   dum_bars_water_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, Unspeckedhardyhead, Freshwatercatfish, EasternGambusia, Goldenperch, Murraycod, Southernpurplespottedgudgeon, Australiansmelt, Goldfish, Spangledperch, Bonyherring, Oliveperchlet, Rainbowfishspeciescomplex) 
                             ~ t2(scale(DateNum), scale(Distance), bs = c("ts", "ts"), k = 20, full = TRUE)  + up_bar + up_bar:scale(DaysSinceOverbank, center = FALSE) + (1|Method) + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
   
   dum_bars_water_fit <- brm(dum_bars_water_bform, data = dum_df_filtered,
                       family = zero_inflated_negbinomial,
                       chains = 4, cores = 4,
                       iter = 5000, warmup = 2000, init = 0,
                       backend = "cmdstanr", threads = threading(8), seed = 123,
                       prior = dum_priors,  control = list(adapt_delta = 0.99, max_treedepth = 12),
                       save_pars = save_pars(all = TRUE))
   
   saveRDS(dum_bars_water_fit, file = "Output/Fitted Models/dum_bars_water_fit.rds")
  
  nam_bars_water_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, Unspeckedhardyhead, Freshwatercatfish, EasternGambusia, Goldenperch, Murraycod, Australiansmelt, Goldfish, Spangledperch, Bonyherring, Rainbowfishspeciescomplex) 
                            ~ t2(scale(DateNum), scale(Distance), bs = c("ts", "ts"), k = 20, full = TRUE)  + up_bar + up_bar:scale(DaysSinceOverbank, center = FALSE) + (1|Method) + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
  
  nam_bars_water_fit <- brm(nam_bars_water_bform, data = nam_df_filtered,
                      family = zero_inflated_negbinomial,
                      chains = 4, cores = 4,
                      iter = 5000, warmup = 2000, init = 0,
                      backend = "cmdstanr", threads = threading(8), seed = 123,
                      prior = nam_priors,  control = list(adapt_delta = 0.99, max_treedepth = 12),
                      save_pars = save_pars(all = TRUE))
  
  saveRDS(nam_bars_water_fit, file = "Output/Fitted Models/nam_bars_water_fit.rds")
  
} else if (i == 6){
  
  mur_bars_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, EasternGambusia, Goldenperch, Murraycod, Australiansmelt, Goldfish, Bonyherring, Redfin, Silverperch, Troutcod, NorthernRiverBlackfish, Rainbowfishspeciescomplex) 
                            ~ t2(scale(DateNum), scale(Distance), bs = c("ts", "ts"), k = 20, full = TRUE)  + up_bar + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
  
  mur_bars_fit <- brm(mur_bars_bform, data = mur_df_filtered,
                      family = zero_inflated_negbinomial,
                      chains = 4, cores = 4,
                      iter = 5000, warmup = 2000, init = 0,
                      backend = "cmdstanr", threads = threading(8), seed = 123,
                      prior = mur_priors,  control = list(adapt_delta = 0.99, max_treedepth = 12),
                      save_pars = save_pars(all = TRUE))
  
  saveRDS(mur_bars_fit, file = "Output/Fitted Models/mur_bars_fit.rds")
    
} else if (i == 7){
  
  lac_bars_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, Unspeckedhardyhead, EasternGambusia, Goldenperch, Murraycod, Australiansmelt, Goldfish, Bonyherring, Redfin, Flatheadgudgeonspeciescomplex) 
                            ~ t2(scale(DateNum), scale(Distance), bs = c("ts", "ts"), k = 20, full = TRUE)  + up_bar + (1|Method) + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
  
  lac_bars_fit <- brm(lac_bars_bform, data = lac_df_filtered,
                      family = zero_inflated_negbinomial,
                      chains = 4, cores = 4,
                      iter = 5000, warmup = 2000, init = 0,
                      backend = "cmdstanr", threads = threading(8), seed = 123,
                      prior = lac_priors,  control = list(adapt_delta = 0.99, max_treedepth = 12),
                      save_pars = save_pars(all = TRUE))
  
  saveRDS(lac_bars_fit, file = "Output/Fitted Models/lac_bars_fit.rds")
  
} else if (i == 8){
  
  mac_bars_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, Unspeckedhardyhead, Freshwatercatfish, EasternGambusia, Goldenperch, Murraycod, Australiansmelt, Goldfish, Spangledperch, Bonyherring, Redfin, Rainbowfishspeciescomplex, Flatheadgudgeonspeciescomplex) 
                            ~ t2(scale(DateNum), scale(Distance), bs = c("ts", "ts"), k = 20, full = TRUE)  + up_bar + (1|Method) + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
  
  mac_bars_fit <- brm(mac_bars_bform, data = mac_df_filtered,
                      family = zero_inflated_negbinomial,
                      chains = 4, cores = 4,
                      iter = 5000, warmup = 2000, init = 0,
                      backend = "cmdstanr", threads = threading(8), seed = 123,
                      prior = mac_priors,  control = list(adapt_delta = 0.99, max_treedepth = 12),
                      save_pars = save_pars(all = TRUE))
  
  saveRDS(mac_bars_fit, file = "Output/Fitted Models/mac_bars_fit.rds")
  
  
} else if (i == 9){
  
  gwy_bars_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, Unspeckedhardyhead, Freshwatercatfish, EasternGambusia, Goldenperch, Murraycod, Australiansmelt, Goldfish, Spangledperch, Bonyherring, Redfin, Rainbowfishspeciescomplex) 
                            ~ t2(scale(DateNum), scale(Distance), bs = c("ts", "ts"), k = 20, full = TRUE)  + up_bar + (1|Method) + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
  
  gwy_bars_fit <- brm(gwy_bars_bform, data = gwy_df_filtered,
                      family = zero_inflated_negbinomial,
                      chains = 4, cores = 4,
                      iter = 5000, warmup = 2000, init = 0,
                      backend = "cmdstanr", threads = threading(8), seed = 123,
                      prior = gwy_priors,  control = list(adapt_delta = 0.99, max_treedepth = 12),
                      save_pars = save_pars(all = TRUE))
  
  saveRDS(gwy_bars_fit, file = "Output/Fitted Models/gwy_bars_fit.rds")
  
} else if (i == 10){
  
   dum_bars_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, Unspeckedhardyhead, Freshwatercatfish, EasternGambusia, Goldenperch, Murraycod, Southernpurplespottedgudgeon, Australiansmelt, Goldfish, Spangledperch, Bonyherring, Oliveperchlet, Rainbowfishspeciescomplex) 
                             ~ t2(scale(DateNum), scale(Distance), bs = c("ts", "ts"), k = 20, full = TRUE)  + up_bar + (1|Method) + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
   
   dum_bars_fit <- brm(dum_bars_bform, data = dum_df_filtered,
                       family = zero_inflated_negbinomial,
                       chains = 4, cores = 4,
                       iter = 5000, warmup = 2000, init = 0,
                       backend = "cmdstanr", threads = threading(8), seed = 123,
                       prior = dum_priors,  control = list(adapt_delta = 0.99, max_treedepth = 12),
                       save_pars = save_pars(all = TRUE))
   
   saveRDS(dum_bars_fit, file = "Output/Fitted Models/dum_bars_fit.rds")
  
  nam_bars_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, Unspeckedhardyhead, Freshwatercatfish, EasternGambusia, Goldenperch, Murraycod, Australiansmelt, Goldfish, Spangledperch, Bonyherring, Rainbowfishspeciescomplex) 
                            ~ t2(scale(DateNum), scale(Distance), bs = c("ts", "ts"), k = 20, full = TRUE)  + up_bar + (1|Method) + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
  
  nam_bars_fit <- brm(nam_bars_bform, data = nam_df_filtered,
                      family = zero_inflated_negbinomial,
                      chains = 4, cores = 4,
                      iter = 5000, warmup = 2000, init = 0,
                      backend = "cmdstanr", threads = threading(8), seed = 123,
                      prior = nam_priors,  control = list(adapt_delta = 0.99, max_treedepth = 12),
                      save_pars = save_pars(all = TRUE))
  
  saveRDS(nam_bars_fit, file = "Output/Fitted Models/nam_bars_fit.rds")
  
} else if (i == 11){
  
  mur_null_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, EasternGambusia, Goldenperch, Murraycod, Australiansmelt, Goldfish, Bonyherring, Redfin, Silverperch, Troutcod, NorthernRiverBlackfish, Rainbowfishspeciescomplex) 
                            ~ t2(scale(DateNum), scale(Distance), bs = c("ts", "ts"), k = 20, full = TRUE)  + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
  
  mur_null_fit <- brm(mur_null_bform, data = mur_df_filtered,
                      family = zero_inflated_negbinomial,
                      chains = 4, cores = 4,
                      iter = 5000, warmup = 2000, init = 0,
                      backend = "cmdstanr", threads = threading(8), seed = 123,
                      control = list(adapt_delta = 0.99, max_treedepth = 12),
                      save_pars = save_pars(all = TRUE))
  
  saveRDS(mur_null_fit, file = "Output/Fitted Models/mur_null_fit.rds")
    
} else if (i == 12){
  
  lac_null_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, Unspeckedhardyhead, EasternGambusia, Goldenperch, Murraycod, Australiansmelt, Goldfish, Bonyherring, Redfin, Flatheadgudgeonspeciescomplex) 
                            ~ t2(scale(DateNum), scale(Distance), bs = c("ts", "ts"), k = 20, full = TRUE)  + (1|Method) + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
  
  lac_null_fit <- brm(lac_null_bform, data = lac_df_filtered,
                      family = zero_inflated_negbinomial,
                      chains = 4, cores = 4,
                      iter = 5000, warmup = 2000, init = 0,
                      backend = "cmdstanr", threads = threading(8), seed = 123,
                      control = list(adapt_delta = 0.99, max_treedepth = 12),
                      save_pars = save_pars(all = TRUE))
  
  saveRDS(lac_null_fit, file = "Output/Fitted Models/lac_null_fit.rds")
  
} else if (i == 13){
  
  mac_null_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, Unspeckedhardyhead, Freshwatercatfish, EasternGambusia, Goldenperch, Murraycod, Australiansmelt, Goldfish, Spangledperch, Bonyherring, Redfin, Rainbowfishspeciescomplex, Flatheadgudgeonspeciescomplex) 
                            ~ t2(scale(DateNum), scale(Distance), bs = c("ts", "ts"), k = 20, full = TRUE)  + (1|Method) + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
  
  mac_null_fit <- brm(mac_null_bform, data = mac_df_filtered,
                      family = zero_inflated_negbinomial,
                      chains = 4, cores = 4,
                      iter = 5000, warmup = 2000, init = 0,
                      backend = "cmdstanr", threads = threading(8), seed = 123,
                      control = list(adapt_delta = 0.99, max_treedepth = 12),
                      save_pars = save_pars(all = TRUE))
  
  saveRDS(mac_null_fit, file = "Output/Fitted Models/mac_null_fit.rds")
  
  
} else if (i == 14){
  
  gwy_null_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, Unspeckedhardyhead, Freshwatercatfish, EasternGambusia, Goldenperch, Murraycod, Australiansmelt, Goldfish, Spangledperch, Bonyherring, Redfin, Rainbowfishspeciescomplex) 
                            ~ t2(scale(DateNum), scale(Distance), bs = c("ts", "ts"), k = 20, full = TRUE)  + (1|Method) + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
  
  gwy_null_fit <- brm(gwy_null_bform, data = gwy_df_filtered,
                      family = zero_inflated_negbinomial,
                      chains = 4, cores = 4,
                      iter = 5000, warmup = 2000, init = 0,
                      backend = "cmdstanr", threads = threading(8), seed = 123,
                      control = list(adapt_delta = 0.99, max_treedepth = 12),
                      save_pars = save_pars(all = TRUE))
  
  saveRDS(gwy_null_fit, file = "Output/Fitted Models/gwy_null_fit.rds")
  
} else if (i == 15){
  
   dum_null_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, Unspeckedhardyhead, Freshwatercatfish, EasternGambusia, Goldenperch, Murraycod, Southernpurplespottedgudgeon, Australiansmelt, Goldfish, Spangledperch, Bonyherring, Oliveperchlet, Rainbowfishspeciescomplex) 
                             ~ t2(scale(DateNum), scale(Distance), bs = c("ts", "ts"), k = 20, full = TRUE)  + (1|Method) + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
   
   dum_null_fit <- brm(dum_null_bform, data = dum_df_filtered,
                       family = zero_inflated_negbinomial,
                       chains = 4, cores = 4,
                       iter = 5000, warmup = 2000, init = 0,
                       backend = "cmdstanr", threads = threading(8), seed = 123,
                       control = list(adapt_delta = 0.99, max_treedepth = 12),
                       save_pars = save_pars(all = TRUE))
   
   saveRDS(dum_null_fit, file = "Output/Fitted Models/dum_null_fit.rds")
  
  nam_null_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, Unspeckedhardyhead, Freshwatercatfish, EasternGambusia, Goldenperch, Murraycod, Australiansmelt, Goldfish, Spangledperch, Bonyherring, Rainbowfishspeciescomplex) 
                            ~ t2(scale(DateNum), scale(Distance), bs = c("ts", "ts"), k = 20, full = TRUE)  + (1|Method) + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
  
  nam_null_fit <- brm(nam_null_bform, data = nam_df_filtered,
                      family = zero_inflated_negbinomial,
                      chains = 4, cores = 4,
                      iter = 5000, warmup = 2000, init = 0,
                      backend = "cmdstanr", threads = threading(8), seed = 123,
                      control = list(adapt_delta = 0.99, max_treedepth = 12),
                      save_pars = save_pars(all = TRUE))
  
  saveRDS(nam_null_fit, file = "Output/Fitted Models/nam_null_fit.rds")
  
} 
