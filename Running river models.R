###Note - This code is computationally intensive, and is set up to run as a batch job on a high performance computing cluster with 32 cores per job. Expect a wait time ranging from 1 hour - 4 weeks per job depending on the number of observations and the number of fish taxa in your data

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
  
  mur_bars_water_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, Unspeckedhardyhead, EasternGambusia, Goldenperch, Murraycod, Australiansmelt, Goldfish, Bonyherring, Redfin, Rainbowtrout, Macquarieperch, Silverperch, Troutcod, NorthernRiverBlackfish, Rainbowfishspeciescomplex, Flatheadgudgeonspeciescomplex) #Creating multivariate response
                             ~ s(scale(DateNum), k = 10, bs = "ts") + s(scale(Distance), k = 5, bs = "ts") + up_bar + up_bar:scale(DaysSinceOverbank) + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR") #Defining model
  
  mur_bars_water_fit <- brm(mur_bars_water_bform, data = mur_df, #Fitting model
                            family = zero_inflated_negbinomial,
                            chains = 4, cores = 4,
                            iter = 4000, warmup = 2000, init = 0, #4000 iterations total, 2000 of which are warm up iterations, initial parameter values set to 0
                            backend = "cmdstanr", threads = threading(8), seed = 123, #Running using multithreading - multiple cores per chain
                            prior = mur_priors,  control = list(adapt_delta = 0.9975, max_treedepth = 15), #Setting priors, decreasing step size and increasing maximum tree depth
                            save_pars = save_pars(all = TRUE), sample_prior = "yes") #Saving all parameters for testing and diagnostics

  saveRDS(mur_bars_water_fit, file = "Output/mur_overbank_fit.rds")
  
} else if (i == 2){
  
  lac_bars_water_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, Unspeckedhardyhead, EasternGambusia, Goldenperch, Murraycod, Australiansmelt, Goldfish, Bonyherring, Redfin, Flatheadgudgeonspeciescomplex) 
                             ~ s(scale(DateNum), k = 10, bs = "ts") + s(scale(Distance), k = 5, bs = "ts") + up_bar + up_bar:scale(DaysSinceOverbank) + (1|Method) + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
  
  lac_bars_water_fit <- brm(lac_bars_water_bform, data = lac_df,
                            family = zero_inflated_negbinomial,
                            chains = 4, cores = 4,
                            iter = 4000, warmup = 2000, init = 0,
                            backend = "cmdstanr", threads = threading(8), seed = 123,
                            prior = lac_priors,  control = list(adapt_delta = 0.9975, max_treedepth = 15),
                            save_pars = save_pars(all = TRUE), sample_prior = "yes")
  
  saveRDS(lac_bars_water_fit, file = "Output/lac_overbank_fit.rds")
  
} else if (i == 3){
  
  mac_bars_water_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, Unspeckedhardyhead, Freshwatercatfish, EasternGambusia, Goldenperch, Murraycod, Australiansmelt, Goldfish, Spangledperch, Bonyherring, Redfin, Rainbowfishspeciescomplex, Flatheadgudgeonspeciescomplex) 
                             ~ s(scale(DateNum), k = 10, bs = "ts") + s(scale(Distance), k = 5, bs = "ts") + up_bar + up_bar:scale(DaysSinceOverbank) + (1|Method) + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
  
  mac_bars_water_fit <- brm(mac_bars_water_bform, data = mac_df,
                            family = zero_inflated_negbinomial,
                            chains = 4, cores = 4,
                            iter = 4000, warmup = 2000, init = 0,
                            backend = "cmdstanr", threads = threading(8), seed = 123,
                            prior = mac_priors,  control = list(adapt_delta = 0.9975, max_treedepth = 15),
                            save_pars = save_pars(all = TRUE), sample_prior = "yes")
  
  saveRDS(mac_bars_water_fit, file = "Output/mac_overbank_fit.rds")
  
} else if (i == 4){
  
  gwy_bars_water_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, Unspeckedhardyhead, Freshwatercatfish, EasternGambusia, Goldenperch, Murraycod, Australiansmelt, Goldfish, Spangledperch, Bonyherring, Redfin, Rainbowfishspeciescomplex) 
                             ~ s(scale(DateNum), k = 10, bs = "ts") + s(scale(Distance), k = 5, bs = "ts") + up_bar + up_bar:scale(DaysSinceOverbank) + (1|Method) + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
  
  gwy_bars_water_fit <- brm(gwy_bars_water_bform, data = gwy_df,
                            family = zero_inflated_negbinomial,
                            chains = 4, cores = 4,
                            iter = 4000, warmup = 2000, init = 0,
                            backend = "cmdstanr", threads = threading(8), seed = 123,
                            prior = gwy_priors,  control = list(adapt_delta = 0.9975, max_treedepth = 15),
                            save_pars = save_pars(all = TRUE), sample_prior = "yes")
  
  saveRDS(gwy_bars_water_fit, file = "Output/gwy_overbank_fit.rds")
  
} else if (i == 5){ #Namoi and dumaresq/mole models were fastest to run and were completed in the same job to reduce number of jobs needed in batch
  
  nam_bars_water_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, Unspeckedhardyhead, Freshwatercatfish, EasternGambusia, Goldenperch, Murraycod, Australiansmelt, Goldfish, Spangledperch, Bonyherring, Silverperch, Rainbowfishspeciescomplex) 
                             ~ s(scale(DateNum), k = 10, bs = "ts") + s(scale(Distance), k = 5, bs = "ts") + up_bar + up_bar:scale(DaysSinceOverbank) + (1|Method) + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
  
  nam_bars_water_fit <- brm(nam_bars_water_bform, data = nam_df,
                            family = zero_inflated_negbinomial,
                            chains = 4, cores = 4,
                            iter = 4000, warmup = 2000, init = 0,
                            backend = "cmdstanr", threads = threading(8), seed = 123,
                            prior = nam_priors,  control = list(adapt_delta = 0.9975, max_treedepth = 15),
                            save_pars = save_pars(all = TRUE), sample_prior = "yes")
  
  saveRDS(nam_bars_water_fit, file = "Output/nam_overbank_fit.rds")
  
  dum_bars_water_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, Unspeckedhardyhead, Freshwatercatfish, EasternGambusia, Goldenperch, Murraycod, Southernpurplespottedgudgeon, Australiansmelt, Goldfish, Spangledperch, Bonyherring, Oliveperchlet, Rainbowfishspeciescomplex) 
                             ~ s(scale(DateNum), k = 10, bs = "ts") + s(scale(Distance), k = 5, bs = "ts") + up_bar + up_bar:scale(DaysSinceOverbank) + (1|Method) + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
  
  dum_bars_water_fit <- brm(dum_bars_water_bform, data = dum_df,
                            family = zero_inflated_negbinomial,
                            chains = 4, cores = 4,
                            iter = 4000, warmup = 2000, init = 0,
                            backend = "cmdstanr", threads = threading(8), seed = 123,
                            prior = dum_priors,  control = list(adapt_delta = 0.9975, max_treedepth = 15),
                            save_pars = save_pars(all = TRUE), sample_prior = "yes")
  
  saveRDS(dum_bars_water_fit, file = "Output/dum_overbank_fit.rds")
  
} else if (i == 6){ #First 'no overbank' model
  
  mur_bars_water_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, Unspeckedhardyhead, EasternGambusia, Goldenperch, Murraycod, Australiansmelt, Goldfish, Bonyherring, Redfin, Rainbowtrout, Macquarieperch, Silverperch, Troutcod, NorthernRiverBlackfish, Rainbowfishspeciescomplex, Flatheadgudgeonspeciescomplex) 
                             ~ s(scale(DateNum), k = 10, bs = "ts") + s(scale(Distance), k = 5, bs = "ts") + up_bar + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
  
  mur_bars_water_fit <- brm(mur_bars_water_bform, data = mur_df,
                            family = zero_inflated_negbinomial,
                            chains = 4, cores = 4,
                            iter = 4000, warmup = 2000, init = 0,
                            backend = "cmdstanr", threads = threading(8), seed = 123,
                            prior = mur_priors,  control = list(adapt_delta = 0.9975, max_treedepth = 15),
                            save_pars = save_pars(all = TRUE), sample_prior = "yes")
  
  saveRDS(mur_bars_water_fit, file = "Output/mur_bar_fit.rds")
  
} else if (i == 7){
  
  lac_bars_water_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, Unspeckedhardyhead, EasternGambusia, Goldenperch, Murraycod, Australiansmelt, Goldfish, Bonyherring, Redfin, Flatheadgudgeonspeciescomplex) 
                             ~ s(scale(DateNum), k = 10, bs = "ts") + s(scale(Distance), k = 5, bs = "ts") + up_bar + (1|Method) + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
  
  lac_bars_water_fit <- brm(lac_bars_water_bform, data = lac_df,
                            family = zero_inflated_negbinomial,
                            chains = 4, cores = 4,
                            iter = 4000, warmup = 2000, init = 0,
                            backend = "cmdstanr", threads = threading(8), seed = 123,
                            prior = lac_priors,  control = list(adapt_delta = 0.9975, max_treedepth = 15),
                            save_pars = save_pars(all = TRUE), sample_prior = "yes")
  
  saveRDS(lac_bars_water_fit, file = "Output/lac_bar_fit.rds")
  
} else if (i == 8){
  
  mac_bars_water_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, Unspeckedhardyhead, Freshwatercatfish, EasternGambusia, Goldenperch, Murraycod, Australiansmelt, Goldfish, Spangledperch, Bonyherring, Redfin, Rainbowfishspeciescomplex, Flatheadgudgeonspeciescomplex) 
                             ~ s(scale(DateNum), k = 10, bs = "ts") + s(scale(Distance), k = 5, bs = "ts") + up_bar + (1|Method) + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
  
  mac_bars_water_fit <- brm(mac_bars_water_bform, data = mac_df,
                            family = zero_inflated_negbinomial,
                            chains = 4, cores = 4,
                            iter = 4000, warmup = 2000, init = 0,
                            backend = "cmdstanr", threads = threading(8), seed = 123,
                            prior = mac_priors,  control = list(adapt_delta = 0.9975, max_treedepth = 15),
                            save_pars = save_pars(all = TRUE), sample_prior = "yes")
  
  saveRDS(mac_bars_water_fit, file = "Output/mac_bar_fit.rds")
  
} else if (i == 9){
  
  gwy_bars_water_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, Unspeckedhardyhead, Freshwatercatfish, EasternGambusia, Goldenperch, Murraycod, Australiansmelt, Goldfish, Spangledperch, Bonyherring, Redfin, Rainbowfishspeciescomplex) 
                             ~ s(scale(DateNum), k = 10, bs = "ts") + s(scale(Distance), k = 5, bs = "ts") + up_bar + (1|Method) + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
  
  gwy_bars_water_fit <- brm(gwy_bars_water_bform, data = gwy_df,
                            family = zero_inflated_negbinomial,
                            chains = 4, cores = 4,
                            iter = 4000, warmup = 2000, init = 0,
                            backend = "cmdstanr", threads = threading(8), seed = 123,
                            prior = gwy_priors,  control = list(adapt_delta = 0.9975, max_treedepth = 15),
                            save_pars = save_pars(all = TRUE), sample_prior = "yes")
  
  saveRDS(gwy_bars_water_fit, file = "Output/gwy_bar_fit.rds")
  
} else if (i == 10){
  
  nam_bars_water_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, Unspeckedhardyhead, Freshwatercatfish, EasternGambusia, Goldenperch, Murraycod, Australiansmelt, Goldfish, Spangledperch, Bonyherring, Silverperch, Rainbowfishspeciescomplex) 
                             ~ s(scale(DateNum), k = 10, bs = "ts") + s(scale(Distance), k = 5, bs = "ts") + up_bar + (1|Method) + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
  
  nam_bars_water_fit <- brm(nam_bars_water_bform, data = nam_df,
                            family = zero_inflated_negbinomial,
                            chains = 4, cores = 4,
                            iter = 4000, warmup = 2000, init = 0,
                            backend = "cmdstanr", threads = threading(8), seed = 123,
                            prior = nam_priors,  control = list(adapt_delta = 0.9975, max_treedepth = 15),
                            save_pars = save_pars(all = TRUE), sample_prior = "yes")
  
  saveRDS(nam_bars_water_fit, file = "Output/nam_bar_fit.rds")
  
  dum_bars_water_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, Unspeckedhardyhead, Freshwatercatfish, EasternGambusia, Goldenperch, Murraycod, Southernpurplespottedgudgeon, Australiansmelt, Goldfish, Spangledperch, Bonyherring, Oliveperchlet, Rainbowfishspeciescomplex) 
                             ~ s(scale(DateNum), k = 10, bs = "ts") + s(scale(Distance), k = 5, bs = "ts") + up_bar + (1|Method) + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
  
  dum_bars_water_fit <- brm(dum_bars_water_bform, data = dum_df,
                            family = zero_inflated_negbinomial,
                            chains = 4, cores = 4,
                            iter = 4000, warmup = 2000, init = 0,
                            backend = "cmdstanr", threads = threading(8), seed = 123,
                            prior = dum_priors,  control = list(adapt_delta = 0.9975, max_treedepth = 15),
                            save_pars = save_pars(all = TRUE), sample_prior = "yes")
  
  saveRDS(dum_bars_water_fit, file = "Output/dum_bar_fit.rds")
  
} else if (i == 11){ #First 'no barriers' model
  
  mur_bars_water_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, Unspeckedhardyhead, EasternGambusia, Goldenperch, Murraycod, Australiansmelt, Goldfish, Bonyherring, Redfin, Rainbowtrout, Macquarieperch, Silverperch, Troutcod, NorthernRiverBlackfish, Rainbowfishspeciescomplex, Flatheadgudgeonspeciescomplex) 
                             ~ s(scale(DateNum), k = 10, bs = "ts") + s(scale(Distance), k = 5, bs = "ts") + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
  
  mur_bars_water_fit <- brm(mur_bars_water_bform, data = mur_df,
                            family = zero_inflated_negbinomial,
                            chains = 4, cores = 4,
                            iter = 4000, warmup = 2000, init = 0,
                            backend = "cmdstanr", threads = threading(8), seed = 123,
                            prior = mur_priors_null,  control = list(adapt_delta = 0.9975, max_treedepth = 15),
                            save_pars = save_pars(all = TRUE), sample_prior = "yes")
  
  saveRDS(mur_bars_water_fit, file = "Output/mur_null_fit.rds")
  
} else if (i == 12){
  
  lac_bars_water_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, Unspeckedhardyhead, EasternGambusia, Goldenperch, Murraycod, Australiansmelt, Goldfish, Bonyherring, Redfin, Flatheadgudgeonspeciescomplex) 
                             ~ s(scale(DateNum), k = 10, bs = "ts") + s(scale(Distance), k = 5, bs = "ts") + (1|Method) + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
  
  lac_bars_water_fit <- brm(lac_bars_water_bform, data = lac_df,
                            family = zero_inflated_negbinomial,
                            chains = 4, cores = 4,
                            iter = 4000, warmup = 2000, init = 0,
                            backend = "cmdstanr", threads = threading(8), seed = 123,
                            prior = lac_priors_null,  control = list(adapt_delta = 0.9975, max_treedepth = 15),
                            save_pars = save_pars(all = TRUE), sample_prior = "yes")
  
  saveRDS(lac_bars_water_fit, file = "Output/lac_null_fit.rds")
  
} else if (i == 13){
  
  mac_bars_water_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, Unspeckedhardyhead, Freshwatercatfish, EasternGambusia, Goldenperch, Murraycod, Australiansmelt, Goldfish, Spangledperch, Bonyherring, Redfin, Rainbowfishspeciescomplex, Flatheadgudgeonspeciescomplex) 
                             ~ s(scale(DateNum), k = 10, bs = "ts") + s(scale(Distance), k = 5, bs = "ts") + (1|Method) + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
  
  mac_bars_water_fit <- brm(mac_bars_water_bform, data = mac_df,
                            family = zero_inflated_negbinomial,
                            chains = 4, cores = 4,
                            iter = 4000, warmup = 2000, init = 0,
                            backend = "cmdstanr", threads = threading(8), seed = 123,
                            prior = mac_priors_null,  control = list(adapt_delta = 0.9975, max_treedepth = 15),
                            save_pars = save_pars(all = TRUE), sample_prior = "yes")
  
  saveRDS(mac_bars_water_fit, file = "Output/mac_null_fit.rds")
  
} else if (i == 14){
  
  gwy_bars_water_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, Unspeckedhardyhead, Freshwatercatfish, EasternGambusia, Goldenperch, Murraycod, Australiansmelt, Goldfish, Spangledperch, Bonyherring, Redfin, Rainbowfishspeciescomplex) 
                             ~ s(scale(DateNum), k = 10, bs = "ts") + s(scale(Distance), k = 5, bs = "ts") + (1|Method) + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
  
  gwy_bars_water_fit <- brm(gwy_bars_water_bform, data = gwy_df,
                            family = zero_inflated_negbinomial,
                            chains = 4, cores = 4,
                            iter = 4000, warmup = 2000, init = 0,
                            backend = "cmdstanr", threads = threading(8), seed = 123,
                            prior = gwy_priors_null,  control = list(adapt_delta = 0.9975, max_treedepth = 15),
                            save_pars = save_pars(all = TRUE), sample_prior = "yes")
  
  saveRDS(gwy_bars_water_fit, file = "Output/gwy_null_fit.rds")
  
} else if (i == 15){
  
  nam_bars_water_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, Unspeckedhardyhead, Freshwatercatfish, EasternGambusia, Goldenperch, Murraycod, Australiansmelt, Goldfish, Spangledperch, Bonyherring, Silverperch, Rainbowfishspeciescomplex) 
                             ~ s(scale(DateNum), k = 10, bs = "ts") + s(scale(Distance), k = 5, bs = "ts") + (1|Method) + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
  
  nam_bars_water_fit <- brm(nam_bars_water_bform, data = nam_df,
                            family = zero_inflated_negbinomial,
                            chains = 4, cores = 4,
                            iter = 4000, warmup = 2000, init = 0,
                            backend = "cmdstanr", threads = threading(8), seed = 123,
                            prior = nam_priors_null,  control = list(adapt_delta = 0.9975, max_treedepth = 15),
                            save_pars = save_pars(all = TRUE), sample_prior = "yes")
  
  saveRDS(nam_bars_water_fit, file = "Output/nam_null_fit.rds")
  
  dum_bars_water_bform <- bf(mvbind(Carpgudgeonspeciescomplex, Commoncarp, Unspeckedhardyhead, Freshwatercatfish, EasternGambusia, Goldenperch, Murraycod, Southernpurplespottedgudgeon, Australiansmelt, Goldfish, Spangledperch, Bonyherring, Oliveperchlet, Rainbowfishspeciescomplex) 
                             ~ s(scale(DateNum), k = 10, bs = "ts") + s(scale(Distance), k = 5, bs = "ts") + (1|Method) + (1|d|SampleID) + offset(log(ElectrofishingDuration)), decomp = "QR")
  
  dum_bars_water_fit <- brm(dum_bars_water_bform, data = dum_df,
                            family = zero_inflated_negbinomial,
                            chains = 4, cores = 4,
                            iter = 4000, warmup = 2000, init = 0,
                            backend = "cmdstanr", threads = threading(8), seed = 123,
                            prior = dum_priors_null,  control = list(adapt_delta = 0.9975, max_treedepth = 15),
                            save_pars = save_pars(all = TRUE), sample_prior = "yes")
  
  saveRDS(dum_bars_water_fit, file = "Output/dum_null_fit.rds")
  
}

