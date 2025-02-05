#Loading relevant packages
library(readr)
library(tidyverse)
library(brms)
library(cmdstanr)
library(pracma)

#Loading in meta-regression data with log-response ratio effect sizes from river models
meta_df <- read_csv("meta_df.csv") %>%
  
  #Mutating columns to the correct type
  mutate(MovementDistance = factor(MovementDistance, levels = c("Short", "Medium", "Long"), ordered = TRUE),
         Longevity = factor(Longevity, levels = c("Short", "Medium", "Long"), ordered = TRUE),
         RemediatedProp = factor(RemediatedProp, levels = c("None","Half", "Full"), ordered = TRUE),
         DaysSinceOverbankScaled = scale(DaysSinceOverbank)[,1]) %>%
  mutate_at(c("SpeciesNames", "Species", "River", "Barrier", "BarrierType", "StructureType", "NorthernBasinGuild", "Depth", "Native", "BodyShape", "TailShape", "SpawningMethod", "MigrationMode", "Fecundity", "LarvalDrift", "EggMorphology"), as.factor)

#Loading in version of dataset with control comparisons included
meta_df_ctrl <- read_csv("meta_df_ctrl.csv") %>%
  
  #Mutating columns to the correct type
  mutate(MovementDistance = factor(MovementDistance, levels = c("Short", "Medium", "Long"), ordered = TRUE),
         Longevity = factor(Longevity, levels = c("Short", "Medium", "Long"), ordered = TRUE),
         RemediatedProp = factor(RemediatedProp, levels = c("None","Half", "Full"), ordered = TRUE),
         DaysSinceOverbankScaled = scale(DaysSinceOverbank)[,1]) %>%
  mutate_at(c("SpeciesNames", "Species", "River", "Barrier", "BarrierType", "StructureType", "NorthernBasinGuild", "Depth", "Native", "BodyShape", "TailShape", "SpawningMethod", "MigrationMode", "Fecundity", "LarvalDrift", "EggMorphology"), as.factor)


#Creating filtered version of dataframe with no fish taxa without USprint data
meta_df_filtered <- subset(meta_df, !is.na(Usprint)) %>%
  droplevels()

#Setting job ID as i to run a batch job on Katana HPC
i = as.numeric(Sys.getenv('PBS_ARRAY_INDEX'))

#Running meta regression models
if (i == 1){
  
  #Model 1 - Barrier ID
  meta_fit_bar <- brm(LRR_trans | se(LRR_SE, sigma = TRUE)
                      ~ Barrier + Barrier:scale(DaysSinceOverbank) + 
                        (1|SpeciesNames) + (1|River),
                      data = meta_df_ctrl, #Control dataset is used only for this model
                      family = gaussian,
                      chains = 4, cores = 4,
                      backend = "cmdstanr",
                      threads = threading(4),
                      iter = 4000, warmup = 2000,
                      seed = 123,
                      control = list(adapt_delta = 0.995, max_treedepth = 15),
                      save_pars = save_pars(all = TRUE),
                      sample_prior = "yes",
                      prior = c(set_prior("student_t(3,0,1)", class = "b"),
                                set_prior("student_t(3,0,1)", class = "sd")))
  saveRDS(meta_fit_bar, file = "meta_fit_bar.rds")
  
  #Model 2 - Barrier features
  meta_fit_bar_features <- brm(bf(LRR_trans | se(LRR_SE, sigma = TRUE)
                                  ~ BarrierType + scale(HeightSum) + scale(BarrierCount) + scale(FragmentLengthDifference) + RemediatedProp +
                                    (BarrierType + scale(HeightSum) + scale(BarrierCount) + scale(FragmentLengthDifference) + RemediatedProp):scale(DaysSinceOverbank) +
                                    (1|SpeciesNames) + (1|River), decomp = "QR"),
                               data = meta_df,
                               family = gaussian,
                               chains = 4, cores = 4,
                               backend = "cmdstanr",
                               threads = threading(4),
                               iter = 4000, warmup = 2000,
                               seed = 123,
                               control = list(adapt_delta = 0.995, max_treedepth = 15),
                               save_pars = save_pars(all = TRUE),
                               sample_prior = "yes",
                               prior = c(set_prior("student_t(3,0,1)", class = "b"),
                                         set_prior("student_t(3,0,1)", class = "sd")))
  saveRDS(meta_fit_bar_features, file = "meta_fit_bar_features.rds")
  
  #Model 3 - Barrier prioritisation
  meta_fit_priority <- brm(LRR_trans | se(LRR_SE, sigma = TRUE)
                           ~ scale(PrioritySum) + scale(PrioritySum):scale(DaysSinceOverbank) +
                             (1|SpeciesNames) + (1|River),
                           data = meta_df,
                           family = gaussian,
                           chains = 4, cores = 4,
                           backend = "cmdstanr",
                           threads = threading(4),
                           iter = 4000, warmup = 2000,
                           seed = 1234,
                           control = list(adapt_delta = 0.995, max_treedepth = 15),
                           save_pars = save_pars(all = TRUE),
                           sample_prior = "yes",
                           prior = c(set_prior('student_t(3,0,1)', class = "b"),
                                     set_prior('student_t(3,0,1)', class = "sd")))
  saveRDS(meta_fit_priority, file = "meta_fit_priority.rds")
  
  #Model 4 - Fish taxa
  meta_fit_species <- brm(LRR_trans | se(LRR_SE, sigma = TRUE)
                          ~ SpeciesNames + SpeciesNames:scale(DaysSinceOverbank) +
                            (1|Barrier),
                          data = meta_df,
                          family = gaussian,
                          chains = 4, cores = 4,
                          backend = "cmdstanr",
                          threads = threading(4),
                          iter = 4000, warmup = 2000,
                          seed = 1234,
                          control = list(adapt_delta = 0.995, max_treedepth = 15),
                          save_pars = save_pars(all = TRUE),
                          sample_prior = "yes",
                          prior = c(set_prior('student_t(3,0,1)', class = "b"),
                                    set_prior('student_t(3,0,1)', class = "sd")))
  
  saveRDS(meta_fit_species, file = "meta_fit_species.rds")
  
  #Model 5 - Fish guilds
  meta_fit_guilds <- brm(LRR_trans | se(LRR_SE, sigma = TRUE)
                         ~ NorthernBasinGuild + NorthernBasinGuild:scale(DaysSinceOverbank) +
                           (1|Barrier),
                         data = meta_df,
                         family = gaussian,
                         chains = 4, cores = 4,
                         backend = "cmdstanr",
                         threads = threading(4),
                         iter = 4000, warmup = 2000,
                         seed = 1234,
                         control = list(adapt_delta = 0.995, max_treedepth = 15),
                         save_pars = save_pars(all = TRUE),
                         sample_prior = "yes",
                         prior = c(set_prior('student_t(3,0,1)', class = "b"),
                                   set_prior('student_t(3,0,1)', class = "sd")))
  saveRDS(meta_fit_guilds, file = "meta_fit_guilds.rds")
  
} else if (i == 2){
  
  #Model 6a - Fish traits with no Usprint (full dataset)
  meta_fit_fish_traits <- brm(bf(LRR_trans | se(LRR_SE, sigma = TRUE)
                                 ~ scale(CommonLength) + mo(MovementDistance) + mo(Longevity) + Fecundity + SpawningMethod + EggMorphology + LarvalDrift + 
                                   (scale(CommonLength) + mo(MovementDistance) + mo(Longevity) + Fecundity + SpawningMethod + EggMorphology + LarvalDrift):DaysSinceOverbankScaled +
                                   (1|Barrier), decomp = "QR"),
                              data = meta_df,
                              family = gaussian,
                              chains = 4, cores = 4,
                              backend = "cmdstanr",
                              threads = threading(4),
                              iter = 6000, warmup = 4000,
                              seed = 1234,
                              control = list(adapt_delta = 0.995, max_treedepth = 15),
                              save_pars = save_pars(all = TRUE),
                              prior = c(set_prior(horseshoe(1, par_ratio = 0.5), class = "b"),
                                        set_prior('student_t(3,0,1)', class = "sd")))
  saveRDS(meta_fit_fish_traits, file = "meta_fit_fish_traits.rds")
  
  #Model 6b - Fish traits with Usprint (truncated dataset)
  meta_fit_fish_traits_usprint <- brm(bf(LRR_trans | se(LRR_SE, sigma = TRUE)
                                         ~ scale(CommonLength) + mo(MovementDistance) + mo(Longevity) + SpawningMethod + EggMorphology + LarvalDrift + scale(Usprint) + 
                                           (scale(CommonLength) + mo(MovementDistance) + mo(Longevity) + SpawningMethod + EggMorphology + LarvalDrift + scale(Usprint)):DaysSinceOverbankScaled +
                                           (1|Barrier), decomp = "QR"),
                                      data = meta_df_filtered,
                                      family = gaussian,
                                      chains = 4, cores = 4,
                                      backend = "cmdstanr",
                                      threads = threading(4),
                                      iter = 6000, warmup = 4000,
                                      seed = 1234,
                                      control = list(adapt_delta = 0.995, max_treedepth = 15),
                                      save_pars = save_pars(all = TRUE),
                                      prior = c(set_prior(horseshoe(1, par_ratio = 0.5), class = "b"),
                                                set_prior('student_t(3,0,1)', class = "sd")))
  saveRDS(meta_fit_fish_traits_usprint, file = "meta_fit_fish_traits_usprint.rds")
  
  #Model 7 - Overbank spline term
  meta_fit_overbank <- brm(bf(LRR_trans | se(LRR_SE, sigma = TRUE) ~ s(DaysSinceOverbank, k = 5) + (DaysSinceOverbank | Barrier) + (DaysSinceOverbank | SpeciesNames)),
                           data = meta_df,
                           family = gaussian,
                           chains = 4, cores = 4,
                           backend = "cmdstanr",
                           threads = threading(4),
                           iter = 4000, warmup = 2000,
                           seed = 1234,
                           control = list(adapt_delta = 0.995, max_treedepth = 15),
                           save_pars = save_pars(all = TRUE),
                           prior = c(set_prior('student_t(3,0,1)', class = "sds"),
                                     set_prior('student_t(3,0,1)', class = "sd")))
  saveRDS(meta_fit_overbank, file = "meta_fit_overbank.rds")
}
