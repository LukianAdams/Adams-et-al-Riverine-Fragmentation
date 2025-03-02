#Defining location of fitted models and list names
fitted_mods_path <- "Data/Fitted Models/"
fitted_mods_names <- list.files(path = fitted_mods_path, pattern = "\\.rds$", full.names = TRUE)

#Creating a list of full river models and adding model fit criteria to each
fitted_mods_bars_water <- list()
for (file in fitted_mods_names) {
  mod_name <- tools::file_path_sans_ext(basename(file))
  if (grepl("bars_water", mod_name)) {
    mod <- readRDS(file)
    mod <- add_criterion(mod, c("loo", "waic", "bayes_R2"))
    fitted_mods_bars_water[[mod_name]] <- mod
  }
}
mods_bars_water_order <- c("mur_bars_water_fit", "lac_bars_water_fit", "mac_bars_water_fit", "nam_bars_water_fit", "gwy_bars_water_fit", "dum_bars_water_fit")
fitted_mods_bars_water <- fitted_mods_bars_water[mods_bars_water_order]

#Creating a list of no overbank models
fitted_mods_bar <- list()
for (file in fitted_mods_names) {
  mod_name <- tools::file_path_sans_ext(basename(file))
  if (grepl("bar", mod_name)) {
    mod <- readRDS(file)
    mod <- add_criterion(mod, c("loo", "waic", "bayes_R2"))
    fitted_mods_bar[[mod_name]] <- mod
  }
}
mods_bar_order <- c("mur_bar_fit", "lac_bar_fit", "mac_bar_fit", "nam_bar_fit", "gwy_bar_fit")
fitted_mods_bar <- fitted_mods_bar[mods_bar_order]

#Creating a list of null models
fitted_mods_null <- list()
for (file in fitted_mods_names) {
  mod_name <- tools::file_path_sans_ext(basename(file))
  if (grepl("null", mod_name)) {
    mod <- readRDS(file)
    mod <- add_criterion(mod, c("loo", "waic", "bayes_R2"))
    fitted_mods_null[[mod_name]] <- mod
  }
}
mods_null_order <- c("mur_null_fit", "lac_null_fit", "mac_null_fit", "nam_null_fit", "gwy_null_fit")
fitted_mods_null <- fitted_mods_null[mods_null_order]

#Using summary function to check Gelman-Rubin convergence diagnostics (Rhats) and effect sample size for each model
lapply(fitted_mods_bars_water, summary)
lapply(fitted_mods_bar, summary)
lapply(fitted_mods_null, summary)

#Plotting traceplots for each model - Note that this code can run very slowly with many models and parameters
lapply(fitted_mods_bars_water, plot)
lapply(fitted_mods_bar, plot)
lapply(fitted_mods_null, plot)

#Posterior predictive checks by plotting rootograms - only done for full river models
#Here, a single context example of Murray Cod in the Murrumbidgee is provided, as each fish taxa was checked individually for each model
pp_check(fitted_mods_bars_water$mur_overbank_fit, type = "rootogram", resp = "Murraycod")
