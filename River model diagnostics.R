#Define location of fitted models and list names
fitted_mods_path <- "Data/Fitted Models/"
fitted_mods_names <- list.files(path = fitted_mods_path, pattern = "\\.rds$", full.names = TRUE)

#Create a list of fitted models, and add model fit criteria to each
fitted_mods_bars_water <- list()
for (file in fitted_mods_names) {
  mod_name <- tools::file_path_sans_ext(basename(file))
  if(grepl("bars_water", mod_name)) {
  mod <- readRDS(file)
  fitted_mods_bars_water[[mod_name]] <- mod
  }
}

mods_bars_water_order <- c("mur_bars_water_fit", "lac_bars_water_fit", "mac_bars_water_fit", "nam_bars_water_fit", "gwy_bars_water_fit", "dum_bars_water_fit")
fitted_mods_bars_water <- fitted_mods_bars_water[mods_bars_water_order]

fitted_mods_null <- list()
for (file in fitted_mods_names) {
  mod_name <- tools::file_path_sans_ext(basename(file))
  if(grepl("null", mod_name)) {
  mod <- readRDS(file)
  #mod <- add_criterion(mod, c("loo", "waic", "bayes_R2"))
  fitted_mods_null[[mod_name]] <- mod
  }
}

mods_null_order <- c("mur_null_fit", "lac_null_fit", "mac_null_fit", "nam_null_fit", "gwy_null_fit")
fitted_mods_null <- fitted_mods_null[mods_null_order]

#Posterior predictive checks - Check species with high error first, then check a few others as well 
pp_check(fitted_mods$mur_bars_fit, ndraws = 20, resp = "Murraycod") +
  scale_x_continuous(trans=scales::pseudo_log_trans(base = 10)) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10))
pp_check(fitted_mods$mur_bars_fit, ndraws = NULL, resp = "Murraycod", type = "stat", stat = "mean")
pp_check(fitted_mods$mur_bars_fit, ndraws = NULL, resp = "Spangledperch", type = "stat_grouped", stat = "mean", group = "up_bar")
q95 <- function(y) quantile(y, 0.95) 
pp_check(fitted_mods$mur_bars_fit, ndraws = 20, resp = "Murraycod", type='stat', stat='q95')
pp_check(fitted_mods$mur_bars_fit, ndraws =NULL, resp = "Murraycod", x = 'Distance', type='error_scatter_avg_vs_x')
pp_check(fitted_mods$mur_bars_fit, type = "stat_2d", stat = c("max", "min"), resp = "Murraycod")
pp_check(fitted_mods$mur_bars_fit, type = "loo_pit_qq", resp = "Commoncarp")

#Leave one out cross validation
plot(loo(lac2))

#Checking for co-linearity
pairs(gwy_bars_fit, pars = "b", off_diag_args = list(size = 0.5, alpha = 0.25))

#Plotting prior against posterior
gwy_draws = gwy_bars_fit %>% posterior_samples() %>% clean_names()
gwy_prior_post_plot <- ggplot(data = gwy_draws) +
  geom_density(aes(b_murraycod_up_bar_copeton_dam), alpha = 0.3, fill = "darkblue")+
  geom_density(aes(prior_b_murraycod_up_bar_copeton_dam), alpha = 0.3, fill = "lightblue")+
  ggtitle("posterior and prior")
