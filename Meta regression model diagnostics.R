##Model 1 - Barrier ID##
meta_fit_bar <- readRDS("Data/Fitted Meta/meta_fit_bar.rds") #Loading model
summary(meta_fit_bar, robust = TRUE) #Checking model summary
pp_check(meta_fit_bar) #Posterior predictive check - Density overlay
pp_check(meta_fit_bar, type = "loo_pit_qq", ndraws = 200) #Posterior predictive check - Leave-one-out QQ plot

##Model 2 - Barrier features##
meta_fit_bar_features <- readRDS("Data/Fitted Meta/meta_fit_bar_features.rds")
summary(meta_fit_bar_features, robust = TRUE, prob = 0.89)
pp_check(meta_fit_bar_features)
pp_check(meta_fit_bar_features, type = "loo_pit_qq", ndraws = 200)

##Model 3 - Barrier prioritisation##
meta_fit_priority <- readRDS("Data/Fitted Meta/meta_fit_priority.rds")
summary(meta_fit_priority, robust = TRUE)
pp_check(meta_fit_priority)
pp_check(meta_fit_priority, type = "loo_pit_qq", ndraws = 200)

##Model 4 - Fish taxa##
meta_fit_taxa <- readRDS("Data/Fitted Meta/meta_fit_taxa.rds")
summary(meta_fit_taxa, robust = TRUE)
pp_check(meta_fit_taxa)
pp_check(meta_fit_taxa, type = "loo_pit_qq", ndraws = 200)

##Model 5 - Fish guilds##
meta_fit_guilds <- readRDS("Data/Fitted Meta/meta_fit_guilds.rds")
summary(meta_fit_guilds, robust = TRUE)
pp_check(meta_fit_guilds)
pp_check(meta_fit_guilds, type = "loo_pit_qq", ndraws = 200)

##Model 6a - Fish traits w/ Usprint##
meta_fit_traits <- readRDS("Data/Fitted Meta/meta_fit_traits.rds")
summary(meta_fit_traits, robust = TRUE, prob = 0.89)
pp_check(meta_fit_traits)
pp_check(meta_fit_traits, type = "loo_pit_qq", ndraws = 200)
plot(meta_fit_traits)

##Model 6b - Fish traits w/o Usprint##
meta_fit_traits_usprint <- readRDS("Data/Fitted Meta/meta_fit_traits_usprint.rds")
summary(meta_fit_traits_usprint, robust = TRUE)
pp_check(meta_fit_traits_usprint)
pp_check(meta_fit_traits_usprint, type = "loo_pit_qq", ndraws = 200)

##Model 7 - Overbank spline term##
meta_fit_overbank <- readRDS("Data/Fitted Meta/meta_fit_overbank.rds")
summary(meta_fit_overbank, robust = TRUE)
pp_check(meta_fit_overbank)
pp_check(meta_fit_overbank, type = "loo_pit_qq", ndraws = 200)
