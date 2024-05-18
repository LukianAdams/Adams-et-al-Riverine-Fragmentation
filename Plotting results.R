#Loading relevant packages
library(tidyverse

#Plot 1 - Comparisons of effects sizes between barriers
bar_pred <- emmeans(meta_fit_bar, ~Barrier, tran = bc_tran, epred = TRUE) %>% #Extracting marginal posterior draws for each barrier or barrier cluster
  regrid(type = "response") %>%
  gather_emmeans_draws() %>%
  as.data.table()

median_hdi(bar_pred$.value, .width = c(0.89, 0.95))

bar_pred_point_estimates <- emmeans(meta_fit_bar, ~Barrier, tran = bc_tran, epred = TRUE) %>%
  regrid(type = "response")
summary(bar_pred_point_estimates, level = 0.89)

bar_pred <- as.data.frame(bar_pred[meta_df_bars_dt, on = "Barrier"]) %>%
  rename(`Number of Barriers` = BarrierCount) %>%
  rename(`Height of Barriers` = HeightSum)
bar_pred

bar_plot <- ggplot(bar_pred) +
  stat_pointinterval(point_interval = "median_hdi", .width = c(0.66, 0.89), interval_size_range = c(0.6,1.4),
    aes(y = reorder(Barrier, .value, FUN = median), x = .value, point_size = `Height of Barriers`, colour = River)) +
  scale_point_size_continuous(range = c(2.2, 8.5), breaks = c(0, 15, 100), trans = "sqrt") +
  scale_colour_manual(values = c("Murrumbidgee" = "red2", "Lachlan" = "blue", "Macquarie" = "goldenrod1", "Namoi" = "chocolate4", "Gwydir" = "green4", "Dumaresq/Mole" = "purple3"), name = "River", breaks = c("Dumaresq/Mole", "Gwydir", "Namoi", "Macquarie", "Lachlan", "Murrumbidgee")) +
  labs(y = "", x = "Effect Size (lnRR)") +
  geom_vline(xintercept = 0, linetype = "longdash", color = "black", linewidth = 0.3, alpha = 0.4) +
  scale_x_continuous(expand = expansion(mult = c(0, 0))) +
  coord_cartesian(xlim = c(0, 5.3)) +
  theme_minimal_vgrid() +
  guides(colour = guide_legend(override.aes = list(size = 10)),
         size = guide_legend(title.theme = element_text(margin = margin(b = 15)))) +
  theme(legend.justification = 'centre', 
        legend.position = 'bottom', legend.box = 'vertical', 
        legend.box.just = 'centre') +
  expand_limits(y = c(0,52.8))
bar_plot

bar_legend <- plot_grid(NULL, get_legend(bar_plot),
                        rel_widths = c(0.2,1))

bar_plot_fixed <- plot_grid(bar_plot + theme(legend.position = "none"), bar_legend,
                            nrow = 2,
                            rel_heights = c(1,0.1))
bar_plot_fixed

ggsave(filename = "Barrier ID Plot.png", plot = bar_plot_fixed, width = 250, height = 350, units = "mm", device='png', dpi=600)

#Plot 2 - Comparison of effects between barrier types
bar_type_pred <- emmeans(meta_fit_bar_features, ~BarrierType, tran = bc_tran, epred = TRUE) %>%
  regrid(type = "response") %>%
  gather_emmeans_draws()

bar_type_pred_point_estimates <- emmeans(meta_fit_bar_features, ~BarrierType, tran = bc_tran, epred = TRUE) %>%
  regrid(type = "response")
summary(bar_type_pred_point_estimates, level = 0.89)

bar_type_plot <- ggplot(bar_type_pred) +
  stat_pointinterval(point_interval = "median_hdi", .width = c(0.66, 0.89), point_size = 5, interval_size_range = c(0.5,1.4),
    aes(y = reorder(BarrierType, .value, FUN = median), x = .value)) +
  labs(y = "", x = "Effect Size (lnRR)") +
  geom_vline(xintercept = 0, linetype = "longdash", color = "black", linewidth = 0.3, alpha = 0.4) +
  scale_x_continuous(expand = expansion(mult = c(0, 0))) +
  coord_cartesian(xlim = c(0, 2.5)) +
  theme_minimal_vgrid() + 
  guides(colour = guide_legend(override.aes = list(size = 10))) +
  ggtitle("Structure Type")
bar_type_plot

#Plot 3 - Comparisons of effects between remediated and non-remediated barriers
remed_pred <- emmeans(meta_fit_bar_features, ~RemediatedProp, tran = bc_tran, epred = TRUE) %>%
  regrid(type = "response") %>%
  gather_emmeans_draws() %>%
  rename(`Remediation Status` = RemediatedProp)

remed_pred$`Remediation Status` <- recode_factor(remed_pred$`Remediation Status`,
                Full = "Full Remediation",
                Half = "Partial Remediation",
                None = "No Remediation")

remed_plot <- ggplot(remed_pred) +
  stat_pointinterval(point_interval = "median_hdi", .width = c(0.66, 0.89), point_size = 5, interval_size_range = c(0.5,1.4),
    aes(y = reorder(`Remediation Status`, .value, FUN = median), x = .value)) +
  labs(y = "", x = "Effect Size (lnRR)") +
  geom_vline(xintercept = 0, linetype = "longdash", color = "black", linewidth = 0.3, alpha = 0.4) +
  scale_x_continuous(expand = expansion(mult = c(0, 0))) +
  coord_cartesian(xlim = c(0, 2.5)) +
  theme_minimal_vgrid() + 
  guides(colour = guide_legend(override.aes = list(size = 10))) +
  ggtitle("Remediation Status")
remed_plot

#Plot 4 - Relationship between barrier count and fragmentation effect
bar_count_pred <- emmeans(meta_fit_bar_features, ~BarrierCount, tran = bc_tran, epred = TRUE,
                          at = list(BarrierCount = c(seq(1, 8, length.out = 10)))) %>%
  regrid(type = "response") %>%
  gather_emmeans_draws()

mean(meta_fit_bar_features$data$BarrierCount)
sd(meta_fit_bar_features$data$BarrierCount)

bar_count_slope <- emtrends(meta_fit_bar_features, ~scale(BarrierCount), tran = bc_tran, epred = TRUE,
                            var = "as.numeric(scale(BarrierCount, 1.943995, 1.805462))") %>%
  regrid(type = "response")
summary(bar_count_slope, level = 0.89)

bar_count_plot <- ggplot(bar_count_pred) +
  stat_lineribbon(aes(x = BarrierCount, y = .value, alpha = after_stat(.width)), .width = c(0, 0.66, 0.89), fill = "lightblue3") +
  scale_alpha(range = c(0.7, 0.5)) +
  coord_cartesian(ylim = c(0, 3.2)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_minimal_hgrid() +
  theme(legend.position = "none") +
  labs(y = "Effect Size (lnRR)", x = "Number of Barriers") +
  ggtitle("Barrier Count")
bar_count_plot

#Plot 5 - Relationship between barrier height and fragmentation effect
bar_height_pred <- emmeans(meta_fit_bar_features, ~HeightSum, tran = bc_tran, epred = TRUE,
                          at = list(HeightSum = c(seq(min(meta_fit_bar_features$data$HeightSum), max(meta_fit_bar_features$data$HeightSum), length.out = 10)))) %>%
  regrid(type = "response") %>%
  gather_emmeans_draws()

mean(meta_fit_bar_features$data$HeightSum)
sd(meta_fit_bar_features$data$HeightSum)

bar_height_slope <- emtrends(meta_fit_bar_features, ~scale(HeightSum), tran = bc_tran, epred = TRUE, 
                            var = "as.numeric(scale(HeightSum, 12.70543, 26.22767))") %>%
  regrid(type = "response")
bar_height_slope
summary(bar_height_slope, level = 0.89)

bar_height_plot <- ggplot(bar_height_pred) +
  stat_lineribbon(aes(x = HeightSum, y = .value, alpha = after_stat(.width)), .width = c(0, 0.66, 0.89), fill = "lightblue3") +
  scale_alpha(range = c(0.7, 0.5)) +
  coord_cartesian(ylim = c(0, 3.2)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_minimal_hgrid() +
  theme(legend.position = "none") +
  labs(y = "Effect Size (lnRR)", x = "Total Height of Barriers (m)") +
  ggtitle("Barrier Height")
bar_height_plot

#Plot 6 - Relationship between segment length and fragmentation effect
frag_diff_pred <- emmeans(meta_fit_bar_features, ~FragmentLengthDifference, tran = bc_tran, epred = TRUE,
                          at = list(FragmentLengthDifference = c(seq(min(meta_fit_bar_features$data$FragmentLengthDifference), max(meta_fit_bar_features$data$FragmentLengthDifference), length.out = 10)))) %>%
  regrid(type = "response") %>%
  gather_emmeans_draws()

mean(meta_fit_bar_features$data$FragmentLengthDifference)
sd(meta_fit_bar_features$data$FragmentLengthDifference)

frag_diff_slope <- emtrends(meta_fit_bar_features, ~scale(FragmentLengthDifference), tran = bc_tran, epred = TRUE,
                            var = "as.numeric(scale(FragmentLengthDifference, 102046.5, 121605.1))") %>%
  regrid(type = "response")
summary(frag_diff_slope, level = 0.89)

frag_diff_plot <- ggplot(frag_diff_pred) +
  stat_lineribbon(aes(x = FragmentLengthDifference, y = .value, alpha = after_stat(.width)), .width = c(0, 0.66, 0.89), fill = "lightblue3") +
  scale_alpha(range = c(0.7, 0.5)) +
  coord_cartesian(ylim = c(0, 3.2)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0), labels = c(0, 100, 200, 300, 400)) +
  theme_minimal_hgrid() +
  theme(legend.position = "none") +
  labs(y = "Effect Size (lnRR)", x = "Difference in Segment Length (km)") +
  ggtitle("Segment Length Difference")
frag_diff_plot

#Plot 7 - Relationship between barrier prioritisation and fragmentation effect

priority_pred <- emmeans(meta_fit_priority, ~PrioritySum, tran = bc_tran, epred = TRUE,
                          at = list(PrioritySum = c(seq(min(meta_fit_priority$data$PrioritySum), max(meta_fit_priority$data$PrioritySum), length.out = 10)))) %>%
  regrid(type = "response") %>%
  gather_emmeans_draws() %>%
  rename(`Priority Score Sum` = PrioritySum)

mean(meta_fit_priority$data$PrioritySum)
sd(meta_fit_priority$data$PrioritySum)

priority <- emtrends(meta_fit_priority, ~PrioritySum, epred = TRUE, tran = bc_tran,
                            var = "as.numeric(scale(PrioritySum, 6.692432, 6.252774))") %>%
  regrid(type = "response")
priority

priority_plot <- ggplot(priority_pred) +
  stat_lineribbon(aes(x = `Priority Score Sum`, y = .value, alpha = after_stat(.width)), .width = c(0, 0.66, 0.89), fill = "lightblue3") +
  scale_alpha(range = c(0.7, 0.5)) +
  coord_cartesian(ylim = c(0, 3.2)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_minimal_hgrid() +
  theme(legend.position = "none") +
  labs(y = "Effect Size (lnRR)", x = "Sum of Prioritisation Scores") +
  ggtitle("DPI Prioritisation")
priority_plot

#Combining barrier feature plots
bar_plots1 <- plot_grid(bar_type_plot + theme(axis.title.x = element_blank()), remed_plot,
                        rel_heights = c(1.8,1),
                        ncol = 1,
                        align = "v",
                        axis = "rblt",
                        labels = c("(a)", "(b)"))

bar_plots1

bar_plots2 <- plot_grid(bar_height_plot, frag_diff_plot + theme(axis.title.y = element_blank()), 
                        bar_count_plot, priority_plot + theme(axis.title.y = element_blank()),
                         ncol = 2,
                         align = "hv",
                         axis = "rblt",
                        labels = c("(c)", "(d)", "(e)", "(f)"))
bar_plots2

bar_plots_combined <- plot_grid(bar_plots1, NULL, bar_plots2,
                                ncol = 3,
                                rel_widths = c(1,0.05,1.5))
bar_plots_combined

ggsave(filename = "Barrier Plots.png", plot = bar_plots_combined, width = 300, height = 250, units = "mm", device='png', dpi=600)

#Plot 8 - Comparison of fragmentation effect between fish taxa
taxa_pred_point_estimates <- emmeans(meta_fit_taxa, ~SpeciesNames, tran = bc_tran, epred = TRUE) %>%
  regrid(type = "response")
summary(taxa_pred_point_estimates, level = 0.89)

taxa_pred <- emmeans(meta_fit_taxa, ~SpeciesNames, tran = bc_tran, epred = TRUE) %>%
  regrid(type = "response") %>%
  gather_emmeans_draws() %>%
  as.data.table()

meta_df_fish_dt <- meta_df[,30:52] %>%
  group_by(SpeciesNames) %>%
  summarise(across(everything(), first)) %>%
  as.data.table()

taxa_pred <- as.data.frame(taxa_pred[meta_df_fish_dt, on = "SpeciesNames"])

taxa_lrr_breaks <- c(0,1,2,3)
taxa_rr_breaks <- exp(taxa_lrr_breaks)

taxa_plot <- ggplot(taxa_pred) +
  stat_pointinterval(point_interval = "median_hdi", .width = c(0.66, 0.89), point_size = 5, interval_size_range = c(0.6,1.4),
    aes(y = reorder(SpeciesNames, .value, FUN = median), x = .value, colour = NorthernBasinGuild)) +
  labs(y = "", x = "Effect Size (lnRR)") +
  coord_cartesian(xlim = c(0,3.5)) +
  scale_x_continuous(expand = c(0,0)) +
  #scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(0, 3.4, by = 1), sec.axis = sec_axis(~., name = "", breaks = seq(0, 3.4, by = 1), labels = round(exp(seq(0, 2.7, by = 0.5)), 1))) +
  scale_colour_manual(values = c("Floodplain Specialists" = "orange1", "Generalists" = "forestgreen", "Generalists (Alien)" = "purple3", "Flow Dependent Specialists" = "blue", "In-Channel Specialists (Flow Independent)" = "red", "In-Channel Specialists (Flow Dependent)" = "palevioletred2", "NA" = "grey")) +
  ggtitle("Fish Taxa") +
  theme_minimal_vgrid()
taxa_plot

#Plot 9 - Comparison of fragmentation effect between fish guilds
guilds_pred_point_estimates <- emmeans(meta_fit_guilds, ~NorthernBasinGuild, tran = bc_tran, epred = TRUE) %>%
  regrid(type = "response")
summary(guilds_pred_point_estimates, level = 0.89)

guilds_pred <- emmeans(meta_fit_guilds, ~NorthernBasinGuild, tran = bc_tran, epred = TRUE) %>%
  regrid(type = "response") %>%
  gather_emmeans_draws()
  
guilds_pred$NorthernBasinGuild <- recode_factor(guilds_pred$NorthernBasinGuild,
                `Flow Dependent Specialists` = "Flow Dependent\nSpecialists",
                `In-Channel Specialists (Flow Independent)` = "In-Channel Specialists\n(Flow Independent)",
                `In-Channel Specialists (Flow Dependent)` = "In-Channel Specialists\n(Flow Dependent)")

guilds_plot <- ggplot(guilds_pred) +
  stat_pointinterval(point_interval = "median_hdi", .width = c(0.66, 0.89), point_size = 5, interval_size_range = c(0.6,1.4),
    aes(y = reorder(NorthernBasinGuild, .value), x = .value, colour = NorthernBasinGuild)) +
  labs(y = "", x = "Effect Size (lnRR)") +
  coord_cartesian(xlim = c(0,3.5)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0))) +
  scale_colour_manual(values = c("Floodplain Specialists" = "orange1", "Generalists" = "forestgreen", "Generalists (Alien)" = "purple3", "Flow Dependent\nSpecialists" = "blue", "In-Channel Specialists\n(Flow Independent)" = "firebrick2", "In-Channel Specialists\n(Flow Dependent)" = "palevioletred2", "NA" = "grey")) +
  ggtitle("Ecological Guilds") +
  theme_minimal_vgrid() +
  theme(legend.spacing.y = unit(0.3, "cm"), legend.position = "bottom") +
  guides(colour = guide_legend(byrow = TRUE, title = "Ecological Guilds"))
guilds_plot

guild_legend <- get_legend(guilds_plot)

#Combining fish taxa plots
fish_taxa_plot1 <- plot_grid(species_plot + theme(legend.position = "none", axis.title.x = element_blank()), guilds_plot + theme(legend.position = "none"),
                        nrow = 2,
                        rel_heights = c(2,1),
                        labels = c("(a)", "(b)"),
                        align = "v")
fish_taxa_plot1

fish_taxa_legend_plot <- plot_grid(NULL, guild_legend,
                                   nrow = 1,
                                   rel_widths = c(0.1,1))

fish_taxa_plots <- plot_grid(fish_taxa_plot1, fish_taxa_legend_plot,
                        nrow = 2,
                        rel_heights = c(1,0.1, 0.2))

fish_taxa_plots

ggsave(filename = "Fish Taxa Plots.png", plot = fish_taxa_plots, width = 225, height = 300, units = "mm", device='png', dpi=600)

#Plot 10 - Comparison of fragmentation effect between migration groups
movement_pred_point_estimate <- emmeans(meta_fit_traits, ~MovementDistance, tran = bc_tran, epred = TRUE) %>%
  regrid(type = "response")
summary(movement_pred_point_estimate, level = 0.89)

movement_pred <- emmeans(meta_fit_traits, ~MovementDistance, tran = bc_tran, epred = TRUE) %>%
  regrid(type = "response") %>%
  gather_emmeans_draws() %>%
  as.data.table()

movement_pred$MovementDistance <- recode_factor(movement_pred$MovementDistance,
                                          Long = "Long Migrators",
                                          Medium = "Mid-Length\nMigrators",
                                          Short = "Short Migrators")

movement_plot <- ggplot(movement_pred) +
  stat_pointinterval(point_interval = "median_hdi", .width = c(0.66, 0.89), point_size = 5,
    aes(y = reorder(MovementDistance, .value), x = .value)) +
  labs(y = "", x = "Effect Size (lnRR)") +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, 1.7)) +
  theme_minimal_vgrid() +
  ggtitle("Migration Distance")
movement_plot

#Plot 11 - Comparison of fragmentation effect between longevity groups
longevity_pred_point_estimate <- emmeans(meta_fit_traits, ~Longevity, tran = bc_tran, epred = TRUE) %>%
  regrid(type = "response")
summary(longevity_pred_point_estimate, level = 0.89)

longevity_pred <- emmeans(meta_fit_traits, ~Longevity, tran = bc_tran, epred = TRUE) %>%
  regrid(type = "response") %>%
  gather_emmeans_draws() %>%
  as.data.table()

longevity_pred$Longevity <- recode_factor(longevity_pred$Longevity,
                                          Long = "Long Lived",
                                          Medium = "Medium\nLifespan",
                                          Short = "Short Lived")

longevity_plot <- ggplot(longevity_pred) +
  stat_pointinterval(point_interval = "median_hdi", .width = c(0.66, 0.89), point_size = 5,
    aes(y = reorder(Longevity, .value), x = .value)) +
  labs(y = "", x = "Effect Size (lnRR)") +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, 1.7)) +
  theme_minimal_vgrid() +
  ggtitle("Longevity")
longevity_plot

#Plot 12 - Comparison of fragmentation effect between spawning methods
spawn_pred <- emmeans(meta_fit_traits, ~SpawningMethod, tran = bc_tran, epred = TRUE) %>%
  regrid(type = "response") %>%
  gather_emmeans_draws() %>%
  as.data.table()

spawn_pred$SpawningMethod <- recode_factor(spawn_pred$SpawningMethod,
                                              `Batch Parental Care` = "Batch\nParental Care",
                                           `Nesting Parental Care` = "Nesting\nParental Care")

spawn_plot <- ggplot(spawn_pred) +
  stat_pointinterval(point_interval = "median_hdi", .width = c(0.66, 0.89), point_size = 5,
    aes(y = reorder(SpawningMethod, .value), x = .value)) +
  labs(y = "", x = "Effect Size (lnRR)") +
  coord_cartesian(xlim = c(0,2)) +
  scale_x_continuous(expand = c(0,0)) +
  theme_minimal_vgrid() +
  ggtitle("Spawning Method")
spawn_plot

#Plot 13 - Comparison of fragmentation effect between egg morphologies
egg_morph_pred <- emmeans(meta_fit_traits, ~EggMorphology, tran = bc_tran, epred = TRUE) %>%
  regrid(type = "response") %>%
  gather_emmeans_draws()

egg_morph_pred$EggMorphology <- recode_factor(egg_morph_pred$EggMorphology,
                                              `Non-Sticky Demersal` = "Non-Sticky\nDemersal",
                                              `Bouyant Pelagic` = "Buoyant Pelagic")

egg_morph_plot <- ggplot(egg_morph_pred) +
  stat_pointinterval(point_interval = "median_hdi", .width = c(0.66, 0.89), point_size = 5,
    aes(y = reorder(EggMorphology, .value), x = .value)) +
  labs(y = "", x = "Effect Size (lnRR)") +
  coord_cartesian(xlim = c(0,1.7)) +
  scale_x_continuous(expand = c(0,0)) +
  theme_minimal_vgrid() +
  ggtitle("Egg Morphology")
egg_morph_plot

#Plot 14 - Comparison of fragmentation effect between longevity groups
longevity_pred <- emmeans(meta_fit_traits, ~Longevity, tran = bc_tran, epred = TRUE) %>%
  regrid(type = "response") %>%
  gather_emmeans_draws() %>%
  as.data.table()

longevity_pred$Longevity <- recode_factor(longevity_pred$Longevity,
                                          Long = "Long Lived",
                                          Medium = "Medium\nLifespan",
                                          Short = "Short Lived")

longevity_plot <- ggplot(longevity_pred) +
  stat_pointinterval(point_interval = "median_hdci", .width = c(0.66, 0.89), point_size = 5,
    aes(y = reorder(Longevity, .value), x = .value)) +
  labs(y = "", x = "Effect Size (lnRR)") +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, 1.7)) +
  theme_minimal_vgrid() +
  ggtitle("Longevity")
longevity_plot

#Plot 15 - Relationship between fish length and fragmentation effect

length_pred <- emmeans(meta_fit_traits, ~CommonLength, tran = bc_tran, epred = TRUE,
                        at = list(CommonLength = c(seq(min(meta_fit_traits$data$CommonLength), max(meta_fit_traits$data$CommonLength), length.out = 10)))) %>%
  regrid(type = "response") %>%
  gather_emmeans_draws()

mean(meta_fit_traits$data$CommonLength)
sd(meta_fit_traits$data$CommonLength)

length_slope <- emtrends(meta_fit_traits, ~scale(CommonLength), tran = bc_tran, epred = TRUE,
                            var = "as.numeric(scale(CommonLength, 226.6736, 192.735))") %>%
  regrid(type = "response")
summary(length_slope, level = 0.89)

length_plot <- ggplot(length_pred) +
  stat_lineribbon(aes(x = CommonLength, y = .value, alpha = after_stat(.width)), .width = c(0, 0.66, 0.89), fill = "lightblue3") +
  scale_alpha(range = c(0.7, 0.5)) +
  coord_cartesian(ylim = c(0, 1.7)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_minimal_hgrid() +
  theme(legend.position = "none") +
  labs(y = "Effect Size (lnRR)", x = "Adult Body Length (mm)") +
  ggtitle("Fish Length")
length_plot

#Plot 16 - Relationship between fish anaerobic swimming performance and fragmentation effect

usprint_pred <- emmeans(meta_fit_traits_usprint, ~Usprint, tran = bc_tran, epred = TRUE,
                        at = list(Usprint = c(seq(min(meta_fit_traits_usprint$data$Usprint), max(meta_fit_traits_usprint$data$Usprint), length.out = 10)))) %>%
  regrid(type = "response") %>%
  gather_emmeans_draws()

mean(meta_fit_traits_usprint$data$Usprint)
sd(meta_fit_traits_usprint$data$Usprint)

usprint_slope <- emtrends(meta_fit_traits_usprint, ~scale(Usprint), tran = bc_tran, epred = TRUE,
                            var = "as.numeric(scale(Usprint, 11.98965, 4.211851))") %>%
  regrid(type = "response")
summary(usprint_slope, level = 0.89)

usprint_plot <- ggplot(usprint_pred) +
  stat_lineribbon(aes(x = Usprint, y = .value, alpha = after_stat(.width)), .width = c(0, 0.8, 0.95), fill = "lightblue3") +
  scale_alpha(range = c(0.7, 0.5)) +
  coord_cartesian(ylim = c(0, 1.7)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_minimal_hgrid() +
  theme(legend.position = "none") +
  labs(y = "Effect Size (lnRR)", x = "Usprint") +
  ggtitle("Swimming Performance")
usprint_plot

#Combining fish trait plots
fish_plot1 <- plot_grid(movement_plot + theme(axis.title.x = element_blank()), 
                        longevity_plot + theme(axis.title.x = element_blank()), 
                        spawn_plot + theme(axis.title.x = element_blank()), 
                        egg_morph_plot + theme(axis.title.x = element_blank()), 
                        larval_drift_plot, 
                        fecundity_plot,
                        ncol = 2,
                        align = "hv",
                        rel_heights = c(1,1.2,0.82,1,1.2,0.82),
                        labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"))
fish_plot1

fish_plot2 <- plot_grid(length_plot, usprint_plot + theme(axis.title.y = element_blank()),
                        ncol = 2,
                        labels = c("(g)", "(h)"))

fish_plot2

fish_plots <- plot_grid(fish_plot1, fish_plot2,
                        ncol = 1,
                        rel_heights = c(2.25,1))
fish_plots

ggsave(filename = "Fish Trait Plots.png", plot = fish_plots, width = 250, height = 350, units = "mm", device='png', dpi=600)


#Plot 16 - Relationship between time since overbanking and fragmentation effect
overbank_pred <- emmeans(meta_fit_overbank, ~DaysSinceOverbank, tran = bc_tran, epred = TRUE,
                     at = list(DaysSinceOverbank = c(seq(min(meta_fit_bar$data$DaysSinceOverbank), max(meta_fit_bar$data$DaysSinceOverbank), length.out = 50)))) %>%
  regrid(type = "response") %>%
  gather_emmeans_draws()

mean(meta_fit_bar$data$DaysSinceOverbank)
sd(meta_fit_bar$data$DaysSinceOverbank)

overbank_slope <- emtrends(meta_fit_overbank, ~scale(DaysSinceOverbank), tran = bc_tran, epred = TRUE,
                            var = "as.numeric(scale(DaysSinceOverbank, 1834.626, 1268.946))") %>%
  regrid(type = "response")
summary(overbank_slope, level = 0.89)

overbank_plot <- ggplot(overbank_pred) +
  stat_lineribbon(aes(x = DaysSinceOverbank, y = .value, alpha = after_stat(.width)), .width = c(0, 0.66, 0.89), fill = "lightblue3") +
  #scale_fill_manual(values = c("Positive" = "#81e843", "Neutral" = "grey80")) +
  scale_alpha(range = c(0.7, 0.5)) +
  coord_cartesian(ylim = c(0, 3)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0), breaks = c(0, 1460, 2920, 4380), labels = c(0,4,8,12)) +
  theme_minimal_hgrid() +
  theme(legend.position = "none") +
  labs(y = "Effect Size (lnRR)", x = "Years Since Overbank") +
  ggtitle("Global Mean Overbank Effect")
overbank_plot

#Plot 17 - Barrier count on overbank effect
bar_count_overbank_pred <- emmeans(meta_fit_bar_features, ~BarrierCount, by = "DaysSinceOverbank", tran = bc_tran, epred = TRUE,
                              at = list(DaysSinceOverbank = c(seq(0,5400, length.out = 15)),
                                        BarrierCount = c(1,2,4,8))) %>%
  regrid(type = "response") %>%
  gather_emmeans_draws() %>%
  mutate_at("BarrierCount", as.factor)

mean(meta_fit_bar_features$data$DaysSinceOverbank)
sd(meta_fit_bar_features$data$DaysSinceOverbank)

bar_count_overbank_slope <- emtrends(meta_fit_bar_features, ~BarrierCount, tran = bc_tran, epred = TRUE,
                            var = "as.numeric(scale(DaysSinceOverbank, 1834.626, 1268.946))",
                            at = list(BarrierCount = c(1,2,4,8))) %>%
  regrid(type = "response")

summary(bar_count_overbank_slope, level = 0.89)

bar_count_overbank_pred$BarrierCount <- recode_factor(bar_count_overbank_pred$BarrierCount,
                                                      `1` = "1 Barrier",
                                                      `2` = "2 Barriers",
                                                      `4` = "4 Barriers",
                                                      `8` = "8 Barriers")

bar_count_overbank_plot <- ggplot(bar_count_overbank_pred) +
  stat_lineribbon(aes(x = DaysSinceOverbank, y = .value, alpha = after_stat(.width), fill = BarrierCount), .width = c(0, 0.66, 0.89)) +
  scale_fill_manual(values = c("1 Barrier" = "grey80", "2 Barriers" = "lightblue3", 
                               "4 Barriers" = "lightblue3", "8 Barriers" = "lightblue3"), name = "Number of Barriers") +
  scale_alpha(range = c(0.7, 0.5), guide = "none") +
  coord_cartesian(ylim = c(0, 6)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0), breaks = c(0, 1460, 2920, 4380), labels = c(0,4,8,12)) +
  theme_minimal_hgrid() +
  facet_wrap(~BarrierCount) +
  labs(y = "Effect Size (lnRR)", x = "Years Since Overbank") +
  guides(fill = guide_legend(override.aes = list(size = 10))) +
  ggtitle("Barrier Count Overbank Effects") +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.spacing = unit(1.2, "lines"),
    legend.position = "none",
    strip.text.x = element_text(size = 14))
bar_count_overbank_plot

#Combining overbank plots

overbank_plots <- plot_grid(overbank_plot, bar_count_overbank_plot,
                            nrow = 2,
                            rel_heights = c(1,1.5),
                            labels = c("(a)", "(b)"))

overbank_plots

ggsave(filename = "Overbank Plots.png", plot = overbank_plots, width = 250, height = 300, units = "mm", device='png', dpi=600)
