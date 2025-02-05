library(tidyverse)

#Loading data for each river
mur_df <- read_csv(mur_df.csv)
lac_df <- read_csv(lac_df.csv)
mac_df <- read_csv(mac_df.csv)
nam_df <- read_csv(nam_df.csv)
gwy_df <- read_csv(gwy_df.csv)
dum_df <- read_csv(dum_df.csv)

#Finding section with most observations
mur_up_bar_freq <- names(sort(table(mur_df$up_bar), decreasing = TRUE))[1]

#Finding central point of control segment based on the mean distance of samples within
median_distance <- mur_df%>%
  filter(up_bar == mur_up_bar_freq) %>%
  summarize(median_distance = median(Distance)) %>%
  pull(median_distance)

#Updating barrier to "Control Barrier" for samples in the upstream half of control segment, i.e. upstream of control barrier
mur_df_ctrl <- mur_df %>%
  mutate(up_bar = if_else(up_bar == mur_up_bar_freq & Distance < median_distance, 
                          "Control Barrier Murrumbidgee", up_bar))

#Lachlan
lac_up_bar_freq <- names(sort(table(lac_df$up_bar), decreasing = TRUE))[1]
lac_up_bar_freq
lac_median_distance <- lac_df %>%
  filter(up_bar == lac_up_bar_freq) %>%
  summarize(median_distance = median(Distance)) %>%
  pull(median_distance)
lac_df_ctrl <- lac_df %>%
  mutate(up_bar = if_else(up_bar == lac_up_bar_freq & Distance < lac_median_distance, 
                          "Control Barrier Lachlan", up_bar))

#Macquarie
mac_up_bar_freq <- names(sort(table(mac_df$up_bar), decreasing = TRUE))[1]
mac_median_distance <- mac_df %>%
  filter(up_bar == mac_up_bar_freq) %>%
  summarize(median_distance = median(Distance)) %>%
  pull(median_distance)
mac_df_ctrl <- mac_df %>%
  mutate(up_bar = if_else(up_bar == mac_up_bar_freq & Distance < mac_median_distance, 
                          "Control Barrier Macquarie", up_bar))

#Namoi
nam_up_bar_freq <- names(sort(table(nam_df$up_bar), decreasing = TRUE))[1]
nam_median_distance <- nam_df %>%
  filter(up_bar == nam_up_bar_freq) %>%
  summarize(median_distance = median(Distance)) %>%
  pull(median_distance)
nam_df_ctrl <- nam_df %>%
  mutate(up_bar = if_else(up_bar == nam_up_bar_freq & Distance < nam_median_distance, 
                          "Control Barrier Namoi", up_bar))

#Gwydir
gwy_up_bar_freq <- names(sort(table(gwy_df$up_bar), decreasing = TRUE))[1]
gwy_median_distance <- gwy_df %>%
  filter(up_bar == gwy_up_bar_freq) %>%
  summarize(median_distance = median(Distance)) %>%
  pull(median_distance)
gwy_df_ctrl <- gwy_df %>%
  mutate(up_bar = if_else(up_bar == gwy_up_bar_freq & Distance < gwy_median_distance, 
                          "Control Barrier Gwydir", up_bar))

#Dumaresq
dum_up_bar_freq <- names(sort(table(dum_df$up_bar), decreasing = TRUE))[1]
dum_median_distance <- dum_df %>%
  filter(up_bar == dum_up_bar_freq) %>%
  summarize(median_distance = median(Distance)) %>%
  pull(median_distance)
dum_df_ctrl <- dum_df %>%
  mutate(up_bar = if_else(up_bar == dum_up_bar_freq & Distance < dum_median_distance, 
                          "Control Barrier Dumaresq", up_bar))

#Writing csvs

df_list_ctrl <- list(mur_df_ctrl, lac_df_ctrl, mac_df_ctrl, nam_df_ctrl, gwy_df_ctrl, dum_df_ctrl)

names(df_list_ctrl) <- c("mur_df_ctrl", "lac_df_ctrl", "mac_df_ctrl", "nam_df_ctrl", "gwy_df_ctrl", "dum_df_ctrl")

for (i in seq_along(df_list_ctrl)) {
  df <- df_list_ctrl[[i]]
  csv_name <- paste0(names(df_list_ctrl)[i], ".csv")
  write.csv(df, file = csv_name, row.names = FALSE)
}

