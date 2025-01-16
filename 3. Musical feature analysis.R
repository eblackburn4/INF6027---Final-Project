## ---------------------------
## Purpose of script: Intro to data science final project part I: R Code for genre and musical fingerprint analysis of hit songs
## Author: Ned Blackburn
## Date Created: 2024-11-27

## ------------------------------------------------------------------------
# RQ1: how have the musical features of popular songs changed over time?
# RQ2: is there a meaningful difference in average musical features across eras?
## ------------------------------------------------------------------------


# -------------visualising the evolution of musical characteristics over time ----------

#define a vector of the characteristics so we can use as an argument in subsequent functions

features_list <- c('Acousticness', 'Danceability', 'Energy', 'Instrumentalness', 'Tempo', 'Loudness','Liveness','Speechiness', 'Valence')

# exploring how related musical characteristics are correlated

song_master |>
  select(features_list) |>
  cor() |>
  corrplot(method = 'square', type = 'lower', order = 'FPC', diag = FALSE, tl.col="black",tl.cex = 0.9)

#exploring distributions of each feature

song_master |>
  select(features_list) |>
  pivot_longer(cols = everything(), names_to = 'Feature', values_to = 'Value') |>
  ggplot(aes(x = Value)) +
  geom_density(aes(fill = Feature, y = ..scaled..)) +
  facet_wrap(~Feature, scales = 'free_y') +
  labs(x = 'Value',
       y = 'Density') +
  theme_ipsum_rc(grid = 'X', axis_text_size = 9, axis_title_size = 11,
                 plot_margin = margin(20, 20, 20, 20), axis_title_just = 'mc') +
  theme(legend.position = 'none',
        strip.text.x = element_text(size = 11, color = 'black'),
        axis.title.x = element_text(vjust = -2, face = 'bold'),
        axis.title.y = element_text(vjust = 3, face = 'bold')) +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  scale_x_continuous(labels = label_number(accuracy = 0.1))

# drop unneeded columns,then calculate yearly average values for each characteristic and convert to long format for faceting 

song_features_long_all <- song_master |>
  select(14:23) |>
  group_by(release_year) |>
  summarise(across(features_list, mean)) |>
  pivot_longer(
    cols = features_list,
    names_to = "feature",
    values_to = "yearly_avg"
  )

#create facet plot for whole dataset

ggplot(data = song_features_long_all, aes(x = release_year, y = yearly_avg, group = feature, color = feature)) +
  geom_smooth(size = 1, alpha = 0.95, se = FALSE) +
  geom_point(size = 0.7, alpha = 0.6, color = 'black') +
  geom_vline(xintercept = 1991, linetype = "dotted", color = "black", size = 0.7) +
  facet_wrap(~ feature, scales = "free_y") +
  labs(
    x = "Year",
    y = "Average Value"
  ) +
  theme_ipsum_rc(grid = 'XY',
                 axis_title_size = 12, 
                 axis_title_face = 'bold', 
                 axis_title_just = 'mc',
                 axis_text_size = 10,
                 plot_margin = margin(10, 10, 10, 10)) +
  theme(legend.position = 'none',
        axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 3)) +
  scale_color_viridis_d(option = 'H') +
  scale_x_continuous(breaks = breaks_pretty(5))

# Boxplot comparing musical features pre and post 1991 ----------------------------

song_features_long_era <- song_master |>
  select(14:23,25) |>
  group_by(release_year, era) |>
  pivot_longer(
    cols = features_list,
    names_to = "Feature",
    values_to = "Value"
  )

#reverse the factor order so the pre- and post- 1991 features are the right way around
song_features_long_era$era <- factor(song_features_long_era$era, levels = rev(levels(song_features_long_era$era)))

#draw the boxplot 

ggplot(song_features_long_era, aes(x = Feature, y = Value, fill = era)) +
  geom_boxplot(
    position = position_dodge(width = 0.8),
    alpha = 0.7,
    outlier.shape = 16,
    outlier.size = 0
  ) +
  stat_summary(
    fun = median,
    geom = "point",
    shape = 18,
    size = 3,
    color = "black",
    position = position_dodge(width = 0.8)
  ) +
  scale_fill_viridis_d(option = "D") +
  labs(x = "", y = "Value", fill = "Era") +
  theme_ipsum_rc(grid = 'XY') +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 12, vjust = 2),
    legend.position = 'bottom',
    legend.text = element_text(size = 12),
    legend.title = element_text(face = 'bold'),
    legend.box.margin = margin(-30, 0, 0, 0)
  )

#pairwise signifiance tests for differences of means for each feature in each era and calculate effect sizes

pairwise_tests_features <- song_features_long_era |>
  group_by(Feature) |>
  rstatix::wilcox_test(Value ~ era, paired = FALSE) |>  # Perform Wilcoxon test
  add_significance()  # Add significance labels

effect_sizes_features <- song_features_long_era |>
  group_by(Feature) |>
  wilcox_effsize(Value ~ era)



