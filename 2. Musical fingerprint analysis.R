## ---------------------------
## Purpose of script: Intro to data science final project part I: R Code for genre and musical fingerprint analysis of hit songs
## Author: Ned Blackburn
## Date Created: 2024-11-27

options(scipen = 6, digits = 5) 
library(tidyverse)
library(hrbrthemes)
library(GGally)
library(ggfortify)
library(tidytext)
library(textdata)
library(scico)
library(rstatix)
library(coin)
library(mcp)
library(segmented)

# -------------visualising the evolution of musical characteristics over time ----------

# drop unneeded columns, then rescale loudness values to between 0 and 1 for consistency with the rest of the dataset
# then calculate yearly average values for each characteristic and convert to long format for faceting 

song_features_long_all <- song_master |>
  select(14:22) |>
  mutate(loudness = (loudness - min(loudness))/(max(loudness) - min(loudness))) |>
  group_by(release_year) |>
  summarise(across(c(acousticness, danceability, energy, instrumentalness, liveness, loudness, speechiness, valence), mean)) |>
  pivot_longer(
    cols = c(acousticness, danceability, energy, instrumentalness, liveness, loudness, speechiness, valence),
    names_to = "feature",
    values_to = "yearly_avg"
  )

song_features_long_nonhits <- song_master |>
  select(14:23) |>
  filter(pop_quartile != 'Top 25%') |>
  mutate(loudness = (loudness - min(loudness))/(max(loudness) - min(loudness))) |>
  group_by(release_year) |>
  summarise(across(c(acousticness, danceability, energy, instrumentalness, liveness, loudness, speechiness, valence), mean)) |>
  pivot_longer(
    cols = c(acousticness, danceability, energy, instrumentalness, liveness, loudness, speechiness, valence),
    names_to = "feature",
    values_to = "yearly_avg"
  )

song_features_long_hits <- song_master |>
  select(14:23) |>
  filter(pop_quartile == 'Top 25%') |>
  mutate(loudness = (loudness - min(loudness))/(max(loudness) - min(loudness))) |>
  group_by(release_year) |>
  summarise(across(c(acousticness, danceability, energy, instrumentalness, liveness, loudness, speechiness, valence), mean)) |>
  pivot_longer(
    cols = c(acousticness, danceability, energy, instrumentalness, liveness, loudness, speechiness, valence),
    names_to = "feature",
    values_to = "yearly_avg"
  )


# Create facet plot comparing top 25% hits to others

ggplot(data = song_features_long_nonhits, aes(x = release_year, y = yearly_avg, group = feature)) +
  geom_smooth(size = 0.8, color = '#0d0887', alpha = 0.8, se = FALSE) +
  geom_point(size = 0.6, color = '#0d0887', alpha = 0.6) +
  geom_smooth(data = song_features_long_hits, size = 0.7, color = '#e3cd02', se = FALSE) +
  geom_point(data = song_features_long_hits, alpha = 0.6, size = 0.6, color = '#fce303') +
  geom_vline(xintercept = 1991, linetype = "dotted", color = "black", size = 0.7) +
  facet_wrap(~ feature, scales = "free_y") +
  labs(
    title = "Average Evolution of Musical Characteristics Over Time",
    x = "Year",
    y = "Average Value"
  ) +
  theme_ipsum_rc() 

#create facet plot for whole dataset

ggplot(data = song_features_long_all, aes(x = release_year, y = yearly_avg, group = feature)) +
  geom_smooth(size = 0.8, color = 'black', alpha = 0.8, se = FALSE) +
  geom_point(size = 0.6, color = 'darkgrey', alpha = 0.6) +
  geom_vline(xintercept = 1991, linetype = "dotted", color = "black", size = 0.7) +
  facet_wrap(~ feature, scales = "free_y") +
  labs(
    title = "Average Evolution of Musical Characteristics Over Time",
    x = "Year",
    y = "Average Value"
  ) +
  theme_ipsum_rc() 


# Boxplot Comparing musical features pre and post 1991 ----------------------------

song_features_long_era <- song_master |>
  select(14:22,24) |>
  mutate(loudness = (loudness - min(loudness))/(max(loudness) - min(loudness))) |>
  group_by(release_year, era) |>
  pivot_longer(
    cols = c(acousticness, danceability, energy, instrumentalness, liveness, loudness, speechiness, valence),
    names_to = "Feature",
    values_to = "Value"
  )

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
  labs(
    title = "Distribution of Musical Features by Era with Pairwise Significance",
    x = "Musical Features",
    y = "Value",
    fill = "Era"
  ) +
  theme_ipsum_rc(grid = 'XY') +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12)
  )

#pairwise signifiance tests for differences of means for each feature in each era and calculate effect sizes

pairwise_tests <- song_features_long_era |>
  group_by(Feature) |>
  rstatix::wilcox_test(Value ~ era, paired = FALSE) |>  # Perform Wilcoxon test
  add_significance()  # Add significance labels

effect_sizes <- song_features_long_era |>
  group_by(Feature) |>
  wilcox_effsize(Value ~ era)


#change point analysis

testdat <- song_features_long_all |>
  filter(feature == 'loudness') |>
  dplyr::select(!feature)

testdatamalg <- song_features_long_all |>
  group_by(release_year) |>
  summarise(avg_prop = mean(yearly_avg))

segtest<-selgmented(testdatamalg$avg_prop, Kmax=10, type='aic', plot.ic=TRUE, check.dslope = FALSE) 
plot(segtest, res=TRUE, col=2, lwd=3)

lm_songs <- lm(avg_prop ~ release_year, data = testdatamalg)

# Fit a segmented model with 2 breakpoints
seg_model <- segmented(lm_songs, seg.Z = ~release_year, npsi = 2)

# Summary of the segmented model
summary(seg_model)

breakpoints <- seg_model$psi[, "Est."]
print(breakpoints)  

testdatamalg$fitted_values <- fitted(seg_model)

ggplot(testdatamalg, aes(x = release_year, y = avg_prop)) +
  geom_point(color = "gray", size = 2, alpha = 0.7) +  # Scatterplot of data
  geom_line(aes(y = fitted_values), size = 1) +  # Segmented model fit
  geom_vline(xintercept = breakpoints, linetype = "dashed")  +
  annotate("text", x=1990, y=0.45, label="1991", angle=90) + # Breakpoints
  labs(
    title = "Two-Breakpoint Model",
    x = "Release Year",
    y = "Average Value"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  ) +
  theme_ipsum_rc()