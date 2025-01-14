
## ---------------------------
## Purpose of script: changepoint and regression analysis
## Author: Ned Blackburn
## Date Created: 2025-01-13
## ---------------------------

# Changepoint analysis: song features ----------------------------------------------------

#detect optimal number of changepoints for the master feature index (the average of all features, per year)

segment_all <- song_features_long_all |>
    group_by(release_year) |>
    summarise(avg_prop = mean(yearly_avg)) 

#finding the optimum number of changepoints using AIC

segtest <- selgmented(segment_all$avg_prop, Kmax=10, type='aic', plot.ic=TRUE, check.dslope = FALSE)
plot(segtest, res=TRUE, col=2, lwd=3) 

#building the segmented model using the automatically-determined changepoints and summarise the coefficients

lm_all <- lm(avg_prop ~ release_year, data = segment_all) 
seg_model_all <- segmented(lm_all, seg.Z = ~release_year, npsi = segtest$selection.psi$npsi)
summary_all <- summary(seg_model_all)

#extract breakpoints and round to nearest year

breakpoints <- round(seg_model_all$psi[, "Est."],0)
segment_all$fitted_values <- fitted(seg_model_all)

#plot the fitted segmented model

ggplot(segment_all, aes(x = release_year, y = avg_prop)) +
  geom_point(color = "gray", size = 2, alpha = 0.7) +  
  geom_line(aes(y = fitted_values), size = 1) +  
  geom_vline(xintercept = breakpoints, linetype = "dashed")  +
  annotate("text", x=1989, y=0.425, label="1991", angle=90, size = 4, color = 'grey30') + 
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


#same thing, but wrapped in a function to easily generate for each individual feature

segment_feature_func <- function(feature_name) {
  
segment_feature <- song_features_long_all |>
  filter(feature == feature_name) |>
  select(!feature)

segtest_feature <- selgmented(segment_feature$yearly_avg, 
                              Kmax=5, 
                              type='aic', 
                              plot.ic=TRUE, 
                              check.dslope = FALSE)  
plot(segtest_feature, res=TRUE, col=2, lwd=3) 

lm_feature <- lm(yearly_avg ~ release_year, data = segment_feature)
seg_mod_feature <- segmented(lm_feature, seg.Z = ~release_year, npsi = segtest_feature$selection.psi$npsi)
summary_feature <- summary(seg_mod_feature)
breakpoints_feat <- round(seg_mod_feature$psi[, "Est."],1)
segment_feature$fitted_values <- fitted(seg_mod_feature)

#plot the fitted segmented model

ggplot(segment_feature, aes(x = release_year, y = yearly_avg)) +
  geom_point(color = "gray", size = 2, alpha = 0.7) +  
  geom_line(aes(y = fitted_values), size = 1) +  
  geom_vline(xintercept = breakpoints_feat, linetype = "dashed")  +
  annotate("text", x=1989, y=0.425, label="1991", angle=90, size = 3.6, color = 'grey30') + 
  labs(
    title = "SEGMENTS!!!!",
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
} 

segment_feature_func('danceability')

