
## ---------------------------
## Purpose of script: changepoint and regression analysis
## Author: Ned Blackburn
## Date Created: 2025-01-13
## ---------------------------

## -----------------------------------------------------------------------
# RQ3: Is there any evidence for paradigm shifts representing rapid changes 
# in the evolution of musical characteristics?
## ------------------------------------------------------------------------


# Changepoint analysis: song features ----------------------------------------------------

# lyrical sentiment changepoints -----------------------------------------

segtest_lyrics <- selgmented(avg_sentiment_year_per_song_afinn$sentiment_score_year, 
                          Kmax=2, 
                          alpha = 0.05,
                          type='aic', 
                          plot.ic=TRUE, 
                          check.dslope = TRUE)

plot(segtest_lyrics, res=TRUE, col=2, lwd=3) 

#building the segmented model using the automatically-determined changepoints and summarise the coefficients

lm_lyrics <- lm(sentiment_score_year ~ release_year, data = avg_sentiment_year_per_song_afinn) 

seg_model_lyrics <- segmented(lm_lyrics, seg.Z = ~release_year, 
                           npsi = ifelse(segtest_lyrics$selection.psi$npsi > 0, segtest_lyrics$selection.psi$npsi, 1))

lyric_params <- summary(seg_model_lyrics)

#get confidence intervals for breakpoints
confint_lyrics <- confint(seg_model_lyrics)

#add fitted values to original data for plotting. If no segments, use the lm fit instead
avg_sentiment_year_per_song_afinn$fitted_values_lyrics <- if (segtest_lyrics$selection.psi$npsi > 0) {
  fitted(seg_model_lyrics)
} else {
  fitted(lm_lyrics)
}


# changepoints: individual features ---------------------------------------

#same thing as before but wrapped in a function to easily generate changepoint analysis for each individual feature

segment_feature_func <- function(feature_name) {
  
  segment_feature <- song_features_long_all |>
    filter(feature == feature_name) |>
    select(!feature)
  
  segtest_feature <- selgmented(segment_feature$yearly_avg,
                                alpha = 0.01,
                                Kmax=2, 
                                type='aic', 
                                plot.ic=TRUE, 
                                check.dslope = TRUE)  
  plot(segtest_feature, res=TRUE, col=2, lwd=3) 
  
  lm_feature <- lm(yearly_avg ~ release_year, data = segment_feature)
  seg_mod_feature <- segmented(lm_feature, seg.Z = ~release_year, 
                               npsi = ifelse(segtest_feature$selection.psi$npsi > 0, segtest_feature$selection.psi$npsi, 1))
  
  segment_feature$fitted_values <- if (segtest_feature$selection.psi$npsi > 0) {
    fitted(seg_mod_feature)
  } else {
    fitted(lm_feature)
  }
  
  print(summary(seg_mod_feature))
  print(confint(seg_mod_feature))
  
} 

#save the date of each feature's breakpoint 

loudness_seg <- segment_feature_func('Loudness')
valence_seg <- segment_feature_func('Valence')
liveness_seg <- segment_feature_func('Liveness')
danceability_seg <- segment_feature_func('Danceability')
energy_seg <- segment_feature_func('Energy')
acousticness_seg <- segment_feature_func('Acousticness')
speechiness_seg <- segment_feature_func('Speechiness')
instrumentalness_seg <- segment_feature_func('Instrumentalness')
tempo_seg <- segment_feature_func('Tempo')

