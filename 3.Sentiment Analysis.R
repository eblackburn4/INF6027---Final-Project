## ---------------------------
## Purpose of script: Intro to data science final project part II: R Code for sentiment analysis 
## of hit song lyrics
## Author: Ned Blackburn
## Date Created: 2024-11-28

## ------------------------------------------------------------------------
# RQ1: how has the lyrical sentiment of popular songs changed over time?
# RQ2: is there a relationship between lyrical sentiment and musical 'valence'?
## ------------------------------------------------------------------------


# Sentiment analysis preprocessing --------------------------------------------

#remove meta-words and linebreaks in the the lyrics column that are not actually part of the lyrics e.g. 'verse 1'

unwantedtext <- c('Verse','Chorus', 'Intro','1','2','3','4')
song_master <- song_master |>
  mutate(lyrics = str_remove_all(lyrics, str_c(unwantedtext, collapse = "|"))) |>
  mutate(lyrics = str_replace_all(lyrics, "\\\\n", " ")) |>
  mutate(lyrics = str_replace_all(lyrics, "'", "")) |>
  mutate(lyrics = str_replace_all(lyrics, "\\\\" , ""))

#load in sentiment lexicon

lexicon_afinn <- get_sentiments("afinn")


#use tidytext's 'unnest tokens' function to split each word into its own row, then inner join with the sentiment lexicon
#songs with no lyrics are filtered out

sm_tokenized_lyrics <- song_master |>
  filter(lyrics != "") |>
  unnest_tokens(word, lyrics)

#join the sentiment lexicon with the tokenized lyrics, and remove stopwords

sm_tokenized_lyrics_sentiment_afinn <- sm_tokenized_lyrics |>
  inner_join(lexicon_afinn, by = 'word')  |>
  anti_join(filter(stop_words), by = 'word')

#exploring number of positive vs negative words, and most common positive and negative words

word_counts_posneg <- sm_tokenized_lyrics_sentiment_afinn |>
  count(word, value, sort = TRUE)

word_counts_posneg |>
  group_by(value) |>
  summarise(incidences = sum(n)) |>
  ggplot(aes(x = as.factor(value), y = incidences, fill = as.factor(value))) +
  geom_col() +
  labs(x = 'Sentiment (5 is most positive, -5 is most negative)',
       y = 'Total number of words') +
  theme_ipsum_rc(grid = 'Y', axis_title_size = 12) +
  theme(
    legend.position = 'none',
    axis.title = element_text(face = 'bold'),
    panel.grid.major.x = element_blank(),  
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(vjust = 3)
  ) +
  scale_fill_scico_d(palette = 'vikO', direction = -1) 

word_counts_posneg |>
  mutate(sentiment = ifelse(value < 0, 'Negative', 'Positive')) |>
  group_by(sentiment) |>
  slice_max(n, n = 10) |>
  ggplot(aes(x= reorder(str_to_title(word), n), y=n, fill = sentiment)) +
  geom_col() +
  facet_wrap(~sentiment, scales = 'free_y') +
  coord_flip() +
  labs(x = '',
       y = 'Word Count')+
  theme_ipsum_rc(grid = 'X', axis_title_size = 12) +
  theme(legend.position = 'none',
        strip.text = element_text(face = "italic")) +
  scale_fill_scico_d(palette = 'vik', direction = -1)


# Sentiment analysis ------------------------------------------------------

#Find average sentiment of songs released in each year, by year

avg_sentiment_year_all_afinn <- sm_tokenized_lyrics_sentiment_afinn |>
  select(c('release_year', 'word', 'value')) |>
  group_by(release_year) |>
  summarise(sentiment_score = sum(value), .groups = 'drop')

avg_sentiment_year_all_afinn |>
  ggplot(aes(x = as.numeric(release_year), y = sentiment_score)) +
  geom_point() +
  geom_smooth(method = 'loess', se = FALSE, color = '#3b528b') +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  theme_ipsum_rc(grid = 'XxY', axis_title_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.3),
        axis.title.x =element_text(size = 12),
        axis.title.y = element_text(size = 12)) 


#alternative: find average song sentiment per year

avg_sentiment_year_per_song_afinn <- sm_tokenized_lyrics_sentiment_afinn |>
  select(c('release_year', 'song_id', 'word', 'value')) |>
  group_by(song_id, release_year) |>
  summarise(sentiment_score = mean(value), .groups = 'drop') |>
  group_by(release_year) |>
  summarise(sentiment_score_year = mean(sentiment_score))

avg_sentiment_year_per_song_afinn |>
  ggplot(aes(x = as.numeric(release_year), y = sentiment_score_year)) +
  geom_point() +
  geom_smooth(method = 'loess', se = FALSE, color = '#3b528b') +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  theme_ipsum_rc(grid = 'XxY', axis_title_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.3),
        axis.title.x =element_text(size = 12),
        axis.title.y = element_text(size = 12)) 


#comparison of mean sentiment across eras with Mann-Whitney (Wilcoxson) test and effect sizes

pairwise_tests_lyrics <- sm_tokenized_lyrics_sentiment_afinn |>
  select(c('release_year', 'song_id', 'word', 'value', 'era')) |>
  group_by(song_id, era) |>
  summarise(sentiment_score = mean(value), .groups = 'drop') |>
  select(c('sentiment_score', 'era')) |>
  rstatix::wilcox_test(sentiment_score ~ era, paired = FALSE) |>  
  add_significance()

effsize_lyrics <- sm_tokenized_lyrics_sentiment_afinn |>
  select(c('release_year', 'song_id', 'word', 'value', 'era')) |>
  group_by(song_id, era) |>
  summarise(sentiment_score = mean(value), .groups = 'drop') |>
  select(c('sentiment_score', 'era')) |>
  wilcox_effsize(sentiment_score ~ era)

# plot of explicitness: percentage of explicit songs released in each year

prop_explicit_plot <- song_master |>
  filter(release_year > 1961) |>
  mutate(is_explicit = ifelse(explicit == 'True',1,0)) |>
  group_by(release_year) |>
  summarise(prop = sum(is_explicit)/n()) |>
  ggplot(aes(x = release_year, y = prop)) +
  geom_col(fill = 'pink') +
  geom_abline(slope = 1, intercept = 1990) +
  theme_ipsum_rc()

#average sentiment by genre

sent_by_genre <- sm_tokenized_lyrics_sentiment_afinn |>
  group_by(genre_agg, era) |>
  summarise(meansent = mean(value)) 

ggplot(data = sent_by_genre, aes(x = genre_agg, y = meansent, color = era, group = era)) +
  geom_line(aes(group = genre_agg), color = "grey90", linewidth = 5) +
  geom_point(position = 'dodge', size = 5) +
  theme_ipsum_rc(axis_text_size = 10, axis_title_size = 12) +
  scale_color_viridis_d()








