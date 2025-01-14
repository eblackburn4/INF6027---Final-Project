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
  anti_join(stop_words, by = 'word')

#exploring number of positive vs negative words, and most common positive and negative words

word_counts_posneg <- sm_tokenized_lyrics_sentiment_afinn |>
  count(word, value, sort = TRUE)

pos_vs_neg_total <- word_counts_posneg |>
  group_by(value) |>
  summarise(incidences = sum(n)) |>
  ggplot(aes(x = as.factor(value), y = incidences, fill = as.factor(value))) +
  geom_col() +
  labs(title = 'Incidence of Unique Positive and Negative Words',
       subtitle = 'Based on a sample of Billboard 100 charting songs from 1962-2018',
       x = 'Sentiment (5 is most positive)',
       y = 'Number of unique words',
       caption = 'Data from MusicOSet. Sentiment analysis using lexicon from Bing et al. (2004)') +
  theme_ipsum_rc() +
  theme(
    legend.position = 'none',
    axis.title = element_text(face = 'bold'),
    panel.grid.major.x = element_blank(),  
    panel.grid.minor.x = element_blank()
  ) +
  scale_fill_scico_d(palette = 'vikO', direction = -1) 

pos_vs_neg_unique <- word_counts_posneg |>
  group_by(value) |>
  summarise(incidences = n()) |>
  ggplot(aes(x = as.factor(value), y = incidences, fill = as.factor(value))) +
  geom_col() +
  labs(title = 'Incidence of Unique Positive and Negative Words',
       subtitle = 'Based on a sample of Billboard 100 charting songs from 1962-2018',
       x = 'Sentiment (5 is most positive)',
       y = 'Number of unique words',
       caption = 'Data from MusicOSet. Sentiment analysis using lexicon from Bing et al. (2004)') +
  theme_ipsum_rc() +
  theme(
    legend.position = 'none',
    axis.title = element_text(face = 'bold'),
    panel.grid.major.x = element_blank(),  
    panel.grid.minor.x = element_blank()
  ) +
  scale_fill_scico_d(palette = 'vikO') 

top10posneg <- word_counts_posneg |>
  mutate(sentiment = ifelse(value < 0, 'Negative', 'Positive')) |>
  group_by(sentiment) |>
  slice_max(n, n = 10) |>
  ggplot(aes(x= reorder(word, n), y=n, fill = sentiment)) +
  geom_col() +
  facet_wrap(~sentiment, scales = 'free_y') +
  coord_flip() +
  labs(title = 'Most common words by sentiment',
       x = '',
       y = 'Word Count',
       caption = 'Data from MusicOSet. Sentiment analysis using the Afinn lexicon')+
  theme_ipsum_rc() +
  theme(legend.position = 'none',
        strip.text = element_text(face = "italic")) +
  scale_fill_viridis_d(option = 'G')


# Sentiment analysis ------------------------------------------------------

#Find average sentiment of songs released in each year, by year

avg_sentiment_year_all_afinn <- sm_tokenized_lyrics_sentiment_afinn |>
  select(c('release_year', 'word', 'value')) |>
  group_by(release_year) |>
  summarise(sentiment_score = sum(value), .groups = 'drop')

avg_sentiment_plot_all_afinn <- avg_sentiment_year_all_afinn |>
  ggplot() +
  geom_point(aes(x = release_year, y = sentiment_score)) +
  geom_smooth(aes(x = release_year, y = sentiment_score), method = 'loess', se = TRUE, color = '#3b528b') +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  theme_ipsum_rc(axis_title_size = 12) +
  scale_colour_viridis_c()


#examine seasonal trends, by grouping data across all years by month of release. I put this into a function for easy generation of different year ranges

generate_sentiment_seasonal_graph_afinn <- function(startdate, enddate) {
  avg_sentiment_seasonal_month <- sm_tokenized_lyrics_sentiment_afinn |>
    filter(release_year >= startdate & release_year <= enddate) |>
    mutate(release_date = as.character(release_date)) |>
    mutate(release_month = substr(release_date, 6,7)) |>
    drop_na(release_month) |>
    select(c('release_month', 'word', 'value')) |>
    group_by(release_month) |>
    summarise(sentiment_score = mean(value), .groups = 'drop')
  
  avg_sentiment_plot_seasonal_month_afinn <- avg_sentiment_seasonal_month |>
    mutate(release_month_day = as.Date(paste0("2022-", release_month, "-01"), "%Y-%m-%d")) |>
    ggplot(aes(x = release_month_day, y = sentiment_score, group = 1)) +
    geom_point(aes(color = sentiment_score > 0), size = 4) +
    geom_line(linewidth = 0.5, color = 'darkgrey') +
    geom_abline(slope = 0, intercept = 0, linetype = 2) +
    theme_ipsum_rc() +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_text(size = 12),
          panel.grid.minor.x = element_blank(),
          legend.position = 'bottom') +
    ylim(-0.6,0.3) +
    labs(title = startdate,caption = 'AFINN') +
    scale_colour_viridis_d(option = 'viridis')
  
  avg_sentiment_plot_seasonal_month_afinn
}

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

