## ---------------------------
## Purpose of script: Intro to data science final project part II: R Code for sentiment analysis 
## of hit song lyrics
## Author: Ned Blackburn
## Date Created: 2024-11-28

options(scipen = 6, digits = 5) 
library(tidyverse)
library(hrbrthemes)
library(GGally)
library(ggfortify)
library(tidytext)
library(textdata)
library(scico)
library(ggside)
library(ggstatsplot)

setwd("~/Desktop/Intro to DS")

## ------------------------------------------------------------------------
# RQ1: how has the lyrical sentiment of popular songs changed over time?
# RQ2: is there a relationship between lyrical sentiment and musical 'valence'?
## ------------------------------------------------------------------------


# Reading in data ------------------------------------------------------

#read in data for lyrics, track metadata (for release date), artist metadata (for genre), and song popularity, and drop unneeded columns

track_meta <- read.csv("Data/musicoset_metadata/tracks.csv", sep = "\t") |>
  select(!c('album_id', 'track_number'))

song_lyrics <- read.csv("Data/musicoset_songfeatures/lyrics.csv", sep = "\t")

artist_meta <- read_delim("Data/musicoset_metadata/artists.csv",
                          delim = "\t",          # Main delimiter seems to be tabs based on a visual inspection
                          escape_double = TRUE,  # Handle stray quotes
                          col_names = TRUE,
                          trim_ws = TRUE
)

#read in song popularity data
song_pop <- read_delim("Data/musicoset_popularity/song_pop.csv", delim = "\t") |>
  arrange(year) |>
  select(song_id, year_end_score) |>
  distinct(song_id, .keep_all = TRUE)


#read in song metadata. This particular CSV isn't formatted properly so will require additional cleaning steps

song_meta <- read_delim(
  "Data/musicoset_metadata/songs.csv",
  delim = "\t",          # Main delimiter seems to be tabs based on a visual inspection
  escape_double = TRUE,  # Handle stray quotes
  col_names = TRUE,
  trim_ws = TRUE
)

# function to clean extraneous symbols and text from the csv using regex

clean_csv <- function(x) {
  if (is.character(x)) {
    x <- str_remove_all(x, '^[",]+|[",]+$')
  }
  return(x)
}

# apply cleaning function to the whole csv and then split the csv manually using known column names from the musicOset data schema

song_meta <- song_meta |> 
  mutate(across(everything(), clean_csv)) |>
  separate(col = names(song_meta)[1], 
           into = c("song_id", "song_name", "billboard", "artists", "popularity", "explicit", "song_type"), 
           sep = "\t", 
           fill = "right", 
           extra = "merge") |>
  select(!c("billboard"))


#check for na values - all checks return 0 on the columns we care about so we're good to proceed
sum(is.na(song_meta))
sum(is.na(song_lyrics))
sum(is.na(track_meta))
sum(is.na(song_pop))
sum(is.na(artist_meta))


#join all datasets on the 'song_id' column to create 'song_master'. We'll use this as the basis for all subsequent analysis

song_master <- inner_join(song_meta, track_meta, by = 'song_id') |>
  inner_join(song_lyrics, by = 'song_id') |>
  inner_join(song_pop, by = 'song_id')

#split out 'year' value from release date for consistency across releases in terms of granularity

song_master <- song_master |>
  mutate(release_year = str_sub(release_date,1,4))

song_master$popularity <- as.numeric(song_master$popularity)
song_master$release_year <- as.numeric(song_master$release_year)
song_master$release_date <- as.Date(song_master$release_date)

#Flag songs in the upper and lower quartiles of popularity, just for interest

song_master <- song_master |>
  mutate(pop_quartile = case_when(
    popularity <= quantile(popularity, 0.25) ~ "Bottom 25%",
    popularity >= quantile(popularity, 0.75) ~ "Top 25%",
    TRUE ~ "Middle 50%"
  )
  )


# Sentiment analysis preprocessing --------------------------------------------

#remove meta-words and linebreaks in the the lyrics column that are not actually part of the lyrics e.g. 'verse 1'

unwantedtext <- c('Verse','Chorus', 'Intro','1','2','3','4')
song_master <- song_master |>
  mutate(lyrics = str_remove_all(lyrics, str_c(unwantedtext, collapse = "|"))) |>
  mutate(lyrics = str_replace_all(lyrics, "\\\\n", " ")) |>
  mutate(lyrics = str_replace_all(lyrics, "'", "")) |>
  mutate(lyrics = str_replace_all(lyrics, "\\\\" , ""))

#load in sentiment lexicon

lexicon_bing <- get_sentiments("bing")
lexicon_afinn <- get_sentiments("afinn")


#use tidytext's 'unnest tokens' function to split each word into its own row, then inner join with the sentiment lexicon
#songs with no lyrics are filtered out

sm_tokenized_lyrics <- song_master |>
  filter(lyrics != "") |>
  unnest_tokens(word, lyrics)

#join the sentiment lexicon with the tokenized lyrics, and remove stopwords

sm_tokenized_lyrics_sentiment_bing <- sm_tokenized_lyrics |>
  inner_join(lexicon_bing, by = 'word')  |>
  anti_join(stop_words, by = 'word')

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
  scale_fill_scico_d(palette = 'vikO') 

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


pos_vs_neg_unique


top10posneg <- word_counts_posneg |>
  group_by(sentiment) |>
  slice_max(n, n = 10) |>
  ggplot(aes(x= reorder(word, n), y=n, fill = sentiment)) +
  geom_col() +
  facet_wrap(~sentiment, scales = 'free_y') +
  coord_flip() +
  labs(title = 'Most common words by sentiment',
       x = '',
       y = 'Word Count',
       caption = 'Data from MusicOSet. Sentiment analysis using lexicon from Bing et al. (2004)')+
  theme_ipsum_rc() +
  theme(legend.position = 'none',
        strip.text = element_text(face = "italic")) +
  scale_fill_viridis_d()


# Sentiment analysis ------------------------------------------------------

#Find average sentiment of songs released in each year, by year

avg_sentiment_year_all <- sm_tokenized_lyrics_sentiment_bing |>
  filter(release_year > 1959) |>
  select(c('release_year', 'word', 'sentiment')) |>
  group_by(release_year, sentiment) |>
  summarise(word_count = n(), .groups = 'drop') |>
  pivot_wider(names_from = sentiment, values_from = word_count) |>
  summarise(release_year, sentscore = ((positive - negative)/(positive + negative)))

avg_sentiment_plot_all <- avg_sentiment_year_all |>
  filter(release_year > 1963) |>
  ggplot(aes(x = release_year, y = sentscore)) +
  geom_point() +
  geom_smooth(method = 'loess', se = FALSE, color = 'purple') +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  theme_ipsum_rc() +
  scale_colour_viridis_c()


#examine seasonal trends, by grouping data across all years by month of release

generate_sentiment_seasonal_graph_bing <- function(startdate, enddate) {
  avg_sentiment_seasonal_month <- sm_tokenized_lyrics_sentiment_bing |>
    filter(release_year >= startdate & release_year <= enddate) |>
    mutate(release_date = as.character(release_date)) |>
    mutate(release_month = substr(release_date, 6,7)) |>
    drop_na(release_month) |>
    select(c('release_month', 'word', 'sentiment')) |>
    group_by(release_month, sentiment) |>
    summarise(word_count = n(), .groups = 'drop') |>
    pivot_wider(names_from = sentiment, values_from = word_count) |>
    summarise(release_month, sentscore = (positive - negative)/(positive + negative))
  
  avg_sentiment_plot_seasonal_month_bing <- avg_sentiment_seasonal_month |>
    mutate(release_month_day = as.Date(paste0("2022-", release_month, "-01"), "%Y-%m-%d")) |>
    ggplot(aes(x = release_month_day, y = sentscore, group = 1)) +
    geom_point(aes(color = sentscore > 0), size = 3) +
    geom_line(linewidth = 0.5, color = 'darkgrey') +
    geom_abline(slope = 0, intercept = 0, linetype = 2) +
    theme_ipsum_rc() +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    ylim(NA, 0.2) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_text(size = 12),
          panel.grid.minor.x = element_blank()) +
    labs(title = startdate) +
    scale_colour_viridis_d(option = 'plasma')
  
  avg_sentiment_plot_seasonal_month_bing
}

#--------AFINN VERSIONS

avg_sentiment_year_all_afinn <- sm_tokenized_lyrics_sentiment_afinn |>
  filter(release_year > 1959) |>
  select(c('release_year', 'word', 'value')) |>
  group_by(release_year) |>
  summarise(sentiment_score = mean(value), .groups = 'drop')

avg_sentiment_plot_all_afinn <- avg_sentiment_year_all_afinn |>
  ggplot() +
  geom_point(aes(x = release_year, y = sentiment_score)) +
  geom_smooth(aes(x = release_year, y = sentiment_score), method = 'loess', se = FALSE, color = 'purple') +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  theme_ipsum_rc() +
  scale_colour_viridis_c()

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

