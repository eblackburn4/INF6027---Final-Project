## ---------------------------
## Purpose of script: Intro to data science final project part I: R Code for sentiment analysis 
## of hit song lyrics and comparison to nob hits
## Author: Ned Blackburn
## Date Created: 2024-11-27

options(scipen = 6, digits = 5) 
library(tidyverse)
library(hrbrthemes)
library(GGally)
library(ggfortify)
library(tidytext)

setwd("~/Desktop/Intro to DS")

## ------------------------------------------------------------------------
# RQ1: how has the lyrical sentiment of popular songs changed over time?
# RQ2: is there a relationship between lyrical sentiment and musical 'valence'?
## ------------------------------------------------------------------------


# 1. Reading in data ------------------------------------------------------

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
  select(!c("billboard","explicit"))


#check for na values - all checks return 0 so we're good to proceed
sum(is.na(song_meta))
sum(is.na(song_lyrics))
sum(is.na(track_meta))


#join all datasets on the 'song_id' column 
song_master <- left_join(song_meta, track_meta, by = 'song_id') |>
  left_join(song_lyrics, by = 'song_id')

#split out 'year' value from release date for consistency across release

song_master <- song_master |>
  mutate(release_year = str_sub(release_date,1,4))

song_master$popularity <- as.numeric(song_master$popularity)
song_master$release_year <- as.numeric(song_master$release_year)
song_master$release_date <- as.Date(song_master$release_date)

#Flag songs in the upper and lower quartiles of popularity

song_master <- song_master |>
  mutate(pop_quartile = case_when(
    popularity <= quantile(popularity, 0.25) ~ "Bottom 25%",
    popularity >= quantile(popularity, 0.75) ~ "Top 25%",
    TRUE ~ "Middle 50%"
    )
  )
  
# EDA ---------------------------------------------------------------------

#plot release dates in dataset by year

songsbyyear <- song_master |>
  filter(release_year > 1959) |>
  ggplot(aes(x = release_year)) +
  geom_bar(fill = 'purple') +
  theme_ipsum_rc() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(size = 11, face = 'bold'),
        axis.title.y = element_text(size = 11, face = 'bold'),
        panel.grid.minor.x = element_blank()) +
  scale_x_continuous(breaks = seq(1960, 2019, by = 5)) 

songsbyyear

#plot proportions of most/least popular songs release in each year

songsbyyearpop <- song_master |>
  filter(release_year > 1959) |>
  ggplot(aes(x = release_year, fill = pop_quartile, color = pop_quartile)) +
  geom_bar(position = 'fill') +
  theme_ipsum_rc() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(size = 12),
        panel.grid.minor.x = element_blank()) +
  scale_x_continuous(breaks = seq(1900, 2019, by = 5)) +
  scale_fill_viridis_d() +
  scale_color_viridis_d()

songsbyyearpop 

#--------------------------------- 2.Sentiment analysis preprocessing --------------------------------------------

#remove meta-words and linebreaks in the the lyrics column that are not actually part of the lyrics e.g. 'verse 1'

unwantedtext <- c('Verse','Chorus', 'Intro','1','2','3','4')
song_master <- song_master |>
  mutate(lyrics = str_remove_all(lyrics, str_c(unwantedtext, collapse = "|"))) |>
  mutate(lyrics = str_replace_all(lyrics, "\\\\n", " ")) |>
  mutate(lyrics = str_replace_all(lyrics, "'", "")) |>
  mutate(lyrics = str_replace_all(lyrics, "\\\\" , ""))

#load in sentiment lexicon

lexicon <- get_sentiments("bing")

#use tidytext's 'unnest tokens' function to split each word into its own row, then inner join with the sentiment lexicon

sm_tokenized_lyrics <- song_master |>
  filter(lyrics != "") |>
  unnest_tokens(word, lyrics)

#join the sentiment lexicon with the tokenized lyrics, and remove stopwords

sm_tokenized_lyrics_sentiment <- sm_tokenized_lyrics |>
  inner_join(lexicon, by = 'word')  |>
  anti_join(stop_words, by = 'word')

#exploring number of positive vs negative words, and most common positive and negative words

word_counts_posneg <- sm_tokenized_lyrics_sentiment |>
  count(word, sentiment, sort = TRUE)

pos_vs_neg <- word_counts_posneg |>
  ggplot(aes(x = sentiment, fill = sentiment)) +
  geom_bar() +
  labs(title = 'Incidence of unique positive and negative words',
       subtitle = 'Based on a sample of Billboard 100 charting songs from 1962-2018',
       x = 'Sentiment',
       y = 'Number of unique words',
       caption = 'Data from MusicOSet. Sentiment analysis using lexicon from Bing et al. (2004)')+
  theme_ipsum_rc() +
  theme(legend.position = 'none',
        axis.title = element_text(face = 'bold')) +
  scale_fill_viridis_d()

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

#Find average sentiment of songs released in each year, by year

avg_sentiment_year_all <- sm_tokenized_lyrics_sentiment |>
  filter(release_year > 1959) |>
  select(c('release_year', 'word', 'sentiment')) |>
  group_by(release_year, sentiment) |>
  summarise(word_count = n(), .groups = 'drop') |>
  pivot_wider(names_from = sentiment, values_from = word_count) |>
  summarise(release_year, sentscore = (positive - negative)/(positive + negative))

avg_sentiment_plot_all <- avg_sentiment_year_all |>
  ggplot() +
  geom_line(aes(x = release_year, y = sentscore)) +
  geom_smooth(aes(x = release_year, y = sentscore), method = 'lm', se = FALSE, color = 'purple') +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  theme_ipsum_rc() +
  scale_colour_viridis_c()


#examine seasonal trends, by grouping data across all years by month and day of release

avg_sentiment_seasonal <- sm_tokenized_lyrics_sentiment |>
  filter(release_year < 2019 & release_year > 2009) |>
  mutate(release_date = as.character(release_date)) |>
  mutate(release_month_day = substr(release_date, nchar(release_date) -4 , nchar(release_date))) |>
  select(c('release_month_day', 'word', 'sentiment')) |>
  group_by(release_month_day, sentiment) |>
  summarise(word_count = n(), .groups = 'drop') |>
  pivot_wider(names_from = sentiment, values_from = word_count) |>
  summarise(release_month_day, sentscore = (positive - negative)/(positive + negative))
  
#plot the graph (I have to add a dummy 'year' onto the start of the 'release_month_date' column otherwise R doesn't allow me to store it as a date)

avg_sentiment_plot_seasonal <- avg_sentiment_seasonal |>
  mutate(release_month_day = as.Date(paste0("2022-", release_month_day), "%Y-%m-%d")) |>
  ggplot(aes(x = release_month_day, y = sentscore)) +
  geom_line() +
  geom_smooth(method = 'loess', se = FALSE) +
  theme_ipsum_rc() +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 month") +
  scale_colour_viridis_c()

#same graph, but grouped by month

avg_sentiment_seasonal_month <- sm_tokenized_lyrics_sentiment |>
  filter(release_year > 1969 & release_year < 1980) |>
  mutate(release_date = as.character(release_date)) |>
  mutate(release_month = substr(release_date, 6,7)) |>
  drop_na(release_month) |>
  select(c('release_month', 'word', 'sentiment')) |>
  group_by(release_month, sentiment) |>
  summarise(word_count = n(), .groups = 'drop') |>
  pivot_wider(names_from = sentiment, values_from = word_count) |>
  summarise(release_month, sentscore = (positive - negative)/(positive + negative))

avg_sentiment_plot_seasonal_month <- avg_sentiment_seasonal_month |>
  mutate(release_month_day = as.Date(paste0("2022-", release_month, "-01"), "%Y-%m-%d")) |>
  ggplot(aes(x = release_month_day, y = sentscore, group = 1)) +
  geom_point(aes(color = sentscore > 0), size = 3) +
  geom_line(linewidth = 0.5, color = 'darkgrey') +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  theme_ipsum_rc() +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(size = 12),
        panel.grid.minor.x = element_blank()) +
  labs(title = '1970s') +
  scale_colour_viridis_d(option = 'plasma')

avg_sentiment_plot_seasonal_month

