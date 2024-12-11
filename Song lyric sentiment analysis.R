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

setwd("~/Desktop/Intro to DS:data vis")

## ------------------------------------------------------------------------
# RQ1: how has the lyrical sentiment of popular songs changed over time?
# RQ2: is there a relationship between lyrical sentiment and musical 'valence'?
## ------------------------------------------------------------------------


# 1. Reading in data ------------------------------------------------------

#read in data for lyrics, track metadata (for release date) and artist metadata (for genre), and drop unneeded columns

track_meta <- read.csv("Data/musicoset_metadata/tracks.csv", sep = "\t") |>
  select(!c('album_id', 'track_number'))
  
song_lyrics <- read.csv("Data/musicoset_songfeatures/lyrics.csv", sep = "\t")

artist_meta <- read_delim('Data/musicoset_metadata/artists.csv',
  delim = "\t",          # Main delimiter seems to be tabs based on a visual inspection
  escape_double = TRUE,  # Handle stray quotes
  col_names = TRUE,
  trim_ws = TRUE
)
#read in data for song information (for popularity). This particular CSV isn't formatted properly so will require additional cleaning steps

song_meta <- read_delim(
  'Data/musicoset_metadata/songs.csv',
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
  select(!c('billboard','explicit'))



#check for na values - all checks return 0 so we're good to proceed
sum(is.na(song_meta))
sum(is.na(song_lyrics))
sum(is.na(track_meta))

#join all datasets on the 'song_id' column and filter out any that don't have lyrics
song_master <- left_join(song_meta, track_meta, by = 'song_id') |>
  left_join(song_lyrics, by = 'song_id') |>
  filter(lyrics != "")

# 2.Data preprocessing and EDA --------------------------------------------

#remove meta-words and linebreaks in the the lyrics column that are not actually part of the lyrics e.g. 'verse 1'

unwantedtext <- c('Verse','Chorus', 'Intro','1','2','3','4')
texttest <- song_master |>
  mutate(lyrics = str_remove_all(lyrics, str_c(unwantedtext, collapse = "|"))) |>
  mutate(lyrics = str_replace_all(lyrics, "\\\\n", ""))

#use tidytext's 'unnest tokens' function to split each word into its own row

texttest <- texttest |>
  unnest_tokens(word, lyrics)



