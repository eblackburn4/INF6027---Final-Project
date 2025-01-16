## ---------------------------
## Purpose of script: Ingesting, processing and joining MusicOSet data prior to analysis
## Author: Ned Blackburn
## Date Created: 2024-11-27
## ---------------------------


#1a. Reading in data ------------------------------------------------------

#read in data for lyrics, track metadata (for release date), artist metadata (for genre), song popularity, and musical features, and drop unneeded columns

track_meta <- read.csv("Data/musicoset_metadata/tracks.csv", sep = "\t") |>
  select(!c('album_id', 'track_number'))

song_lyrics <- read.csv("Data/musicoset_songfeatures/lyrics.csv", sep = "\t")

artist_meta <- read_delim("Data/musicoset_metadata/artists.csv",
                          delim = "\t",          # Main delimiter seems to be tabs based on a visual inspection
                          escape_double = TRUE,  # Handle stray quotes
                          col_names = TRUE,
                          trim_ws = TRUE) 

artist_meta <- artist_meta |>
  select(artist_id, main_genre)

#read in song popularity data
song_pop <- read_delim("Data/musicoset_popularity/song_pop.csv", delim = "\t") |>
  arrange(year) |>
  select(song_id, year_end_score) |>
  distinct(song_id, .keep_all = TRUE)

#read in data on musical fingerprints, drop unneeded columns
song_features <- read_delim("Data/musicoset_songfeatures/acoustic_features.csv", delim = "\t") |>
  select(-c(duration_ms,key,mode,time_signature)) |>
  drop_na() |>
  filter(tempo != 0)

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

#split up 'artists' column so to isolate the artist_ids, for matching purposes, and clean up extraneous characters

song_meta <- song_meta |>
  separate(artists, into = c("artist_id", "artist_name"), sep = ": ", remove = FALSE)
  
song_meta$artist_id <- gsub("[{}']", "", song_meta$artist_id)
song_meta$artist_name <- gsub("[{}']", "", song_meta$artist_name)  
song_meta$artist_name <- trimws(song_meta$artist_name)  

#check for na values - all checks return 0 on the columns we care about so we're good to proceed

sum(is.na(song_meta))
sum(is.na(song_lyrics))
sum(is.na(track_meta))
sum(is.na(song_pop))
sum(is.na(artist_meta))
sum(is.na(song_features))


#Join everything together and drop extraneous values to create the master dataset

song_master <- inner_join(song_meta, track_meta, by = 'song_id') |>
  inner_join(song_lyrics, by = 'song_id') |>
  inner_join(song_pop, by = 'song_id') |>
  inner_join(artist_meta, by = 'artist_id') |>
  inner_join(song_features, by = 'song_id')
  
#split out 'year' value from release date for consistency across releases in terms of granularity
#filter for only songs released after 1964, where N = at least 100 for each year to suppress noise

song_master <- song_master |>
  mutate(release_year = str_sub(release_date,1,4)) |>
  filter(release_year > 1963) 

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

#flag songs that were released pre 1991 vs post 1991

song_master <- song_master |>
  mutate(era = as_factor(case_when(
    release_year < 1991 ~ 'Pre-1991',
    release_year >= 1991 ~ 'Post-1991'
    )))

#rescale all musical feature variables to a relative index (0 to 1, min max scaling) so they can be combined and compared
#rename feature variables to title case so they look nice on graphs

song_master <- song_master |>
  mutate(across(14:22, rescale)) |>
  rename_with(~ str_to_title(.), .cols = 14:22)

# Genre Classification -------------------------------------------------

#We have way too many niche genres. Let's amalgamate them based on keywords so we can actually analyse them
#NB: Genre analysis was not included in the report but I left this in the code as it was part of my EDA

#define keywords (based on genres and subgenres from allmusic.com)

hip_hop <- c('hip hop', 'hip-hop', 'rap', 'g funk', 'crunk')
jazz_funk <- c('jazz', 'funk', 'bebop')
soul_randb <- c('r&b', 'soul', 'funk', 'jack', 'gospel', 'quiet storm', 'disco', 'groove','motown')
country <- c('country', 'bluegrass')
blues <- c('blues')
folk <- c('folk')
rock_metal <- c('rock', 'indie','permanent wave','metal','nu','emo','punk','core','screamo', 'british invasion') 
pop <- c('pop', 'neo mellow', 'boy band', 'girl group')
electronic <- c('house', 'techno', 'dance', 'edm', 'electro','big room','dubstep','brostep','downtempo', 'reggaeton', 'miami', 'bounce')
adult_contemporary_classical <- c('adult', 'standards', 'easy','soundtrack','classical','symphony','orchestra')


song_master <- song_master |>
  mutate(main_genre = str_to_lower(main_genre)) |>
  mutate(
    genre_agg = case_when(
      str_detect(main_genre, str_c(hip_hop, collapse = "|")) ~ "Hip-Hop",
      str_detect(main_genre, str_c(jazz_funk, collapse = "|")) ~ "Jazz/Funk",
      str_detect(main_genre, str_c(soul_randb, collapse = "|")) ~ "Soul/R&B",
      str_detect(main_genre, str_c(country, collapse = "|")) ~ "Country",
      str_detect(main_genre, str_c(blues, collapse = "|")) ~ "Blues",
      str_detect(main_genre, str_c(folk, collapse = "|")) ~ "Folk",
      str_detect(main_genre, str_c(rock_metal, collapse = "|")) ~ "Rock/Metal",
      str_detect(main_genre, str_c(pop, collapse = "|")) ~ "Pop",
      str_detect(main_genre, str_c(electronic, collapse = "|")) ~ "Electronic",
      str_detect(main_genre, str_c(adult_contemporary_classical, collapse = "|")) ~ "Adult Contemporary",
      TRUE ~ "Other"
    )
  )

# initial EDA ---------------------------------------------------------------------

#plot release dates in dataset by year

song_master |>
  ggplot(aes(x = release_year)) +
  geom_bar(fill = '#3b528b') +
  labs(x = 'Release year',
       y = 'Number of releases') +
  theme_ipsum_rc(grid = 'XY') +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8, size = 10),
        axis.text.y = element_text(hjust = 1.3, size = 10),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.title= element_text(hjust = 1)) +
  scale_x_continuous(breaks = breaks_pretty(10))


#plot proportions of most/least popular songs released in each year

song_master |>
  ggplot(aes(x = release_year, fill = pop_quartile)) +
  geom_bar(position = 'fill', width = 1) +
  theme_ipsum_rc(grid = 'Y') +
  labs(x = 'Release year',
       y = 'Proportion',
       fill = 'Year-end score quartile') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
        axis.text.y = element_text(angle = 0, hjust = -1, vjust = 0.5, size = 10),
        axis.title.x =element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.ticks.x = element_line(size = 0.5, color = 'grey'),
        axis.ticks.y = element_line(size = 0.5, color = 'grey'),
        legend.position = 'bottom',
        legend.title = element_text(size = 11)) +
  scale_x_continuous(
    limits = c(1964, 2019), 
    breaks = seq(1964, 2019, by = 5)) +
  scale_fill_viridis_d() 


#popularity vs year-end score: are they the same?

song_master |>
  ggplot(aes(x = popularity, y = year_end_score)) +
  geom_point(color = '#3b528b', alpha = 0.5) +
  geom_xsidehistogram(fill = '#f89540') +
  geom_ysidehistogram(fill = '#cc4778') +
  labs(x = 'Popularity', y = 'Year-end score') +
  theme_ipsum_rc(axis_title_size = 12, axis_text_size = 11, 
                 axis_title_just = 'tr') +
  theme_ggside_void()
  




