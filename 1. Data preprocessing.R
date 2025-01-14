## ---------------------------
## Purpose of script: Ingesting, processing and joining MusicOSet data prior to analysis, and (roughly) categorising each song into one of 10 high-level genres
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
  select(-c(duration_ms,key,mode,time_signature, liveness)) |>
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
#filter for only songs released after 1962, when the dataset is supposed to begin

song_master <- song_master |>
  mutate(release_year = str_sub(release_date,1,4)) |>
  filter(release_year > 1961) 



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

# 2. Genre Classification -------------------------------------------------

#We have way too many niche genres. Let's amalgamate them based on keywords so we can actually analyse them

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
  filter(main_genre != '-') |>
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

#1b. EDA ---------------------------------------------------------------------

#plot release dates in dataset by year

songsbyyear <- song_master |>
  filter(release_year > 1959) |>
  ggplot(aes(x = release_year)) +
  geom_bar() +
  theme_ipsum_rc() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(size = 11, face = 'bold'),
        axis.title.y = element_text(size = 11, face = 'bold'),
        panel.grid.minor.x = element_blank()) +
  scale_x_continuous(breaks = seq(1960, 2019, by = 5)) +
  scale_fill_viridis_d()


#plot proportions of most/least popular songs released in each year

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


#popularity vs year-end score: are they the same?

song_master |>
  filter(release_year > 1961) |>
  ggscatterstats(
    x = popularity,
    y = year_end_score,
    type = 'non-parametric',
    point.args = list(alpha = 0.2)
  ) +
  theme_ipsum_rc() +
  labs(title = 'Popularity vs Year end score',
       subtitle = 'As popularity increases, so does year end score') 

#graph analysing genre popularity over time

genre_pop_year <- song_master |>
  filter(genre_agg != 'Other') |>
  group_by(genre_agg, release_year) |>
  summarise(yearscore = sum(year_end_score))

ggplot(data = genre_pop_year, aes(x = release_year, y = yearscore, group = genre_agg, color = genre_agg)) +
  geom_smooth(se = FALSE) +
  scale_color_viridis_d(option = 'H') +
  geom_vline(xintercept = 1991) +
  theme_ipsum_rc(grid = "XY")

#table with number of songs in each genre and percentages
genre_summary <- song_master |>
  group_by(genre_agg) |>
  summarise(n = n(), avgpop = mean(year_end_score)) |>
  mutate(prop = percent(n/sum(n), accuracy = 0.1)) |>
  arrange(by = desc(n))

