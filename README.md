This is the github repository for my INF6027 - Introduction to Data Science final project. It contains all the code necessary to generate the analysis and plots contained in the final report. It does not contain the source data; this can be found at https://marianaossilva.github.io/DSW2019/. 

The code is structured across 4 numbered scripts:
  - '1. Data preprocessing' reads in the various MusicOSet datasets, cleans and joins them to create a master dataset that is used for all subsequent analysis, as well as doing some         exploratory analysis on the data as a whole.
  - '2. Musical fingerprints' examines how the musical characteristics of popular songs (danceability, valence, etc) have evolved over time, and how they vary across genres
  - '3. Lyrical Sentiment' examines how the lyrical sentiment of popular songs has evolved over time, and the influence of seasonality on sentiment
  - '4. Logistic regression' builds on the previous scripts, by attempting to classify songs into one of two eras according to their musical and lyrical characteristics
  - '0. Master Script' simply runs all the other scripts in order, to avoid having to repeatedly load in and process the source data in each time.
