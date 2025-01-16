<b> Project summary </b>

This study considered the evolution of musical and lyrical features of Billboard-charting songs from 1964-2018 using the MusicOSet database, examining their time trends and the extent to which these trends provided evidence for the existence of distinct ‘stylistic revolutions’; periods of rapid change in the direction or slope of musical and lyrical features. I used time series analysis and changepoint detection techniques to address the following research questions:

1.	How have the musical and lyrical features of charting songs evolved across 1964-2019? 
2.	Are there significant differences in average lyrical or musical features between songs released in the pre-1991 and post-1991 eras? 
3.	Is there evidence to support the existence of a ‘stylistic revolution’ in 1991 or otherwise?

<b> Key findings </b>

1.	Since the 1960s, popular music has become sadder both musically and lyrically, but simultaneously more danceable, energetic and loud. This aligns with the findings of previous research.
2.	Though most musical features post-1991 differ significantly on average compared to pre-1991, there is great overlap at the level of individual songs and little evidence for an ‘era effect’ centered around this date.
3.	The early 1980s represent the strongest candidate for an era of 'stylistic revolution': danceability and energy peaked, acousticness reached its low point, loudness began to increase, and valence began to drop. This may be as a result of genre shifts, particularly the rise of hip hop and electronic music.


<b> How to run the code </b>

The code is structured across 5 numbered scripts:
  - '0. Packages' defines the R packages used in the project, and what they are used for
  - '1. Master Script' runs all the other scripts in order
  - '2. Data preprocessing' reads in the various MusicOSet datasets, cleans and joins them to create a master dataset that is used for all subsequent analysis, as well as doing some         exploratory analysis on the data as a whole.
  - '3. Musical feature analysis' examines how the musical characteristics of popular songs (danceability, valence, etc) have evolved over time
  - '4. Sentiment analysis' examines how the lyrical sentiment of popular songs has evolved over time
  - '5. Change point analysis' builds on the previous two scripts to algorithmically identify points of significant change in feature time trends

To run the code, simply download all the scripts in this repository, organise the scripts and the source data (see below) in a single directory, then open the 'Master Script' file in RStudio and press 'Source'. This will run all the other scripts in the correct order. Individual scripts can be run from within the 'Master Script' file, but will throw an error if not run in the order listed (e.g. trying to run the sentiment analysis before loading in the data).

CSV source data can be found at https://marianaossilva.github.io/DSW2019/. The 'data preprocessing' script expects this to be in a folder called 'Data' with subfolders for metadata, popularity and features, which the source data is already organised into. All other file paths are completely dynamic provided all five scripts and the Data folder are located in the same working directory.
