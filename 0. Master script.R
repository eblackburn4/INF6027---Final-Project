
## ---------------------------
## Purpose of script: Master script to run the other individual script files sequentially
## Author: Ned Blackburn
## Date Created: 2024-12-28
## ---------------------------


#These must be run in order, or they won't work - other than scripts 3 and 4, which can be run in either order

source('1. Packages.R') #loads necessary packages
source('2. Data preprocessing.R') #reads in data, joins it all together, adds genre information, does some EDA
source('3. Musical feature analysis.R') #analysis of musical features
source('4. Sentiment Analysis.R') #sentiment analysis of song lyrics
source('5. Change point Analysis.R') #changepoint analysis of paradigm shifts in lyrical sentiment and musical features 