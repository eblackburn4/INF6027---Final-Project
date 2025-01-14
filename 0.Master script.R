
## ---------------------------
## Purpose of script: Master script to run the other individual script files sequentially
## Author: Ned Blackburn
## Date Created: 2024-12-28
## ---------------------------

source('00.Packages.R') #loads necessary libraries, stored separately for ease
source('1. Data preprocessing.R') #reads in data, joins it all together, adds genre information, does some EDA
source('2. Musical fingerprint analysis.R') #analysis of musical features
source('3. Lyric Sentiment analysis') #sentiment analysis of song lyrics
source('4. Changepoint and regression analysis.R') #changepoint analysis of eras and regression analysis to predict song popularity