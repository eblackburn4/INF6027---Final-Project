
## ---------------------------
## Purpose of script: Master script to run the other individual script files sequentially
## Author: Ned Blackburn
## Date Created: 2024-12-28
## ---------------------------

source('1. Data preprocessing.R') #reads in data, joins it all together, adds genre information, does EDA
source('2. Musical fingerprint analysis.R')
source('3. Lyric Sentiment analysis')