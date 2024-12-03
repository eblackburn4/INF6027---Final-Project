## ---------------------------
## Purpose of script: Intro to data science final project: R Code
## Author: Ned Blackburn
## Date Created: 2024-11-27

options(scipen = 6, digits = 5) 
library(tidyverse)
library(hrbrthemes)
library(GGally)
library(ggfortify)

setwd("~/Desktop/Intro to DS:data vis")
## ---------------------------

song_pop <- read.csv('musicoset_popularity/song_pop.csv', sep = "")
song_chart <- read.csv('musicoset_popularity/song_chart.csv', sep = "")
hits <- read.csv('hits_dataset.csv', sep = "\t")
nonhits <- read.csv('nonhits_dataset.csv', sep = "\t")

