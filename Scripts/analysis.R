#SET UP----
## Packages ----
library(tidyverse)
library(ggplot2)
library(colorBlindness)
## Import data ----
cricket_original <- read_csv("Data/cricket_song.csv")
head (cricket_original) #check data loaded sucessfully
