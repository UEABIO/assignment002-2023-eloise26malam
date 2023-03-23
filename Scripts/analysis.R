#SET UP----
## Packages ----
library(tidyverse)
library(ggplot2)
library(colorBlindness)
## Import data ----
cricket_original <- read_csv("Data/cricket_song.csv")
head (cricket_original) #check data loaded successfully

#DATA WRANGLING----
##Clean ----
cricket_clean <- janitor::clean_names(cricket_original) #snakecase
colnames(cricket_clean)
cricket_clean <- rename(cricket_clean, "size_mm"="pronotum",
                        "start_mass"="mass0",
                        "song_duration"="song_week1",
                        "weight_change"="delta_smi")
#names easier to work with 
#better descriptions of variables

## Error Check ----
cricket_clean%>%duplicated()%>%sum() #no duplicates

cricket_clean%>% summarise(min=min(song_duration, na.rm=TRUE), 
              max=max(song_duration, na.rm=TRUE))
#minimum values not possible as cannot have negative duration
cricket_abs<-mutate(.data=cricket_clean, song_duration = abs(song_duration))
cricket_abs #removes neg values


