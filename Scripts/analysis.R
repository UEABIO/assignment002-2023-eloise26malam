#SET UP----
## Packages 🎁 ----
library(tidyverse)
library(ggplot2)
library(colorBlindness)
## Import data 📥 ----
cricket_original <- read_csv("Data/cricket_song.csv")
head (cricket_original) #check data loaded successfully

#DATA WRANGLING----
##Clean 🧹 ----
cricket_clean <- janitor::clean_names(cricket_original) #snakecase
colnames(cricket_clean)
cricket_clean <- rename(cricket_clean, "size_mm"="pronotum",
                        "start_mass"="mass0",
                        "song_duration"="song_week1",
                        "weight_change"="delta_smi")
#names easier to work with 
#better descriptions of variables

## Error Check 🧐----
cricket_clean%>%duplicated()%>%sum() #no duplicates

summary(cricket_clean)
#minimum value for song duration not possible  
#cannot have negative duration
cricket_abs<-mutate(.data=cricket_clean, song_duration = abs(song_duration))
cricket_abs #removes neg values

#PLOTS 📊 ----
## size, start mass----
cricket_abs %>% ggplot(aes(x=size_mm, y=start_mass))+
  geom_point()

## diet, weight change ----
cricket_abs %>% ggplot(aes(x=diet, y=weight_change, group=diet, fill=diet))+
  geom_boxplot()

##diet, duration----
cricket_abs %>% ggplot(aes(x=diet, y=song_duration, group=diet, fill=diet))+
  geom_boxplot()

high_diet <- cricket_abs %>% filter(diet>=60, song_duration != 0) %>% ggplot(aes(x=song_duration, y=weight_change))+
  geom_point()
high_diet  

low_diet <- cricket_abs %>% filter(diet<=36, song_duration != 0) %>% ggplot(aes(x=song_duration, y=weight_change))+
  geom_point()
low_diet

mid_diet <- cricket_abs %>% filter(diet==48, song_duration != 0) %>% ggplot(aes(x=song_duration, y=weight_change))+
  geom_point()
mid_diet
