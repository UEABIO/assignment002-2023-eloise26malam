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
  geom_point()+ geom_smooth(method="lm",    
                           se=FALSE)

## diet, weight change ----
cricket_abs %>% ggplot(aes(x=diet, y=weight_change, group=diet))+
  geom_rect(xmin= -Inf,
            xmax= Inf,
            ymin= -Inf, 
            ymax=0, fill="#f54842")+
  geom_rect(xmin= -Inf,
            xmax= Inf,
            ymin= 0, 
            ymax= Inf, fill="#6bff72")+
  geom_boxplot(aes(fill=diet)) +
  scale_fill_gradient(low="#616161", high="#fcfcfc")+
  scale_x_continuous(name="Diet- nutritional %", breaks=seq(12,84,12))

##diet, duration----
cricket_abs %>% ggplot(aes(x=diet, y=song_duration, group=diet))+
  geom_boxplot(aes(fill=diet)) + abline(h=0)
  theme_minimal()

##high, med, low diet, song----
high_diet <- cricket_abs %>% filter(diet>=60, song_duration != 0) %>% ggplot(aes(x=song_duration, y=weight_change))+
  geom_point() +  geom_smooth(method="lm",    
                              se=FALSE)
high_diet  

mid_diet <- cricket_abs %>% filter(diet==48, song_duration != 0) %>% ggplot(aes(x=song_duration, y=weight_change))+
  geom_point() + geom_smooth(method="lm",    
                             se=FALSE)
mid_diet

low_diet <- cricket_abs %>% filter(diet<=36, song_duration != 0) %>% ggplot(aes(x=song_duration, y=weight_change))+
  geom_point()+ geom_smooth(method="lm",    
                            se=FALSE)
low_diet


