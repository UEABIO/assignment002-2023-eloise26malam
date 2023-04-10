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
diet_weightchange <- cricket_abs %>% filter(song_duration !=0) %>%
  ggplot(aes(x=diet, y=weight_change, group=diet))+
  theme_classic()+
  geom_rect(xmin= -Inf,
            xmax= Inf,
            ymin= -Inf, 
            ymax=0, fill="#ff4f4f")+
  geom_rect(xmin= -Inf,
            xmax= Inf,
            ymin= 0, 
            ymax= Inf, fill="#6bff72")+
  geom_boxplot(aes(fill=diet), show.legend = FALSE, outlier.size=0.5) +
  scale_fill_gradient(low="#616161", high="#fcfcfc")+
  scale_x_continuous(name="Diet (nutritional %)", 
                     breaks=seq(12,84,12))+
  scale_y_continuous(name= "Weight Change (g)",
                     breaks=seq(-0.060, 0.090, 0.03))
diet_weightchange
ggsave("Graphs/diet_weightchange_march23.png", width=14, height=7.5, units="cm", dpi=300)
colorBlindness::cvdPlot() #colours are accessible
##diet, duration----
diet_duration <- cricket_abs %>% filter(song_duration !=0) %>% 
  ggplot(aes(x=diet, y=song_duration, group=diet))+
  geom_boxplot(aes(fill=diet), show.legend = FALSE, outlier.size=0.5) +
  scale_fill_gradient(low="#616161", high="#fcfcfc")+
  scale_x_continuous(name="Diet (nutritional %)", 
                     breaks=seq(12,84,12))+
  scale_y_continuous(name= "Song Duration (minutes)")+
  theme_classic()
diet_duration 
ggsave("Graphs/diet_duration_march23.png", width=14, height=7.5, units="cm", dpi=300)
##high, med, low diet, song----
high_diet <- cricket_abs %>% filter(diet>=60, song_duration != 0) %>% 
  ggplot(aes(x=song_duration, y=weight_change))+
  geom_point() +  geom_smooth(method="lm",    
                              se=FALSE)
high_diet  

mid_diet <- cricket_abs %>% filter(diet==48, song_duration != 0) %>% 
  ggplot(aes(x=song_duration, y=weight_change))+
  geom_point() + geom_smooth(method="lm",    
                             se=FALSE)
mid_diet

low_diet <- cricket_abs %>% filter(diet<=36, song_duration != 0) %>% 
  ggplot(aes(x=song_duration, y=weight_change))+
  geom_point()+ geom_smooth(method="lm",    
                            se=FALSE)
low_diet

##weight change, duration----
cricket_abs %>% filter(song_duration !=0) %>%
  ggplot(aes(x=song_duration, y=weight_change, colour=diet))+
  geom_point()+geom_smooth(method="lm", colour="BLACK",   
                           se=FALSE)+
  scale_colour_gradient(low="#2b0002", high="#ff2930")+
  theme_classic()
colorBlindness::cvdPlot() #colours are accessible

#MODEL----
lsmodel1 <- lm(song_duration ~ diet, data=cricket_abs)
checklsm1 <- performance::check_model(lsmodel1)
checklsm1

##Categorised model----
cricket_categories <- mutate(.data=cricket_abs, diet_category = cut(as.numeric(cricket_abs$diet), 
                                                                     breaks=c(0,36,48,84), labels = c("Low","Medium","High")))

lsmodel_cat <- lm(song_duration ~ diet_category, data=cricket_categories)
summary(lsmodel_cat)
checklsm_cat <- performance::check_model(lsmodel_cat)
checklsm_cat
#t value = 

#Diet, weight-----
lsmodel_dw <- lm(weight_change ~ diet_category, data=cricket_categories)
summary(lsmodel_dw)
