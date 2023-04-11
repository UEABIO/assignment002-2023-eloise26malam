#SET UP----
## Packages 🎁 ----
library(tidyverse)
library(ggplot2)
library(colorBlindness)
library(patchwork)
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
#as cannot have negative duration
cricket_abs<-mutate(.data=cricket_clean, song_duration = abs(song_duration))
cricket_abs #removes neg values

##Mutate----
cricket_categories <- mutate(.data=cricket_abs, diet_category = cut(as.numeric(cricket_abs$diet), 
                                                                    breaks=c(0,36,48,84), labels = c("Low","Medium","High")))
#PLOTS 📊 ----
group_colour <-c("#d90b15", "#f79011", "#05f52d")


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
                     breaks=seq(-0.060, 0.090, 0.03))+
  theme(axis.title = element_text(size = 7))
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
  theme_classic()+
  theme(axis.title = element_text(size = 7))
diet_duration 
ggsave("Graphs/diet_duration_march23.png", width=14, height=7.5, units="cm", dpi=300)

diet_duration2 <- cricket_abs %>% filter(song_duration !=0) %>% 
  ggplot(aes(x=diet, y=song_duration, group=diet))+
  geom_boxplot(aes(fill=diet), show.legend = FALSE, outlier.size=0.5) +
  scale_fill_gradient2(low="#d90b15", mid="#f79011", high="#05f52d",
                       midpoint=48)+
  scale_x_continuous(name="Diet (nutritional %)", 
                     breaks=seq(12,84,12))+
  scale_y_continuous(name= "Song Duration (minutes)")+
  theme_classic()+
  theme(axis.title = element_text(size = 7))
diet_duration2

diet_duration3 <- cricket_categories %>% filter(song_duration !=0) %>% 
  ggplot(aes(x=diet, y=song_duration,group=diet))+
  geom_rect(xmin= -Inf,
            xmax= 42,
            ymin= -Inf, 
            ymax=Inf, fill="#d90b15")+
  geom_rect(xmin= 42,
            xmax= 54,
            ymin= -Inf, 
            ymax=Inf, fill="#f79011")+
  geom_rect(xmin= 54,
            xmax= Inf,
            ymin= -Inf, 
            ymax=Inf, fill="#05f52d")+
  geom_boxplot(aes(), show.legend = FALSE, outlier.size=0.5, fill="#fcfcfc") +
  scale_x_continuous(name="Diet (nutritional %)", 
                     breaks=seq(12,84,12))+
  scale_y_continuous(name= "Song Duration (minutes)")+
  theme_classic()+
  theme(axis.title = element_text(size = 7))
  
diet_duration3

##weight change, duration----
cricket_abs %>% filter(song_duration !=0) %>%
  ggplot(aes(x=song_duration, y=weight_change, colour=diet))+
  geom_point()+geom_smooth(method="lm", colour="BLACK",   
                           se=FALSE)+
  scale_colour_gradient2(low="#d90b15", mid="#f79011", high="#05f52d",
                         midpoint=48)+
  theme_classic()
colorBlindness::cvdPlot() #colours are accessible


#MODEL----
lsmodel1 <- lm(song_duration ~ diet, data=cricket_abs)
checklsm1 <- performance::check_model(lsmodel1)
checklsm1

##Categorised----

#allows easier comparison between groups by reducing num of categories

##Diet, weight-----
lsmodel_dw <- lm(weight_change ~ diet_category, data=cricket_categories)
summary(lsmodel_dw)


##Diet, duration ----
lsmodel_cat <- lm(song_duration ~ diet_category, data=cricket_categories)
summary(lsmodel_cat)
checklsm_cat <- performance::check_model(lsmodel_cat)
checklsm_cat


##Emmeans Plots----
###Diet, weight -----
means_dw <- emmeans::emmeans(lsmodel_dw, specs = ~ diet_category)
means_dw

plot_means_dw<-means_dw %>% 
  as_tibble() %>% 
  ggplot(aes(x=diet_category, 
             y=emmean, colour=diet_category))+
  scale_colour_manual(values = group_colour)+
  geom_pointrange(aes(
    ymin=lower.CL, 
    ymax=upper.CL), show.legend = FALSE)+
  labs(x="Diet Category", y="Mean Weight Change (g)")+
  scale_y_continuous(position="right")+
  theme_classic()+
  theme(axis.title = element_text(size = 7))

plot_means_dw

### Diet, duration----
means_dd <- emmeans::emmeans(lsmodel_cat, specs = ~ diet_category)
means_dd

plot_means_dd<-means_dd %>% 
  as_tibble() %>% 
  ggplot(aes(x=diet_category, 
             y=emmean, colour=diet_category))+
  scale_colour_manual(values = group_colour)+
  geom_pointrange(aes(
    ymin=lower.CL, 
    ymax=upper.CL), show.legend = FALSE)+
  labs(x="Diet Category", y="Mean Song Duration")+
  scale_y_continuous(position="right")+
  theme_classic()+
  theme(axis.title = element_text(size = 7))
plot_means_dd

#PATCHWORK 🧶----

##Diet, weight----
patchwork_dw <- diet_weightchange + plot_means_dw + 
  plot_layout (guides = "collect", widths = c(2, 1))
 
patchwork_dw

ggsave("Graphs/dw_means_april23.png", width=14, height=7.5, units="cm", dpi=300)

##Diet, duration----
patchwork_dd <- diet_duration3 + plot_means_dd + 
  plot_layout (guides = "collect", widths = c(2, 1))
patchwork_dd

ggsave("Graphs/dd_means_april23.png", width=14, height=7.5, units="cm", dpi=300)
