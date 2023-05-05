# 🦗🦗🦗----
#SET UP----
## Packages 🎁 ----
library(tidyverse)
library(colorBlindness)
library(rstatix)
library(patchwork)
## Import data 📥 ----
cricket_original <- read_csv("Data/cricket_song.csv")
head (cricket_original) #check data loaded successfully

#DATA WRANGLING----
##Clean 🧹 ----
cricket_clean <- janitor::clean_names(cricket_original) #snakecase
colnames(cricket_clean)
cricket_clean <- dplyr:: rename(cricket_clean, "size_mm"="pronotum",
                        "start_mass"="mass0",
                        "song_duration"="song_week1",
                        "weight_change"="delta_smi")
#names easier for me to work with 
#better descriptions of variables

## Error Check 🧐----
cricket_clean%>%duplicated()%>%sum() #no duplicates

summary(cricket_clean)
#minimum value for song duration not possible  
#cannot have negative duration
cricket_abs<-mutate(.data=cricket_clean, song_duration = abs(song_duration)) 
summary(cricket_abs) #removes neg values for song duration

##Mutate ----

cricket_categories <- mutate(.data=cricket_abs, diet_category = cut(as.numeric(cricket_abs$diet), 
                      breaks=c(0,36,48,84), labels = c("Low","Medium","High")))
#allows comparison between groups

#PLOTS 📊 ----
##Initial Exploration----
group_colour <-c("#d90b15", "#f79011", "#05f52d")

cricket_abs %>% filter(song_duration !=0) %>%
  ggplot(aes(x=weight_change, y=song_duration, colour=diet))+
  geom_point()+geom_smooth(method="lm", colour="BLACK",   
                           se=FALSE)+
  scale_colour_gradient2(low="#c20a13", mid="#f79011", high="#0cf734",
                         midpoint=48)+
  theme_classic() 
#Both weight change and diet may have an effect on song duration

##Relationships between variables----

cricket_abs %>% ggplot(aes(x=size_mm, y=weight_change))+
  geom_point()+ geom_smooth(method="lm",    
                            se=FALSE)
#suggests possible interaction between size and weight change

cricket_abs %>% ggplot(aes(x=diet, y=weight_change))+
  geom_point()+ geom_jitter()+ geom_smooth(method="lm",    
                                           se=FALSE)
#suggests possible interaction between diet and weight change

cricket_abs %>% filter(song_duration !=0)%>% ggplot(aes(x=size_mm, y=song_duration))+
  geom_point()+ geom_smooth(method="lm",    
                            se=FALSE)
#size may have an effect on song duration



#MODEL 📈----
##Linear model----
###Song----
lsmodel1 <- lm(song_duration ~ diet + weight_change +size_mm
               + weight_change:diet + weight_change:size_mm, data=cricket_abs)


performance::check_model(lsmodel1, check=c("qq", "homogeneity"))
# qq= slight curve, particularly at lower end 
# homogeneity =both ends curl up/down, could be improved
performance::check_model(lsmodel1, check="outliers")
#no outliers

drop1(lsmodel1, test= "F")
#test suggests that the interaction terms do not improve model fit
# aic does not change much, do not keep interaction terms in model

lsmodel2 <- lm(song_duration ~ diet + weight_change + size_mm, data=cricket_abs)
performance::check_model(lsmodel2, check=c("qq", "homogeneity"))
drop1(lsmodel2, test= "F")
# size does not have a significant imact on model 
# remove size from model 

lsmodel3 <-  lm(song_duration ~ diet + weight_change, data=cricket_abs)
performance::check_model(lsmodel3, check=c("qq", "homogeneity"))
performance::check_model(lsmodel3, check="outliers")
#acceptable fit, no outliers
drop1(lsmodel3, test= "F")
# all terms are relevant to model
broom::tidy(lsmodel3)

###Weight----
lsmodel_weight <- lm(weight_change ~ diet, data=cricket_abs)
performance::check_model(lsmodel_weight, check=c("qq", "homogeneity"))
performance::check_model(lsmodel_weight, check="outliers")
#acceptable fit, no outliers
broom::tidy(lsmodel_weight)

##Log transformation----
lsmodel_plus <- lm(song_duration +1 ~ diet + weight_change, data=cricket_abs)
MASS::boxcox(lsmodel_plus)
# 0 is outside the conf interval so log data may not help
lsmodel_log <- lm(log(song_duration+1) ~ diet 
               +weight_change, data=cricket_abs)

performance::check_model(lsmodel_log, check=c("qq", "homogeneity"))
#even worse fit
#Use model 3 


#PLOTS FOR REPORT----
## Diet, Weight change ----
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

##Diet, Duration----
diet_duration <- cricket_categories %>% filter(song_duration !=0) %>% 
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
  scale_y_continuous(name= "Song Duration (seconds)")+
  theme_classic()+
  theme(axis.title = element_text(size = 7))

diet_duration
ggsave("Graphs/diet_duration_march23.png", width=14, height=7.5, units="cm", dpi=300)
colorBlindness::cvdPlot() #colours are accessible

##Means----
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

### Weight, diet----
summary_weight <- data_summary(cricket_categories, varname="weight_change", 
                    groupnames="diet_category")
plot_means_dw <- ggplot(summary_weight, aes(x=diet_category, y=weight_change, 
                           group=diet_category, colour=diet_category)) + 
  geom_pointrange(aes(ymin=weight_change-sd, ymax=weight_change+sd), 
                  show.legend = FALSE)+
  scale_colour_manual(values = group_colour)+
  geom_label(aes(label=c(0.00359, 0.0151, 0.0273)), 
             size=2, show.legend = FALSE)+
  labs(x="Diet Category", y="Mean Weight Change (g)")+
  scale_y_continuous(position="right")+
  theme_classic()+
  theme(axis.title = element_text(size = 7))
plot_means_dw

###Duration, diet----
summary_duration <- data_summary(cricket_categories, varname="song_duration", 
                               groupnames="diet_category")
plot_means_dd <- ggplot(summary_duration, aes(x=diet_category, y=song_duration, 
                                            group=diet_category, colour=diet_category)) + 
  geom_pointrange(aes(ymin=song_duration-sd, ymax=song_duration+sd), 
                  show.legend = FALSE)+
  scale_colour_manual(values = group_colour)+
  geom_label(aes(label=c(4.513, 6.314, 7.393)), 
             size=2, show.legend = FALSE)+
  labs(x="Diet Category", y="Mean Song Duration (seconds)")+
  scale_y_continuous(position="right")+
  theme_classic()+
  theme(axis.title = element_text(size = 7))
plot_means_dd

##Three factor Scatter----
triple <- cricket_abs %>% filter(song_duration !=0) %>%
  ggplot(aes(x=weight_change, y=song_duration, colour=diet))+
  geom_point()+geom_smooth(method="lm", colour="BLACK",   
                           se=FALSE)+
  scale_colour_gradient2(low="#c20a13", mid="#f79011", high="#0cf734",
                         midpoint=48,
                         breaks=seq(12,84,12),
                         labels=c(12,24,36,48,60,72,84))+
  labs(x="Weight Change (g)", y="Song Duration (s)", 
       colour= "Diet\n(Nutritional\nPercentage)")+
  theme_classic()+
  theme(legend.key.width  = unit(1.9, "cm"),
        legend.title = element_text(size=8),
        legend.title.align=0.5,
        legend.text=element_text(size=7),
        legend.position="bottom",
        legend.justification = "left")+
   guides(colour =guide_colourbar(title.vjust = 2))
 
triple
ggsave("Graphs/triple_gradient_april23.png", width=14, height=7.5, units="cm", dpi=300)

##Patchworks 🧶----

###Diet, weight----
patchwork_dw <- diet_weightchange + plot_means_dw + 
  plot_layout (guides = "collect", widths = c(2, 1))
 
patchwork_dw

ggsave("Graphs/dw_means_april23.png", width=14, height=7.5, units="cm", dpi=300)

###Diet, duration----
patchwork_dd <- diet_duration + plot_means_dd + 
  plot_layout (guides = "collect", widths = c(2, 1))
patchwork_dd

ggsave("Graphs/dd_means_april23.png", width=14, height=7.5, units="cm", dpi=300)



