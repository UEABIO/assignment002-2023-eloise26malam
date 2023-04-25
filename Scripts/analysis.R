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
cricket_abs#removes neg values for song duration

##Mutate ----

cricket_categories <- mutate(.data=cricket_abs, diet_category = cut(as.numeric(cricket_abs$diet), 
                      breaks=c(0,36,48,84), labels = c("Low","Medium","High")))

#allows comparison between groups
#PLOTS 📊 ----
##Exploratory----
group_colour <-c("#d90b15", "#f79011", "#05f52d")

cricket_categories %>% drop_na(song_duration) %>% filter(song_duration !=0)%>%
  ggplot(aes(x=weight_change, y=song_duration, colour=diet_category))+
  geom_point()+ geom_jitter()+
  geom_smooth(method="lm",se=FALSE)

##Relationships between variables----
cricket_abs %>% ggplot(aes(x=size_mm, y=start_mass))+
  geom_point()+ geom_smooth(method="lm",    
                            se=FALSE)
#strong relationship between size and start mass
cricket_abs %>% ggplot(aes(x=size_mm, y=weight_change))+
  geom_point()+ geom_smooth(method="lm",    
                            se=FALSE)
#suggests interaction between size and weight change
cricket_abs %>% ggplot(aes(x=diet, y=weight_change))+
  geom_point()+ geom_jitter()+ geom_smooth(method="lm",    
                                           se=FALSE)
#suggests interaction between diet and weight change

##Affects on duration----
###Size----
cricket_abs %>% filter(song_duration !=0)%>% ggplot(aes(x=size_mm, y=song_duration))+
  geom_point()+ geom_smooth(method="lm",    
                            se=FALSE)
#size may have an effect on song duration

###Weight Change----
cricket_weight %>% filter(song_duration !=0) %>%
  ggplot(aes(x=song_duration, y=weight_change, colour=diet))+
  geom_point()+geom_smooth(method="lm", colour="BLACK",   
                           se=FALSE)+
  scale_colour_gradient2(low="#d90b15", mid="#f79011", high="#05f52d",
                         midpoint=48)+
  theme_classic() #colours are accessible

#MODEL 📈----
##Linear model----
lsmodel1 <- lm(song_duration +1 ~ diet_category + weight_change +size_mm
               + weight_change:diet_category + weight_change:size_mm, data=cricket_categories)


performance::check_model(lsmodel1, check=c("qq", "homogeneity"))
# qq= slight curve, particularly at lower end 
# homogeneity =not flat, could be improved
performance::check_model(lsmodel1, check="outliers")
#no outliers

lsmodel1 %>%broom::tidy(conf.int = T)
summary(lsmodel1)
drop1(lsmodel1, test= "F")
#test suggests that the additional terms improve model fit
#keep interaction terms in model

##Log transformation----
MASS::boxcox(lsmodel1)
# 0 is outside the conf interval so log data may not help
lsmodel2 <- lm(log(song_duration+1) ~ diet_category 
               +weight_change+size_mm
               + weight_change:diet_category
               + size_mm:weight_change, data=cricket_categories)

performance::check_model(lsmodel2, check=c("qq", "homogeneity"))
#even worse fit

##GLM----
glm1 <- glm(song_duration ~ diet_category + weight_change + size_mm + 
           weight_change:diet_category + weight_change:size_mm,
           data=cricket_categories, family=poisson(link="log"))

lsmodel1 <- lm(song_duration ~ diet_category + weight_change +size_mm
               + weight_change:diet_category + weight_change:size_mm, data=cricket_categories)


performance::check_model(lsmodel1, check=c("qq", "homogeneity"))
# qq= slight curve, particularly at lower end 
# homogeneity =not flat, could be improved
performance::check_model(lsmodel1, check="outliers")
#no outliers

lsmodel1 %>%broom::tidy(conf.int = T)
summary(lsmodel1)
drop1(lsmodel1, test= "F")
#test suggests that the additional terms improve model fit
#keep interaction terms in model

##Log transformation----
MASS::boxcox(lsmodel1)
# 0 is outside the conf interval so log data may not help
lsmodel2 <- lm(log(song_duration+1) ~ diet_category 
               +weight_change+size_mm
               + weight_change:diet_category
               + size_mm:weight_change, data=cricket_categories)

performance::check_model(lsmodel2, check=c("qq", "homogeneity"))
#even worse fit

##GLM----
glm1 <- glm(song_duration ~ diet_category + weight_change + size_mm + 
           weight_change:diet_category + weight_change:size_mm,
           data=cricket_categories, family=poisson(link="log"))


performance::check_model(glm1, 
                         check = c("homogeneity",
                                   "qq"))
summary(glm1)

#PLOTS FOR REPORT----
## Diet, Weight change ----
diet_weightchange <- cricket_weight %>% filter(song_duration !=0) %>%
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
diet_duration <- cricket_abs %>% filter(song_duration !=0) %>% 
  ggplot(aes(x=diet, y=song_duration, group=diet))+
  geom_boxplot(aes(fill=diet), show.legend = FALSE, outlier.size=0.5) +
  scale_fill_gradient(low="#616161", high="#fcfcfc")+
  scale_x_continuous(name="Diet (nutritional %)", 
                     breaks=seq(12,84,12))+
  scale_y_continuous(name= "Song Duration (seconds)")+
  theme_classic()+
  theme(axis.title = element_text(size = 7))
diet_duration 

diet_duration2 <- cricket_abs %>% filter(song_duration !=0) %>% 
  ggplot(aes(x=diet, y=song_duration, group=diet))+
  geom_boxplot(aes(fill=diet), show.legend = FALSE, outlier.size=0.5) +
  scale_fill_gradient2(low="#d90b15", mid="#f79011", high="#05f52d",
                       midpoint=48)+
  scale_x_continuous(name="Diet (nutritional %)", 
                     breaks=seq(12,84,12))+
  scale_y_continuous(name= "Song Duration (seconds)")+
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
  scale_y_continuous(name= "Song Duration (seconds)")+
  theme_classic()+
  theme(axis.title = element_text(size = 7))

diet_duration3
ggsave("Graphs/diet_duration_march23.png", width=14, height=7.5, units="cm", dpi=300)

##Emmeans Plots----
means <- cricket_categories %>%                           # Get mean & standard deviation by group
  group_by(diet_category) %>%
  summarise_at(vars(song_duration, weight_change),
               list(mean = mean,
                    sd = sd), na.rm=TRUE) %>% 
  as.data.frame()
means

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
##Patchworks 🧶----

###Diet, weight----
patchwork_dw <- diet_weightchange + plot_means_dw + 
  plot_layout (guides = "collect", widths = c(2, 1))
 
patchwork_dw

ggsave("Graphs/dw_means_april23.png", width=14, height=7.5, units="cm", dpi=300)

###Diet, duration----
patchwork_dd <- diet_duration3 + plot_means_dd + 
  plot_layout (guides = "collect", widths = c(2, 1))
patchwork_dd

ggsave("Graphs/dd_means_april23.png", width=14, height=7.5, units="cm", dpi=300)

