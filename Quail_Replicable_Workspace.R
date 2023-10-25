
# R script for "The Average and Variable Thermal Environment Delays 
#               and Extends Critical Reproductive Behaviors in Scaled and Bobwhite Quail"

# Authors: William Kirkpatrick*, Rachel Carroll, Craig Davis, Sam Fuhlendorf, Erin Sauer, Sarah DuRant
# *primary contact

#### Library - all necessary packages and data paths ####

# Install 'pack' to continue
pack = c('data.table','ggplot2','ggthemes', 'ggpubr', 'dplyr','tidyverse','plyr','readr','gtools',
         'gridExtra','stringr','lme4','lmerTest','car','AICcmodavg','MuMIn')
#install.packages(pack)

library(data.table)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(dplyr)
library(tidyverse)
library(plyr)
library(readr)
library(gtools)
library(gridExtra)
library(stringr)
library(lme4)
library(lmerTest)
library(car)
library(AICcmodavg)
library(MuMIn)
library(showtext)


# Change 'path' as needed
path = "C:/Users/Will/Dropbox/Quail/Manuscript/Replicable_Data" 
setwd(path)


#### Read in Data ####

Qdat_2hr       = read.csv("High_Resolution_data_FINAL.csv")
Qdat_allbut2hr = read.csv("Low_Resolution_data_FINAL.csv")

st.time.dat = read.csv("Start.Time.LowRes_FINAL.csv")

# Split into morning and afternoon
am.st.time.dat = st.time.dat[st.time.dat$AMPM == "AM",]
pm.st.time.dat = st.time.dat[st.time.dat$AMPM == "PM",]

#### Model Selection ####

### High Resolution Data

## Duration

Univ_2hr_mod = lmer(data = Qdat_2hr, 
                    log_Duration ~ (1|ID) + (1|Year)
                    + Air.Mean.2hr + Air.SD.2hr 
                    + AMPM + Species
                    + Species:Air.Mean.2hr 
                    + Species:Air.SD.2hr
                    + AMPM:Species, 
                    na.action = na.fail)

TwohrNull = lmer(data = Qdat_2hr, 
                 log_Duration ~ (1|ID) + (1|Year))

AICc(TwohrNull)

# Dredge the universal model

TwoHrDredge = dredge(Univ_2hr_mod, beta = "none", trace = 1,
                     evaluate = T)
view(TwoHrDredge)

# Save the dredge
write.csv(data.frame(TwoHrDredge), file = "TwoHourDredge.csv")

## Timing

Univ_2hr_modTim = lmer(data = Qdat_2hr,
                       Start.Time ~ (1|ID) + (1|Year)
                       + Air.Mean.2hr + Air.SD.2hr 
                       + Species
                       + Species:Air.Mean.2hr 
                       + Species:Air.SD.2hr, 
                       na.action = na.fail)

TwohrNullTim = lmer(data = Qdat_2hr, Start.Time ~ (1|ID) + (1|Year))

AICc(TwohrNullTim)

# Dredge the universal model

TwoHrDredgeTim = dredge(Univ_2hr_modTim, beta = "none", trace = 1,
                        evaluate = T)
view(TwoHrDredgeTim)

# Save the dredge
write.csv(data.frame(TwoHrDredgeTim), file = "TwoHourDredgeTiming.csv")


### Low Resolution Data

## Duration

Univ_24hr_mod = lmer(data = Qdat_allbut2hr, 
                     log_Duration ~ (1|ID) + (1|Year)
                     + Air.Mean.24hr + Air.SD.24hr
                     + Air.Mean.Morn + Air.SD.Morn
                     + Air.Mean.Aft  + Air.SD.Aft
                     + Air.Mean.PrevAft + Air.SD.PrevAft
                     + Air.Mean.PrevDay + Air.SD.PrevDay
                     + Air.Mean.PrevNight + Air.SD.PrevNight
                     + Species:Air.Mean.24hr 
                     + Species:Air.SD.24hr
                     + Species:Air.Mean.Morn 
                     + Species:Air.SD.Morn
                     + Species:Air.Mean.Aft  
                     + Species:Air.SD.Aft
                     + Species:Air.Mean.PrevAft 
                     + Species:Air.SD.PrevAft
                     + Species:Air.Mean.PrevDay 
                     + Species:Air.SD.PrevDay
                     + Species:Air.Mean.PrevNight 
                     + Species:Air.SD.PrevNight
                     + Species,
                     na.action = na.fail)


Null_24hr = lmer(data = Qdat_allbut2hr, log_Duration ~ (1|ID) + (1|Year))

AICc(Null_24hr)

# Dredge - WARNING: This may take 10-20 minutes depending on the device. Feel free to skip to best fit models.

TwentyFourDredge = dredge(Univ_24hr_mod, beta = "none", trace = 1, evaluate = T,
                          m.lim = c(0,6))
view(TwentyFourDredge)

# Save the dredge
write.csv(data.frame(TwentyFourDredge), file = "TwentyFourDredge.csv")

## Timing - AM

Univ_24hr_modTim.AM = lmer(data = am.st.time.dat, 
                           Start.Time ~ (1|ID) + (1|Year)
                           + Air.Mean.24hr + Air.SD.24hr
                           + Air.Mean.Morn + Air.SD.Morn
                           + Air.Mean.Aft  + Air.SD.Aft
                           + Air.Mean.PrevAft + Air.SD.PrevAft
                           + Air.Mean.PrevDay + Air.SD.PrevDay
                           + Air.Mean.PrevNight + Air.SD.PrevNight
                           + Species:Air.Mean.24hr 
                           + Species:Air.SD.24hr
                           + Species:Air.Mean.Morn 
                           + Species:Air.SD.Morn
                           + Species:Air.Mean.Aft  
                           + Species:Air.SD.Aft
                           + Species:Air.Mean.PrevAft 
                           + Species:Air.SD.PrevAft
                           + Species:Air.Mean.PrevDay 
                           + Species:Air.SD.PrevDay
                           + Species:Air.Mean.PrevNight 
                           + Species:Air.SD.PrevNight
                           + Species,
                           na.action = na.fail)

Null_24hrTim.AM = lmer(data = am.st.time.dat, Start.Time ~ (1|ID) + (1|Year))

AICc(Null_24hrTim.AM)

# Dredge - WARNING: This may take 10-20 minutes depending on the device. Feel free to skip to best fit models.
TwentyFourDredgeTim.AM = dredge(Univ_24hr_modTim.AM, beta = "none", trace = 1, evaluate = T,
                                m.lim = c(0,6))
view(TwentyFourDredgeTim.AM)

# Save the dredge
write.csv(data.frame(TwentyFourDredgeTim.AM), file = "TwentyFourDredgeTiming.AM.csv")

# Timing - PM


Univ_24hr_modTim.PM = lmer(data = pm.st.time.dat, 
                           Start.Time ~ (1|ID) + (1|Year)
                           + Air.Mean.24hr + Air.SD.24hr
                           + Air.Mean.Morn + Air.SD.Morn
                           + Air.Mean.Aft  + Air.SD.Aft
                           + Air.Mean.PrevAft + Air.SD.PrevAft
                           + Air.Mean.PrevDay + Air.SD.PrevDay
                           + Air.Mean.PrevNight + Air.SD.PrevNight
                           + Species:Air.Mean.24hr 
                           + Species:Air.SD.24hr
                           + Species:Air.Mean.Morn 
                           + Species:Air.SD.Morn
                           + Species:Air.Mean.Aft  
                           + Species:Air.SD.Aft
                           + Species:Air.Mean.PrevAft 
                           + Species:Air.SD.PrevAft
                           + Species:Air.Mean.PrevDay 
                           + Species:Air.SD.PrevDay
                           + Species:Air.Mean.PrevNight 
                           + Species:Air.SD.PrevNight
                           + Species,
                           na.action = na.fail)

Null_24hrTim.PM = lmer(data = pm.st.time.dat, Start.Time ~ (1|ID) + (1|Year))

AICc(Null_24hrTim.PM)

# Dredge - WARNING: This may take 10-20 minutes depending on the device. Feel free to skip to best fit models.
TwentyFourDredgeTim.PM = dredge(Univ_24hr_modTim.PM, beta = "none", trace = 1, evaluate = T,
                                m.lim = c(0,6))
view(TwentyFourDredgeTim.PM)

# Save the dredge
write.csv(data.frame(TwentyFourDredgeTim.PM), file = "TwentyFourDredgeTiming.PM.csv")

#### Best Fit Models ####

## Duration
# High Res

BestMod_2hr = lmer(data = Qdat_2hr, log_Duration ~ 
                     (1|ID) + (1|Year)
                   + Air.Mean.2hr 
                   + AMPM + Species
                   + AMPM:Species)

summary(BestMod_2hr)

# Low Res

BestMod_24hr = lmer(data = Qdat_allbut2hr, log_Duration ~ 
                      (1|ID) + (1|Year)
                    +  Species)

summary(BestMod_24hr)

## Timing
# High Res

BestMod_2hrTim = lmer(data = Qdat_2hr, Start.Time ~ 
                        (1|ID) + (1|Year)
                      + Air.Mean.2hr + Air.SD.2hr 
                      + Species 
                      + Species:Air.Mean.2hr)

summary(BestMod_2hrTim)

# Low Res - AM

BestMod_24hrTim.AM = lmer(data = am.st.time.dat, Start.Time ~ 
                            (1|ID) + (1|Year) 
                          + Air.SD.PrevDay 
                          + Species)

summary(BestMod_24hrTim.AM)

# Low Res - PM

BestMod_24hrTim.PM = lmer(data = pm.st.time.dat, Start.Time ~ 
                            (1|ID) + (1|Year) 
                          + Air.Mean.Aft 
                          + Species)

summary(BestMod_24hrTim.PM)

#### Visualization ####

# Dimensions for various text sizes, margins, and plot outputs
axis.title.size = 110
axis.text.size = 100
plot.margins = c(10,10,10,50)
axis.title.margins = 25

point.size = 5

break.height = 0.25

Final.fig.dim.w = 8000
Final.fig.dim.h = 6000
Final.fig.dim.h.thin = 6000
Final.fig.dim.w.thin = 4500
Final.fig.dim.h.double = 9000
Final.fig.dim.w.double = 6000


# Adding Calibri and Calibri (bold) as fonts
font_add("Calibri", "C:/Windows/Fonts/calibri.ttf")
font_add("Calibri-Bold", "C:/Windows/Fonts/calibrib.ttf")
showtext_auto()

### High Res

# duration

HighMean.2015 = ggplot(data = Qdat_2hr[Qdat_2hr$Year == "2015",], aes(x = Air.Mean.2hr, y = predict_BestMod2hr))+
  geom_point(aes(color = Species, shape = Species), size = point.size)+
  scale_color_manual(values = c("BW" = "forestgreen", "SC" = "darkblue"))+
  geom_smooth(data=subset(Qdat_2hr, Year == 2015), method = "lm", linewidth = 3, colour = "black")+
  xlab("2 Hour Mean preceding Off-Bout")+
  ylab("Individual Off-bout Duration")+
  theme_light()+
  theme(panel.grid = element_line(color = "#CDD8DA", linewidth = 0.5, linetype = 2),
        axis.text = element_text(size = axis.text.size, family = "Calibri"),
        axis.title.x = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(t = axis.title.margins)),
        axis.title.y = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(r = axis.title.margins)))+
  theme(legend.key.size = unit(1, "cm"),
        legend.title = element_text(size = axis.title.size, family = "Calibri"), 
        legend.text =  element_text(size = axis.title.size, family = "Calibri"))

HighMean.2016 = ggplot(data = Qdat_2hr[Qdat_2hr$Year == "2016",], aes(x = Air.Mean.2hr, y = predict_BestMod2hr))+
  geom_point(aes(color = Species, shape = Species), size = point.size)+
  scale_color_manual(values = c("BW" = "forestgreen", "SC" = "darkblue"))+
  geom_smooth(data=subset(Qdat_2hr, Year == 2016), method = "lm", linewidth = 3, colour = "black")+
  xlab("2 Hour Mean preceding Off-Bout")+
  ylab("Individual Off-bout Duration")+
  theme_light()+
  theme(panel.grid = element_line(color = "#CDD8DA", linewidth = 0.5, linetype = 2),
        axis.text = element_text(size = axis.text.size, family = "Calibri"),
        axis.title.x = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(t = axis.title.margins)),
        axis.title.y = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(r = axis.title.margins)))+
  theme(legend.key.size = unit(1, "cm"),
        legend.title = element_text(size = axis.title.size, family = "Calibri"), 
        legend.text =  element_text(size = axis.title.size, family = "Calibri"))



Fig1 = ggarrange(HighMean.2015, HighMean.2016, ncol = 1, nrow = 2,
                 labels = c("2015", "2016"),
                 font.label = list(size = 120,family = "Calibri"),
                 hjust = -0.25, vjust = 1.25)
#Fig1

ggsave(filename = "Fig1_HighRes_2hrmean.png", path = path, 
       units = c("px"), dpi = 300, width = Final.fig.dim.w.double, height = Final.fig.dim.h.double)

# start time

MeanTime = ggplot(data = Qdat_2hr, aes(x = Air.Mean.2hr, y = predict_BestMod2hrTim))+
  geom_point(aes(color = Species, shape = Species), size = point.size)+
  scale_color_manual(values = c("BW" = "forestgreen", "SC" = "darkblue"))+
  geom_smooth(method = "lm", linewidth = 3)+
  xlab("2 Hour Mean preceding Off-Bout")+
  ylab("Off-bout Start Time")+
  theme_light()+
  theme(panel.grid = element_line(color = "#CDD8DA", linewidth = 0.5, linetype = 2),
        axis.text = element_text(size = axis.text.size, family = "Calibri"),
        axis.title.x = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(t = axis.title.margins)),
        axis.title.y = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(r = axis.title.margins)))+
  theme(legend.key.size = unit(1, "cm"),
        legend.title = element_text(size = axis.title.size, family = "Calibri"), 
        legend.text =  element_text(size = axis.title.size, family = "Calibri"))

SDTime = ggplot(data = Qdat_2hr, aes(x = Air.SD.2hr, y = predict_BestMod2hrTim))+
  geom_point(aes(color = Species, shape = Species), size = point.size)+
  scale_color_manual(values = c("BW" = "forestgreen", "SC" = "darkblue"))+
  geom_smooth(method = "lm", linewidth = 3)+
  xlab("2 Hour SD preceding Off-Bout")+
  ylab("Off-bout Start Time")+
  theme_light()+
  theme(panel.grid = element_line(color = "#CDD8DA", linewidth = 0.5, linetype = 2),
        axis.text = element_text(size = axis.text.size, family = "Calibri"),
        axis.title.x = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(t = axis.title.margins)),
        axis.title.y = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(r = axis.title.margins)))+
  theme(legend.key.size = unit(1, "cm"),
        legend.title = element_text(size = axis.title.size, family = "Calibri"), 
        legend.text =  element_text(size = axis.title.size, family = "Calibri"))



Fig2 = ggarrange(MeanTime, SDTime, ncol = 1, nrow = 2,
                 labels = c("A", "B"),
                 font.label = list(size = 120,family = "Calibri-Bold"),
                 hjust = -0.25, vjust = 1.25)
#Fig2

ggsave(filename = "Fig2_HighRes_Timing.png", path = path, 
       units = c("px"), dpi = 300, width = Final.fig.dim.w.double, height = Final.fig.dim.h.double)


### Low Res

# dur - no sig relationship 

LowResSpec = ggplot(data = Qdat_allbut2hr, aes(x = Species, y = log_Duration))+
  geom_boxplot(aes(fill = Species), show.legend = F)+
  scale_fill_manual(values=c("forestgreen", "darkblue"))+
  xlab("Species")+
  ylab("log(Total Daily Off-Bout)")+
  theme_light()+
  theme(panel.grid = element_line(color = "#CDD8DA", linewidth = 0.5, linetype = 2),
        axis.text = element_text(size = axis.text.size, family = "Calibri"),
        axis.title.x = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(t = axis.title.margins)),
        axis.title.y = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(r = axis.title.margins)))+
  theme(legend.key.size = unit(1, "cm"),
        legend.title = element_text(size = axis.title.size, family = "Calibri"), 
        legend.text =  element_text(size = axis.title.size, family = "Calibri"))+
  scale_x_discrete(labels=c("BW" = "Bobwhite", "SC" = "Scaled"))

Fig3 = ggarrange(LowResSpec, ncol = 1, nrow = 1,
                 font.label = list(size = 120,family = "Calibri-Bold"),
                 hjust = -0.25, vjust = 1.25)
#Fig3

ggsave(filename = "Fig3_LowRes_dur.png", path = path, 
       units = c("px"), dpi = 300, width = Final.fig.dim.w.thin, height = Final.fig.dim.h.thin)




# timing

#AM
am = ggplot(data = am.st.time.dat, aes(x = Air.SD.PrevDay, y = predictedtime))+
  geom_point(aes(color = Species, shape = Species), size = point.size)+
  scale_color_manual(values = c("BW" = "forestgreen", "SC" = "darkblue"))+
  geom_smooth(method = "lm", linewidth = 3)+
  xlab("Standard Deviation - Previous Day Temp")+
  ylab("Off-bout Start Time")+
  theme_light()+
  theme(panel.grid = element_line(color = "#CDD8DA", linewidth = 0.5, linetype = 2),
        axis.text = element_text(size = axis.text.size, family = "Calibri"),
        axis.title.x = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(t = axis.title.margins)),
        axis.title.y = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(r = axis.title.margins)))+
  theme(legend.key.size = unit(1, "cm"),
        legend.title = element_text(size = axis.title.size, family = "Calibri"), 
        legend.text =  element_text(size = axis.title.size, family = "Calibri"))

#PM
pm = ggplot(data = pm.st.time.dat, aes(x = Air.Mean.Aft, y = predictedtime))+
  geom_point(aes(color = Species, shape = Species), size = point.size)+
  scale_color_manual(values = c("BW" = "forestgreen", "SC" = "darkblue"))+
  geom_smooth(method = "lm", linewidth = 3)+
  xlab("Mean - Afternoon Temp")+
  ylab("Off-bout Start Time")+
  theme_light()+
  theme(panel.grid = element_line(color = "#CDD8DA", linewidth = 0.5, linetype = 2),
        axis.text = element_text(size = axis.text.size, family = "Calibri"),
        axis.title.x = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(t = axis.title.margins)),
        axis.title.y = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(r = axis.title.margins)))+
  theme(legend.key.size = unit(1, "cm"),
        legend.title = element_text(size = axis.title.size, family = "Calibri"), 
        legend.text =  element_text(size = axis.title.size, family = "Calibri"))




Fig4 = ggarrange(am, ncol = 1, nrow = 1,
                 labels = c("AM"),
                 font.label = list(size = 120,family = "Calibri-Bold"),
                 hjust = -0.25, vjust = 1.25)
#Fig4

ggsave(filename = "Fig4_LowRes_PrevDay.png", path = path, 
       units = c("px"), dpi = 300, width = Final.fig.dim.w, height = Final.fig.dim.h)

Fig5 = ggarrange(pm, ncol = 1, nrow = 1,
                 labels = c("PM"),
                 font.label = list(size = 120,family = "Calibri-Bold"),
                 hjust = -0.25, vjust = 1.25)

#Fig5

ggsave(filename = "Fig5_LowRes_Aft.png", path = path, 
       units = c("px"), dpi = 300, width = Final.fig.dim.w, height = Final.fig.dim.h)

# Supplementary Materials

# Interactive Effects

a = ggplot(data = Qdat_2hr, aes(x = AMPM, y = log_Duration, color = Species))+
  scale_color_manual(values = c("BW" = "forestgreen", "SC" = "darkblue"))+
  geom_smooth(method = "lm", linewidth = 3, aes(group = Species), se = F)+
  xlab("Morning (AM) or Afternoon (PM)")+
  ylab("log(Off-Bout Duration)")+
  theme_light()+
  theme(panel.grid = element_line(color = "#CDD8DA", linewidth = 0.5, linetype = 2),
        axis.text = element_text(size = axis.text.size, family = "Calibri"),
        axis.title.x = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(t = axis.title.margins)),
        axis.title.y = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(r = axis.title.margins)))+
  theme(legend.key.size = unit(1, "cm"),
        legend.title = element_text(size = axis.title.size, family = "Calibri"), 
        legend.text =  element_text(size = axis.title.size, family = "Calibri"))

b = ggplot(data = Qdat_2hr, aes(x = Air.Mean.2hr, y = Start.Time, color = Species))+
  #(aes(color = Species, shape = Species), size = point.size)+
  scale_color_manual(values = c("BW" = "forestgreen", "SC" = "darkblue"))+
  geom_smooth(method = "lm", linewidth = 3, aes(group = Species), se = F)+
  xlab("2 Hour Mean Temperature")+
  ylab("Start Time")+
  theme_light()+
  theme(panel.grid = element_line(color = "#CDD8DA", linewidth = 0.5, linetype = 2),
        axis.text = element_text(size = axis.text.size, family = "Calibri"),
        axis.title.x = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(t = axis.title.margins)),
        axis.title.y = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(r = axis.title.margins)))+
  theme(legend.key.size = unit(1, "cm"),
        legend.title = element_text(size = axis.title.size, family = "Calibri"), 
        legend.text =  element_text(size = axis.title.size, family = "Calibri"))



sup1 = ggarrange(a,b, ncol = 1, nrow = 2,
                 labels = c("A", "B"),
                 font.label = list(size = 120,family = "Calibri-Bold"),
                 hjust = -0.25, vjust = 1.25)
# Supp 1

ggsave(filename = "fig_supp1_interactive_effects.png", path = path, 
       units = c("px"), dpi = 300, width = Final.fig.dim.w.double, height = Final.fig.dim.h.double)



#### End ####