
# R script for "Critical reproductive behaviors in Scaled Quail and Northern Bobwhite 
#               are affected by thermal variability and mean temperature"

# Authors: William Kirkpatrick*, Rachel Carroll, Craig Davis, Sam Fuhlendorf, Erin Sauer, Sarah DuRant
# *primary contact

#### Library - all necessary packages and data paths ####

library(tidyverse)
library(plyr)
library(readr)
library(gtools)
library(car)
library(here)

library(MuMIn)
library(lme4)
library(lmerTest)
library(AICcmodavg)

library(ggfortify)
library(ggthemes)
library(ggpubr)
library(data.table)
library(gridExtra)
library(showtext)

#library(redres)

# Change 'path' as needed
path = "C:/Users/whkir/OneDrive - University of Arkansas/Active Manuscripts_WHK/Quail Side Project/Submission/Code"
  #"C:/Users/Will/Dropbox/Quail/Manuscript/Replicable_Data" 
setwd(path)


#### Read in Data ####

Qdat_2hr       = read.csv("Two_hour_data_new.csv")
Qdat_allbut2hr = read.csv("Daily_data_new.csv")

am.high = Qdat_2hr[Qdat_2hr$AMPM == "AM",]

st.time.dat = read.csv("Start.Time.LowRes.csv")

# Split into morning and afternoon
am.st.time.dat = st.time.dat[st.time.dat$AMPM == "AM",]
pm.st.time.dat = st.time.dat[st.time.dat$AMPM == "PM",]

am_Qdat_2hr = Qdat_2hr[Qdat_2hr$AMPM == "AM",]

#### Model Selection ####

### High Resolution Data

## Duration
Univ_2hr_mod = lmer(data = Qdat_2hr, Duration_min ~ (1|ID) + (1|Year) + (1|Date)
                    + Air.Mean.2hr + Air.SD.2hr 
                    + AMPM + Species
                    + Species:Air.Mean.2hr + Species:Air.SD.2hr
                    + AMPM:Species, 
                    na.action = na.fail)

Univ_2hr_mod2 = lmer(data = Qdat_2hr, Duration_min ~ (1|ID) + (1|Year) + (1|Date)
                    + Air.Mean.2hr + Air.SD.2hr 
                    + AMPM + Species
                    + Species:Air.Mean.2hr + Species:Air.SD.2hr
                    + Air.Mean.2hr:Air.SD.2hr +
                    + AMPM:Species, 
                    na.action = na.fail)



TwohrNull = lmer(data = Qdat_2hr, log_Duration ~ (1|ID) + (1|Year) + (1|Date))

AICc(TwohrNull)

# Dredge

TwoHrDredge = dredge(Univ_2hr_mod, beta = "none", trace = 1,
                     evaluate = T)
view(TwoHrDredge)

# Save the Dredge
write.csv(data.frame(TwoHrDredge), file = "TwoHourDredge.csv")

## Timing
# Timing - AM

Univ_2hr_modTim_AM = lmer(data = am.high, Start.Time ~ (1|ID) + (1|Year) + (1|Date) +
                          + Air.Mean.2hr + Air.SD.2hr + Species
                          + Species:Air.Mean.2hr + Species:Air.SD.2hr, 
                          na.action = na.fail)

univ_highres_tim = dredge(Univ_2hr_modTim_AM, beta = "none", trace = 1,
                          evaluate = T)
View(univ_highres_tim)

# Save the dredge
write.csv(data.frame(univ_highres_tim), file = "Timing_Dredge_HighRes_AM.csv")

# Timing - PM

pm.high = Qdat_2hr[Qdat_2hr$AMPM == "PM",]
Univ_2hr_modTim_PM = lmer(data = pm.high, Start.Time ~ (1|ID) + (1|Year) + (1|Date) +
                          + Air.Mean.2hr + Air.SD.2hr + Species
                          + Species:Air.Mean.2hr + Species:Air.SD.2hr, 
                          na.action = na.fail)

univ_highres_timp = dredge(Univ_2hr_modTim_PM, beta = "none", trace = 1,
                           evaluate = T)
View(univ_highres_timp)

# Save the dredge
write.csv(data.frame(univ_highres_timp), file = "Timing_Dredge_HighRes_PM.csv")


### Low Resolution Data

## Duration

Univ_24hr_mod = lmer(data = Qdat_allbut2hr, Duration_min_new ~ (1|ID) + (1|Year) + 
                     + Air.Mean.24hr + Air.SD.24hr
                     + Air.Mean.Morn + Air.SD.Morn
                     + Air.Mean.Aft  + Air.SD.Aft
                     + Air.Mean.PrevAft + Air.SD.PrevAft
                     + Air.Mean.PrevDay + Air.SD.PrevDay
                     + Air.Mean.PrevNight + Air.SD.PrevNight
                     + Species:Air.Mean.24hr + Species:Air.SD.24hr
                     + Species:Air.Mean.Morn + Species:Air.SD.Morn
                     + Species:Air.Mean.Aft  + Species:Air.SD.Aft
                     + Species:Air.Mean.PrevAft + Species:Air.SD.PrevAft
                     + Species:Air.Mean.PrevDay + Species:Air.SD.PrevDay
                     + Species:Air.Mean.PrevNight + Species:Air.SD.PrevNight
                     + Species,
                     na.action = na.fail)


Null_24hr = lmer(data = Qdat_allbut2hr, log_Duration ~ (1|ID) + (1|Year))
AICc(Null_24hr)

# Dredge - WARNING: This may take 10-20 minutes depending on the device. Feel free to skip to best fit models.
TwentyFourDredge = dredge(Univ_24hr_mod, beta = "none", trace = 1, evaluate = T,
                          m.lim = c(0,6))
view(TwentyFourDredge)

write.csv(data.frame(TwentyFourDredge), file = "TwentyFourDredge.csv")

# Timing - AM


Univ_24hr_modTim.AM = lmer(data = am.st.time.dat, Start.Time ~ (1|ID) + (1|Year) + (1|Date) +
                           + Air.Mean.24hr + Air.SD.24hr
                           + Air.Mean.Morn + Air.SD.Morn
                           + Air.Mean.Aft  + Air.SD.Aft
                           + Air.Mean.PrevAft + Air.SD.PrevAft
                           + Air.Mean.PrevDay + Air.SD.PrevDay
                           + Air.Mean.PrevNight + Air.SD.PrevNight
                           + Species:Air.Mean.24hr + Species:Air.SD.24hr
                           + Species:Air.Mean.Morn + Species:Air.SD.Morn
                           + Species:Air.Mean.Aft  + Species:Air.SD.Aft
                           + Species:Air.Mean.PrevAft + Species:Air.SD.PrevAft
                           + Species:Air.Mean.PrevDay + Species:Air.SD.PrevDay
                           + Species:Air.Mean.PrevNight + Species:Air.SD.PrevNight
                           + Species,
                           na.action = na.fail)

Null_24hrTim.AM = lmer(data = am.st.time.dat, Start.Time ~ (1|ID) + (1|Year))
AICc(Null_24hrTim.AM)

# Dredge - WARNING: This may take 10-20 minutes depending on the device. Feel free to skip to best fit models.
TwentyFourDredgeTim.AM = dredge(Univ_24hr_modTim.AM, beta = "none", trace = 1, evaluate = T,
                                m.lim = c(0,6))
view(TwentyFourDredgeTim.AM)

write.csv(data.frame(TwentyFourDredgeTim.AM), file = "TwentyFourDredgeTiming.AM.csv")

# Timing - PM


Univ_24hr_modTim.PM = lmer(data = pm.st.time.dat, Start.Time ~ (1|ID) + (1|Year) + (1|Date) +
                           + Air.Mean.24hr + Air.SD.24hr
                           + Air.Mean.Morn + Air.SD.Morn
                           + Air.Mean.Aft  + Air.SD.Aft
                           + Air.Mean.PrevAft + Air.SD.PrevAft
                           + Air.Mean.PrevDay + Air.SD.PrevDay
                           + Air.Mean.PrevNight + Air.SD.PrevNight
                           + Species:Air.Mean.24hr + Species:Air.SD.24hr
                           + Species:Air.Mean.Morn + Species:Air.SD.Morn
                           + Species:Air.Mean.Aft  + Species:Air.SD.Aft
                           + Species:Air.Mean.PrevAft + Species:Air.SD.PrevAft
                           + Species:Air.Mean.PrevDay + Species:Air.SD.PrevDay
                           + Species:Air.Mean.PrevNight + Species:Air.SD.PrevNight
                           + Species,
                           na.action = na.fail)

Null_24hrTim.PM = lmer(data = pm.st.time.dat, Start.Time ~ (1|ID) + (1|Year))
AICc(Null_24hrTim.PM)

# Dredge - WARNING: This may take 10-20 minutes depending on the device. Feel free to skip to best fit models.
TwentyFourDredgeTim.PM = dredge(Univ_24hr_modTim.PM, beta = "none", trace = 1, evaluate = T,
                                m.lim = c(0,6))
view(TwentyFourDredgeTim.PM)

write.csv(data.frame(TwentyFourDredgeTim.PM), file = "TwentyFourDredgeTiming.PM.csv")

#### Best Fit Models ####

# Duration
# 2hr 

IndDuration = lmer(data = Qdat_2hr, Duration_min ~ 
                      (1|ID) + (1|Year) + (1|Date) +
                      + Air.Mean.2hr + Air.SD.2hr + AMPM + Species
                    + Species:Air.Mean.2hr + Species:Air.SD.2hr
                    + AMPM:Species)

summary(IndDuration)

Qdat_2hr$predict_BestMod2hr = predict(IndDuration)

#examine residuals
plot_resqq(IndDuration)
plot_ranef(IndDuration)

#extract residuals
Qdat_2hr$Ind_Duration_Resid = residuals(IndDuration)




# 24hr

# Removed Air.SD.Morn due to correlation w/ Air.SD.24hr + similar AIC value 

BestMod_24hr = lmer(data = Qdat_allbut2hr, Duration_min_new ~ 
                      (1|ID) + (1|Year) +
                    + Air.SD.24hr
                    + Air.SD.PrevNight
                    + Species
                    + Air.SD.24hr:Species
                    + Air.SD.PrevNight:Species)

summary(BestMod_24hr)

Qdat_allbut2hr$predict_BestMod_24hr = predict(BestMod_24hr)

# examine residuals
plot_resqq(BestMod_24hr)
plot_ranef(BestMod_24hr)

# extract residuals
Qdat_allbut2hr$resid_BestMod_24hr = residuals(BestMod_24hr)

# Timing
# 2hr - AM

am_Qdat_2hr = Qdat_2hr[Qdat_2hr$AMPM == "AM",]

HighStartTime_AM = lmer(data = am_Qdat_2hr, Start.Time ~ 
                          (1|ID) + (1|Year) + (1|Date) +
                        + Air.Mean.2hr + Air.SD.2hr
                        + Species)

summary(HighStartTime_AM)

am_Qdat_2hr$predict_BestMod2hrTimAM = predict(HighStartTime_AM)

# examine residuals
plot_resqq(HighStartTime_AM)
plot_ranef(HighStartTime_AM)

# 2hr - PM

PM_Qdat_2hr = Qdat_2hr[Qdat_2hr$AMPM == "PM",]

HighStartTime_PM = lmer(data = PM_Qdat_2hr, Start.Time ~ 
                          (1|ID) + (1|Year) + (1|Date) +
                        + Air.Mean.2hr + Air.SD.2hr)

summary(HighStartTime_PM)

PM_Qdat_2hr$predict_BestMod2hrTimPM = predict(HighStartTime_PM)

# examine residuals
plot_resqq(HighStartTime_PM)
plot_ranef(HighStartTime_PM)

# extract residuals
PM_Qdat_2hr$HighStartTimePM_resid = residuals(HighStartTime_PM)



# 24hr - AM

BestMod_24hrTim.AM = lmer(data = am.st.time.dat, Start.Time ~ 
                            (1|ID) + (1|Year) + (1|Date) +
                          + Air.SD.PrevDay
                          + Species)

summary(BestMod_24hrTim.AM)

# examine residuals
plot_resqq(BestMod_24hrTim.AM)
plot_ranef(BestMod_24hrTim.AM)


# Start time
am.st.time.dat$predictedtime = predict(BestMod_24hrTim.AM)

# 24hr - PM

BestMod_24hrTim.PM = lmer(data = pm.st.time.dat, Start.Time ~ 
                            (1|ID) + (1|Year) + (1|Date) +
                          + Air.Mean.Aft
                          + Species)

summary(BestMod_24hrTim.PM)

# Start time
pm.st.time.dat$predictedtime = predict(BestMod_24hrTim.PM)

# examine residuals
plot_resqq(BestMod_24hrTim.PM)
plot_ranef(BestMod_24hrTim.PM)


#### Visualization ####


#### Make Fig 1 ####
axis.title.size = 110
main.title.size = 150
axis.text.size = 100
plot.margins = c(10,10,10,50)
axis.title.margins = 25
main.title.margin = 0

point.size = 5

break.height = 0.25

Final.fig.dim.w = 8000
Final.fig.dim.h = 6000

font_add("Calibri", "C:/Windows/Fonts/calibri.ttf")
font_add("Calibri-Bold", "C:/Windows/Fonts/calibrib.ttf")
showtext_auto()

### High Res

# duration

HighMean = ggplot(data = Qdat_2hr, aes(x = Air.Mean.2hr, y = predict_BestMod2hr))+
  #geom_line(aes(color = nest_number), show.legend = F)+
  geom_point(data = Qdat_2hr[Qdat_2hr$Year == "2016",],aes(color = Species), size = point.size)+
  geom_point(data = Qdat_2hr[Qdat_2hr$Year == "2015",],aes(color = Species), size = point.size, shape = 1)+
  geom_smooth(data=subset(Qdat_2hr, Species == "BW"), method = "lm", linewidth = 3, colour = "forestgreen")+
  geom_smooth(data=subset(Qdat_2hr, Species == "SC"), method = "lm", linewidth = 3, colour = "darkblue")+
  scale_color_manual(values = c("BW" = "forestgreen", "SC" = "darkblue"))+
  #geom_smooth(data=subset(Qdat_2hr, Year == 2015), method = "lm", linewidth = 3, colour = "gray")+
  #geom_smooth(data=subset(Qdat_2hr, Year == 2016), method = "lm", linewidth = 3, colour = "black")+
  #geom_smooth(data=Qdat_2hr, method = "lm", linewidth = 3, colour = "purple")+
  #geom_smooth(data = test[test$AMPM == "AM",], aes(x = Air.SD.PrevDay, y = Start.Time))+
  xlab("")+
  ylab("Individual Off-bout Duration")+
  #ggtitle("High Resolution Outcomes")+
  theme_light()+
  theme(panel.grid = element_line(color = "#CDD8DA", linewidth = 0.5, linetype = 2),
        axis.text = element_text(size = axis.text.size, family = "Calibri"),
        axis.title.x = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(t = axis.title.margins)),
        axis.title.y = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(r = axis.title.margins)))+
  theme(legend.key.size = unit(0.5, "cm"),
        legend.title = element_blank(), 
        legend.text =  element_text(size = axis.title.size, family = "Calibri"), 
        legend.position = "top", legend.justification = "right")+
  guides(colour = guide_legend(override.aes = list(size=20)))


HighSD = ggplot(data = Qdat_2hr, aes(x = Air.SD.2hr, y = predict_BestMod2hr))+
  #geom_line(aes(color = nest_number), show.legend = F)+
  geom_point(data = Qdat_2hr[Qdat_2hr$Year == "2016",],aes(color = Species), size = point.size)+
  geom_point(data = Qdat_2hr[Qdat_2hr$Year == "2015",],aes(color = Species), size = point.size, shape = 1)+
  #geom_smooth(method = "lm", linewidth = 3, aes(group = Species))+
  scale_color_manual(values = c("BW" = "forestgreen", "SC" = "darkblue"))+
  geom_smooth(data=subset(Qdat_2hr, Species == "BW"), method = "lm", linewidth = 3, colour = "forestgreen")+
  geom_smooth(data=subset(Qdat_2hr, Species == "SC"), method = "lm", linewidth = 3, colour = "darkblue")+
  #geom_smooth(data=Qdat_2hr, method = "lm", linewidth = 3, colour = "purple")+
  #geom_smooth(method = "lm")+
  #geom_smooth(data = Qdat_2hr[Qdat_2hr$AMPM == "AM",], aes(x = Air.SD.PrevDay, y = Start.Time))+
  xlab("")+
  ylab("")+
  xlim(c(0.0, 9.0))+
  #ggtitle("High Resolution Outcomes")+
  theme_light()+
  theme(panel.grid = element_line(color = "#CDD8DA", linewidth = 0.5, linetype = 2),
        axis.text = element_text(size = axis.text.size, family = "Calibri"),
        axis.title.x = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(t = axis.title.margins)),
        axis.title.y = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(r = axis.title.margins)))+
  theme(legend.key.size = unit(0.5, "cm"),
        legend.title = element_blank(), 
        legend.text =  element_text(size = axis.title.size, family = "Calibri"), 
        legend.position = "top", legend.justification = "right")+
  guides(colour = guide_legend(override.aes = list(size=20)))


Fig1 = ggarrange(HighMean, HighSD, ncol = 2, nrow = 1, heights = c(4, 4),
                 labels = c("A", "B"),
                 font.label = list(size = 120,family = "Calibri-Bold"),
                 hjust = -1, vjust = 3)
#####
# start time

AM_mean = ggplot(data = am.high, aes(x = Air.Mean.2hr, y = predict_BestMod2hrTimAM))+
  #geom_line(aes(color = nest_number), show.legend = F)+
  geom_point(data = am_Qdat_2hr[am_Qdat_2hr$Year == "2016",],aes(color = Species), size = point.size)+
  geom_point(data = am_Qdat_2hr[am_Qdat_2hr$Year == "2015",],aes(color = Species), size = point.size, shape = 1)+
  scale_color_manual(values = c("BW" = "forestgreen", "SC" = "darkblue"))+
  geom_smooth(method = "lm", linewidth = 3)+
  #geom_smooth(data = test[test$AMPM == "AM",], aes(x = Air.SD.PrevDay, y = Start.Time))+
  xlab("")+
  ylab("Morning Off-bout Start Time")+
  #ggtitle("Previous Day Temperature Variation vs. AM Off-bout Start Time")+
  theme(legend.position = "none")+
  theme_light()+
  theme(panel.grid = element_line(color = "#CDD8DA", linewidth = 0.5, linetype = 2),
        axis.text = element_text(size = axis.text.size, family = "Calibri"),
        axis.title.x = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(t = axis.title.margins)),
        axis.title.y = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(r = axis.title.margins)),
        legend.position = "none")
# theme(legend.key.size = unit(1, "cm"),
#   legend.title = element_text(size = axis.title.size, family = "Calibri"), 
#  legend.text =  element_text(size = axis.title.size, family = "Calibri"))

AM_SD = ggplot(data = am_Qdat_2hr, aes(x = Air.SD.2hr, y = predict_BestMod2hrTimAM))+
  #geom_line(aes(color = nest_number), show.legend = F)+
  geom_point(data = am_Qdat_2hr[am_Qdat_2hr$Year == "2016",],aes(color = Species), size = point.size)+
  geom_point(data = am_Qdat_2hr[am_Qdat_2hr$Year == "2015",],aes(color = Species), size = point.size, shape = 1)+
  scale_color_manual(values = c("BW" = "forestgreen", "SC" = "darkblue"))+
  geom_smooth(method = "lm", linewidth = 3)+
  #geom_smooth(data = test[test$AMPM == "AM",], aes(x = Air.SD.PrevDay, y = Start.Time))+
  xlab("")+
  ylab("")+
  xlim(c(0.0, 9.0))+
  #ggtitle("Previous Day Temperature Variation vs. AM Off-bout Start Time")+
  theme(legend.position = "none")+
  theme_light()+
  theme(panel.grid = element_line(color = "#CDD8DA", linewidth = 0.5, linetype = 2),
        axis.text = element_text(size = axis.text.size, family = "Calibri"),
        axis.title.x = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(t = axis.title.margins)),
        axis.title.y = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(r = axis.title.margins)), 
        legend.position = "none")
#theme(legend.key.size = unit(1, "cm"),
#   legend.title = element_text(size = axis.title.size, family = "Calibri"), 
#  legend.text =  element_text(size = axis.title.size, family = "Calibri"))



Fig2.1 = ggarrange(AM_mean, AM_SD, ncol = 2, nrow = 1, heights = c(4, 4),
                   labels = c("C", "D"),
                   font.label = list(size = 120,family = "Calibri-Bold"),
                   hjust = -1, vjust = 3)

PM_mean = ggplot(data = PM_Qdat_2hr, aes(x = Air.Mean.2hr, y = predict_BestMod2hrTimPM))+
  #geom_line(aes(color = nest_number), show.legend = F)+
  geom_point(data = PM_Qdat_2hr[PM_Qdat_2hr$Year == "2016",],aes(color = Species), size = point.size)+
  geom_point(data = PM_Qdat_2hr[PM_Qdat_2hr$Year == "2015",],aes(color = Species), size = point.size, shape = 1)+
  scale_color_manual(values = c("BW" = "forestgreen", "SC" = "darkblue"))+
  geom_smooth(data = PM_Qdat_2hr[PM_Qdat_2hr$Year == 2015,], method = "lm", linewidth = 3, color = "gray")+
  geom_smooth(data = PM_Qdat_2hr[PM_Qdat_2hr$Year == 2016,], method = "lm", linewidth = 3, color = "black")+
  #geom_smooth(data = test[test$AMPM == "AM",], aes(x = Air.SD.PrevDay, y = Start.Time))+
  xlab("2-hour Mean Temperature (C)")+
  ylab("Afternoon Off-bout Start Time")+
  #ggtitle("Previous Day Temperature Variation vs. AM Off-bout Start Time")+
  theme(legend.position = "none")+
  theme_light()+
  theme(panel.grid = element_line(color = "#CDD8DA", linewidth = 0.5, linetype = 2),
        axis.text = element_text(size = axis.text.size, family = "Calibri"),
        axis.title.x = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(t = axis.title.margins)),
        axis.title.y = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(r = axis.title.margins)),
        legend.position = "none")
# theme(legend.key.size = unit(1, "cm"),
#   legend.title = element_text(size = axis.title.size, family = "Calibri"), 
#  legend.text =  element_text(size = axis.title.size, family = "Calibri"))

PM_SD = ggplot(data = PM_Qdat_2hr, aes(x = Air.SD.2hr, y = predict_BestMod2hrTimPM))+
  #geom_line(aes(color = nest_number), show.legend = F)+
  geom_point(data = PM_Qdat_2hr[PM_Qdat_2hr$Year == "2016",],aes(color = Species), size = point.size)+
  geom_point(data = PM_Qdat_2hr[PM_Qdat_2hr$Year == "2015",],aes(color = Species), size = point.size, shape = 1)+
  scale_color_manual(values = c("BW" = "forestgreen", "SC" = "darkblue"))+
  geom_smooth(method = "lm", linewidth = 3)+
  #geom_smooth(data = test[test$AMPM == "AM",], aes(x = Air.SD.PrevDay, y = Start.Time))+
  xlab("2-hour Standard Deviation of Temperature (C)")+
  ylab("")+
  xlim(c(0.0, 9.0))+
  theme(legend.position = "none")+
  theme_light()+
  theme(panel.grid = element_line(color = "#CDD8DA", linewidth = 0.5, linetype = 2),
        axis.text = element_text(size = axis.text.size, family = "Calibri"),
        axis.title.x = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(t = axis.title.margins)),
        axis.title.y = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(r = axis.title.margins)), 
        legend.position = "none")
#theme(legend.key.size = unit(1, "cm"),
#   legend.title = element_text(size = axis.title.size, family = "Calibri"), 
#  legend.text =  element_text(size = axis.title.size, family = "Calibri"))

Fig2.2 = ggarrange(PM_mean, PM_SD, ncol = 2, nrow = 1, heights = c(4, 4),
                   labels = c("E", "F"),
                   font.label = list(size = 120,family = "Calibri-Bold"),
                   hjust = -1, vjust = 3)


#### Fig1 Export ####

Fig1

Final.fig.dim.h.fig1 = 3000
Final.fig.dim.w.fig1 = 6000

ggsave(filename = "5_28_24_Fig1.png", path = path, 
       units = c("px"), dpi = 300, width = Final.fig.dim.w.fig1, height = Final.fig.dim.h.fig1)

#### Make Fig 2 ####
### Low Res

#Fig2
# dur

LowRes_24 = ggplot(data = Qdat_allbut2hr, aes(x = Air.SD.24hr, y = predict_BestMod_24hr))+
  #geom_line(aes(color = nest_number), show.legend = F)+
  geom_point(data = Qdat_allbut2hr[Qdat_allbut2hr$Year == "2016",],aes(color = Species), size = point.size)+
  geom_point(data = Qdat_allbut2hr[Qdat_allbut2hr$Year == "2015",],aes(color = Species), size = point.size, shape = 1)+
  scale_color_manual(values = c("BW" = "forestgreen", "SC" = "darkblue"))+
  #geom_smooth(data=subset(Qdat_2hr, Year == 2015), method = "lm", linewidth = 3, colour = "gray")+
  #geom_smooth(data=subset(Qdat_2hr, Year == 2016), method = "lm", linewidth = 3, colour = "black")+
  geom_smooth(method = "lm", linewidth = 3)+
  #geom_smooth(data=Qdat_2hr, method = "lm", linewidth = 3, colour = "purple")+
  #geom_smooth(data = test[test$AMPM == "AM",], aes(x = Air.SD.PrevDay, y = Start.Time))+
  #xlab("24hr SD Temperature (C)")+
  #ylab("Total Daily Off-bout Duration")+
  #ggtitle("High Resolution Outcomes")+
  theme_light()+
  theme(panel.grid = element_line(color = "#CDD8DA", linewidth = 0.5, linetype = 2),
        axis.text = element_text(size = axis.text.size, family = "Calibri"),
        axis.title.x = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(t = axis.title.margins)),
        axis.title.y = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(r = axis.title.margins)),
        legend.position = "none")+
  guides(colour = guide_legend(override.aes = list(size=20)))


LowRes_Morn = ggplot(data = Qdat_allbut2hr, aes(x = Air.SD.Morn, y = predict_BestMod_24hr))+
  #geom_line(aes(color = nest_number), show.legend = F)+
  geom_point(data = Qdat_allbut2hr[Qdat_allbut2hr$Year == "2016",],aes(color = Species), size = point.size)+
  geom_point(data = Qdat_allbut2hr[Qdat_allbut2hr$Year == "2015",],aes(color = Species), size = point.size, shape = 1)+
  scale_color_manual(values = c("BW" = "forestgreen", "SC" = "darkblue"))+
  #geom_smooth(data=subset(Qdat_2hr, Year == 2015), method = "lm", linewidth = 3, colour = "gray")+
  #geom_smooth(data=subset(Qdat_2hr, Year == 2016), method = "lm", linewidth = 3, colour = "black")+
  geom_smooth(method = "lm", linewidth = 3)+
  #geom_smooth(data=Qdat_2hr, method = "lm", linewidth = 3, colour = "purple")+
  #geom_smooth(data = test[test$AMPM == "AM",], aes(x = Air.SD.PrevDay, y = Start.Time))+
  xlab("Morning SD Temperature (C)")+
  ylab("")+
  #ylab("Individual Off-bout Duration")+
  #ggtitle("High Resolution Outcomes")+
  theme_light()+
  theme(panel.grid = element_line(color = "#CDD8DA", linewidth = 0.5, linetype = 2),
        axis.text = element_text(size = axis.text.size, family = "Calibri"),
        axis.title.x = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(t = axis.title.margins)),
        axis.title.y = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(r = axis.title.margins)))+
  theme(legend.key.size = unit(0.5, "cm"),
        legend.title = element_blank(), 
        legend.text =  element_text(size = axis.title.size, family = "Calibri"), 
        legend.position = "top", legend.justification = "right")+
  guides(colour = guide_legend(override.aes = list(size=20)))


# timing

#AM
am = ggplot(data = am.st.time.dat, aes(x = Air.SD.PrevDay, y = predictedtime))+
  geom_point(data = am.st.time.dat[am.st.time.dat$Year == "2016",],aes(color = Species), size = point.size)+
  geom_point(data = am.st.time.dat[am.st.time.dat$Year == "2015",],aes(color = Species), size = point.size, shape = 1)+
  scale_color_manual(values = c("BW" = "forestgreen", "SC" = "darkblue"))+
  geom_smooth(method = "lm", linewidth = 3)+
  theme_light()+
  theme(panel.grid = element_line(color = "#CDD8DA", linewidth = 0.5, linetype = 2),
        axis.text = element_text(size = axis.text.size, family = "Calibri"),
        axis.title.x = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(t = axis.title.margins)),
        axis.title.y = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(r = axis.title.margins)),
        legend.position = "none")+
  guides(colour = guide_legend(override.aes = list(size=20)))

#PM
pm = ggplot(data = pm.st.time.dat, aes(x = Air.Mean.Aft, y = predictedtime))+
  #geom_line(fill = ID)+
  geom_point(data = pm.st.time.dat[pm.st.time.dat$Year == "2016",],aes(color = Species), size = point.size)+
  geom_point(data = pm.st.time.dat[pm.st.time.dat$Year == "2015",],aes(color = Species), size = point.size, shape = 1)+
  scale_color_manual(values = c("BW" = "forestgreen", "SC" = "darkblue"))+
  geom_smooth(method = "lm", linewidth = 3)+
  #geom_smooth(data = test[test$AMPM == "AM",], aes(x = Air.SD.PrevDay, y = Start.Time))+
  xlab("Afternoon Mean Temperature (C)")+
  ylab("PM Off-bout Start Time")+
  #theme(legend.position = "none")+
  #ggtitle("Afternoon Mean vs. PM Off-bout Start Time")+
  theme_light()+
  theme(panel.grid = element_line(color = "#CDD8DA", linewidth = 0.5, linetype = 2),
        axis.text = element_text(size = axis.text.size, family = "Calibri"),
        axis.title.x = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(t = axis.title.margins)),
        axis.title.y = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(r = axis.title.margins)),
        legend.position = "none")

Fig2.1 = ggarrange(LowRes_24, LowRes_Morn, ncol = 2, nrow = 1,
                   labels = c("A", "B"),
                   font.label = list(size = 120,family = "Calibri-Bold"),
                   hjust = -1, vjust = 3)

Fig2.2 = ggarrange(am, pm, ncol = 2, nrow = 1,
                   labels = c("C", "D"),
                   font.label = list(size = 120,family = "Calibri-Bold"),
                   hjust = -1, vjust = 3)

Fig2 = ggarrange(LowRes_24, am, ncol = 1, nrow = 2, widths = c(1, 1))


#### Export Fig 2 ####


Fig2 = ggarrange(Fig2.1, Fig2.2, ncol = 1, nrow = 2, widths = c(2, 2),
                 font.label = list(size = 120,family = "Calibri-Bold"),
                 hjust = -1, vjust = 3)

Final.fig.dim.h = 3000
Final.fig.dim.w = 3000

ggsave(filename = "Fig2B.png", path = path, 
       units = c("px"), dpi = 300, width = Final.fig.dim.w, height = Final.fig.dim.h)


#### Supplementary Materials ####

# Interactive Effects

# Replace shorthand for full name
Qdat_2hr.1 = Qdat_2hr
Qdat_2hr.1$Species = replace(Qdat_2hr$Species, Qdat_2hr$Species == "BW", "Bobwhite")
Qdat_2hr.1$Species = replace(Qdat_2hr.1$Species, Qdat_2hr.1$Species == "SC", "Scaled")

Qdat_allbut2hr.1 = Qdat_allbut2hr
Qdat_allbut2hr.1$Species = replace(Qdat_allbut2hr$Species, Qdat_allbut2hr$Species == "BW", "Bobwhite")
Qdat_allbut2hr.1$Species = replace(Qdat_allbut2hr.1$Species, Qdat_allbut2hr.1$Species == "SC", "Scaled")

Final.fig.dim.w = 6000
Final.fig.dim.h = 8000

a = ggplot(data = Qdat_2hr.1, aes(x = AMPM, y = Duration_min, color = Species))+
  #geom_line(fill = ID)+
  #(aes(color = Species, shape = Species), size = point.size)+
  scale_color_manual(values = c("Bobwhite" = "darkblue", "Scaled" = "forestgreen"))+
  geom_smooth(method = "lm", linewidth = 3, aes(group = Species), se = F)+
  #geom_smooth(data = test[test$AMPM == "AM",], aes(x = Air.SD.PrevDay, y = Start.Time))+
  xlab("Morning (AM) or Afternoon (PM)")+
  ylab("Individual Off-Bout Duration (min)")+
  theme_light()+
  theme(panel.grid = element_line(color = "#CDD8DA", linewidth = 0.5, linetype = 2),
        axis.text = element_text(size = axis.text.size, family = "Calibri"),
        axis.title.x = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(t = axis.title.margins)),
        axis.title.y = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(r = axis.title.margins)))+
  theme(legend.key.size = unit(1, "cm"),
        legend.title = element_text(size = axis.title.size, family = "Calibri"), 
        legend.text =  element_text(size = axis.title.size, family = "Calibri"))

b = ggplot(data = Qdat_2hr.1, aes(x = Air.Mean.2hr, y = Duration_min, color = Species))+
  #geom_line(fill = ID)+
  #(aes(color = Species, shape = Species), size = point.size)+
  scale_color_manual(values = c("Bobwhite" = "darkblue", "Scaled" = "forestgreen"))+
  geom_smooth(method = "lm", linewidth = 3, aes(group = Species), se = F)+
  #geom_smooth(data = test[test$AMPM == "AM",], aes(x = Air.SD.PrevDay, y = Start.Time))+
  xlab("2 Hour Mean Temperature")+
  ylab("Individual Off-bout Duration (min)")+
  theme_light()+
  theme(panel.grid = element_line(color = "#CDD8DA", linewidth = 0.5, linetype = 2),
        axis.text = element_text(size = axis.text.size, family = "Calibri"),
        axis.title.x = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(t = axis.title.margins)),
        axis.title.y = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(r = axis.title.margins)))+
  theme(legend.key.size = unit(1, "cm"),
        legend.title = element_text(size = axis.title.size, family = "Calibri"), 
        legend.text =  element_text(size = axis.title.size, family = "Calibri"))

c = ggplot(data = Qdat_allbut2hr.1, aes(x = Air.SD.24hr, y = Duration_min_new, color = Species))+
  #geom_line(fill = ID)+
  #(aes(color = Species, shape = Species), size = point.size)+
  scale_color_manual(values = c("Bobwhite" = "darkblue", "Scaled" = "forestgreen"))+
  geom_smooth(method = "lm", linewidth = 3, aes(group = Species), se = F)+
  #geom_smooth(data = test[test$AMPM == "AM",], aes(x = Air.SD.PrevDay, y = Start.Time))+
  xlab("24 Hour Thermal Variation (C)")+
  ylab("Daily Off-bout Duration (min)")+
  theme_light()+
  theme(panel.grid = element_line(color = "#CDD8DA", linewidth = 0.5, linetype = 2),
        axis.text = element_text(size = axis.text.size, family = "Calibri"),
        axis.title.x = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(t = axis.title.margins)),
        axis.title.y = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(r = axis.title.margins)))+
  theme(legend.key.size = unit(1, "cm"),
        legend.title = element_text(size = axis.title.size, family = "Calibri"), 
        legend.text =  element_text(size = axis.title.size, family = "Calibri"))


sup1 = ggarrange(a,b, c, ncol = 1, nrow = 3,
                 labels = c("A", "B", "C"),
                 font.label = list(size = 120,family = "Calibri-Bold"),
                 hjust = -5, vjust = 3)

# Supp 1

ggsave(filename = "fig_supp1_interactive_effects_916.png", path = path, 
       units = c("px"), dpi = 300, width = Final.fig.dim.w, height = Final.fig.dim.h)


#### +40 Degree Exposure ANOVA ####

# Define Aesthetics for plots
axis.title.size = 130
axis.text.size = 70
plot.margins = c(10,10,10,50)
axis.title.margins = 25
legend.title.size = axis.text.size*(7/8)

export.width  = 2500
export.height = 2500

break.height = 0.25

font_add("Calibri", "C:/Windows/Fonts/calibri.ttf")
font_add("Calibri-Bold", "C:/Windows/Fonts/calibrib.ttf")
showtext_auto()

# Here, I conduct an ANOVA to determine the differences in +40 deg. air temp. exposure to quail nests

fortydata = read.csv("kaledata.csv")
fortydata = fortydata[fortydata$Exp.Len < 750,]

# Total Minutes exposed Plot

a = ggplot(data = fortydata, aes(x = SP, y = Exp.Len))+
  geom_boxplot(aes(fill = F_H), show.legend = F)+
  scale_fill_manual(values=c("red", "darkblue"))+
  xlab("Species")+
  ylab("Total Minutes Above 40C")+
  theme(panel.grid = element_line(color = "#CDD8DA",
                                  linewidth = 0.5,
                                  linetype = 2),
        axis.text = element_text(size = axis.text.size, family = "Calibri"),
        plot.margin = margin(plot.margins),
        axis.title.x = element_text(lineheight = break.height, size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(t = axis.title.margins)),
        axis.title.y = element_text(size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(r = axis.title.margins))
  )


fig.path = "C:/Users/whkir/OneDrive - University of Arkansas/Active Manuscripts_WHK/Quail Side Project/Submission/Code"

ggsave(filename = "MinutesAbv40.png", path = fig.path,
       units = c("px"), dpi = 300, width = export.width, height = export.height)

# Days Exposed Plot

b = ggplot(data = fortydata, aes(x = SP, y = Exp.Days))+
  geom_boxplot(aes(fill = F_H), show.legend = T)+
  scale_fill_manual(values=c("red", "darkblue"))+
  xlab("Species")+
  ylab("Total Days Above 40C")+
  theme(panel.grid = element_line(color = "#CDD8DA",
                                  linewidth = 0.5,
                                  linetype = 2),
  axis.text = element_text(size = axis.text.size, family = "Calibri"),
  plot.margin = margin(plot.margins),
  axis.title.x = element_text(lineheight = break.height, 
                              size = axis.title.size, face = "bold", family = "Calibri-Bold", margin = margin(t = axis.title.margins)),
  axis.title.y = element_text(size = axis.title.size, face = "bold", 
                              family = "Calibri-Bold", margin = margin(r = axis.title.margins)))+
  theme(legend.key.size = unit(1, "cm"),
        legend.title = element_text(size = legend.title.size, family = "Calibri"), 
        legend.text =  element_text(size = legend.title.size, family = "Calibri"), 
        legend.position = "right")+
  guides(fill=guide_legend(title=""))


b

ggsave(filename = "DaysAbv40.png", path = fig.path,
       units = c("px"), dpi = 300, width = export.width, height = export.height)


# Conduct ANOVA

min.aov = aov(Exp.Len ~ F_H + SP + SP:F_H, data = fortydata)
summary(min.aov)

days.aov = aov(Exp.Days ~ F_H + SP + SP:F_H, data = fortydata)
summary(days.aov)

tukey.two.way<-TukeyHSD(min.aov)

tukey.two.way


#### End ####


# Please direct any questions to Will Kirkpatrick - whkirkpa@uark.edu or whkirk97@gmail.com
# Thank you!

