#Linear regression analysis (Dependent Var = #of interactions; Independent Var = time difference to sunrise/sunset)
### MM acoustic detections at La Paguera seaweed farm, Puerto Rico
### AVC Autumn 2023

##### set working environment --------------------------------------------------

library(tidyverse)
library(DBI)
library(RSQLite)
library(lubridate)
library(MuMIn)
library(ggsci)
library(bioRad)
library(dplyr)
library(MASS)


#### Read in data ----------------------------------------------------------

load("click_event_interactions.Rdata")

#### Plot showing number of minutes analyzed -----------------------------------

Nmin_record %>% 
  separate(recordName, sep = "_", into = c(NA,"Year",'month'), remove = FALSE) %>% 
  separate(month, sep = "-", into = c("month", NA)) %>% 
  mutate(date = as.Date(month, tryFormats = "%m%d")) %>% 
  mutate(month = month(date)) %>% 
  mutate(month = as.factor(month)) %>% 
  ggplot(aes(x = month, y = nMinTotal, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_uchicago()

#### Add sunrise and sunset times, get the time difference between event and closest sunrise/sunset time

clickpos_min <- PosDB_filt %>%
  mutate(date = as.Date(UTC,tryFormats = c("%Y-%m-%d"))) %>%
  mutate(datetime = strptime(UTC, tz = c("UTC"), format = c("%Y-%m-%d %H:%M:%S"))) %>%
  mutate(sunrise = sunrise(datetime, lon = -67.0467,lat = 17.9455))%>%
  mutate(sunset = sunset(datetime, lon = -67.0467, lat = 17.9455))%>%
  mutate(rise.event = abs(difftime(datetime, sunrise, tz="UTC", units= c("hours"))))%>%
  mutate(set.event = abs(difftime(datetime, sunset, tz="UTC", units= c("hours"))))%>%
  mutate(riseset.event =
    ifelse(rise.event < set.event, rise.event, set.event))
  
#### Plots showing the time difference between event and sunrise on y axis, date on x axis 
##720 minutes = 12 hours (the time between sunrise and sunset at the equinox)
# plot(datetime, rise.event, ylab = c("difftime event to sunrise (hrs)"), xlab = c("event"), main = c("Time Difference Between Event and Sunrise vs Event"))
# 
# ggplot(clickpos_min, aes(x=date, y=rise.event)) + 
#   geom_violin() + 
#   ggtitle("Time Difference Between Event and Sunrise vs Event")+
#   labs(y= "difftime event to sunrise(hrs)", x = "event")
# 
# #### Plots showing the time difference between event and sunset on y axis, date on x axis 
# plot(date, set.event, ylab = c("difftime event to sunset (min)"), xlab = c("event"), main = c("Time Difference Between Event and Sunset vs Event"))
# 
# ggplot(clickpos_min, aes(x=date, y=set.event)) + 
#   geom_violin() + 
#   ggtitle("Time Difference Between Event and Sunset vs Event") +
#   labs(y= "difftime event to sunset(hrs)", x = "event")

#### Histogram showing number of click positive minutes vs the time since either sunrise or sunset
hist(clickpos_min$riseset.event, main= c("Frequency of Click Positive Minutes per Hours after Sunrise or Sunset"), xlab = c("Hours Since Sunrise/Sunset"))

ggplot(data = clickpos_min, aes(x = riseset.event, fill = Year)) +
  geom_histogram(binwidth = 0.17) +
  theme_minimal()

#### Divides data into 10 minute bins from sunrise or sunset (e.g. 0-10 min from rise/set)
#### Groups bins by recording period and counts the number of events in each bin
riseset_binned <- clickpos_min %>%
  mutate(riseset.min = riseset.event*60) %>%
  mutate(diel.bins = cut(x=riseset.min, breaks=seq(0,700,10), labels = FALSE))


bin_summary <- riseset_binned %>% 
  group_by(diel.bins, recordName) %>%
  summarize(nMin=n()) %>% 
  ungroup() %>% 
  left_join(metadata, by = "recordName")


#### Models binned sunrise and sunset data (glm)

# Generalized Linear Model with poisson family distribution
poiss.model <- glm(nMin ~ diel.bins + Quarter, family = poisson, data = bin_summary)
# summary results
summary(poiss.model)

# Generalized linear model with Negative binomial family distribution
nb.model <- glm.nb(nMin ~ diel.bins, data = bin_summary)
# summar results
summary(nb.model)

# Use MuMIn package to iteratively build and choose best model

nMin.crepuscular.dredge <- dredge(global.model = glm(formula = nMin ~ diel.bins + 
                                             Year + 
                                             Quarter +
                                             Farm_location +
                                             Farm_type,
                                           data = bin_summary,
                                           family = poisson,
                                           na.action = na.fail), 
                          extra = "R^2")

plot(nMin.crepuscular.dredge)

best.poiss.model <- glm(nMin ~ diel.bins + Farm_location , family = poisson, data = bin_summary)
# summary results
summary(best.poiss.model)

ggplot(data = bin_summary, aes(x=diel.bins, y = nMin, fill = Farm_location)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  scale_fill_manual(values=pnw_palette(n=6,name="Cascades")[c(3,6)],
                     name = "Farm location") 

