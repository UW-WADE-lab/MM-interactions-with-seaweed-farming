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
library(performance)
library(PNWColors)
library(corrplot)
library(see)
library(parameters)

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

ggplot(data = clickpos_min, aes(x = riseset.event)) + #removed fill = Year due to error "object 'Year' not found
  geom_histogram(binwidth = 0.17) +
  theme_minimal()

#### Divides data into 10 minute bins from sunrise or sunset (e.g. 0-10 min from rise/set)

riseset_binned <- clickpos_min %>%
  mutate(riseset.min = riseset.event*60) %>% #converts hours since sunrise or sunset to minutes since sunrise or sunset
  mutate(diel.bins = cut(x=riseset.min, breaks=seq(0,700,10), labels = FALSE))

#### Groups bins by recording period and counts the number of click positive minutes(events?) in each bin
bin_summary <- riseset_binned %>% 
  group_by(diel.bins, recordName) %>%
  summarize(nMin=n()) %>% 
  ungroup() %>% 
  left_join(metadata, by = "recordName")

#### Pearson correlation test
# change categorical to binary
bin_summary2 <- bin_summary%>%
  dplyr::select(-recordName, -nMin, -Farm_depth)

#rom = 1 ML = 0
bin_summary2$Farm_location <- ifelse(bin_summary2$Farm_location== "Rom",1,0)
#cat = 1 5-line = 0
bin_summary2$Farm_type <- ifelse(bin_summary2$Farm_type== "cat",1,0)

cor.test <- cor(bin_summary2[1:5])
corrplot(cor.test, type="upper", order="hclust", addCoef.col = "black")

#mean and variance of response variable
mean(bin_summary$nMin) #1.78
var(bin_summary$nMin) #1.88

#### Models binned sunrise and sunset data (glm)

# Generalized Linear Model with poisson family distribution
poiss.model.q <- glm(nMin ~ diel.bins + Quarter, family = poisson, data = bin_summary)
# summary results
summary(poiss.model)
check_overdispersion(poiss.model.q)
check_zeroinflation(poiss.model.q)


# Generalized linear model with Negative binomial family distribution
# nb.model <- glm.nb(nMin ~ diel.bins, data = bin_summary)
# # summar results
# summary(nb.model)

# Use MuMIn package to iteratively build and choose best model

# nMin.crepuscular.dredge <- dredge(global.model = glm(formula = nMin ~ diel.bins + 
#                                              Year + 
#                                              Quarter +
#                                              Farm_location +
#                                              Farm_type,
#                                            data = bin_summary,
#                                            family = poisson,
#                                            na.action = na.fail), 
#                           extra = "R^2")
# 
# plot(nMin.crepuscular.dredge)

#Determining the best model based on AIC; adding in one vairable at a time
poiss.model.one <- glm(nMin ~ diel.bins, family = poisson, data = bin_summary)
check_overdispersion(poiss.model.one)
check_zeroinflation(poiss.model.one)

poiss.model.two <- glm(nMin ~ diel.bins + Farm_location, family = poisson, data = bin_summary)
check_overdispersion(poiss.model.two)
check_zeroinflation(poiss.model.two)

poiss.model.three <- glm(nMin ~ diel.bins + Farm_location + Quarter, family = poisson, data = bin_summary)
summary(poiss.model.three)
check_overdispersion(poiss.model.three)
check_zeroinflation(poiss.model.three)

AIC(poiss.model.q, poiss.model.one, poiss.model.two, poiss.model.three)

nMin.crepuscular.dredge <- dredge(global.model = glm(formula = nMin ~ diel.bins +
                                             Quarter +
                                             Farm_location,
                                           data = bin_summary,
                                           family = poisson,
                                           na.action = na.fail),
                          extra = "R^2")

plot(nMin.crepuscular.dredge)

#plots residuals; some pattern expected because categorical data
res <- residuals(poiss.model.two)
plot(res)

#plots model coefficients; connects data to response variable (when it doesn't cross zero, it is significant)
plot(parameters(poiss.model.two))

ggplot(data = bin_summary, aes(x=diel.bins, y = nMin, fill = Farm_location)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  scale_fill_manual(values=pnw_palette(n=6,name="Starfish")[c(3,6)],
                     name = "Farm location") 


