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
library(ggridges)

#### Read in data ----------------------------------------------------------

load("click_event_interactions.Rdata")
load("put_file_name_here.Rdata") #data objects from MBL_MM_crepuscular-plots.R

#### Plot showing number of minutes analyzed -----------------------------------

# Nmin_record %>% 
#   separate(recordName, sep = "_", into = c(NA, "Year", "month"), remove = FALSE) %>% 
#   separate(month, sep = "-", into = c("month", NA)) %>% 
#   mutate(date = as.Date(month, tryFormats = "%m%d")) %>% 
#   mutate(month = month(date)) %>% 
#   mutate(month = as.factor(month)) %>% 
#   ggplot(aes(x = month, y = nMinTotal, fill = Year)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   scale_fill_manual(values = c("2021" = "#005A32", "2022" = "lightblue", "2023" = "steelblue", "2023-2024" = "#ADDD8E", "2024" = "green3")) +
#   theme_minimal()

#### Add sunrise and sunset times, get the time difference between event and closest sunrise/sunset time

# clickpos_min <- PosDB_filt %>%
#   mutate(date = as.Date(UTC, tryFormats = c("%Y-%m-%d"))) %>%
#   mutate(datetime = strptime(UTC, tz = c("UTC"), format = c("%Y-%m-%d %H:%M:%S"))) %>%
#   mutate(sunrise = sunrise(datetime, lon = -67.0467, lat = 17.9455)) %>%
#   mutate(sunset = sunset(datetime, lon = -67.0467, lat = 17.9455)) %>%
#   mutate(rise.event = abs(difftime(datetime, sunrise, tz = "UTC", units = "hours"))) %>%
#   mutate(set.event = abs(difftime(datetime, sunset, tz = "UTC", units = "hours"))) %>%
#   mutate(riseset.event = ifelse(abs(rise.event) < abs(set.event), rise.event, set.event)) %>%
#   mutate(closest_event = ifelse(abs(rise.event) < abs(set.event), "sunrise", "sunset"))

#### Divides data into 10 minute bins from sunrise or sunset (e.g. 0-10 min from rise/set)
riseset_binned_min <- clickpos_long_filtered2 %>%
  mutate(time_difference = as.numeric(time_difference)) %>% 
  mutate(riseset.min = abs(time_difference)*60) %>% #converts hours since sunrise or sunset to minutes since sunrise or sunset
  mutate(diel.bins = cut(x=riseset.min, breaks=seq(0,700,10), labels = FALSE))

#### Groups bins by recording period and counts the number of click positive minutes(events?) in each bin
bin_summary <- riseset_binned_min %>% 
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
mean(bin_summary$nMin) #1.74
var(bin_summary$nMin) #0.865

#### Models binned sunrise and sunset data (glm)

# Generalized Linear Model with poisson family distribution
poiss.model.q <- glm(nMin ~ diel.bins + Quarter, family = poisson, data = bin_summary)
# summary results
summary(poiss.model.q)
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
summary(poiss.model.one)
check_overdispersion(poiss.model.one)
check_zeroinflation(poiss.model.one)

poiss.model.two <- glm(nMin ~ diel.bins + Farm_location, family = poisson, data = bin_summary)
summary(poiss.model.two)
check_overdispersion(poiss.model.two)
check_zeroinflation(poiss.model.two)

poiss.model.three <- glm(nMin ~ diel.bins + Farm_location + Quarter, family = poisson, data = bin_summary)
summary(poiss.model.three)
check_overdispersion(poiss.model.three)
check_zeroinflation(poiss.model.three)

AIC(poiss.model.q, poiss.model.one, poiss.model.two, poiss.model.three)

# nMin.crepuscular.dredge <- dredge(global.model = glm(formula = nMin ~ diel.bins +
#                                              Quarter +
#                                              Farm_location,
#                                            data = bin_summary,
#                                            family = poisson,
#                                            na.action = na.fail),
#                           extra = "R^2")
# 
# plot(nMin.crepuscular.dredge)

# #Model selection using dredge
# nMin.crepuscular.dredge <- dredge(global.model = glm(formula = nMin ~ diel.bins +
#                                              Quarter +
#                                              Farm_location,
#                                            data = bin_summary,
#                                            family = poisson,
#                                            na.action = na.fail),
#                           extra = "R^2")
# 
# plot(nMin.crepuscular.dredge)
# 

#plots residuals; some pattern expected because categorical data
res <- residuals(poiss.model.two)
plot(res)

#residual boxplots (for categorical variables)
fitted <- fitted(poiss.model.two)

bin_summary$diel.bins_binned <- cut(bin_summary$diel.bins, seq(0,60, by=10))
plot(bin_summary$diel.bins_binned, res, xlab = "Diel bins", ylab = "residuals")

plot(as.factor(bin_summary$Farm_location), res, xlab = "Farm_location", ylab = "residuals")

#plots model coefficients; connects data to response variable (when it doesn't cross zero, it is significant)
plot(parameters(poiss.model.two))


# histogram showing number of click positive minutes by diel bin
ggplot(data = bin_summary, aes(x=diel.bins, y = nMin)) + # Can be changed to diel.bins for higher resolution
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values=pnw_palette(n=6,name="Winter")[c(4.5,5.5)],
                    name = "Farm location") +
  labs(x= "Absolute time difference to sunrise/sunset event", y= "Number of events") +
  ggtitle("Time Difference Between Event and Sunrise vs Event")

# Create a ridgeline plot based on the number of interactions by diel bins
ggplot(bin_summary, aes(x = diel.bins, y = as.factor(diel.bins), fill = Farm_location)) +
  geom_density_ridges(alpha = 0.8, scale = 2.5, rel_min_height = 0.01) +  # Adjust scale for spacing
  scale_x_continuous(breaks = seq(0, 70, by = 10), limits = c(0, 70)) +  # Adjust x-axis breaks
  scale_y_discrete(labels = unique(bin_summary$diel.bins)) +  # Y-axis labels from binned data
  scale_fill_manual(values = c("Rom" = "aquamarine3", "ML" = "steelblue1")) +  # Custom colors
  labs(
    title = "Ridgeline Plot of Click Positive Minutes by Diel Bins",
    x = "Diel Bins (Minutes from Sunrise/Sunset)",
    y = "Diel Bins"
  ) +
  theme_minimal() +
  theme(legend.position = "right")  # Adjust legend position if needed

#SECOND RIDGELINE

ggplot(data = bin_summary, aes(x = diel.bins_binned, y = as.factor(nMin), fill = Farm_location)) +
  geom_density_ridges(alpha = 0.8, scale = 1.5, rel_min_height = 0.01) +
  scale_x_discrete(
    expand = expansion(mult = c(0.00001, 0.00001)),  # Add space on both sides of the x-axis
    breaks = unique(bin_summary$diel.bins_binned)  # Unique values for breaks
  ) +  
  scale_fill_manual(values = c("Rom" = "aquamarine3", "ML" = "steelblue1")) + 
  labs(
    x = "Number of Bins (Time in min since Sunrise or Sunset)", 
    y = "Binned Time (min)"
  ) +
  ggtitle("Time Difference Between Event and Sunrise vs Event") +
  theme_minimal() +
  theme(legend.position = "right")  # Adjust legend position as needed

