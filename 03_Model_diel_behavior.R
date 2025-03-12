#Linear regression analysis (Dependent Var = #of interactions; Independent Var = time difference to sunrise/sunset)
### MM acoustic detections at La Paguera seaweed farm, Puerto Rico
### AVC Autumn 2023

##### set working environment --------------------------------------------------

library(tidyverse)
library(lubridate)
library(MuMIn)
library(MASS)
library(performance)
library(PNWColors)
library(see)
library(parameters)
library(mgcv)

year_colors <- c("2021" = "#9e6374",
                 "2022" = "#efbc82",
                 "2023" = "#c67b6f")

#### Read in data ----------------------------------------------------------

load("click_event_interactions.Rdata")
load("delphinid_diel_activity_data.Rdata") 


#### Binomial data model -------------------------------------------------------
# detect_timediff_meta <- detect_timediff_eventNearest %>% 
#   mutate(year = year(UTC), month = month(UTC)) %>% 
#   #left_join(metadata, by = c("recordName", "year" = "Year")) %>% 
#   mutate(time_difference = as.numeric(time_difference)) %>% 
#   mutate(JulianDate = yday(UTC))
# 
# dielDetect_dredge <- dredge(global.model = gam(formula = detect ~ s(time_difference, by = as.factor(year))+
#                                                  s(JulianDate, by = as.factor(Farm_location)) +
#                                                  s(time_difference, by = as.factor(Farm_location)) +
#                                                  s(JulianDate, by = as.factor(year)) +
#                                                  JulianDate + time_difference + year + Farm_location,
#                                                data = detect_timediff_meta,
#                                                family = binomial,
#                                                na.action = na.fail),
#                             extra = "R^2")
# 
# plot(dielDetect_dredge)
# 
# save(dielDetect_dredge, file = "Binomial_behavior_model.Rdata")
load("Binomial_behavior_model.Rdata")

dielDetect_all <- gam(formula = detect ~ s(time_difference, by = as.factor(year))+
                        s(JulianDate, by = as.factor(Farm_location)) +
                        s(time_difference, by = as.factor(Farm_location)) +
                        s(JulianDate, by = as.factor(year)) +
                        JulianDate + time_difference + year + Farm_location,
                      data = detect_timediff_meta,
                      family = binomial,
                      na.action = na.fail)

summary(dielDetect_all)

# predictions for diel behavior by farm type
dielDetect_farm <- gam(detect ~ s(abs(time_difference), by = as.factor(Farm_location), k = 3),
                       data = detect_timediff_meta,
                       family = binomial,
                       na.action = na.fail,
                       method = "REML", select = TRUE)

summary(dielDetect_farm)

dielDetect_farm_predictGrid <- expand_grid(time_difference = seq(0,max(detect_timediff_meta$time_difference),by = 0.1), 
                                            Farm_location = as.factor(unique(detect_timediff_meta$Farm_location)))

dielDetect_farm_preds <- predict(dielDetect_farm, 
                                 dielDetect_farm_predictGrid,
                                  se.fit = TRUE)

dielDetect_farm_sePreds <- data.frame(dielDetect_farm_predictGrid,
                                       mu   = exp(dielDetect_farm_preds$fit),
                                       low  = exp(dielDetect_farm_preds$fit - 1.96 * dielDetect_farm_preds$se.fit),
                                       high = exp(dielDetect_farm_preds$fit + 1.96 * dielDetect_farm_preds$se.fit))

dielDetect_farm_plot <- ggplot(dielDetect_farm_sePreds, aes(x = time_difference, y = mu, 
                                                           color = Farm_location, fill = Farm_location)) +
  geom_line() +
  geom_smooth(aes(ymin = low, ymax = high, y = mu), stat = "identity") +
  scale_color_manual(name = "", values = c("#440154FF", "#2A788EFF")) +
  scale_fill_manual(name = "", values = c("#440154FF", "#2A788EFF")) +
  #scale_x_continuous(labels=function(x)(x*10)) +
  #coord_cartesian(ylim = c(0,8)) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  ylab("Probability of detection") +
  xlab("Proximity to sunrise/sunset (hr)")

save(dielDetect_farm_plot, dielDetect_farm, file = "dietDetect_farm_binom.Rdata")

# predictions for seasonal behavior by farm type
seasonalDetect_farm <- gam(detect ~ s(JulianDate, by = as.factor(Farm_location), k = 2),
                       data = detect_timediff_meta,
                       family = binomial,
                       na.action = na.fail)

summary(seasonalDetect_farm)

seasonalDetect_farm_predictGrid <- expand_grid(JulianDate = seq(0,365,by=1), 
                                           Farm_location = as.factor(unique(detect_timediff_meta$Farm_location)))

seasonalDetect_farm_preds <- predict(seasonalDetect_farm, 
                                 seasonalDetect_farm_predictGrid,
                                 se.fit = TRUE)

seasonalDetect_farm_sePreds <- data.frame(seasonalDetect_farm_predictGrid,
                                      mu   = exp(seasonalDetect_farm_preds$fit),
                                      low  = exp(seasonalDetect_farm_preds$fit - 1.96 * seasonalDetect_farm_preds$se.fit),
                                      high = exp(seasonalDetect_farm_preds$fit + 1.96 * seasonalDetect_farm_preds$se.fit))

seasonalDetect_farm_plot <- ggplot(seasonalDetect_farm_sePreds, aes(x = JulianDate, y = mu, 
                                                            color = Farm_location, fill = Farm_location)) +
  geom_line() +
  geom_smooth(aes(ymin = low, ymax = high, y = mu), stat = "identity") +
  scale_color_manual(name = "", values = c("#440154FF", "#2A788EFF")) +
  scale_fill_manual(name = "", values = c("#440154FF", "#2A788EFF")) +
  theme_minimal() +
  coord_cartesian(ylim = c(0,0.1)) +
  theme(legend.title = element_blank()) +
  ylab("Probability of detection") +
  xlab("Julian Date")

save(seasonalDetect_farm, seasonalDetect_farm_plot, file = "seasonalDetect_farm_binom.Rdata")

# predictions for seasonal behavior by year
seasonalDetect_year <- gam(detect ~ s(JulianDate, by = as.factor(year)),
                       data = detect_timediff_meta,
                       family = binomial,
                       na.action = na.fail)

summary(seasonalDetect_year)

seasonalDetect_year_predictGrid <- expand_grid(JulianDate = seq(0,365, by = 1), 
                                           year = as.factor(unique(detect_timediff_meta$year)))

seasonalDetect_year_preds <- predict(seasonalDetect_year, 
                                 seasonalDetect_year_predictGrid,
                                 se.fit = TRUE)

seasonalDetect_year_sePreds <- data.frame(seasonalDetect_year_predictGrid,
                                      mu   = exp(seasonalDetect_year_preds$fit),
                                      low  = exp(seasonalDetect_year_preds$fit - 1.96 * seasonalDetect_year_preds$se.fit),
                                      high = exp(seasonalDetect_year_preds$fit + 1.96 * seasonalDetect_year_preds$se.fit)) %>% 
  filter(year == "2023")

seasonalDetect_year_plot <- ggplot(seasonalDetect_year_sePreds, aes(x = JulianDate, y = mu, 
                                                            color = year, fill = year)) +
  geom_line() +
  geom_smooth(aes(ymin = low, ymax = high, y = mu), stat = "identity") +
  scale_color_manual(name = "", values = year_colors) +
  scale_fill_manual(name = "", values = year_colors) +
  theme_classic() +
  theme(legend.title = element_blank()) +
  #coord_cartesian(ylim = c(0,0.1)) +
  ylab("Probability of detection") +
  xlab("Julian Date")

save(seasonalDetect_year_plot, seasonalDetect_year, file = "seasonalDetect_year_binom.Rdata")

# predictions for diel behavior by year
dielDetect_year <- gam(detect ~ s(abs(time_difference), by = as.factor(year)),
                           data = detect_timediff_meta,
                           family = binomial,
                           na.action = na.fail)

summary(dielDetect_year)

dielDetect_year_predictGrid <- expand_grid(time_difference = seq(0,6,by = 0.1), 
                                               year = as.factor(unique(detect_timediff_meta$year)))

dielDetect_year_preds <- predict(dielDetect_year, 
                                     dielDetect_year_predictGrid,
                                     se.fit = TRUE)

dielDetect_year_sePreds <- data.frame(dielDetect_year_predictGrid,
                                          mu   = exp(dielDetect_year_preds$fit),
                                          low  = exp(dielDetect_year_preds$fit - 1.96 * dielDetect_year_preds$se.fit),
                                          high = exp(dielDetect_year_preds$fit + 1.96 * dielDetect_year_preds$se.fit)) %>% 
  filter(year %in% c("2021","2022"))

dielDetect_year_plot <- ggplot(dielDetect_year_sePreds, aes(x = time_difference, y = mu, 
                                                                    color = year, fill = year)) +
  geom_line() +
  geom_smooth(aes(ymin = low, ymax = high, y = mu), stat = "identity") +
  scale_color_manual(name = "", values = year_colors) +
  scale_fill_manual(name = "", values = year_colors) +
  theme_classic() +
  #coord_cartesian(ylim = c(0,0.15), xlim = c(0,5.9)) +
  theme(legend.title = element_blank()) +
  ylab("Probability of detection") +
  xlab("Proximity to sunrise/sunset (hr)") +
  #facet_wrap(~year, ncol = 1, strip.position = "right", scales = "free_y")

save(dielDetect_year_plot, dielDetect_year, file = "dielDetect_year_binom.Rdata")
####Count data model -----------------------------------------------------------

#### Bin data by timediff to nearest event -------------------------------------
bin_summary <- eventNearest %>%
  mutate(month = month(UTC), year = year(UTC)) %>%
  mutate(time_difference = abs(as.numeric(time_difference))*60) %>% 
  mutate(diel.bins = cut(x=time_difference, breaks=seq(0,700,10), labels = FALSE)) %>% 
  group_by(diel.bins, recordName, year) %>%
  summarize(nMin=n()) %>% 
  ungroup() %>% 
  left_join(metadata, by = c("recordName" = "recordName", "year" = "Year"))

#### Check input variable correlation ------------------------------------------
# change categorical to binary
bin_summary2 <- bin_summary%>%
  dplyr::select(-recordName, -nMin, -Farm_depth)

#rom = 1 ML = 0
bin_summary2$Farm_location <- ifelse(bin_summary2$Farm_location== "Rom",1,0)
#cat = 1 5-line = 0
bin_summary2$Farm_type <- ifelse(bin_summary2$Farm_type== "cat",1,0)

cor.test <- cor(bin_summary2[1:5]) #farm location, type, and year all correlated
#corrplot(cor.test, type="upper", order="hclust", addCoef.col = "black")

#### Model timediff to event ---------------------------------------------------

# Generalized Linear Model check for overdispersion
poiss.model.full <- glm(nMin ~ diel.bins + 
                          diel.bins*as.factor(year) +
                          diel.bins*as.factor(Quarter) +
                          diel.bins*Farm_location,
                     family = poisson, data = bin_summary)
# summary results
summary(poiss.model.full)
#AIC 1280.8
check_overdispersion(poiss.model.full) #not overdispersed
check_zeroinflation(poiss.model.full) #no zeros

# Model Selection --------------------------------------------------------------

nMin.crepuscular.dredge <- dredge(global.model = glm(formula = nMin ~ diel.bins +
                                             diel.bins*as.factor(year) +
                                             diel.bins*as.factor(Quarter) +
                                             diel.bins*Farm_location,
                                           data = bin_summary,
                                           family = poisson,
                                           na.action = na.fail),
                          extra = "R^2")

#plot(nMin.crepuscular.dredge) 
#   best model includes all parameters
#   except year x diel.bins (high correlation with farm location)

### Plot residuals -------------------------------------------------------------

plot(residuals(poiss.model.full))

#TODO plot categorical residuals here

# Try a GAM --------------------------------------------------------------------

poiss.model.farm <- gam(nMin ~ s(diel.bins, by = as.factor(Farm_location)),
                        data = bin_summary,
                        family = poisson,
                        na.action = na.fail)

AIC(poiss.model.farm)
#AIC 1698.268, glm is better

# glm plots --------------------------------------------------------------------

# Best model coefficients
plot(parameters(poiss.model.full))

# Predict diel behavior by farm location
poiss.model.farm <- glm(nMin ~ diel.bins*Farm_location,
                        data = bin_summary,
                        family = poisson,
                        na.action = na.fail)

AIC(poiss.model.farm)
#AIC 1383.112

poiss.model.farm_predictGrid <- expand_grid(diel.bins = 1:40, 
                                      Farm_location = as.factor(unique(bin_summary$Farm_location)))

poiss.model.farm_preds <- predict(poiss.model.farm, 
                                     poiss.model.farm_predictGrid,
                                     se.fit = TRUE)

poiss.model.farm_sePreds <- data.frame(poiss.model.farm_predictGrid,
                           mu   = exp(poiss.model.farm_preds$fit),
                           low  = exp(poiss.model.farm_preds$fit - 1.96 * poiss.model.farm_preds$se.fit),
                           high = exp(poiss.model.farm_preds$fit + 1.96 * poiss.model.farm_preds$se.fit))

predDiel_farm_plot <- ggplot(poiss.model.farm_sePreds, aes(x = diel.bins, y = mu, 
                                     color = Farm_location, fill = Farm_location)) +
  geom_line() +
  geom_smooth(aes(ymin = low, ymax = high, y = mu), stat = "identity") +
  scale_color_manual(name = "", values = c("#440154FF", "#2A788EFF")) +
  scale_fill_manual(name = "", values = c("#440154FF", "#2A788EFF")) +
  scale_x_continuous(labels=function(x)(x*10)) +
  coord_cartesian(ylim = c(0,8)) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  ylab("Click positive minutes per bin") +
  xlab("Absolute time difference (min)")

# Predict diel behavior by quarter
poiss.model.quarter <- glm(nMin ~ diel.bins*as.factor(Quarter),
                        data = bin_summary,
                        family = poisson,
                        na.action = na.fail)

AIC(poiss.model.quarter)

poiss.model.quarter_predictGrid <- expand_grid(diel.bins = 1:40, 
                                            Quarter = as.factor(unique(bin_summary$Quarter)))

poiss.model.quarter_preds <- predict(poiss.model.quarter, 
                                  poiss.model.quarter_predictGrid,
                                  se.fit = TRUE)

poiss.model.quarter_sePreds <- data.frame(poiss.model.quarter_predictGrid,
                                       mu   = exp(poiss.model.quarter_preds$fit),
                                       low  = exp(poiss.model.quarter_preds$fit - 1.96 * poiss.model.quarter_preds$se.fit),
                                       high = exp(poiss.model.quarter_preds$fit + 1.96 * poiss.model.quarter_preds$se.fit))

predDiel_quarter_plot <- ggplot(poiss.model.quarter_sePreds, aes(x = diel.bins, y = mu, 
                                     color = Quarter, fill = Quarter)) +
  geom_line() +
  geom_smooth(aes(ymin = low, ymax = high, y = mu), stat = "identity") +
  scale_fill_manual(values = c(pnw_palette("Moth",5, type = "continuous"))) +
  scale_color_manual(values = c(pnw_palette("Moth",5, type = "continuous"))) +
  scale_x_continuous(labels=function(x)(x*10)) +
  coord_cartesian(ylim = c(0,8)) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  ylab("Click positive minutes per bin") +
  xlab("Absolute time difference (min)")

save(predDiel_farm_plot, predDiel_quarter_plot, file = "predDiel_plots.Rdata")
save(poiss.model.full, poiss.model.quarter, poiss.model.farm, file = "predDiel_models.Rdata")
save(dielDetect_dredge, dielDetect_all,
     dielDetect_farm, dielDetect_year,
     dielDetect_farm_plot, #dielDetect_year_plot,
     seasonalDetect_farm, seasonalDetect_year,
     seasonalDetect_farm_plot, seasonalDetect_year_plot,
     file = "detect_binom.Rdata")
