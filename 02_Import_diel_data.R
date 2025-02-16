### MM acoustic detections
### Calculate time difference to sunrise/sunset
### AVC Autumn 2023

##### set working environment --------------------------------------------------

library(tidyverse)
library(lubridate)
library(bioRad)
library(MASS)
library(see)

#### Read in data ----------------------------------------------------------

load("click_event_interactions.Rdata")

#### Binomial data format ------------------------------------------------------
detect_data_timediff <- detect_data %>% 
  mutate(sunrise = sunrise(UTC, lon = -67.0467, lat = 17.9455)) %>%
  mutate(sunrise.before = sunrise(UTC - 24*60*60, lon = -67.0467, lat = 17.9455)) %>% 
  mutate(sunset = sunset(UTC, lon = -67.0467, lat = 17.9455)) %>%
  mutate(sunset.before = sunset(UTC - 24*60*60, lon = -67.0467, lat = 17.9455)) %>% 
  mutate(rise.event = difftime(UTC, sunrise, tz = "UTC", units = c("hours"))) %>%  # Keep the sign
  mutate(rise.event.before = difftime(UTC, sunrise.before, tz = "UTC", units = c("hours"))) %>%  # Keep the sign
  mutate(set.event = difftime(UTC, sunset, tz = "UTC", units = c("hours"))) %>%  # Keep the sign
  mutate(set.event.before = difftime(UTC, sunset.before, tz = "UTC", units = c("hours"))) %>%  # Keep the sign
  pivot_longer(set.event:set.event.before, names_to = "sunset.option", values_to = "closest.sunset") %>% 
  group_by(UTC) %>% 
  mutate(abs.time = abs(closest.sunset)) %>% 
  slice_min(abs.time) %>%
  dplyr::select(-c(abs.time, sunset.option)) %>% 
  ungroup() %>% 
  pivot_longer(rise.event:rise.event.before, names_to = "sunrise.option", values_to = "closest.sunrise") %>% 
  group_by(UTC) %>% 
  mutate(abs.time = abs(closest.sunrise)) %>% 
  slice_min(abs.time) %>% 
  dplyr::select(-sunrise.option, -abs.time)

detect_timediff_eventNearest <- detect_data_timediff %>%
  pivot_longer(cols = c(closest.sunrise, closest.sunset), 
               names_to = "event_type", 
               values_to = "time_difference") %>% 
  group_by(UTC) %>%
  slice_min(abs(time_difference)) %>%
  ungroup()

### Count data format ----------------------------------------------------------

# Add sunrise and sunset times, get the time difference between event and closest sunrise/sunset time
clickPos_timediff_event <- clickPosDB_filt %>%
  mutate(date = as.Date(UTC, tryFormats = c("%Y-%m-%d"))) %>%
  mutate(UTC = strptime(UTC, tz = c("UTC"), format = c("%Y-%m-%d %H:%M:%S"))) %>%
  mutate(local.event.time = with_tz(UTC, tz = "Etc/GMT+4")) %>% 
  mutate(sunrise = sunrise(UTC, lon = -67.0467, lat = 17.9455)) %>%
  mutate(sunrise.before = sunrise(UTC - 24*60*60, lon = -67.0467, lat = 17.9455)) %>% 
  mutate(sunset = sunset(UTC, lon = -67.0467, lat = 17.9455)) %>%
  mutate(sunset.before = sunset(UTC - 24*60*60, lon = -67.0467, lat = 17.9455)) %>% 
  mutate(rise.event = difftime(UTC, sunrise, tz = "UTC", units = c("hours"))) %>%  # Keep the sign
  mutate(rise.event.before = difftime(UTC, sunrise.before, tz = "UTC", units = c("hours"))) %>%  # Keep the sign
  mutate(set.event = difftime(UTC, sunset, tz = "UTC", units = c("hours"))) %>%  # Keep the sign
  mutate(set.event.before = difftime(UTC, sunset.before, tz = "UTC", units = c("hours"))) %>%  # Keep the sign
  pivot_longer(set.event:set.event.before, names_to = "sunset.option", values_to = "closest.sunset") %>% 
  group_by(UTC) %>% 
  mutate(abs.time = abs(closest.sunset)) %>% 
  slice_min(abs.time) %>%
  dplyr::select(-c(abs.time, sunset.option)) %>% 
  ungroup() %>% 
  pivot_longer(rise.event:rise.event.before, names_to = "sunrise.option", values_to = "closest.sunrise") %>% 
  group_by(UTC) %>% 
  mutate(abs.time = abs(closest.sunrise)) %>% 
  slice_min(abs.time) %>% 
  dplyr::select(-sunrise.option, -abs.time) %>%
  mutate(eventTime_hour = hour(local.event.time) + 
           minute(local.event.time)/60 + 
           second(local.event.time)/3600) %>% 
  ungroup()
  
# Click positive minutes by time of day
clickPos_diel_plot <- clickPos_timediff_event %>%
ggplot(aes(x = eventTime_hour)) +
  geom_histogram(breaks = seq(0,24,by=1), fill = "#675478", alpha = 0.6) +  
  scale_x_continuous(breaks = seq(0, 24, by = 1), limits = c(0, 24)) +  
  labs(x = "Local Time of Day (AST)",
    y = "Number of Farm Interactions") +
  theme_minimal() +
  theme(legend.position = "none")

# Filter data to keep only the closest event (sunrise or sunset)
eventNearest <- clickPos_timediff_event %>%
  pivot_longer(cols = c(closest.sunrise, closest.sunset), 
               names_to = "event_type", 
               values_to = "time_difference") %>% 
  group_by(UTC) %>%
  slice_min(abs(time_difference)) %>%
  ungroup()

#Click positive minutes by time to nearest event
clickPos_nearEvent_plot <- ggplot(eventNearest, aes(x = as.numeric(time_difference), 
                         fill = event_type,
                         color = event_type)) +
  geom_density(alpha = 0.5)+
  scale_x_continuous(limits = c(-8, 8), breaks = seq(-8, 8, by = 2)) +
  scale_fill_manual(values = pnw_palette("Bay"), labels = c("Sunrise", "Sunset")) +
  scale_color_manual(values = pnw_palette("Bay"), labels = c("Sunrise", "Sunset")) +
  labs(x = "Time Difference (hrs)", y = "") +
  theme_minimal() +
  theme(legend.title = element_blank())

save(clickPos_nearEvent_plot, clickPos_diel_plot, file = "delphinid_diel_activity_plots.Rdata")
save(clickPos_timediff_event, eventNearest, detect_timediff_eventNearest,
     detect_data_timediff, file = "delphinid_diel_activity_data.Rdata")
