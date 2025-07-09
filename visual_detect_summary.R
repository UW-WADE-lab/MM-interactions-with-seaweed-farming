#### Algae farm visual detection data
#### AVC Summer 2025

### Set up environment ---------------------------------------------------------

library(tidyverse)

### Format visual data ---------------------------------------------------------

visual.detect <- read_csv("Eucheuma Farm animal sighting.csv")[,1:5] %>% 
  mutate(across(Manatee:Dolphins, ~case_when(.x == "A"~0,
                                             .x == "P"~1,
                                             TRUE~NA))) %>% 
  mutate(Date = str_replace_all(Date, pattern = "/", replacement = "-")) %>% 
  mutate(Date = as_date(Date, format = "%d-%m-%Y")) 

### Format acoustic data -------------------------------------------------------

attach("click_event_interactions.Rdata")
Event_dur_near <- Event_dur_near
Nmin_record <- Nmin_record
detach("file:click_event_interactions.Rdata")

Event_date <- Event_dur_near %>% 
  mutate(date = as.Date(StartTime)) %>% 
  group_by(date) %>% 
  summarize(nOcc = n())

### Summarize visual data ------------------------------------------------------

visuals_per_month <- visual.detect %>% 
  mutate(month = month(Date)) %>% 
  mutate(year = year(Date)) %>% 
  group_by(month,year) %>% 
  summarize(totDol = sum(Dolphins), 
            totTur = sum(`Sea Turtle`), 
            totMan = sum(Manatee))

visuals_per_farm <- visual.detect %>% 
  group_by(Farm) %>% 
  summarize(totDol = sum(Dolphins), 
            totTur = sum(`Sea Turtle`), 
            totMan = sum(Manatee))

visualProp_per_farm <- visual.detect %>% 
  group_by(Farm) %>% 
  summarize(propDol = sum(Dolphins)/n(), 
            propTur = sum(`Sea Turtle`)/n(), 
            propMan = sum(Manatee)/n())

obs_per_farm <- visual.detect %>% 
  group_by(Farm) %>% 
  summarise(n())

farm_visit_rate <- visual.detect %>% 
  mutate(time = difftime(Date,lag(Date))) %>% 
  mutate(time = as.numeric(time)) %>% 
  group_by(Farm) %>% 
  summarize(mean = mean(time, na.rm = TRUE), sd = sd(time, na.rm = TRUE))

### Compare visual to acoustic data --------------------------------------------

visual_acoustic <- visual.detect %>% 
  filter(Dolphins == 1) %>% 
  left_join(Event_date, by = c("Date" = "date"))

visual0_acoustic <- visual.detect %>% 
  filter(Dolphins == 0) %>% 
  left_join(Event_date, by = c("Date" = "date")) %>% 
  filter(!(is.na(nOcc)))

### Save data ------------------------------------------------------------------

save(visual.detect,
     obs_per_farm,
     visualProp_per_farm,
     visuals_per_farm,
     visuals_per_month,
     farm_visit_rate,
     visual_acoustic,
     file = "visual_detect_summary.Rdata")
