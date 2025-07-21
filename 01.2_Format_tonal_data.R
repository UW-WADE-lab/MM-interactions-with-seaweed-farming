### MM whistle and moan detections
### AVC Summer 2025

##### set working environment --------------------------------------------------

library(tidyverse)
library(DBI)
library(RSQLite)
library(lubridate)
library(PNWColors)
library(MASS)
library(viridis)
library(patchwork)
library(fuzzyjoin)

#### Read in data ----------------------------------------------------------

metadata <- read.csv("MBL_farm_PR_acoustic_data.csv") %>% 
  dplyr::select(recordName, Farm_location, Farm_type, Farm_depth, Year, Quarter,
                Record.Start.Date.Time, Record.End.Date.Time, Analysis.Start.Date.Time, Analysis.End.Date.Time, Num_click_events) %>% 
  filter(!is.na(Year))

dbList <- grep(list.files("../Whistle Database", full.names = TRUE), pattern = "journal", invert = TRUE, value = TRUE)

dbNames <- data.frame(dbList) %>% 
  separate(dbList, sep = "/", into = c(NA,NA, "name")) %>% 
  separate(name, sep = "\\.", into = c("name", NA)) %>% 
  pull(name)

dbNameList <- dbList
names(dbNameList) <- dbNames

WhisPosDB <- data.frame()

for (i in 1:length(dbList)) {
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbList[i])
  
  result <- tryCatch({
    
    tempWhisPos <- dbGetQuery(conn, "SELECT * from Whistle_and_Moan_Detector") %>% 
      mutate(recordName = names(dbNameList)[i])
    
    WhisPosDB <- bind_rows(WhisPosDB, tempWhisPos)
    
  }, error = function(e) {
    message(sprintf("Error in iteration %d: %s", i, e$message))
    NULL  # or other error handling
  })
  
  dbDisconnect(conn)
  
}

rm(tempWhisPos)
rm(dbList, dbNameList, dbNames)
rm(conn, result, i)

### Format data ----------------------------------------------------------------

#Filter detections in the first two seconds, 
#and those outside the 10-day analysis window
ws_detect <- WhisPosDB %>% 
  filter(startSeconds > 2) %>% 
  mutate(UTC = ymd_hms(UTC)) %>% 
  mutate(year = year(UTC)) %>% 
  mutate(recordName = str_replace(recordName, "WD", "PR")) %>% 
  left_join(metadata, by = c("recordName", "year" = "Year")) %>% 
  mutate(Analysis.Start.Date.Time = mdy_hm(Analysis.Start.Date.Time)) %>% 
  mutate(Analysis.End.Date.Time = mdy_hm(Analysis.End.Date.Time)) %>% 
  filter(UTC >= Analysis.Start.Date.Time & UTC < Analysis.End.Date.Time) %>% 
  mutate(time_diff = difftime(UTC, lag(UTC))) %>% 
  mutate(time_diff = as.numeric(time_diff)) %>% 
  mutate(eventStart = case_when(is.na(time_diff) ~ UTC,
                                time_diff > 10 ~ UTC,
                                time_diff <= 10 ~ NA)) %>% 
  fill(eventStart, .direction= "down") %>% 
  mutate(duration = duration/96000) %>% 
  filter(highFreq > 1000) %>% 
  group_by(eventStart) %>% 
  mutate(tonalEventDur = sum(duration)) %>% 
  filter(tonalEventDur > 1)

  ### Align with click events ----------------------------------------------------

attach("click_event_interactions.Rdata")
Event_dur_near <- Event_dur_near %>% 
  mutate(preStart = StartTime - 60) %>% 
  mutate(postEnd = EndTime + 60)
detach("file:click_event_interactions.Rdata")

ws_click_align <- ws_detect %>% 
  dplyr::select(Id, UID, UTC, eventStart, tonalEventDur) %>% 
  fuzzy_left_join(Event_dur_near, by = c(
                                         "UTC" = "preStart",
                                         "UTC" = "postEnd"),
                  match_fun = list(`>=`, `<=`)) %>% 
  filter(is.na(recordName)) %>% 
  dplyr::select(Id, UID, UTC, eventStart, tonalEventDur) %>% 
  mutate(tonalEventID = group_indices())


