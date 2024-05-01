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

#### Read in data ----------------------------------------------------------

metadata <- read.csv("M:/MBL Box SoundTrap folder/02 Metadata/MBL_farm_PR_acoustic_data.csv") %>% 
  select(recordName, Farm_location, Farm_type, Farm_depth, Year, Quarter,
         Record.Start.Date.Time, Record.End.Date.Time, Analysis.Start.Date.Time, Analysis.End.Date.Time, Num_click_events) %>% 
  filter(!is.na(Year))

#write.csv(metadata, file = "Table S1. Acoustic recording metadata.csv")

dbList <- grep(list.files("M:/MBL Box SoundTrap folder/sqlite", full.names = TRUE), pattern = "journal", invert = TRUE, value = TRUE)

dbNames <- data.frame(dbList) %>% 
  separate(dbList, sep = "/", into = c(NA,NA, "name")) %>% 
  separate(name, sep = "\\.", into = c("name", NA)) %>% 
  pull(name)

dbNameList <- dbList
names(dbNameList) <- dbNames

clickPosDB <- data.frame()
recordDB <- data.frame()
PosDB <- data.frame()

for (i in 1:length(dbList)) {
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbList[i])
  
  #alltables <- dbListTables(conn)
  
  tempClickPos <- dbGetQuery(conn, "SELECT * from Click_Detector_OfflineClicks") %>% 
    select(Id, UID,UTC,BinaryFile,EventId,ClickNo,Amplitude) %>% 
    mutate(recordName = names(dbNameList)[i])
  
  clickPosDB <- bind_rows(clickPosDB, tempClickPos)
  
  tempRecord <- dbGetQuery(conn, "SELECT * from Sound_Acquisition") %>% 
    select(Id, UID,UTC,Status,SystemName, duration) %>% 
    mutate(recordName = names(dbNameList)[i])
  
  recordDB <- bind_rows(recordDB, tempRecord)
  
  tempPos <- dbGetQuery(conn, "SELECT * from Click_Detector_OfflineEvents") %>% 
    select(Id, UID,UTC,EventEnd, eventType, nClicks) %>% 
    mutate(recordName = names(dbNameList)[i])
  
  PosDB <- bind_rows(PosDB, tempPos)
  
  dbDisconnect(conn)
  
}

rm(tempClickPos, tempRecord, tempPos)

#### Filter click events by amplitude ------------------------------------------
#### 160 DB ~ 10 m from hydrophone


clickPosDB_filt <- clickPosDB %>% 
  group_by(EventId) %>% 
  mutate(EventMean = mean(Amplitude)) %>% 
  mutate(nClicks_near = sum(Amplitude >= 160)) %>% 
  filter(nClicks_near >= 20) %>% 
  ungroup()

# df of click positive minutes near farm
clickPos_near_farm <- clickPosDB_filt %>% 
  distinct(recordName, EventId)

# join above df with PosDB to get click positive minutes near farm
PosDB_filt <- clickPos_near_farm %>% 
  left_join(PosDB, by = c("recordName" = "recordName", "EventId" = "Id"))

#### Summarize number and proportion of click positive minutes near farm -------

# number click positive minutes near farm per recording period
NclickPos_near_farm <- clickPos_near_farm %>% 
  group_by(recordName) %>% 
  summarise(nMinPos = n())

# total number of minutes analyzed per recording period
Nmin_record <- recordDB %>% 
  filter(!Status == "Stop                ") %>% 
  group_by(recordName, Status) %>% 
  summarise(num.min = n()) %>% 
  mutate(num.min.new = case_when(Status == "Continue            " ~ floor(num.min/5),
                                 Status == "Start               " ~ floor(num.min),
                                 TRUE ~ 0)) %>% 
  ungroup() %>% 
  group_by(recordName) %>% 
  summarise(nMinTotal = sum(num.min.new))


# add total minutes to NclickPos_near_farm, calculate proportion per recording period
NclickPos_near_farm <- NclickPos_near_farm %>% 
  left_join(Nmin_record, by = "recordName") %>% 
  mutate(propClickPos = nMinPos/nMinTotal)

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

##### Join click pos minutes into events ---------------------------------------

EventDBdur_near <- PosDB_filt %>% 
  mutate(UTC = ymd_hms(UTC)) %>% 
  mutate(EventEnd = ymd_hms(EventEnd)) %>%
  arrange(UTC) %>% 
  mutate(time.diff = UTC - lag(UTC)) %>% 
  mutate(cont = case_when(time.diff > 6 ~ 0,
                          time.diff < 6 ~ 1,
                          TRUE ~ NA)) %>% 
  mutate(GroupEventId = case_when(cont == 0 ~ EventId,
                                  cont == 1 ~ NA,
                                  TRUE ~ NA)) %>% 
  fill(GroupEventId, .direction = "down") %>% 
  mutate(GroupEventId = case_when(row_number() == 1 ~ 1,
                                  TRUE ~ GroupEventId)) %>% 
  group_by(recordName, GroupEventId) %>% 
  mutate(StartTime = min(UTC)) %>% 
  mutate(EndTime = (max(EventEnd))) %>% 
  mutate(eventDur = difftime(EndTime, StartTime, units = "min")) %>% 
  mutate(nClicksEvent = sum(nClicks)) %>% 
  select(recordName, EventId, GroupEventId, StartTime, EndTime, eventDur, nClicksEvent) %>% 
  slice_head() %>% 
  ungroup()

EventDbnum_near <- EventDBdur_near %>% 
  group_by(recordName) %>% 
  summarize(nEvent = n())

#### Add metadata to EventDBnum_near and EventDBdur_near -----------------------

EventDBdur_near <- EventDBdur_near %>% 
  left_join(metadata, by = "recordName") %>% 
  mutate(eventDur = as.numeric(eventDur))

EventDbnum_near <- EventDbnum_near %>% 
  left_join(metadata, by = "recordName") %>% 
  left_join(NclickPos_near_farm, by = "recordName")

#### Add sunrise and sunset times, get the time difference between event and closest sunrise/sunset time

clickpos_min <- PosDB_filt %>%
  mutate(date = as.Date(clickpos_min$UTC,tryFormats = c("%Y-%m-%d"))) %>%
  mutate(datetime = strptime(clickpos_min$UTC, tz = c("UTC"), format = c("%Y-%m-%d %H:%M:%S"))) %>%
  mutate(sunrise = sunrise(clickpos_min$datetime, lon = -67.0467,lat = 17.9455))%>%
  mutate(sunset = sunset(clickpos_min$datetime, lon = -67.0467, lat = 17.9455)) %>%
  mutate(rise.event = abs(difftime(clickpos_min$datetime, clickpos_min$sunrise, tz="UTC", units= c("hours"))))%>%
  mutate(set.event = abs(difftime(clickpos_min$datetime, clickpos_min$sunset, tz="UTC", units= c("hours"))))
  

plot(datetime, rise.event, ylab = c("difftime event to sunrise (hrs)"), xlab = c("event"), main = c("Time Difference Between Event and Sunrise vs Event"))

ggplot(clickpos_min, aes(x=date, y=rise.event)) + 
  geom_violin() + 
  ggtitle("Time Difference Between Event and Sunrise vs Event")+
  labs(y= "difftime event to sunrise(hrs)", x = "event")

plot(date, set.event, ylab = c("difftime event to sunset (min)"), xlab = c("event"), main = c("Time Difference Between Event and Sunset vs Event"))

ggplot(clickpos_min, aes(x=date, y=set.event)) + 
  geom_violin() + 
  ggtitle("Time Difference Between Event and Sunset vs Event") +
  labs(y= "difftime event to sunset(hrs)", x = "event")
##720 minutes = 12 hours (the time between sunrise and sunset at the equinox)
