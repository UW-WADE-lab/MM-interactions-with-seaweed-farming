### MM acoustic detections at La Paguera seaweed farm, Puerto Rico
### AVC Autumn 2023

##### set working environment --------------------------------------------------

library(tidyverse)
library(DBI)
library(RSQLite)
library(lubridate)
library(MuMIn)
library(ggsci)
library(MASS)

#### Read in data ----------------------------------------------------------

metadata <- read.csv("MBL_farm_PR_acoustic_data.csv") %>% 
  dplyr::select(recordName, Farm_location, Farm_type, Farm_depth, Year, Quarter,
         Record.Start.Date.Time, Record.End.Date.Time, Analysis.Start.Date.Time, Analysis.End.Date.Time, Num_click_events) %>% 
  filter(!is.na(Year))

#write.csv(metadata, file = "Table S1. Acoustic recording metadata.csv")

dbList <- grep(list.files("../sqlite", full.names = TRUE), pattern = "journal", invert = TRUE, value = TRUE)

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

result <- tryCatch({
  
tempClickPos <- dbGetQuery(conn, "SELECT * from Click_Detector_OfflineClicks") %>% 
  dplyr::select(Id, UID,UTC,BinaryFile,EventId,ClickNo,Amplitude) %>% 
  mutate(recordName = names(dbNameList)[i])

clickPosDB <- bind_rows(clickPosDB, tempClickPos)

tempRecord <- dbGetQuery(conn, "SELECT * from Sound_Acquisition") %>% 
  dplyr::select(Id, UID,UTC,Status,SystemName, duration) %>% 
  mutate(recordName = names(dbNameList)[i])

recordDB <- bind_rows(recordDB, tempRecord)

tempPos <- dbGetQuery(conn, "SELECT * from Click_Detector_OfflineEvents") %>% 
  dplyr::select(Id, UID,UTC,EventEnd, eventType, nClicks) %>% 
  mutate(recordName = names(dbNameList)[i])

PosDB <- bind_rows(PosDB, tempPos)


}, error = function(e) {
  message(sprintf("Error in iteration %d: %s", i, e$message))
  NULL  # or other error handling
})

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
  dplyr::select(recordName, EventId, GroupEventId, StartTime, EndTime, eventDur, nClicksEvent) %>% 
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

#### Plot number of interactions by quarter and year ---------------------------

EventDbnum_near %>% 
  group_by(Year, Quarter, Farm_location) %>% 
  summarize(nEventQuarterly = sum(nEvent)/nMinTotal, meanPosClic = mean(propClickPos)) %>% 
ggplot(aes(x = Quarter, y = meanPosClic, 
           group = interaction(Year, Farm_location), 
           color = as.factor(Year), linetype = Farm_location)) +
  geom_line() +
  labs(color = "Year", linetype = "Farm location")

#barplot
EventDbnum_near %>% 
  group_by(Year, Quarter, Farm_location) %>% 
  summarize(nEventQuarterly = (sum(nEvent)/sum(nMinTotal)), 
            meanPosClic = mean(propClickPos)) %>% 
  ggplot(aes(x = Quarter, y = nEventQuarterly, 
             group = interaction(Year, Farm_location), 
             fill = as.factor(Year))) +
  facet_wrap("Farm_location") +
  geom_bar(stat = "identity", position = "dodge") +
  #geom_point() +
  labs(fill = "Year") +
  ylab("Num. interactions:minutes analysed") +
  scale_fill_aaas()

#boxplot
EventDbnum_near %>% 
  group_by(Year, Quarter, Farm_location) %>% 
  summarize(nEventQuarterly = (sum(nEvent)/sum(nMinTotal)), 
            meanPosClic = mean(propClickPos)) %>% 
  ggplot(aes(x = Quarter, y = nEventQuarterly, 
             group = interaction(Year, Farm_location), 
             fill = as.factor(Year))) +
  facet_wrap("Farm_location") +
  geom_boxplot(stat = "identity", position = "dodge") +
  #geom_point() +
  labs(fill = "Year") +
  ylab("Num. interactions:minutes analysed") +
  scale_fill_aaas()

#### Plot duration of interactions by quarter and year ------------------------

#barplot
EventDBdur_near %>% 
  group_by(Year, Quarter, Farm_location) %>% 
  summarize(EventDurQuarterly = mean(eventDur)) %>% 
  ggplot(aes(x = Quarter, y = EventDurQuarterly, 
             fill = Farm_location)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap("Year") +
  labs(fill = "Farm location") +
  scale_fill_aaas() +
  ylab("Duration of farm interactions (min)")

#boxplot
EventDBdur_near %>% 
  group_by(Year, Quarter, Farm_location) %>% 
  #summarize(EventDurQuarterly = mean(eventDur)) %>% 
  ggplot(aes(x = Quarter, y = eventDur, 
             fill = Farm_location)) +
  geom_boxplot() +
  facet_wrap("Year") +
  labs(fill = "Farm location") +
  scale_fill_aaas() +
  ylab("Duration of farm interactions (min)")+
  ylim(0,10)

EventDBdur_near %>% 
  ggplot(aes(x = eventDur)) +
  geom_histogram(fill = "purple") +
  theme_light() +
  ylab("Number of interactions") +
  xlab("Duration of farm interactions (min)")

#### Model number of interactions ---------------------------------------------

nEvent.dredge <- dredge(global.model = glm(formula = nEvent ~ Farm_location + 
                                                            Farm_type + 
                                                            Year + 
                                                            Quarter +
                                                            nMinTotal,
                                                                                      data = EventDbnum_near,
                          family = poisson,
                          na.action = na.fail), 
       fixed = "nMinTotal",
       extra = "R^2")

plot(nEvent.dredge)

ggplot(data = EventDbnum_near, aes(x = Farm_location, 
                                   y = nEvent, 
                                   fill = Farm_location))+
  geom_violin()+
  scale_fill_jama()+
  theme_classic() +
  ylab("Number of interactions")+
  xlab("Farm location") +
  theme(legend.position = "none")

numIntBest <- glm(formula = nEvent ~ Farm_location + 
                    Year + 
                    Quarter, 
                          data = EventDbnum_near,
                          family = poisson,
                          na.action = na.fail)

summary(numIntBest)

numIntNB <- glm.nb(formula = nEvent ~ Farm_location + 
                     Year + 
                     Quarter, 
                   data = EventDbnum_near,
                   na.action = na.fail)

summary(numIntNB)

#### Model duration of interactions -------------------------------------------

dur.dredge <- dredge(global.model = glm(formula = eventDur ~ Farm_location + 
                                           Farm_type + 
                                           Year + 
                                           Quarter,
                                         data = EventDBdur_near,
                                        family = poisson,
                                         na.action = na.fail), 
                      extra = "R^2")

plot(dur.dredge)

ggplot(data = EventDBdur_near, aes(x = as.factor(Quarter), y = eventDur, fill = Farm_location))+
  geom_violin()+
  scale_fill_jama()+
  theme_classic()+
  ylim(0,50)+
  ylab("Duration of interactions")+
  xlab("Quarter")+
  labs(fill = "Farm location")

ggplot(data = EventDBdur_near, aes(x = as.factor(Year), y = eventDur, fill = Farm_location))+
  geom_violin()+
  scale_fill_jama()+
  theme_classic()+
  #ylim(0,50)+
  ylab("Duration of interactions")+
  xlab("Year") +
  labs(fill = "Farm location")



save(Nmin_record, metadata, PosDB_filt, NclickPos_near_farm, EventDBdur_near, EventDbnum_near, nEvent.dredge, dur.dredge, file = "click_event_interactions.Rdata")

