### MM acoustic detections at La Paguera seaweed farm, Puerto Rico
### AVC Autumn 2023

##### set working environment --------------------------------------------------

library(tidyverse)
library(DBI)
library(RSQLite)
library(lubridate)
library(PNWColors)
library(MASS)
library(viridis)
library(patchwork)

#### Read in data ----------------------------------------------------------

metadata <- read.csv("MBL_farm_PR_acoustic_data.csv") %>% 
  dplyr::select(recordName, Farm_location, Farm_type, Farm_depth, Year, Quarter,
         Record.Start.Date.Time, Record.End.Date.Time, Analysis.Start.Date.Time, Analysis.End.Date.Time, Num_click_events) %>% 
  filter(!is.na(Year))

dbList <- grep(list.files("../sqlite", full.names = TRUE), pattern = "journal", invert = TRUE, value = TRUE)

dbNames <- data.frame(dbList) %>% 
  separate(dbList, sep = "/", into = c(NA,NA, "name")) %>% 
  separate(name, sep = "\\.", into = c("name", NA)) %>% 
  pull(name)

dbNameList <- dbList
names(dbNameList) <- dbNames

clickPosDB <- data.frame()
recordDB <- data.frame()
eventPosDB <- data.frame()

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

eventPosDB <- bind_rows(eventPosDB, tempPos)


}, error = function(e) {
  message(sprintf("Error in iteration %d: %s", i, e$message))
  NULL  # or other error handling
})

dbDisconnect(conn)

}

rm(tempClickPos, tempRecord, tempPos)
rm(dbList, dbNameList, dbNames)
rm(conn, result, i)

#### Filter click events by amplitude ------------------------------------------
#### 150 DB ~ 15 m from hydrophone

clickPosDB_filt <- clickPosDB %>% 
  group_by(recordName, EventId) %>% 
  mutate(EventMean = mean(Amplitude)) %>% 
  mutate(nClicks_near = sum(Amplitude >= 150)) %>% 
  filter(nClicks_near >= 10) %>% 
  ungroup() %>% 
  distinct(recordName, EventId, .keep_all = TRUE)

# join above df with PosDB to get click positive minutes near farm
eventPosDB_filt <- clickPosDB_filt %>% 
  left_join(eventPosDB, by = c("recordName" = "recordName", "EventId" = "Id")) %>% 
  dplyr::select(-UTC.x, -UID.y, -UID.x) %>% 
  rename("startUTC" = "UTC.y")

#### Binomial detection dataset ------------------------------------------------

#format datetime columns
eventPosDB_filt_temp <- eventPosDB_filt %>% 
  mutate(startUTC = ymd_hms(startUTC)) %>% 
  mutate(EventEnd = ymd_hms(EventEnd))

#combine recording effort and detections, make a detect column
detect_data_temp <- recordDB %>% 
  mutate(UTC = ymd_hms(UTC)) %>% 
  filter(Status != "Stop                ") %>% 
  distinct(UTC, .keep_all = TRUE) %>% 
  mutate(recordEnd = UTC + 60) %>% 
  left_join(eventPosDB_filt_temp, join_by(recordName, UTC <= startUTC, recordEnd >= startUTC)) %>% 
  mutate(detect = ifelse(is.na(EventId), 0 ,1)) %>% 
  dplyr::select(-Id.x, -Id.y, -UID) %>% 
  group_by(Status, recordName, EventId) 

# are there duplicate detections in the dataset
dups <- detect_data_temp %>% 
  filter(!(is.na(EventId))) %>% 
  group_by(recordName, EventId) %>% 
  mutate(dupe = n() >1) %>% 
  filter(dupe == TRUE) %>% 
  distinct(UTC, .keep_all = TRUE)
  
# check that number of detections here matches number of rows in clickPosDB_filt
nrow(clickPosDB_filt)
sum(detect_data_temp$detect)

#subset to 1/5 of the two recording periods with continuous recording
detect_data_cont_detect <- detect_data_temp %>% 
  filter(recordName %in% c("PR_2021_0430-0512", "PR_2021_0126-0204") & 
           Status == "Continue            " &
           detect == 1) %>% 
  group_by(recordName) %>% 
  tally()

detect_data_cont_subset <- detect_data_temp %>% 
  filter(recordName %in% c("PR_2021_0430-0512", "PR_2021_0126-0204") & 
           Status == "Continue            " &
           detect == 0) %>% 
  slice_sample(prop = 1/5) %>% 
  group_by(recordName) %>% 
  inner_join(detect_data_cont_detect, by = "recordName") %>% 
  filter(row_number() > n)

#add this back into the data

detect_data_all <- detect_data_temp %>% 
  filter(!(recordName %in% c("PR_2021_0430-0512", "PR_2021_0126-0204") & 
             Status == "Continue            " &
             detect == 0)) %>% 
  bind_rows(detect_data_cont_subset)
### PR_2021_0430-0512 2 2021-05-01 07:04:40
### This record is showing up twice and I can't figure out why. 

# remove unanalyzed recording periods
detect_data <- detect_data_all %>% 
  mutate(year = year(UTC)) %>% 
  left_join(metadata, by = c("recordName", "year" = "Year")) %>% 
  mutate(Analysis.Start.Date.Time = mdy_hm(Analysis.Start.Date.Time)) %>% 
  mutate(Analysis.End.Date.Time = mdy_hm(Analysis.End.Date.Time)) %>% 
  filter(UTC >= Analysis.Start.Date.Time & UTC < Analysis.End.Date.Time)

# remove temporary files
rm(eventPosDB_filt_temp, dups, detect_data_temp, detect_data_cont_detect,
   detect_data_cont_subset)

#### Summarize number and proportion of click positive minutes near farm -------

# number click positive minutes near farm per recording period, month, and year
NminPos_near_farm <- eventPosDB_filt %>% 
  mutate(date = as.Date(startUTC)) %>% 
  mutate(month = month(date), year = year(date)) %>% 
  group_by(recordName, year, month) %>% 
  summarise(nMinPos = n())

# total number of minutes analyzed per recording period, month, and year
Nmin_record <- recordDB %>% 
  mutate(date = as.Date(UTC)) %>% 
  mutate(month = month(date), year = year(date)) %>%
  filter(!Status == "Stop                ") %>% 
  group_by(recordName, year, month, Status) %>% 
  summarise(num.min = n()) %>% 
  mutate(num.min.new = case_when(Status == "Continue            " ~ floor(num.min/5),
                                 Status == "Start               " ~ floor(num.min),
                                 TRUE ~ 0)) %>% 
  ungroup() %>%
  group_by(recordName, year,month) %>% 
  summarise(nMinTotal = sum(num.min.new)) %>% 
  left_join(metadata, by = c("recordName","year" = "Year"))


# add total minutes to NclickPos_near_farm, calculate proportion per 
# recording period, month, and year
NminPos_near_farm <- NminPos_near_farm %>% 
  left_join(Nmin_record, by = c("recordName", "year", "month")) %>% 
  mutate(propClickPos = nMinPos/nMinTotal)

#### Plot showing number of minutes analyzed -----------------------------------

effort <- Nmin_record %>% 
  group_by(Farm_location, year, month) %>% 
  mutate(totalMinYear = sum(nMinTotal)) %>% 
  slice_head() %>% 
  ungroup() %>% 
  ggplot(aes(x = as.factor(month), y = totalMinYear, fill = interaction(as.factor(Farm_location), as.factor(year)))) +
  geom_col(position = "dodge2") +
  #scale_fill_manual(name = "", values = c("#440154FF", "#2A788EFF")) +
  scale_fill_manual(name = "", values = c(
                    "ML.2021" = alpha("#440154FF", 0.2),
                    "ML.2022" = alpha("#440154FF", 0.45),
                    "ML.2023" = alpha("#440154FF", 0.7),
                    "ML.2024" = alpha("#440154FF", 1),
                    "Rom.2021" = alpha("#2A788EFF", 0.25),
                    "Rom.2022" = alpha("#2A788EFF", 0.6),
                    "Rom.2023" = alpha("#2A788EFF", 0.7),
                    "Rom.2024" = alpha("#2A788EFF", 1)
  ),
                    labels = c("Rom 2021", "Rom 2022", "ML 2022", "ML 2023", "ML 2024"),
  breaks = c("Rom.2021","Rom.2022", "ML.2022", "ML.2023", "ML.2024")) +
  scale_alpha_discrete(name = "", limits = factor(c(2021, 2022, 2023, 2024)), range = c(0.2,1)) +
  theme_classic() +
  xlab("") +
  ylab("Total effort (min)") +
  guides(fill = "none")

##### Join click pos minutes into events ---------------------------------------

Event_dur_near <- eventPosDB_filt %>% 
  separate(startUTC, into = c("startUTC", NA), sep = "\\.") %>% 
  separate(EventEnd, into = c("EventEnd", NA), sep = "\\.") %>% 
  mutate(startUTC = ymd_hms(startUTC)) %>% 
  mutate(EventEnd = ymd_hms(EventEnd)) %>%
  arrange(startUTC) %>% 
  mutate(time.diff = startUTC - lag(startUTC)) %>% 
  mutate(cont = case_when(time.diff > 6*60 ~ 0,
                          time.diff < 6*60 ~ 1,
                          TRUE ~ NA)) %>% 
  mutate(GroupEventId = case_when(cont == 0 ~ EventId,
                                  cont == 1 ~ NA,
                                  TRUE ~ NA)) %>% 
  fill(GroupEventId, .direction = "down") %>% 
  mutate(GroupEventId = case_when(row_number() == 1 ~ EventId,
                                  TRUE ~ GroupEventId)) %>% 
  group_by(recordName, GroupEventId) %>% 
  mutate(StartTime = min(startUTC)) %>% 
  mutate(EndTime = (max(EventEnd))) %>% 
  mutate(eventDur = difftime(EndTime, StartTime, units = "min")) %>% 
  mutate(nClicksEvent = sum(nClicks)) %>% 
  dplyr::select(recordName, EventId, GroupEventId, StartTime, EndTime, eventDur, nClicksEvent) %>% 
  slice_head() %>% 
  mutate(year = year(StartTime), month = month(StartTime)) %>% 
  ungroup()

Event_num_near <- Event_dur_near %>% 
  group_by(recordName,month,year) %>% 
  summarize(nEvent = n())

#### Add metadata to EventDBnum_near and EventDBdur_near -----------------------

Event_dur_near <- Event_dur_near %>% 
  left_join(metadata, by = c("recordName" = "recordName", "year" = "Year")) %>% 
  mutate(eventDur = as.numeric(eventDur))

Event_num_near <- Event_num_near %>% 
  left_join(metadata, c("recordName" = "recordName", "year" = "Year")) %>% 
  left_join(NminPos_near_farm, by = c("recordName" = "recordName", 
                                      "year" = "year",
                                      "month" = "month",
                                      "Quarter" = "Quarter",
                                      "Farm_location" = "Farm_location")) %>% 
  group_by(year, Quarter, Farm_location) %>% 
  mutate(normEvents = sum(nEvent)/sum(nMinTotal))

#### Plot number of interactions by quarter/month and year ---------------------------

#boxplot
eventNumtime <- Event_num_near %>% 
  ggplot(aes(x = as.factor(Quarter), y = normEvents, 
           )) +
  geom_boxplot(outliers = FALSE) +
  labs(color = "", shape = "") +
  theme_minimal() +
  #facet_wrap(~Farm_location) +
  #theme(legend.position = "none") +
  #geom_jitter(aes(color = as.factor(year), shape = Farm_location), position=position_jitter(0.05), size=2.0, alpha=0.9) +
  #scale_color_manual(name = "Year", values = pnw_palette("Sunset",7)[c(1,3,4,7)]) +
  xlab("Quarter") +
  ylab("Number of interactions/month")+ 
  ylab("# interactions/month")+
  theme(element_text(size=12))

#barplot
eventsMonthly <- Event_num_near %>%
  group_by(year, month, Farm_location) %>%
  summarize(nEventMonthly = (sum(nEvent)/sum(nMinTotal)),
            meanPosClic = mean(propClickPos)) %>%
  ggplot(aes(x = as.factor(month), y = nEventMonthly,
             group = interaction(as.factor(Farm_location), as.factor(year)),
             fill = interaction(as.factor(Farm_location), as.factor(year)))) +
  geom_bar(stat = "identity", position = "dodge2") +
  #geom_point() +
  labs(fill = "Year") +
  ylab("Normalized occurrences/min") +
  xlab("Month") +
  scale_fill_manual(name = "", values = c(
    "ML.2021" = alpha("#440154FF", 0.2),
    "ML.2022" = alpha("#440154FF", 0.45),
    "ML.2023" = alpha("#440154FF", 0.7),
    "ML.2024" = alpha("#440154FF", 1),
    "Rom.2021" = alpha("#2A788EFF", 0.25),
    "Rom.2022" = alpha("#2A788EFF", 0.6),
    "Rom.2023" = alpha("#2A788EFF", 0.7),
    "Rom.2024" = alpha("#2A788EFF", 1)
  ),
  labels = c("Rom 2021", "Rom 2022", "ML 2022", "ML 2023", "ML 2024"),
  breaks = c("Rom.2021","Rom.2022", "ML.2022", "ML.2023", "ML.2024")) +
  scale_alpha_discrete(name = "", limits = factor(c(2021, 2022, 2023, 2024)), range = c(0.2,1)) +
  theme_classic() +
  theme(legend.position = "right", 
        legend.title = element_blank(),
        element_text(size=10),
        plot.margin = unit(c(1, 1, 1, 1), "pt"),
        legend.margin=margin(c(1,1,1,1))) 

#### Plot number of interactions by location -----------------------------------

eventNumloc <- ggplot(data = Event_num_near, aes(x = Farm_location, 
                                  y = nEvent/nMinTotal, 
                                  fill = Farm_location,
                                  color = Farm_location))+
  geom_violin(alpha = 0.6)+
  theme_classic() +
  ylab("Monthly occurrences/min")+
  xlab("Farm location") +
  theme(legend.position = "bottom") +
  scale_fill_manual(name = "Farm\nlocation", 
                    values = c("#440154FF", "#2A788EFF")) +
  scale_color_manual(name = "Farm\nlocation", 
                    values = c("#440154FF", "#2A788EFF")) +
  theme(legend.position = "none")

### ANOVA differentiation -----------------------------------------------------

Num_event_loc <- aov(nEvent/nMinTotal ~ Farm_location, data = Event_num_near)
summary(Num_event_loc)

new_data <- Event_num_near %>%
  filter(!(Farm_location == "Rom" & month %in% c(1,5,8)))

Num_event_loc_outlier <- aov(nEvent/nMinTotal ~ Farm_location, data = new_data)
summary(Num_event_loc_outlier)
#### Plot duration of interactions by quarter and year ------------------------

#boxplot
eventDurtime <- Event_dur_near %>% 
  group_by(year, month, Farm_location) %>% 
  mutate(EventDurMonthly = mean(eventDur)) %>% 
  ggplot(aes(x = as.factor(Quarter), y = eventDur)) +
  geom_boxplot(outliers = FALSE) +
  labs(shape = "", color = "") +
  #geom_point(aes(y = EventDurMonthly, color = as.factor(year), shape = Farm_location)) +
  #geom_jitter(aes(color = as.factor(year), shape = Farm_location), position=position_jitter(0.05), size=2.0, alpha=0.9) +
  #scale_color_manual(name = "Year", values = pnw_palette("Sunset",7)[c(1,3,4,7)]) +
  xlab("Quarter") +
  ylab("Duration of interactions (min)") +
  theme_minimal() + 
  ylab("Duration (min)")+
  theme(element_text(size=12)) +
  guides(fill = guide_legend(nrow = 1))

eventDurhist <- Event_dur_near %>% 
  ggplot(aes(x = eventDur, fill = Farm_location, color = Farm_location)) +
  geom_histogram(aes(y=..count../sum(..count..)), 
                 position = "dodge", binwidth = 5,
                 alpha = 0.6) +
  theme_minimal() +
  ylab("Prop. interactions") +
  xlab("Duration (min)") +
  scale_fill_manual(name = "Farm\nlocation", 
                    values = c("#440154FF", "#2A788EFF")) +
  scale_color_manual(name = "Farm\nlocation", 
                     values = c("#440154FF", "#2A788EFF")) +
  theme(legend.position = "bottom")

test <- Event_dur_near %>% 
  mutate(dur.bin = cut(eventDur, breaks = seq(0,45,5), labels = seq(5,45, 5))) %>% 
  group_by(year, month, Farm_location, dur.bin) %>%
  summarize(n.dur.bin = n()) %>% 
  group_by(dur.bin, Farm_location) %>% 
  summarize(mean_nDur.bin = median(n.dur.bin), sd_nDur.bin = sd(n.dur.bin)) %>% 
  filter(!(is.na(dur.bin)))

eventDurbar <- ggplot(test, aes(x=dur.bin, y = mean_nDur.bin, fill = Farm_location, color = Farm_location)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.6) +
  geom_errorbar(aes(ymin = mean_nDur.bin - sd_nDur.bin, ymax = mean_nDur.bin + sd_nDur.bin), position = "dodge") +
  scale_fill_manual(name = "Farm\nlocation", 
                    values = c("#440154FF", "#2A788EFF")) +
  scale_color_manual(name = "Farm\nlocation", 
                     values = c("#440154FF", "#2A788EFF")) +
  theme(legend.position = "bottom") +
  theme_minimal() +
  xlab("Duration (min)") +
  ylab("# Events Monthly")

test2 <- Event_dur_near %>%
  group_by(year, month, Farm_location) %>% 
  summarize(mean_dur = median(eventDur), sd_dur = sd(eventDur)) 

eventDurMonthly <- ggplot(test2, aes(x=as.factor(month), y = mean_dur, 
                                group = interaction(as.factor(Farm_location), as.factor(year)),
                                fill = interaction(as.factor(Farm_location), as.factor(year)),
                                color = interaction(as.factor(Farm_location), as.factor(year)))) +
  geom_bar(stat = "identity", position = "dodge2") +
  geom_errorbar(aes(ymin = ifelse(mean_dur - sd_dur <0, 0, mean_dur - sd_dur), ymax = mean_dur + sd_dur), position = "dodge2") +
  #geom_point() +
  labs(fill = "Year") +
  ylab("Interaction duration") +
  xlab("Month") +
  scale_fill_manual(name = "", values = c(
    "ML.2021" = alpha("#440154FF", 0.2),
    "ML.2022" = alpha("#440154FF", 0.45),
    "ML.2023" = alpha("#440154FF", 0.7),
    "ML.2024" = alpha("#440154FF", 1),
    "Rom.2021" = alpha("#2A788EFF", 0.25),
    "Rom.2022" = alpha("#2A788EFF", 0.6),
    "Rom.2023" = alpha("#2A788EFF", 0.7),
    "Rom.2024" = alpha("#2A788EFF", 1)
  ),
  labels = c("Rom 2021", "Rom 2022", "ML 2022", "ML 2023", "ML 2024"),
  breaks = c("Rom.2021","Rom.2022", "ML.2022", "ML.2023", "ML.2024")) +
  scale_color_manual(name = "", values = c(
    "ML.2021" = alpha("#440154FF", 0.2),
    "ML.2022" = alpha("#440154FF", 0.45),
    "ML.2023" = alpha("#440154FF", 0.7),
    "ML.2024" = alpha("#440154FF", 1),
    "Rom.2021" = alpha("#2A788EFF", 0.25),
    "Rom.2022" = alpha("#2A788EFF", 0.6),
    "Rom.2023" = alpha("#2A788EFF", 0.7),
    "Rom.2024" = alpha("#2A788EFF", 1)
  ),
  labels = c("Rom 2021", "Rom 2022", "ML 2022", "ML 2023", "ML 2024"),
  breaks = c("Rom.2021","Rom.2022", "ML.2022", "ML.2023", "ML.2024")) +
  scale_alpha_discrete(name = "", limits = factor(c(2021, 2022, 2023, 2024)), range = c(0.2,1)) +
  theme_classic() +
  theme(legend.position = "none", 
        legend.title = element_blank(),
        element_text(size=10),
        plot.margin = unit(c(2, 2, 2, 2), "pt"),
        legend.margin=margin(c(1,1,1,1))) +
  guides(fill=guide_legend(nrow=2, byrow=TRUE)) +
  coord_cartesian(ylim = c(0,10)) +
  guides(fill = "none", color = "none")

eventDur_study1 <- ggplot(Event_dur_near, aes(x=StartTime, y= eventDur)) +
  geom_point(aes(color = Farm_location)) +
  theme_classic() +
  xlab("Interaction date") +
  ylab("Interaction duration") +
  geom_smooth(color = "grey30") +
  coord_cartesian(ylim=c(0,2)) +
  scale_color_manual(name = "Farm\nlocation", 
                     values = c("#440154FF", "#2A788EFF")) +
  guides(color= "none")

eventDur_study2 <- ggplot(Event_dur_near, aes(x=StartTime, y= eventDur)) +
  geom_point(aes(color = Farm_location)) +
  theme_classic() +
  xlab("") +
  ylab("") +
  geom_smooth(color = "grey30") +
  scale_color_manual(name = "Farm\nlocation", 
                     values = c("#440154FF", "#2A788EFF")) +
  guides(color= "none") +
  theme(plot.margin = unit(c(1, 0, 1, 1), "pt")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

eventDur_study <- eventDur_study1 +
  inset_element(eventDur_study2,
        left = 0.5,
        right = 0.99,
        bottom = 0.5,
        top = 0.99)

eventDur_study_lm <- lm(eventDur ~ StartTime, data = Event_dur_near)
summary(eventDur_study_lm)

### Event Duration by farm location ---------------------------------

dur_by_farm <- aov(eventDur ~ Farm_location, data = Event_dur_near)
summary(dur_by_farm)

eventDurloc <- ggplot(data = Event_dur_near, aes(x = Farm_location, 
                                                 y = eventDur, 
                                                 fill = Farm_location,
                                                 color = Farm_location))+
  geom_violin(alpha = 0.6)+
  theme_classic() +
  ylab("Occurrence duration")+
  xlab("Farm location") +
  theme(legend.position = "bottom") +
  scale_fill_manual(name = "Farm\nlocation", 
                    values = c("#440154FF", "#2A788EFF")) +
  scale_color_manual(name = "Farm\nlocation", 
                     values = c("#440154FF", "#2A788EFF")) +
  theme(legend.position = "none")

#save data objects
save(Nmin_record, metadata,
     eventPosDB_filt, clickPosDB_filt, NminPos_near_farm, 
     Event_dur_near, Event_num_near, 
     detect_data, eventDur_study_lm,
     Num_event_loc, Num_event_loc_outlier,
     dur_by_farm,
     file = "click_event_interactions.Rdata")

#save ggplot objects
save(effort, eventNumloc, eventDurtime, eventDurloc,
     eventNumtime, eventDurhist, eventDurbar,
     eventsMonthly, eventDurMonthly, eventDur_study,
     file = "Farm_interactions_figs.Rdata")

