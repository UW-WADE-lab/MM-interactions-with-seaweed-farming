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
library(corrplot)
library(see)
library(ggplot2)
library(tidyr)
library(patchwork)
library(ggridges)

#### Read in data ----------------------------------------------------------

load("click_event_interactions.Rdata")

#### Plot showing number of minutes analyzed -----------------------------------

Nmin_record %>% 
  separate(recordName, sep = "_", into = c(NA, "Year", "month"), remove = FALSE) %>% 
  separate(month, sep = "-", into = c("month", NA)) %>% 
  mutate(date = as.Date(month, tryFormats = "%m%d")) %>% 
  mutate(month = month(date)) %>% 
  mutate(month = as.factor(month)) %>% 
  ggplot(aes(x = month, y = nMinTotal, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("2021" = "#005A32", "2022" = "lightblue", "2023" = "steelblue", "2023-2024" = "#ADDD8E", "2024" = "green3"))  # Customize colors here

# Add sunrise and sunset times, get the time difference between event and closest sunrise/sunset time
clickpos_min <- PosDB_filt %>%
  mutate(date = as.Date(UTC, tryFormats = c("%Y-%m-%d"))) %>%
  mutate(datetime = strptime(UTC, tz = c("UTC"), format = c("%Y-%m-%d %H:%M:%S"))) %>%
  mutate(local.event.time = format(datetime, tz = "Etc/GMT-4")) %>% 

  mutate(sunrise = sunrise(datetime, lon = -67.0467, lat = 17.9455)) %>%
  mutate(sunrise.before = sunrise(datetime - 24*60*60, lon = -67.0467, lat = 17.9455)) %>% 
  mutate(sunset = sunset(datetime, lon = -67.0467, lat = 17.9455)) %>%
  mutate(sunset.before = sunset(datetime - 24*60*60, lon = -67.0467, lat = 17.9455)) %>% 
  mutate(rise.event = difftime(datetime, sunrise, tz = "UTC", units = c("hours"))) %>%  # Keep the sign
  mutate(rise.event.before = difftime(datetime, sunrise.before, tz = "UTC", units = c("hours"))) %>%  # Keep the sign
  mutate(set.event = difftime(datetime, sunset, tz = "UTC", units = c("hours"))) %>%  # Keep the sign
  mutate(set.event.before = difftime(datetime, sunset.before, tz = "UTC", units = c("hours"))) %>%  # Keep the sign
  pivot_longer(set.event:set.event.before, names_to = "sunset.option", values_to = "closest.sunset") %>% 
  group_by(datetime) %>% 
  mutate(abs.time = abs(closest.sunset)) %>% 
  slice_min(abs.time) %>%
  dplyr::select(-c(abs.time, sunset.option)) %>% 
  ungroup() %>% 
  pivot_longer(rise.event:rise.event.before, names_to = "sunrise.option", values_to = "closest.sunrise") %>% 
  group_by(datetime) %>% 
  mutate(abs.time = abs(closest.sunrise)) %>% 
  slice_min(abs.time) %>% 
  dplyr::select(-sunrise.option, -abs.time)
  

# Extract the time component (hours and minutes) from 'sunrise' and 'sunset' columns
times_df <- clickpos_min %>%
  mutate(sunrise_time = format(sunrise, "%H:%M:%S"),  # Extract time from sunrise column
         sunset_time = format(sunset, "%H:%M:%S")) %>%  # Extract time from sunset column
  dplyr::select(sunrise_time, sunset_time)  # Select only the sunrise and sunset time columns

# Checks ranges for sunrise and sunset times
range(times_df$sunrise_time, na.rm = TRUE)
range(times_df$sunset_time, na.rm = TRUE)

# View the new dataframe with sunrise and sunset times
head(times_df)

# Extract time from datetime and convert to decimal hours
clickpos_min <- clickpos_min %>%
  mutate(event_time = hour(datetime) + minute(datetime) / 60)  # Convert to decimal hours

# Reshape the data to long format
clickpos_long <- clickpos_min %>%
  pivot_longer(cols = c(closest.sunrise, closest.sunset), 
               names_to = "event_type", 
               values_to = "time_difference")


# Reshape the data to long format to include sunrise and sunset times
## change this to select the appropriate sunrise or sunset
clickpos_long <- clickpos_min %>%
  dplyr::select(datetime, sunrise, sunset) %>%  # Select relevant columns
  pivot_longer(cols = c(datetime, sunrise, sunset), 
               names_to = "event_type", 
               values_to = "event_time") 

# Extract hours from the event time for plotting
clickpos_long <- clickpos_long %>%
  mutate(event_hour = format(event_time, "%H")) %>%
  mutate(event_hour = as.numeric(event_hour))  # Convert to numeric

# Filter to include only datetime event_type
clickpos_long_filtered <- clickpos_long %>%
  filter(event_type == "datetime")

# Adjust the x-axis for Puerto Rico time (UTC-4)
simple.hist <- ggplot(data = clickpos_long_filtered, aes(x = (event_hour - 4) %% 24)) + 
  # Add the histogram bars
  geom_histogram(breaks = seq(0,24,by=1), fill = "darkblue", color = "black") +  
  scale_x_continuous(breaks = seq(0, 24, by = 1), limits = c(0, 24)) +  
  labs(
    #title = "Number of Farm Interactions by Time of Day",
    x = "Local Time of Day (AST)",
    y = "Number of Farm Interactions"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Print the updated plot
simple.hist






#### Add sunrise and sunset times and calculate time difference for each event ------
clickpos_min2 <- PosDB_filt %>%
  mutate(date = as.Date(UTC, tryFormats = c("%Y-%m-%d"))) %>%
  mutate(datetime = strptime(UTC, tz = "UTC", format = c("%Y-%m-%d %H:%M:%S"))) %>%
  mutate(sunrise = sunrise(datetime, lon = -67.0467, lat = 17.9455)) %>%
  mutate(sunset = sunset(datetime, lon = -67.0467, lat = 17.9455)) %>%
  mutate(rise.event = difftime(datetime, sunrise, tz = "UTC", units = "hours")) %>%
  mutate(set.event = difftime(datetime, sunset, tz = "UTC", units = "hours")) %>% 
  mutate(rise.event = as.numeric(rise.event)) %>%
  mutate(set.event = as.numeric(set.event))

# Filtering to keep only the closest event (sunrise or sunset)
clickpos_long_filtered2 <- clickpos_min %>%
  pivot_longer(cols = c(closest.sunrise, closest.sunset), 
               names_to = "event_type", 
               values_to = "time_difference") %>% 
  group_by(datetime) %>%
  slice_min(abs(time_difference)) %>%
  ungroup()

#### Histogram showing number of click positive minutes vs the time since either sunrise or sunset
# Histogram using base R

# Filter to only keep the closest event (sunrise or sunset) for each row
# clickpos_filtered2 <- clickpos_min2 %>%
#   mutate(closest_event = ifelse(abs(rise.event) < abs(set.event), "sunrise", "sunset")) %>%
#   mutate(closest_time = ifelse(abs(rise.event) < abs(set.event), rise.event, set.event)) %>%
#   dplyr::select(closest_event, closest_time)

# Reshape the data into long format for ggplot
# clickpos_long_filtered2 <- clickpos_min2 %>%
#   pivot_longer(cols = c(rise.event, set.event), 
#                names_to = "event_type", 
#                values_to = "time_difference") %>%
#   group_by(datetime) %>%  # Group by each event's timestamp
#   filter(abs(time_difference) == min(abs(time_difference))) %>%  # Keep only the closest event
#   ungroup()  # Ungroup to return to original data structure

# Plot the data with stacked panels
ggplot(clickpos_long_filtered2, aes(x = time_difference)) +
  geom_histogram(binwidth = 0.17, fill = "steelblue", color = "black") +
  facet_grid(event_type ~ ., scales = "free_y") +  # Stack panels on top of each other
  scale_x_continuous(limits = c(-12, 8), breaks = seq(-12, 12, by = 2)) +
  labs(
    title = "Histogram of Time Difference to Sunrise and Sunset",
    x = "Time (hrs) Since Sunrise or Sunset",
    y = "Number of Click Positive Minutes"
  ) +
  theme_minimal()

# Define new labels for the event types
event_labels <- c("closest.sunrise" = "Sunrise", "closest.sunset" = "Sunset")

# Define custom colors for the events
custom_colors <- c("closest.sunrise" = "aquamarine3", "closest.sunset" = "steelblue1")  # Example colors

# Create the ridgeline plot with renamed event types and specified order
ggplot(clickpos_long_filtered2, aes(x = time_difference, 
                                   y = reorder(event_type, desc(event_type)),  # Reorder here
                                   fill = event_type)) +
  geom_density_ridges(alpha = 0.8, scale = .9, rel_min_height = 0.001) +  # Increase scale for more spacing
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1) +  # Add vertical line at 0
  scale_x_continuous(limits = c(-12, 8), breaks = seq(-12, 8, by = 2)) +
  scale_y_discrete(labels = event_labels) +  # Apply new labels
  scale_fill_manual(values = custom_colors) +  # Apply custom colors
  labs(
    title = "Time Difference from Click Positive Minute to Sunrise, Sunset",
    x = "Time Difference (hrs)",
    y = ""
  ) +
  theme_minimal() +
  theme(legend.position = "none")


#### OLD CODE

# Reshaped data is now ready for analysis and plotting.
# Convert time differences to numeric format
# clickpos_min2 <- clickpos_min2 %>%
#   mutate(rise.event = as.numeric(rise.event)) %>%
#   mutate(set.event = as.numeric(set.event)) 
# ### Plots showing the time difference between event and sunrise on y axis, date on x axis
# #720 minutes = 12 hours (the time between sunrise and sunset at the equinox)
# plot(clickpos_min$datetime, clickpos_min$rise.event, ylab = c("difftime event to sunrise (hrs)"), xlab = c("event"), main = c("Time Difference Between Event and Sunrise vs Event"))
# 
# ggplot(clickpos_min, aes(x=date, y=rise.event)) +
#   geom_violin() +
#   ggtitle("Time Difference Between Event and Sunrise vs Event")+
#   labs(y= "difftime event to sunrise(hrs)", x = "event")
# 
# #### Plots showing the time difference between event and sunset on y axis, date on x axis
# plot(clickpos_min$date, clickpos_min$set.event, ylab = c("difftime event to sunset (min)"), xlab = c("event"), main = c("Time Difference Between Event and Sunset vs Event"))
# 
# ggplot(clickpos_min, aes(x=date, y=set.event)) +
#   geom_violin() +
#   ggtitle("Time Difference Between Event and Sunset vs Event") +
#   labs(y= "difftime event to sunset(hrs)", x = "event")

