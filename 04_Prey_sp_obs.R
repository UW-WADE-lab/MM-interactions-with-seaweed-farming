###Fish species at farms
###AVC Summer 2025

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

#### Load data -----------------------------------------------------------------
#scomberomorus are mackerel and have a positive effect on likelihood 
#of present if in large numbers. Caranx are jacks and do not have a positive effect (Rechimont et al. 2018)
fish.data <- read.csv("Fish list by month.csv") %>% 
  fill(Date, .direction = "down") %>% 
  separate(Fish.ID.2022, into = c("com_name", "Sp_name"), sep = "\\(") %>% 
  mutate(Sp_name = str_sub(Sp_name, end = -2)) %>% 
  distinct(Sp_name, .keep_all = TRUE)

#### Prey items include:

prey <- c("Scomberomorus maculatus", "Caranx bartholomaei", "Caranx latus", 
          "Caranx ruber", "Euthynnus teratus", "Scomberomorus cavalla",
          "Caranx crysos", "Cetengraulis edentulus")

#### Find occurrence of common prey items ---------------------------------------

prey.occurence.data <- read.csv("Fish list by month.csv") %>% 
  fill(Date, .direction = "down") %>% 
  separate(Fish.ID.2022, into = c("com_name", "Sp_name"), sep = "\\(") %>% 
  mutate(Sp_name = str_sub(Sp_name, end = -2)) %>% 
  group_by(Sp_name) %>% 
  summarize(nObs = n()) %>% 
  filter(Sp_name %in% prey)
