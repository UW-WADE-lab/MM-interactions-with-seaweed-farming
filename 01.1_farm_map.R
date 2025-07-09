### MBL acoustic farm locations map
### AVC Winter 2024

##### set working environment --------------------------------------------------

library(tidyverse)
library(ggmap)
library(ggOceanMaps)
library(ggspatial)
library(rnaturalearth)
library(sf)
library(patchwork)

options(ggOceanMaps.userpath = "G:/My Drive/03 Current Research/05 MBL algae farm/01-MM-interactions-with-seaweed-farming/crm_vol9.nc")

pr_shp <- read_sf("G:/My Drive/03 Current Research/05 MBL algae farm/01-MM-interactions-with-seaweed-farming/PR_shp/pvishrpl.shp") %>% 
  st_set_crs(4326)

#### make map ------------------------------------------------------------------

# farm locations

farm_locs <- data.frame(farm = c("Media Luna", "Romero"), lat = c(17.93888, 17.95135), long = c(-67.045555,-66.98997))

#inset_box <- data.frame(lon = c(-67.25, -67.25, -66.7, -66.7), lat = c(17.8, 18.1, 18.1, 17.8))

#world <- ne_coastline(scale = 10, returnclass = "sf")

# map of PR farms
small_map <- basemap(limits = c(-67.3,-66.25,17.7,18.3), 
                     rotate = FALSE, bathy.style = "rub", grid.col = NA,
                     crs = 4326, land.col = "transparent", land.border.col = "transparent") +
  geom_sf(data = pr_shp) +
  coord_sf(xlim = c(-67.15,-66.6), ylim = c(17.8,18.15)) +
  ggspatial::geom_spatial_point(data = farm_locs, 
                                aes(x = long, y = lat, color = farm),
                                size = 4, alpha = 0.8) +
  geom_text(data = data.frame(lat = c(17.99), long = c(-67.04)), 
            aes(x = long, y = lat, inherit.aes = FALSE),
            label="La Parguera", size=3) +
  ggspatial::geom_spatial_point(data = data.frame(lat = c(17.98), long = c(-67.04)), 
                     aes(x = long, y = lat, inherit.aes = FALSE),
                     size = 1.5) +
  ggspatial::geom_spatial_point(data = data.frame(lat = c(18.2), long = c(-67.14)), 
                                aes(x = long, y = lat, inherit.aes = FALSE),
                                size = 1.5) +
  geom_text(data = data.frame(lat = c(18.19), long = c(-67.11)), 
            aes(x = long, y = lat, inherit.aes = FALSE),
            label=paste0("Mayag\U00FC", "ez"), size=3) +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(),  
        axis.ticks.y=element_blank(),
        legend.position = "bottom",
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  # theme(panel.background = element_rect(fill = "lightblue"),
  #       panel.ontop = FALSE) +
  scale_color_manual(name = "", values = c("#440154FF", "#2A788EFF")) +
  guides(fill = "none") +
  guides(color=guide_legend(override.aes=list(fill=NA)))

# map of Carribbean 
  
big_map <- basemap(limits = c(-86,-65,16,28), 
                   rotate = FALSE, bathy.style = "rcb", grid.col = NA) +
  # ggspatial::geom_spatial_point(data = farm_locs, aes(x = long, y = lat,
  #                                                     color = farm),
  #                               size = 1) +
  geom_text(data = data.frame(lat = c(18.3), long = c(-67.045555)), 
            aes(x = long, y = lat, inherit.aes = FALSE),
            label="★", size=6, family = "HiraKakuPro-W3", color = "yellow") +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(),  
        axis.ticks.y=element_blank(),
        legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  # ggspatial::geom_spatial_polygon(
  #   data = inset_box, 
  #   aes(x = lon, y = lat), fill = NA, color = "grey20")  +
  scale_fill_continuous(guide = "none") 


#farm.labs <- c("Media Luna", "Romero")
#names(farm.labs) <- c("ML", "Rom")

# Combine maps
map <- small_map + 
  theme(plot.margin = unit(c(1, 0, 1, 1), "pt"),
        legend.margin=margin(c(0,0,0,0))) +
  inset_element(big_map, 
                left = 0.55, 
                right = 1.001, 
                top = 1.001, 
                bottom = 0.62) 

# Save
save(map, file = "farmLoc_map.Rdata")
