### MBL acoustic farm locations map
### AVC Winter 2024

##### set working environment --------------------------------------------------

library(tidyverse)
library(ggmap)
library(ggOceanMaps)
library(ggspatial)
library(ggsci)
library(sf)
library(ggnewscale)
library(patchwork)

#options(ggOceanMaps.userpath = "C:/Users/Amy/Downloads/gebco_2023_sub_ice_topo/GEBCO_2023_sub_ice_topo.nc")


#### make map ------------------------------------------------------------------

# farm locations

farm_locs <- data.frame(farm = c("Media Luna", "Romero"), lat = c(17.93888, 17.95135), long = c(-67.045555,-66.98997))

farm_map <- basemap(limits = c(-67.5,-66.5,17.7,18.3), 
                    rotate = FALSE, bathy.style = "rcb", grid.col = NA) +
  ggspatial::geom_spatial_point(data = farm_locs, aes(x = long, y = lat),
                                size = 2) +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(),  
        axis.ticks.y=element_blank(),
        legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  ggspatial::annotation_scale(location = "br") + 
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true")
