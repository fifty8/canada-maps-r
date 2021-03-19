# libs = c("sf", "rgdal", 'geojsonio', 'spdplyr', 'rmapshaper', 
# 	'magrittr', 'dplyr', 'tidyr', 'ggplot2')
# install.packages(libs, lib = "~/.rlib")

library(sf) # the base package manipulating shapes
library(rgdal) # geo data abstraction library
library(geojsonio) # geo json input and output
library(spdplyr) # the `dplyr` counterpart for shapes
library(rmapshaper) # the package that allows geo shape transformation
library(magrittr) # data wrangling
library(dplyr)
library(tidyr)
library(ggplot2)

# Read and simplify the geo data ####
canada_raw <- readOGR(dsn = "data", layer = "gcd_000b11a_e", encoding = 'latin1')
canada_raw_json <- geojson_json(canada_raw)
canada_raw_sim <- ms_simplify(canada_raw_json)
geojson_write(canada_raw_sim, file = "data/canada_cd_sim.geojson")

# Read in simplified data ####
canada_cd <- st_read("data/canada_cd_sim.geojson", quiet = TRUE) 

# Define geo transformation ####
crs_string = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" 

# Define ggplot themes and colours ####
# Define the maps' theme -- remove axes, ticks, borders, legends, etc.
theme_map <- function(base_size=9, base_family="") { # 3
	require(grid)
	theme_bw(base_size=base_size, base_family=base_family) %+replace%
		theme(axis.line=element_blank(),
			  axis.text=element_blank(),
			  axis.ticks=element_blank(),
			  axis.title=element_blank(),
			  panel.background=element_blank(),
			  panel.border=element_blank(),
			  panel.grid=element_blank(),
			  panel.spacing=unit(0, "lines"),
			  plot.background=element_blank(),
			  legend.justification = c(0,0),
			  legend.position = c(0,0)
		)
}
# Define the filling colors for each province; max allowed is 9 but good enough for the 13 provinces + territories
map_colors <- RColorBrewer::brewer.pal(9, "Pastel1") %>% rep(37) # 4

# Define major Canadian cities ####
conn = textConnection("city	lat	long
Vancouver	49.2827	-123.1207
Calgary	51.0447	-114.0719
Edmonton	53.5461	-113.4938
Toronto	43.6532	-79.3832
Ottawa	45.4215	-75.6972
Montreal	45.5017	-73.5673")
city_coords = read.delim(conn, stringsAsFactors = F)

sf_cities = city_coords %>%
  select(long, lat) %>%
  as.matrix() %>%
  st_multipoint(dim = 'XY') %>%
  st_sfc() %>%
  st_set_crs(4269)

# Plot the maps ####
ggplot() +
  geom_sf(aes(fill = PRUID), color = "gray60", size = 0.1, data = canada_cd) +
  geom_sf(data = sf_cities, color = 'red', alpha = 0.5, size = 3) + # 17
  coord_sf(crs = crs_string) +
  scale_fill_manual(values = map_colors) +
  guides(fill = FALSE) +
  theme_map() +
  theme(panel.grid.major = element_line(color = "white"),
        legend.key = element_rect(color = "gray40", size = 0.1))
