##############################
#
#    NE103: Headstarted Curlew Resighting
#    13/07/2023
#
##############################


# ======================   Variables to pass to setup code source  ===========

# Header code to install and load packages, set directory paths, set seed, capture session info

# project_details <- list(project_name, output_version_name, workspace_version_name)
# package_details <- c("package name 1", "package name 2")

project_details <- list(project_name="curlew", output_version_date="resighting_histories", workspace_version_date="2023-07")
package_details <- c("sf","tidyverse","patchwork","move","moveVis","RColorBrewer","viridisLite","rcartocolor","lubridate","rnrfa","basemaps", "RStoolbox", "cowplot")
seed_number <- 1



# =======================    Read header source code   =================

# header code source:
# 1. sets working directories
# 2. loads required packages
# 3. prints session info to file
# 4. sets seed for reproducibility

# should run on either PC or Mac if using .Rproj
source(file.path("code/source_setup_code_rproj.R"))

# project directories created:
# parentwd = Git
# projectwd = eurasian_african_bird_migration_atlas
# codewd = directory containing code, functions, source, etc
# datawd = directory containing data
# outputwd = directory containing outputs and results (within the appropriate version date)
# workspacewd = directory containing workspace files (.rds, .rda, .RData; within the appropriate version date)
# topoutputwd = top level output directory
# topworkspacewd= top level workspace directory


# =======================    Control values   =================

individual_id <- "4P"


# =======================    Load functions   =================

# Function to convert coordinates in degrees, minutes, seconds to decimal degrees

dms2dec <- function(dms, separators = c("º", "°", "\'", "’", "’’", "\"", "\'\'", "\\?", "′")) {
  
  # version 1.4 (2 Feb 2022)
  # dms: a vector of latitude or longitude in degrees-minutes-seconds-hemisfere, e.g. 41° 34' 10.956" N (with or without spaces)
  # separators: the characters that are separating degrees, minutes and seconds in 'dms'; mind these are taken in the order in which they appear and not interpreted individually, i.e. 7'3º will be taken as 7 degrees, 3 minutes! input data are assumed to be properly formatted
  
  dms <- as.character(dms)
  dms <- gsub(pattern = " ", replacement = "", x = dms)
  for (s in separators) dms <- gsub(pattern = s, replacement = "_splitHere_", x = dms)
  
  splits <- strsplit(dms, split = "_splitHere_")
  n <- length(dms)
  deg <- min <- sec <- hem <- vector("character", n)
  
  for (i in 1:n) {
    deg[i] <- splits[[i]][1]
    min[i] <- splits[[i]][2]
    
    if (length(splits[[i]]) < 4) {
      hem[i] <- splits[[i]][3]
    } else {
      sec[i] <- splits[[i]][3]
      hem[i] <- splits[[i]][4]
    }
  }
  
  dec <- colSums(rbind(as.numeric(deg), (as.numeric(min) / 60), (as.numeric(sec) / 3600)), na.rm = TRUE)
  sign <- ifelse (hem %in% c("N", "E"), 1, -1)
  hem_miss <- which(is.na(hem))
  if (length(hem_miss) > 0) {
    warning("Hemisphere not specified at position(s) ", hem_miss, ", so the sign of the resulting coordinates may be wrong.")
  }
  dec <- sign * dec
  return(dec)
}  # end dms2dec function


# =======================    Load data   =================

today_date <- format(Sys.Date(), "%d-%b-%Y")

# Load data
source(file.path("code", "headstart_CU_database.R"))

resighting_field_names <- c(
  "qa_qc_comments",
  "flag_id",
  "timestamp",
  "observer",
  "date",
  "time",
  "location",
  "description",
  "colour_combo",
  "observer_comments",
  "photos",
  "email",
  "privacy_agreement"
)

  
names(dt_resighting) <- resighting_field_names

dt_resighting_filtered <- dt_resighting %>% 
  filter(flag_id != "")

# =======================    Generate history text file  =================

dt_history <- dt_resighting_filtered %>% 
  filter(flag_id == individual_id) %>% 
  dplyr::select(flag_id, date, time, location) %>% 
  mutate(datetime = paste(date, time)) %>% 
  mutate(new_datetime = strptime(datetime, format = "%d/%m/%Y %H:%M:%S", tz="UTC")) %>% 
  arrange(new_datetime)

dt_history

# do quite a bit of manipulation to get grid refs and lat/lons in different formats (dms and dec degrees) into WGS84 lat/lon coordinates only
dt_location <- dt_history %>% 
  mutate(grid_ref = ifelse(substr(location, 1, 1) %in% LETTERS, location, "")) %>% 
  mutate(grid_ref = ifelse(grid_ref == "", NA, grid_ref)) %>%
  mutate(lat_lon = ifelse(!substr(location, 1, 1) %in% LETTERS, location, "")) %>% 
  mutate(lat_lon_dec = ifelse(str_detect(lat_lon, ","), lat_lon %>% str_remove(" "), NA)) %>% 
  mutate(lat_lon_deg = ifelse(!str_detect(lat_lon, ","), lat_lon, NA)) %>% 
  mutate(lat_lon_deg = ifelse(lat_lon_deg == "", NA, lat_lon_deg)) %>% 
  mutate(lat_deg = lapply(str_split(lat_lon_deg, "N"), function(x) {x[1]}) %>% unlist) %>% 
  mutate(lat_deg = ifelse(!is.na(lat_deg), (paste0(lat_deg, "N")), lat_deg)) %>% 
  mutate(lon_deg = lapply(str_split(lat_lon_deg, "N"), function(x) {x[2]}) %>% unlist) %>% 
  mutate(lat_deg_2_dec = dms2dec(lat_deg)) %>% 
  mutate(lon_deg_2_dec = dms2dec(lon_deg)) %>% 
  mutate(lat_deg_2_dec = ifelse(lat_deg_2_dec > 0, lat_deg_2_dec, NA)) %>% 
  mutate(lon_deg_2_dec = ifelse(lon_deg_2_dec > 0, lon_deg_2_dec, NA)) %>% 
  mutate(lat_lon_deg_2_dec = ifelse(is.na(lat_deg_2_dec), NA, paste(format(lat_deg_2_dec, digits = 6, nsmall = 6), format(lon_deg_2_dec, digits = 6, nsmall = 6), sep=",")))

# create separate dataframe of just WGS84 coordinate rows
dt_lat_lon <- dt_location %>% 
  mutate(lat_lon_dec_new = ifelse(!is.na(lat_lon_dec), lat_lon_dec, ifelse(!is.na(lat_lon_deg_2_dec), lat_lon_deg_2_dec, NA))) %>% 
  filter(!is.na(lat_lon_dec_new))

# split lat/lon from a single column separated by a comma into 2 separate columns
dt_lat_lon <- dt_lat_lon %>% 
  cbind(str_split_fixed(dt_lat_lon$lat_lon_dec_new, ",", n=2) %>% 
          as.data.frame() %>% 
          rename(lat = V1, lon = V2)) %>% 
  dplyr::select(flag_id, date, time, lat, lon, new_datetime)

# convert OSGB grid references into eastings and northings
dt_osgb <- dt_location %>% 
  filter(!is.na(grid_ref)) %>% 
  mutate(easting = osg_parse(grid_ref)[[1]]) %>% 
  mutate(northing = osg_parse(grid_ref)[[2]])

# transform OSGB spatial points into WGS84 spatial points and back to datafraem
dt_osgb_wgs84 <- dt_osgb %>% 
  st_as_sf(coords = c("easting", "northing"), crs = 27700) %>%
  st_transform(4326) %>%
  st_coordinates() %>%
  as.data.frame() %>% 
  rename(lat = Y, lon = X) %>% 
  cbind(dt_osgb) %>% 
  dplyr::select(flag_id, date, time, lat, lon, new_datetime)
  
# rbind former grid refs back together with lat-lon rows and arrange earliest to most recent sighting
dt_all <- rbind(dt_lat_lon, dt_osgb_wgs84) %>% 
  arrange(new_datetime)

dt_all

# output final sightings table
write.csv(dt_all %>% dplyr::select(flag_id, date, time, lat, lon), file = file.path(outputwd, paste0(individual_id, "_resighting_history.csv")), row.names = FALSE)


# =======================    Generate map png  =================

# provide a map using static cowplot png code in headstart_curlew_gps_movements.R

# --------- Prepare bird data into spatial and set mapping extents ------

# turn bird_df into spatial points
bird_df_sf <- dt_all %>% 
  sf::st_as_sf(.,
               coords = c("lon",
                          "lat"),
               crs = 4326) %>% 
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>% 
  arrange(new_datetime)

bird_df_sf_3857 <- st_transform(bird_df_sf, crs = 3857)

bird_df_sf <- bird_df_sf %>% 
  mutate(lon_3857 = st_coordinates(bird_df_sf_3857)[,1],
         lat_3857 = st_coordinates(bird_df_sf_3857)[,2])

current_extent <- st_bbox(bird_df_sf)

set_extent <- st_bbox(c(current_extent[1] - 0.5,
                        current_extent[2] - 0.2, 
                        current_extent[3] + 0.5,
                        current_extent[4] + 0.2), crs = st_crs(4326))

set_extent_inset <- st_bbox(c(current_extent[1] - 7,
                              current_extent[2] - 4, 
                              current_extent[3] + 7,
                              current_extent[4] + 4), crs = st_crs(4326))


# ----- Get basemap -------

# set defaults for the basemap

# Esri aerial imagery
set_defaults(map_service = "esri", map_type = "world_imagery", map_dir = file.path("gis"))

# OSM 

# Mapbox
set_defaults(map_service = "mapbox", map_type = "hybrid", map_token = "pk.eyJ1Ijoic2ZyYW5rczgyIiwiYSI6ImNrcmZkb2QybDVla2wyb254Y2ptZnlqb3UifQ.YNOfGD1SeRMZJDur73Emyg", map_dir = file.path("gis"))


# Plot inset map wider area
basemap_raster_inset <- basemap_raster(set_extent_inset)
# plotRGB(basemap_raster_inset)

# Plot main map raster
basemap_raster <- basemap_raster(set_extent)
# plotRGB(basemap_raster)


# ------- Plot main map & inset map  --------

# Plot RGB rasterbrick with points geom on top
gg_main_map <- RStoolbox::ggRGB(basemap_raster, r=1, g=2, b=3) +
  geom_sf(data = st_transform(bird_df_sf$geometry, projection(basemap_raster)), fill = "magenta", col = "magenta", size = 2) +
  geom_sf(data = st_as_sfc(st_bbox(basemap_raster)), fill = NA, color = "black", size = 1) +
  coord_sf(crs = projection(basemap_raster)) +
  # theme(axis.text.x=element_blank(), #remove x axis labels
  #       axis.ticks.x=element_blank(), #remove x axis ticks
  #       axis.text.y=element_blank(),  #remove y axis labels
  #       axis.ticks.y=element_blank()  #remove y axis ticks
  # ) +
  theme_void()

# Create rectangle bounding box for buffered area of the zoomed in basemap raster
# Buffer by 20000m
bb <- st_bbox(basemap_raster)
set_extent_buffer <- st_bbox(c(bb[1] - 20000,
                               bb[2] - 20000, 
                               bb[3] + 20000,
                               bb[4] + 20000), crs = projection(basemap_raster))
set_extent_buffer <- st_as_sfc(set_extent_buffer)

# create rectangle boundary for outline of the inset map
outline_box_inset <- st_as_sfc(st_bbox(basemap_raster_inset))

# create inset map
# plot inset raster basemap using ggplot RGB conversion
# plot zoomed in basemap bounding box (white rectangle)
# plot black outline around inset map
gg_inset_map <-  RStoolbox::ggRGB(basemap_raster_inset, r=1, g=2, b=3) +
  # geom_sf(data = points_bb, fill = NA, color = "white", size = 0.7) +
  geom_sf(data = set_extent_buffer, fill = NA, color = "white", size = 0.7) +  
  geom_sf(data = outline_box_inset, fill = NA, color = "black", size = 1) +
  theme_void()

# use cowplot package to layer ggplots using ggdraw
cowplot::ggdraw() +
  cowplot::draw_plot(gg_main_map) +
  cowplot::draw_plot(gg_inset_map, -0.08, -0.08, scale = 0.7, width = 0.5, height = 0.5)

ggsave(
  filename = paste0(individual_id, "_static_inset_map_", today_date, ".png"),
  device="png",
  path = outputwd,
  # height = 10,
  # width = 12,
  # units = "in",
  # width = 800,
  # units = "px",
  dpi=300
)
