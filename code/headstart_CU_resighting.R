##############################
#
#    NE103: Headstarted Curlew Resighting
#    13/07/2023, 16/11/2023
#
##############################


# ======================   Variables to pass to setup code source  ===========

# Header code to install and load packages, set directory paths, set seed, capture session info

# project_details <- list(project_name, output_version_name, workspace_version_name)
# package_details <- c("package name 1", "package name 2")

project_details <- list(project_name="curlew", output_version_date="resighting_histories", workspace_version_date="2023-07")
package_details <- c("sf","tidyverse","patchwork","move","RColorBrewer","viridisLite","rcartocolor","lubridate","rnrfa", "RStoolbox", "cowplot", "maptiles", "tidyterra")
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

# which individual for resighting history
individual_list <- c("2X","5U","6H","6X","7K","8H","8N","YJ","XX","YE","XP","XV","XU","YN","YC","YA","9K","XL","KK","LV","LC")

# "2X","5U","6H","6X","7K","8H","8N","YJ","XX","YE","XP","XV","XU","YN","YC","YA","9K","XL","KK","LV","LC"

individual_id <- "YJ"


# =======================    Load functions   =================

source(paste(codewd, "functions", "run_all_functions.R", sep="/"))


# =======================    Load data   =================

today_date <- format(Sys.Date(), "%d-%b-%Y")

# Load data
source(file.path("code", "headstart_CU_database.R"))


# =======================    Prepare resighting data   =================

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

# FIRST
# Tidy up the Google sheet of form responses, and convert any 'descriptive' locations or locations which include text to a OSGB grid ref or rough lat lon location
# E.g. if observer enters "Snettisham Pits mudflats opposite Shore Hide" for location, use Gooogle Maps or similar to determine a rough position and replace the observer descriptive location with a spatial one
# Similarly, if observer includes text with a spatial location, remove the text leaving only the spatial location; e.g. Change "52.4976, 0.62823 Heacham Beach" to just "52.4976, 0.62823"
# Make a note in the column QA-QC comments on the Google sheet if you make any manual changes, retaining the information of what the observer originally entered in the form

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

# convert any OSGB grid references into eastings and northings
if (dt_location %>% 
    filter(!is.na(grid_ref)) %>% nrow > 0) {
  dt_osgb <- dt_location %>% 
    filter(!is.na(grid_ref)) %>% 
    mutate(grid_ref = str_replace_all(grid_ref, fixed(" "), "")) %>% 
    mutate(easting = osg_parse(grid_ref)$easting) %>% 
    mutate(northing = osg_parse(grid_ref)$northing)
  
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
  dt_resight_all <- rbind(dt_lat_lon, dt_osgb_wgs84) %>% 
    arrange(new_datetime)
  
} else {
  
  # if only lat-lon rows, then just use those for resighting history
  dt_resight_all <- rbind(dt_lat_lon) %>% 
    arrange(new_datetime)
}

dt_resight_all

# rbind original release data with resighting data
# Sandringham 1 10km square = TF62; Wolferton village lat lon = 52.828315, 0.456422
# Sandringham 2 10km square = TF73; Great Bircham lat lon = 52.861603, 0.625233
# Ken Hill 1 and 2 10km square = TF63; Ken Hill estate office lat lon = 52.893391, 0.497013
dt_all <- dt_meta %>%
  filter(flag_id == individual_id) %>% 
  rename(date = release_date) %>% 
  mutate(lat = ifelse("Sandringham 1" %in% release_location, 52.828315, ifelse("Sandringham 2" %in% release_location, 52.861603, 52.893391))) %>% 
  mutate(lon = ifelse("Sandringham 1" %in% release_location, 0.456422, ifelse("Sandringham 2" %in% release_location, 0.625233, 0.497013))) %>% 
  dplyr::select(flag_id, date, lat, lon) %>% 
  rbind(., dt_resight_all %>% dplyr::select(flag_id, date, lat, lon))

dt_all


# output final sightings table
write.csv(dt_all %>% dplyr::select(flag_id, date, lat, lon), file = file.path(outputwd, paste0(individual_id, "_resighting_history.csv")), row.names = FALSE)




# =======================    Generate map of resightings  =================

# provide a map using static cowplot png code in headstart_curlew_gps_movements.R

# --------- Prepare bird data into spatial  ------

# turn bird_df into spatial points
bird_df_sf <- dt_all %>% 
  sf::st_as_sf(.,
               coords = c("lon",
                          "lat"),
               crs = 4326) %>% 
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2])

bird_df_sf_3857 <- st_transform(bird_df_sf, crs = 3857)

bird_df_sf <- bird_df_sf %>% 
  mutate(lon_3857 = st_coordinates(bird_df_sf_3857)[,1],
         lat_3857 = st_coordinates(bird_df_sf_3857)[,2])


# # turn bird_df into spatial points
# bird_release_sf <- dt_all[1,] %>% 
#   sf::st_as_sf(.,
#                coords = c("lon",
#                           "lat"),
#                crs = 4326) %>% 
#   mutate(lon = st_coordinates(.)[,1],
#          lat = st_coordinates(.)[,2])
# 
# bird_release_sf_3857 <- st_transform(bird_release_sf, crs = 3857)


# --------- Create basemaps for main & inset maps  ------

# sf_data = spatial points or polygon
# buff_dist_main = buffer distance (in metres) around sf_data for giving main map some padding; 30km about right
# buff_dist_inset = buffer distance around sf_data for wider context; 500-800km about right
# maptype = "main" for main map showing resightings, or "inset" for showing wider geographic context
# map_provider = see ?maptiles::get_tiles for list of providers
# returns a list where [[1]] is the ggplot map, and [[2]] is the geom object of the map bbox

basemap_main <- make_basemap(sf_data = bird_df_sf_3857,
                             buff_dist = 30*1000,
                             map_type = "main",
                             map_provider = "Esri.WorldImagery")

basemap_main

basemap_inset <- make_basemap(sf_data = bird_df_sf_3857,
                             buff_dist = 800*1000,
                             map_type = "inset",
                             map_provider = "Esri.WorldImagery")

basemap_inset



# ------- Plot main map & inset map  --------

# Main map

gg_main_map <- basemap_main$map +
  geom_sf(data = bird_df_sf_3857[2:nrow(bird_df_sf_3857),], shape = 21, fill = "magenta", col = "white", size = 3, stroke = 0.5) +
  geom_sf(data = bird_df_sf_3857[1,], shape = 21, fill = "cyan", col = "white", size = 3, stroke = 0.5, show.legend = TRUE) +
  geom_sf_text(data = bird_df_sf_3857[1,], hjust = -0.2, vjust = 0, size = 3, col = "white", aes(label = "release site")) +
  theme_void()
gg_main_map


# Inset map

# create rectangle boundary for outline of the main map region to show on the inset map
outline_box_main <- st_as_sfc(st_bbox(basemap_main$geom))

gg_inset_map <- basemap_inset$map +
  geom_sf(data = outline_box_main, fill = NA, color = "white", size = 0.7) +  
  theme_void()
gg_inset_map

# extract the legend from one of the plots
legend <- get_legend(
  # create some space to the left of the legend
  gg_main_map + theme(legend.box.margin = margin(0, 0, 0, 12))
)

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
