##############################
#
#    NE103: Headstarted Curlew Map of All Resightings
#    13/07/2023, 16/11/2023
#
##############################


# Code for outputting map of all resightings from the project combined


# Done 04/01/2023
# removed duplicate WWRG records that have also been entered in NE103 google form sheet - inspected by eye and removed duplicate records from the same day / time / location / observer


# =======================    Control values   =================

# TRUE = fresh download of google drive data
update_gdrive_data <- TRUE

# if maps should focus on records around the Wash / N Norfolk only
wash_obs_only <- FALSE

# create mapped points coloured by month of resighting
month_map <- FALSE

# select release year cohorts to map resightings of
select_year <- c(2021, 2022, 2023, 2024)


# ======================   Variables to pass to setup code source  ===========

# Header code to install and load packages, set directory paths, set seed, capture session info

# project_details <- list(project_name, output_version_name, workspace_version_name)
# package_details <- c("package name 1", "package name 2")

project_details <- list(project_name="curlew", output_version_date="resighting_histories", workspace_version_date="2024-09")
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



# =======================    Load functions   =================

source(paste(codewd, "functions", "run_all_functions.R", sep="/"))


# =======================    Load data   =================

today_date <- format(Sys.Date(), "%d-%b-%Y")

# Load data
# Toggle logic value above if fresh download of google drive data is needed
# will need to provide authentication for R to access Google Drive
if (update_gdrive_data) source(file.path("code", "source", "download_gdrive_data.R"))
source(file.path("code", "source", "load_gdrive_data.R"))


# =======================    Prepare resighting data   =================

# FIRST (before running any code!)  ---------------------

# Tidy up the Google sheet form responses
# Check the flag / colour combo column to verify it is indeed a Norfolk headstart
# If yes, then enter the flag code in the flag_id column
# WWRG birds and headstarts from other projects sometimes get submitted to the Google Form
# NE103 birds are sometimes submitted to the WWRG Shiny app - these sightings have all been extracted and transferred to the NE103 Google sheet up to 02/01/2024
# Check the 'location' column particularly: convert any 'descriptive' locations or locations which include text to a OSGB grid ref or rough lat lon location
# E.g. if observer enters "Snettisham Pits mudflats opposite Shore Hide" for location, use Gooogle Maps or similar to determine a rough position and replace the observer descriptive location with a spatial one
# Similarly, if observer includes text in the field together with a georeferenced location, remove the text leaving only the spatial location; e.g. Change "52.4976, 0.62823 Heacham Beach" to just "52.4976, 0.62823"
# Make a note in the column QA-QC comments on the Google sheet if you make any manual changes, retaining the information of what the observer originally entered in the form

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
  "privacy_agreement",
  "entered_DemOn",
  "replied_to_resighter"
) # new column names from KMB's work

names(dt_resighting) <- resighting_field_names

# get rid of any rows which don't have a filled flag_id value
dt_resighting_filtered <- dt_resighting %>% 
  filter(flag_id != "")

# =======================    Generate history text file  =================


# Prepare resighting data  -------------------


dt_history <- dt_resighting_filtered %>% 
  dplyr::select(flag_id, date, time, location) %>% 
  mutate(time = ifelse(time %in% "", "12:00:00", time)) %>% 
  mutate(datetime = paste(date, time)) %>% 
  mutate(new_datetime = strptime(datetime, format = "%d/%m/%Y %H:%M:%S", tz="UTC")) %>% 
  arrange(new_datetime)

dt_history

# do quite a bit of manipulation to get grid refs and lat/lons in different formats (dms and dec degrees) into WGS84 lat/lon coordinates only
# uses bespoke dms2dec function in functions folder
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

dt_resight_all <- merge(dt_resight_all, dt_meta, by = "flag_id", all.x = TRUE) %>% 
  dplyr::select(flag_id, year, date, time, lat, lon, new_datetime)


# Final data for mapping  -------------------

# otherwise, just use resighting data for mapping step

dt_all <- dt_resight_all %>% 
  dplyr::select(flag_id, year, date, lat, lon) %>% 
  mutate(date = strptime(date, format = "%d/%m/%Y", tz="UTC")) %>%
  mutate(month = lubridate::month(date, label = TRUE, abbr = TRUE)) %>% 
  arrange(flag_id, date)

# output final sightings table
write.csv(dt_all %>% dplyr::select(-month), file = file.path(outputwd, paste0("all_birds_resighting_history.csv")), row.names = FALSE)

dt_all



# =======================    Generate map of resightings  =================

# provide a map using static cowplot png code in headstart_curlew_gps_movements.R

# --------- Prepare bird data into spatial  ------

# turn dataset into spatial points
bird_df_sf <- dt_all %>% 
  filter(year %in% select_year) %>% 
  sf::st_as_sf(.,
               coords = c("lon",
                          "lat"),
               crs = 4326) %>% 
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2])

# if wanting to focus on Wash area only, use this shapefile
# Can be found in Google Drive 3. Data > gis_shapefiles https://drive.google.com/drive/folders/1fF0__azUoCUOO8yW2nnkdRJjBfrjVs4C

if (wash_obs_only) {
  # bounding box polygon around the Wash / North Norfolk coast
  gis_wash_dir <- file.path("../../GIS/curlew/wwrg") # Sam's computer GIS filepath. Katharine's need only 1 set of ../
  
  # Load WWRG Wash study shapefile -----------------
  wash_area <- st_read(file.path(gis_wash_dir, "wwrg_wash_study_area_polygon.shp"))
  
  # filter all_tags_points (sf object)
  # clip to only those points falling within the Wash study area
  bird_df_sf <- bird_df_sf %>% 
    # filter(individual_local_identifier %in% unique_birds_study_area) %>% 
    st_intersection(., wash_area)
}


# transform to epsg 3857 used by base maps
bird_df_sf_3857 <- st_transform(bird_df_sf, crs = 3857)

bird_df_sf <- bird_df_sf %>% 
  mutate(lon_3857 = st_coordinates(bird_df_sf_3857)[,1],
         lat_3857 = st_coordinates(bird_df_sf_3857)[,2])


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

if (!month_map) {
  # single colour for points
  gg_main_map <- basemap_main$map +
    geom_sf(data = bird_df_sf_3857, shape = 21, fill = "magenta", col = "white", size = 3, stroke = 0.5) +
    theme_void()
  
} else {
  
  # month colour palette
  month_pal <- c(viridisLite::viridis(12/2, begin = 0.2, end = 0.9), viridisLite::inferno(12/2, begin = 0.2, end = 0.9, direction=-1))
  month_pal[6:7] <- "#F6D645FF" # both Jun/Jul = Jul inferno hue; preference for this one; should hopefully show ok on positron maps
  month_pal[c(1,12)] <- "#420A68FF"
  scales::show_col(month_pal)
  
  # map coloured by month of sighting
  gg_main_map <- basemap_main$map +
    geom_sf(data = bird_df_sf_3857, aes(fill = month), shape = 21, col = "white", size = 3, stroke = 0.5) +
    scale_fill_manual(values = month_pal, name = "Month") +
    theme_void()
}

gg_main_map


# Inset map

# create rectangle boundary for outline of the main map region to show on the inset map
outline_box_main <- st_as_sfc(st_bbox(basemap_main$geom))

gg_inset_map <- basemap_inset$map +
  geom_sf(data = outline_box_main, fill = NA, color = "white", size = 0.7) +  
  theme_void()

# # use cowplot package to layer ggplots using ggdraw
# cowplot::ggdraw() +
#   cowplot::draw_plot(gg_main_map) +
#   cowplot::draw_plot(gg_inset_map, -0.08, -0.08, scale = 0.7, width = 0.5, height = 0.5)

# ------- Output map  --------

# output plot

if (wash_obs_only) out_file <- paste0("all_resighting_obs_static_map_WASH")
if (!wash_obs_only) out_file <- paste0("all_resighting_obs_static_map")
if (month_map) out_file <- paste0(out_file, "_by_month")
out_file <- paste0(out_file, "_", today_date, ".png")

ggsave(
  gg_main_map,
  filename = out_file,
  device="png",
  path = outputwd,
  # height = 10,
  # width = 12,
  # units = "in",
  # width = 800,
  # units = "px",
  dpi=300
)

# -------- Output map of only birds of breeding interest all together ----------

# Update from KMB 2024

# select release year cohorts to map resightings for
select_birds <- c("9L","7V","6A") # three birds that had nests in 2024

# Filter down to birds of interest in 2024
bird_df_sf_breeders <- bird_df_sf_3857 %>%
  filter(flag_id %in% select_birds) %>%
  filter(date >= "2024-01-01")

bird_df_sf_breeders

# make a map of these locations to include in the report

# ------- Plot main map --------

# new base map fitted to resightings of breeders
basemap_main <- make_basemap(sf_data = bird_df_sf_breeders,
                             buff_dist = 30*3000,
                             map_type = "main",
                             map_provider = "Esri.WorldImagery")
basemap_main

# Main map
if (!month_map) {
  # single colour for points
  gg_main_map_breed <- basemap_main$map +
    geom_sf(data = bird_df_sf_breeders, shape = 21, fill = "magenta", col = "white", size = 3, stroke = 0.5) +
    theme_void()
  
} else {
  
  # month colour palette
  month_pal <- c(viridisLite::viridis(12/2, begin = 0.2, end = 0.9), viridisLite::inferno(12/2, begin = 0.2, end = 0.9, direction=-1))
  month_pal[6:7] <- "#F6D645FF" # both Jun/Jul = Jul inferno hue; preference for this one; should hopefully show ok on positron maps
  month_pal[c(1,12)] <- "#420A68FF"
  scales::show_col(month_pal)
  
  # map coloured by month of sighting
  gg_main_map_breed <- basemap_main$map +
    geom_sf(data = bird_df_sf_breeders, aes(fill = month), shape = 21, col = "white", size = 3, stroke = 0.5) +
    scale_fill_manual(values = month_pal, name = "Month") +
    theme_void()
}

gg_main_map_breed

# ------- Output map with breeding locations  --------

# output plot
if (wash_obs_only) out_file_br <- paste0("all_breeding_resighting_obs_static_map_WASH")
if (!wash_obs_only) out_file_br <- paste0("all_breeding_resighting_obs_static_map")
if (month_map) out_file_br <- paste0(out_file_br, "_by_month")
out_file_br <- paste0(out_file_br, "_", today_date, ".png")

ggsave(
  gg_main_map_breed,
  filename = out_file_br,
  device="png",
  path = outputwd,
  # height = 10,
  # width = 12,
  # units = "in",
  # width = 800,
  # units = "px",
  dpi=300
)

