######################################################
#
#    NE103: Headstarted Curlew recruitment survey sampling 2023-
#
#    Updated 11/03/2025
#
######################################################

# ======================   Variables to pass to setup code source  ===========

# Header code to install and load packages, set directory paths, set seed, capture session info

# project_details <- list(project_name, output_version_name, workspace_version_name)
# package_details <- c("package name 1", "package name 2")

project_details <- list(project_name="curlew", output_version_date="2024-01", workspace_version_date="2024-01")
package_details <- c("sf","tidyverse","patchwork","RColorBrewer","viridisLite","rcartocolor","lubridate", "patchwork", "truncnorm")
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
# projectwd = project name
# codewd = directory containing code, functions, source, etc
# datawd = directory containing data
# outputwd = directory containing outputs and results (within the appropriate version date)
# workspacewd = directory containing workspace files (.rds, .rda, .RData; within the appropriate version date)
# topoutputwd = top level output directory
# topworkspacewd= top level workspace directory

# GIS working directory
giswd <- file.path("../../GIS")
giswd_buffers <- file.path("../../GIS/projects/curlew/headstarting/recruitment_surveys/survey_zones_2024")
giswd_breed <- file.path("../../GIS/projects/curlew/git_shapefiles")



# =======================    Step 1: Preparation in QGIS   =================

# Create stratified survey zone boundaries -----

# mark all release pen locations (don't bother with second Ken Hill pen as it is so close to the first one)
# create 5, 10, 20, and 30 km buffers around each release pen location
# union the buffered polygons for each scale (i.e. union the 5 km buffers, then the 10 km buffers, etc)
# dissolve the unionised buffered polygons to merge overlap areas
# should then have 5 resulting separate polygons for the stratified survey zones; two 5 km polygons (one around SH-pen-02, one dissolved one for SH-pen-01 / KH-pen-01 as their areas overlap); one 10 km polyon; one 20 km polygon; one 30 km polygon
# recode the attribute tables to include two fields: ID and BUFFER (the latter of which values are 5,10,20 or 30 respectively)
# use Data Management > Merge Vector Layers to create a single vector layer merging all of the polygons into a single file (stratified_survey_zones_2024.shp)
# display using a categorised symbology scale according to BUFFER field

# Identify selectable tetrads within overall survey area -----

# take GB002kmgrid shapefile and select a subset of tetrads from the whole file using a rectangle around the survey area, to make geoprocessing faster in the next step
# clip subsetted 2 km grid by the stratified survey zone shapefile; this includes both land and sea (and partial both) tetrads
# use Geoprocessing > Difference to exclude all tetrads wholely within the 1) Greater Wash SPA and 2) Wash SPA shapefiles
# this leaves selectable land-based tetrads within the overall survey area (including partial tetrads along the coast)
# NOTE: the tetrad values in this layer are CORRECT

# Assign strata values to selectable tetrads within overall survey area -----

# assign strata (buffer) values to each tetrad within the overall survey area by using Data Management > Join Attributes by Location
# join the selectable tetrads layer with stratified_survey_zones_2024.shp as the overlay
# choose intersect as the geometric predicate and the attributes of the feature with largest overlap for the join type
# 5 km strata tetrads are for some reason incorrectly assigned strata buffer values of 10 km - manually select tetrads and edit attribute table (see *****NOTE)
# by default, tetrads straddling a boundary between two strata are put into the larger strata (e.g. if a tetrad overlap occurs between the 20 and 30 km strata, the tetrad is assigned to the 30 km strata), even if only a tiny corner of the tetrad actually overlaps the larger strata
# visually assess which strata these boundary tetrads should be in, assigning them (by editing the attribute table) to the strata covering the majority of the tetrad
# use this layer (stratified_survey_zones_selectable_tetrads_2024.shp) to randomly sample from below in R

# *****NOTE*****
# the tetrad and fid_1 values are INCORRECTLY assigned during the join in the step above, but the geometry location is correct
# this was not noticed and fixed for 2024 (it is fixed at a later stage), but in future, it can be fixed by using Join Attributes by Location between the layers: stratified_survey_zones_selectable_tetrads_2024, GB002kmgrid_survey_area_land_all_selectable_tetrads_2024
# before joining, label the tetrad values in stratified_survey_zones_selectable_tetrads_2024 as TETRAD_X (X = incorrect), as the join will then add the TETRAD field from GB002kmgrid_survey_area_land_all_selectable_tetrads_2024, which are correct
# this fix is used later on to correct the 2024 tetrad names (see below)



# =======================    Load GIS data   =================

# ----  Load breeding evidence Wild Sands extended area  ----

shp_breed_ev <- st_read(file.path(giswd_breed, "CU_tetrads_breeding_evidence_Brecks_WildSands.shp"))

# ----  Load tetrads within each buffer zone   ----

# 2024 code

shp_survey_strata <- st_read(file.path(giswd_buffers, "stratified_survey_zones_selectable_tetrads_2024.shp"))

# N 5km tetrads = 46
# N 10km tetrads = 72
# N 20km tetrads = 216
# N 30km tetrads = 394


# 2023 code --------

# # tetrad buffer zones have been created in QGIS
# # all terrestrial tetrads within the buffer zones are included
# # all tetrads which overlap in their entirety with the Wash have been excluded
# 
# shp_buffer_5km <- st_read(file.path(giswd_buffers, "GB002kmgrid_buffer_land_5km.shp"))
# shp_buffer_10km <- st_read(file.path(giswd_buffers, "GB002kmgrid_buffer_land_10km.shp"))
# shp_buffer_20km <- st_read(file.path(giswd_buffers, "GB002kmgrid_buffer_land_20km.shp"))
# shp_buffer_30km <- st_read(file.path(giswd_buffers, "GB002kmgrid_buffer_land_30km.shp"))
# 
# # N 5km tetrads = 33
# # N 10km tetrads = 70
# # N 20km tetrads = 213
# # N 30km tetrads = 383


# =======================    Step 2: Stratified tetrad sampling   =================

###  RULES for survey sampling methdology  ###
# include any squares with previous breeding evidence in last Atlas, and especially all squares around Roydon Common
# 25 days available for surveying, max 3 tetrad surveys possible per day if it takes about 90 min per tetrad
# therefore there is capacity for ca. 75 tetrads to be surveyed, but if we want to leave capacity for revisiting any sites later in the season to e.g. check for breeding evidence, then budget for ca. 50 tetrads
# randomly select tetrads
# stratify sampling according to buffer distance from release sites (more intensive sampling nearer release sites, based on Gerritsen 2021 paper data on average dispersal distance)
# exclude tetrads with > 50% forest, urban, water (assess by eye in QGIS in later step)

### Stratification criteria
# 80% of Dutch chicks return to study area, with average dispersal distance of 3.8km (range = 0-12km)
# Gerritsen, G.J. 2021. De broedbiologie van Wulpen in West-Overijssel. Limosa 94: 19–29.
# generate a truncated random normal distribution with mean = mean Dutch dispersal distance in km, and an sd that takes the right tail of the distribution to approx 30km (sd=12 works)
# curtail the random distribution to the 95th percentile
# group the rnorm values into the buffer strata divisions to get the number of samples per strata
# generate two lots of strata sample totals:
# one to sum to ca. 50 samples (n_small_sample) the number of tetrads that will actually be searched)
# one of ca. 80 samples (n_large_sample) to provide 'back up' options should the randomly selected tetrads from sample of ca. 50 above not meet the minimum suitable habitat criteria

# in 2023 and 2024, n_small_sample was 50 and large sample was 80
# in 2025, we are changing the methodology to reduce the number of randomly sampled tetrads and increase survey time at known breeding sites
# 2025, small sample should be ca. 24, and the large sample ca. 40
n_small_sample <- 24
n_large_sample <- 40

# set sample sizes per buffer strata to end up with ca. 50 tetrads (55 is a good ballpark)
set.seed(1)
r_samp <- truncnorm::rtruncnorm(n_small_sample, a = 0, mean  = 4, sd = 12)
hist(r_samp)

r_samp_ci <- r_samp[r_samp <= quantile(r_samp, 0.95)] %>% sort

samp_buffer_zones <- r_samp_ci %>% 
  as_tibble() %>% 
  # mutate(BUFFER = ifelse(value <= 5, 5, 10))
  mutate(BUFFER = ifelse(value <= 5, 5, ifelse(value > 5 & value <= 10, 10, ifelse(value > 10 & value <= 20, 20, 30))))
samp_buffer_zones %>% as.data.frame()
prop_samp_buffer_zones_50 <- samp_buffer_zones %>% group_by(BUFFER) %>% tally %>% mutate(prop = n / sum(n))


# set sample sizes per buffer strata to end up with ca. 80 tetrads
# this is to have 'back up' tetrads if randomly selected ones do not meet minimum suitable habitat criteria
set.seed(1)
r_samp <- truncnorm::rtruncnorm(n_large_sample, a = 0, mean  = 4, sd = 12)
hist(r_samp)

quantile(r_samp, 0.05)
quantile(r_samp, 0.95)

r_samp_ci <- r_samp[r_samp <= quantile(r_samp, 0.95)] %>% sort

samp_buffer_zones <- r_samp_ci %>% 
  as_tibble() %>% 
  # mutate(BUFFER = ifelse(value <= 5, 5, 10))
  mutate(BUFFER = ifelse(value <= 5, 5, ifelse(value > 5 & value <= 10, 10, ifelse(value > 10 & value <= 20, 20, 30))))
samp_buffer_zones %>% as.data.frame()
prop_samp_buffer_zones_extras <- samp_buffer_zones %>% group_by(BUFFER) %>% tally %>% mutate(prop = n / sum(n))


# 2024 code ---------

# randomly sample rows (i.e. tetrads) from each strata, according to the sample sizes from rtruncnorm above

samp_5km <- sample(nrow(shp_survey_strata %>% filter(BUFFER==5)), size = prop_samp_buffer_zones_extras %>% filter(BUFFER == 5) %>% pull(n), replace = FALSE) %>% as_tibble() %>% mutate(select = 1:nrow(.)) %>% rename(row_num = value)

samp_10km <- sample(nrow(shp_survey_strata %>% filter(BUFFER==10)), size = prop_samp_buffer_zones_extras %>% filter(BUFFER == 10) %>% pull(n), replace = FALSE) %>% as_tibble() %>% mutate(select = 1:nrow(.)) %>% rename(row_num = value)

samp_20km <- sample(nrow(shp_survey_strata %>% filter(BUFFER==20)), size = prop_samp_buffer_zones_extras %>% filter(BUFFER == 20) %>% pull(n), replace = FALSE) %>% as_tibble() %>% mutate(select = 1:nrow(.)) %>% rename(row_num = value)

samp_30km <- sample(nrow(shp_survey_strata %>% filter(BUFFER==30)), size = prop_samp_buffer_zones_extras %>% filter(BUFFER == 30) %>% pull(n), replace = FALSE) %>% as_tibble() %>% mutate(select = 1:nrow(.)) %>% rename(row_num = value)


shp_buffer_5km <- shp_survey_strata %>% filter(BUFFER==5) %>%
  mutate(row_num = row_number()) %>%
  merge(., samp_5km, by = "row_num", all.x = TRUE)
shp_buffer_10km <- shp_survey_strata %>% filter(BUFFER==10) %>%
  mutate(row_num = row_number()) %>%
  merge(., samp_10km, by = "row_num", all.x = TRUE)
shp_buffer_20km <- shp_survey_strata %>% filter(BUFFER==20) %>%
  mutate(row_num = row_number()) %>%
  merge(., samp_20km, by = "row_num", all.x = TRUE)
shp_buffer_30km <- shp_survey_strata %>% filter(BUFFER==30) %>%
  mutate(row_num = row_number()) %>%
  merge(., samp_30km, by = "row_num", all.x = TRUE)

# bind all buffer shapefiles
shp_buffer_all <- rbind(shp_buffer_5km, shp_buffer_10km, shp_buffer_20km, shp_buffer_30km)
shp_buffer_all %>% group_by(select) %>% tally

# # 2023 code ---------
# 
# # randomly sample rows (i.e. tetrads) from each buffer strata, according to the sample sizes from rtruncnorm above
# samp_5km <- sample(nrow(shp_buffer_5km), size = prop_samp_buffer_zones_extras %>% filter(BUFFER == 5) %>% pull(n), replace = FALSE) %>% as_tibble() %>% mutate(select = 1:nrow(.)) %>% rename(row_num = value)
# samp_10km <- sample(nrow(shp_buffer_10km), size = prop_samp_buffer_zones_extras %>% filter(BUFFER == 10) %>% pull(n), replace = FALSE) %>% as_tibble() %>% mutate(select = 1:nrow(.)) %>% rename(row_num = value)
# samp_20km <- sample(nrow(shp_buffer_20km), size = prop_samp_buffer_zones_extras %>% filter(BUFFER == 20) %>% pull(n), replace = FALSE) %>% as_tibble() %>% mutate(select = 1:nrow(.)) %>% rename(row_num = value)
# samp_30km <- sample(nrow(shp_buffer_30km), size = prop_samp_buffer_zones_extras %>% filter(BUFFER == 30) %>% pull(n), replace = FALSE) %>% as_tibble() %>% mutate(select = 1:nrow(.)) %>% rename(row_num = value)
# 
# shp_buffer_5km <- shp_buffer_5km %>% 
#   mutate(row_num = row_number()) %>% 
#   merge(., samp_5km, by = "row_num", all.x = TRUE)
# shp_buffer_10km <- shp_buffer_10km %>% 
#   mutate(row_num = row_number()) %>% 
#   merge(., samp_10km, by = "row_num", all.x = TRUE)
# shp_buffer_20km <- shp_buffer_20km %>% 
#   mutate(row_num = row_number()) %>% 
#   merge(., samp_20km, by = "row_num", all.x = TRUE)
# shp_buffer_30km <- shp_buffer_30km %>% 
#   mutate(row_num = row_number()) %>% 
#   merge(., samp_30km, by = "row_num", all.x = TRUE)
# 
# # bind all buffer shapefiles
# shp_buffer_all <- rbind(shp_buffer_5km, shp_buffer_10km, shp_buffer_20km, shp_buffer_30km)
# shp_buffer_all %>% group_by(select) %>% tally

# write shapefile for all buffers with random selection values added
st_write(shp_buffer_all,
         dsn = giswd_buffers,
         layer = "stratified_survey_zones_selected_tetrads_longlist_2025.shp",
         driver = "ESRI Shapefile")

write.csv(prop_samp_buffer_zones_50, file.path(giswd_buffers, "n_tetrads_strata_shortlist_2025.csv"), row.names = FALSE)


# ==============  Step 3a: Tidy CORRECT selected tetrads for output in QGIS===================

# *****NOTE*****
# the tetrad and fid_1 values were INCORRECTLY assigned when producing the file stratified_survey_zones_selectable_tetrads_2025, though the geometry location is correct
# it was fixed by using Join Attributes by Location between the layers: stratified_survey_zones_selected_tetrads_longlist_2025.shp, GB002kmgrid_survey_area_land_all_selectable_tetrads_2024.shp
# before joining, label the tetrad values in stratified_survey_zones_selected_tetrads_longlist_2025.shp as TETRAD_X (X = incorrect), as the join will then add the TETRAD field from GB002kmgrid_survey_area_land_all_selectable_tetrads_2024, which are correct
# select predicate = intersects, type = 1:1 join, take attributes of feature with largest overlap only, tick discard records which could not be joined




# =======================    Step 3b: Shortlist tetrads in QGIS   =================

# Work in QGIS using the Brecks / Wild Sands Atlas breeding evidence layer and the longlist of randomly selected tetrads per strata layer created in Step 2
# For each strata, priority criteria #1 is to select any tetrad with previous Atlas breeding evidence / known or suspected breeding evidence 2021-23
# In QGIS and using the longlist of randomly selected tetrads per strata, visually select up to prop_samp_buffer_zones_50 tetrads per buffer which occur in suitable habitat, starting with #1 in each strata, as per random selection order (i.e. using 'select' column)


# Create exported shp files of selected tetrads per buffer strata
# stratified_survey_zones_selected_tetrads_shortlist_2025.shp
# use PRIORITY field, together with SUITABLE field (SUITABLE = habitat & access have been assessed as suitable for surveying methodology)
# if both PRIORITY = Y and SUITABLE = Y, then shortlist
# if SUITABLE = N?, then leave until the end and assess if there is time



# ======================  Step 4: Tidy selected tetrads for output  =========================

# read in shp files of selected tetrads created above in QGIS
shp_select_survey <- st_read(file.path(giswd_buffers, "stratified_survey_zones_selected_tetrads_shortlist_2025.shp"))

shp_select_survey_final <- shp_select_survey %>%
  filter(PRIORITY == "Y" & SUITABLE == "Y") %>%
  rename(SELECT_NUM = select)

st_write(shp_select_survey_final,
         dsn = giswd_buffers,
         layer = "stratified_survey_zones_selected_tetrads_final_2025.shp",
         driver = "ESRI Shapefile")

write.csv(shp_select_survey_final %>% st_drop_geometry() %>% dplyr::select(TETRAD, BUFFER, SELECT_NUM, PRIORITY, SUITABLE), file.path(giswd_buffers, "stratified_survey_zones_selected_tetrads_final_2025.csv"), row.names = FALSE)

prop_samp_buffer_zones_50


# # ==============  Step 6: Tidy CORRECT selected tetrads for output in QGIS===================
# 
# # not needed as correction to TETRAD field happens in 3a now
# 
# # read in shp files of selected tetrads created above in QGIS
# shp_select_survey <- st_read(file.path(giswd_buffers, "stratified_survey_zones_selected_tetrads_final_correct_2024.shp"))
# 
# write.csv(shp_select_survey %>% st_drop_geometry() %>% dplyr::select(TETRAD, BUFFER, SELECT_NUM, PRIORITY, SUITABLE), file.path(giswd_buffers, "stratified_survey_zones_selected_tetrads_final_correct_2024.csv"), row.names = FALSE)



# # ======================== LOAD FUNCTIONS ===========================
# 
# # # uses the birdatlas package on Github:
# # # https://github.com/BritishTrustForOrnithology/birdatlas
# # remotes::install_github("BritishTrustForOrnithology/birdatlas", auth_token = "42e2d1d35aa57a58fecc5715773fb994b707f6b4", quiet = FALSE)
# 
# library(birdatlas)
# 
# # # use the source file if not using BTO Atlas package installed using devtools from Github
# # source(paste(parentwd, "Git/atlas_core_functions/include_all_functions.R", sep="/"), chdir=TRUE)
# 
# 
# # # ======================== TTV EFFORT ==========================
# # 
# # # # load details of TTVs surveyed and create a tetrad column with tenkm and tetlet
# # # allttv <- load.ttv.details()
# # # allttv$tetrad <- paste(allttv$tenkm, allttv$tetlet, sep="")
# # # allttv$month <- month(allttv$obsdt)
# # # allttv.breedseason <- filter(allttv, month >= 4 & month <= 7)
# # 
# # # load shapefile of Brecks tetrads that are desired
# # # Brecks tetrad shapefile created based on selecting all tetrads in a rectangular grid + 1 that overlap with the Breckland SPA
# # GB2kmBrecks <- readOGR(paste(parentwd, "GIS/projects/curlew", sep="/"), "GB002km_Brecks")
# # GB2kmBrecks@data$ttv_surveyed <- ifelse(GB2kmBrecks@data$TETRAD %in% allttv.breedseason$tetrad, 1, 0)
# # GB2kmBrecks.surveyed <- subset(GB2kmBrecks, ttv_surveyed==1)
# # writeOGR(GB2kmBrecks.surveyed, outputwd, layer="ttv effort Brecks", driver="ESRI Shapefile")
# 
# 
# # =========================   LOAD & MANIPULATE DATA   ========================
# 
# # readme for protocol_id:
# # AROV = roving record
# # ATTV1 = 1st hour of TTV
# # ATTV2 = 2nd hour of TTV
# # BTC = casual Bird Track records
# # BTL = Bird Track list
# 
# # # ---------------  Load entire Atlas dataset, and subset & save CU only  ------------
# # 
# # # load entire raw 2010 Atlas dataset
# # # convert to data.table and rm/gc dataframe
# # dat_all <- load_raw_data_2010()
# # dt_all <- dat_all %>% as.data.table(.)
# # rm(dat_all)
# # gc()
# # 
# # # # read in Atlas CU data which was extracted by Lucy
# # # dat0 <- read.csv(paste(datawd, "tetrad_raw_data_for_CU_Brecks.csv", sep="/"), header=TRUE)
# # 
# # # subset to CU only, remove unnecessary columns and create tetrad field
# # dt_CU <- dt_all[speccode == 203,]
# # dt_CU[, user_id := NULL]
# # dt_CU[, tetrad := paste0(tenkm, tetlet)]
# # 
# # # save dt_CU as its own dataset
# # saveRDS(dt_CU, file.path(datawd, "all_atlas_2010_curlew.rds"))
# 
# 
# # ---------------  Load all CU Atlas dataset, subset to study tetrads  ------------
# 
# # load RDS of dt_CU
# dt_CU <- readRDS(file.path(datawd, "all_atlas_2010_curlew.rds"))
# 
# # load Breckland study area tetrads from GB002 shapefile
# brecks_tetrads <- foreign::read.dbf(file.path(parentwd, "GIS/projects/curlew", "GB002km_Brecks.dbf")) %>% as.data.table
# brecks_tetrads[, `:=` (FID_1 = NULL,
#                        LAND = NULL)]
# setnames(brecks_tetrads, "TETRAD", "tetrad")
# brecks_tetrads[, area := "brecks"]
# 
# 
# # # load Wild Sands study area tetrads from GB002 shapefile
# # wildsands_tetrads <- foreign::read.dbf(file.path(parentwd, "GIS/projects/curlew", "GB002km_WildSands.dbf")) %>% as.data.table
# # wildsands_tetrads[, `:=` (FID_1 = NULL,
# #                        LAND = NULL)]
# # setnames(wildsands_tetrads, "TETRAD", "tetrad")
# # wildsands_tetrads[, area := "wild sands"]
# 
# # load Wild Sands EXTENDED study area tetrads from GB002 shapefile
# wildsands_tetrads <- foreign::read.dbf(file.path(parentwd, "GIS/projects/curlew", "GB002km_WildSands_extended.dbf")) %>% as.data.table
# wildsands_tetrads[, `:=` (FID_1 = NULL,
#                           LAND = NULL)]
# setnames(wildsands_tetrads, "TETRAD", "tetrad")
# wildsands_tetrads[, area := "wild sands"]
# 
# # combine Brecks / Wild Sands into single 'study area' object list of tetrads
# study_tetrads <- list(brecks_tetrads, wildsands_tetrads) %>% rbindlist
# 
# # subset dt_CU by study areas, remove any tetrads with NA for speccode, select only April through July, Breeding season records, and category possible, probable and confirmed breeding
# dt_CU_study_tetrads <- dt_CU[study_tetrads, on = "tetrad"][!is.na(speccode), ][obsmonth >= 4 & obsmonth <= 7,][season =="B" & cat %in% c(1, 2, 3),]
# 
# # create table showing number of obs in each breeding category status for each tetrad
# tetrad_cat <- as.matrix(table(dt_CU_study_tetrads$tetrad, dt_CU_study_tetrads$cat))
# colnames(tetrad_cat) <- c("possible","probable","confirmed")
# 
# # create new dataset with tetrad and numbers of instances of each breeding category status in each tetrad
# tetrad_cat <- dt_CU_study_tetrads[,.N, .(tetrad, cat)] %>% dcast(., tetrad ~ cat, value.var = "N")
# setnames(tetrad_cat, c("tetrad", "1", "2", "3"), c("tetrad", "possible", "probable", "confirmed"))
# 
# # create a new variable showing highest breeding evidence for a tetrad over the course of the Atlas (can be from any type of Atlas record, TTV1, TTV2, roving record or Bird Track evidence, in any year)
# tetrad_cat[, breedevidence := ifelse(!is.na(confirmed), "confirmed", 
#                                      ifelse(!is.na(probable), "probable", "possible")
#                                      )]
# 
# # add study area to dataset of tetrads and highest breeding status
# study_tetrad_area_lookup <- dt_CU_study_tetrads[,.N, .(tetrad, area)][, .(tetrad, area)]
# tetrad_cat[study_tetrad_area_lookup, area := i.area]
# 
# 
# # ---------------  Write breeding evidence status to shapefile attribute table  ------------
# 
# # Merge breeding evidence status dt to Brecks and Wild Sands shapefile attribute table
# GB2kmBrecks <- st_read(file.path(parentwd, "GIS/projects/curlew/GB002km_Brecks.shp"), stringsAsFactors = FALSE) %>% select(., tetrad = TETRAD)
# GB2kmBrecks_CU <- GB2kmBrecks %>% 
#   merge(., tetrad_cat[area == "brecks",], by = "tetrad")
# st_write(GB2kmBrecks_CU, 
#          dsn = file.path(outputwd, "shapefiles"),
#          layer = "CU_tetrads_breeding_evidence_Brecks",
#          driver = "ESRI Shapefile",
#          update = TRUE)
# 
# 
# GB2kmWildSands <- st_read(file.path(parentwd, "GIS/projects/curlew/GB002km_WildSands_extended.shp"), stringsAsFactors = FALSE) %>% select(., tetrad = TETRAD)
# GB2kmWildSands_CU <- GB2kmWildSands %>% 
#   merge(., tetrad_cat[area == "wild sands",], by = "tetrad")
# st_write(GB2kmWildSands_CU, 
#          dsn = file.path(outputwd, "shapefiles"),
#          layer = "CU_tetrads_breeding_evidence_WildSands_extended",
#          driver = "ESRI Shapefile",
#          update = TRUE)
# 
# 
