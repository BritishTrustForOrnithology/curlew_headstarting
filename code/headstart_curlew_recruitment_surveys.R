##############################
#
#    NE103: Headstarted Curlew recruitment survey sampling
#
##############################

# ======================   Variables to pass to setup code source  ===========

# Header code to install and load packages, set directory paths, set seed, capture session info

# project_details <- list(project_name, output_version_name, workspace_version_name)
# package_details <- c("package name 1", "package name 2")

project_details <- list(project_name="curlew", output_version_date="2023-03", workspace_version_date="2023-03")
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
giswd_buffers <- file.path("../../GIS/curlew/headstarting/recruitment_surveys")
giswd_breed <- file.path("../../GIS/curlew/git_shapefiles/")


# =======================    Load GIS data   =================

# ----  Load breeding evidence Wild Sands extended area  ----

shp_wildsands_breed <- st_read(file.path(giswd_breed, "CU_tetrads_breeding_evidence_Brecks.shp"))

# ----  Load tetrads within each buffer zone   ----

# tetrad buffer zones have been created in QGIS
# all terrestrial tetrads within the buffer zones are included
# all tetrads which overlap in their entirety with the Wash have been excluded

shp_buffer_5km <- st_read(file.path(giswd_buffers, "GB002kmgrid_buffer_land_5km.shp"))
shp_buffer_10km <- st_read(file.path(giswd_buffers, "GB002kmgrid_buffer_land_10km.shp"))
shp_buffer_20km <- st_read(file.path(giswd_buffers, "GB002kmgrid_buffer_land_20km.shp"))
shp_buffer_30km <- st_read(file.path(giswd_buffers, "GB002kmgrid_buffer_land_30km.shp"))

# N 5km tetrads = 33
# N 10km tetrads = 70
# N 20km tetrads = 213
# N 30km tetrads = 383


# =======================    Stratified tetrad sampling   =================

###  RULES for survey sampling methdology  ###
# include any squares with previous breeding evidence in last Atlas, and especially all squares around Roydon Common
# 25 days available for surveying, max 3 tetrad surveys possible per day if it takes about 90 min per tetrad
# capacity for ca. 75 tetrads to be surveyed, but if we want to leave capacity for revisiting any sites later in the season to e.g. check for breeding evidence, then budget for 50 tetrads
# as for 2021 surveys, randomly select tetrads
# stratify sampling according to buffer distance from release sites (more intensive sampling nearer release sites)
# exclude tetrads with > 50% forest, urban, water

### Stratification criteria
# 80% of Dutch chicks return to study area, with average dispersal distance of 3.8km (range = 0-12km)
# Gerritsen, G.J. 2021. De broedbiologie van Wulpen in West-Overijssel. Limosa 94: 19–29.
# generate a truncated random normal distribution with mean = mean Dutch dispersal distance in km, and an sd that takes the right tail of the distribution to approx 30km
# curtail the random distribution to the 95th percentile
# group the rnorm values into the buffer strata divisions to get the number of samples per strata
# generate two lots of strata sample totals:
# one to sum to ca. 50 samples (the number of tetrads that will actually be searched)
# one of ca. 100 samples to provide 'back up' options should the randomly selected tetrad not meet the minimum suitable habitat criteria

# set sample sizes per buffer strata to end up with ca. 50 tetrads
set.seed(1)
r_samp <- truncnorm::rtruncnorm(55, a = 0, mean  = 4, sd = 12)
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
r_samp <- truncnorm::rtruncnorm(80, a = 0, mean  = 4, sd = 12)
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

# randomly sample rows from each buffer strata, according to the sample sizes from rtruncnorm above
samp_5km <- sample(nrow(shp_buffer_5km), size = prop_samp_buffer_zones_extras %>% filter(BUFFER == 5) %>% pull(n), replace = FALSE) %>% as_tibble() %>% mutate(select = 1:nrow(.)) %>% rename(row_num = value)
samp_10km <- sample(nrow(shp_buffer_10km), size = prop_samp_buffer_zones_extras %>% filter(BUFFER == 10) %>% pull(n), replace = FALSE) %>% as_tibble() %>% mutate(select = 1:nrow(.)) %>% rename(row_num = value)
samp_20km <- sample(nrow(shp_buffer_20km), size = prop_samp_buffer_zones_extras %>% filter(BUFFER == 20) %>% pull(n), replace = FALSE) %>% as_tibble() %>% mutate(select = 1:nrow(.)) %>% rename(row_num = value)
samp_30km <- sample(nrow(shp_buffer_30km), size = prop_samp_buffer_zones_extras %>% filter(BUFFER == 30) %>% pull(n), replace = FALSE) %>% as_tibble() %>% mutate(select = 1:nrow(.)) %>% rename(row_num = value)

shp_buffer_5km <- shp_buffer_5km %>% 
  mutate(row_num = row_number()) %>% 
  merge(., samp_5km, by = "row_num", all.x = TRUE)
shp_buffer_10km <- shp_buffer_10km %>% 
  mutate(row_num = row_number()) %>% 
  merge(., samp_10km, by = "row_num", all.x = TRUE)
shp_buffer_20km <- shp_buffer_20km %>% 
  mutate(row_num = row_number()) %>% 
  merge(., samp_20km, by = "row_num", all.x = TRUE)
shp_buffer_30km <- shp_buffer_30km %>% 
  mutate(row_num = row_number()) %>% 
  merge(., samp_30km, by = "row_num", all.x = TRUE)

# bind all buffer shapefiles
shp_buffer_all <- rbind(shp_buffer_5km, shp_buffer_10km, shp_buffer_20km, shp_buffer_30km)
shp_buffer_all %>% group_by(select) %>% tally

# write shapefile for all buffers with random selection values added
st_write(shp_buffer_all,
         dsn = giswd_buffers,
         layer = "GB002kmgrid_buffer_land_all_selected.shp",
         driver = "ESRI Shapefile")

# In QGIS, visually select up to prop_samp_buffer_zones_50 tetrads per buffer which occur in suitable habitat, starting with #1 per random selection order
# Create exported shp files of selected tetrads per buffer strata


# ========================  Tidy selected tetrads for output  ===========================

# read in shp files of selected tetrads created above in QGIS
shp_select_files <- list("selected_tetrads_5km.shp", "selected_tetrads_10km.shp", "selected_tetrads_20km.shp", "selected_tetrads_30km.shp")
shp_select_all <- lapply(shp_select_files, function(x){
  st_read(file.path(giswd_buffers, x))
}) %>% do.call(rbind, .) %>% 
  mutate(area = st_area(geometry))

# identify distinct tetrads and filter GB002km master file to these
gb002km <- st_read(file.path(giswd, "British Isles/National Grids/GB/GB002kmgrid.shp"))
gb002km %>% 
  filter(TETRAD %in% (shp_select_all %>% distinct(TETRAD) %>% pull)) %>% 
  st_write(., 
           dsn = giswd_buffers,
           layer = "GB002kmgrid_buffer_survey_selected_tetrads_shortlist_2023.shp",
           driver = "ESRI Shapefile")


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
