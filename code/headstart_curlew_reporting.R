##############################
#
#    NE103: Headstarted Curlew Ringing & Tagging Reporting
#    10/11/2023
#
##############################


# ======================   Variables to pass to setup code source  ===========

# Header code to install and load packages, set directory paths, set seed, capture session info

# project_details <- list(project_name, output_version_name, workspace_version_name)
# package_details <- c("package name 1", "package name 2")

project_details <- list(project_name="curlew", output_version_date="2025_headstarting", workspace_version_date="2025_headstarting")
package_details <- c("sf","tidyverse","patchwork","move","RColorBrewer","viridisLite","rcartocolor","lubridate", "nlme", "lme4", "ggeffects", "broom.mixed", "patchwork")
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


# =======================    Control values   =================

current_year <- 2025

# TRUE = fresh download of google drive data
update_gdrive_data <- TRUE


# =======================    Load functions   =================

source(paste(codewd, "functions", "run_all_functions.R", sep="/"))


# =======================    Load data   =================

today_date <- format(Sys.Date(), "%d-%b-%Y")

# Load data
# Toggle logic value above if fresh download of google drive data is needed
# will need to provide authentication for R to access Google Drive
if (update_gdrive_data) source(file.path("code", "source", "download_gdrive_data.R"))
source(file.path("code", "source", "load_gdrive_data.R"))

# Load demon bulk upload field ref template
dt_demon_ref <- read.csv(file.path(datawd, "demon_bulk_upload_field_reference.csv"), header = TRUE, stringsAsFactors = FALSE)


# =======================    Prepare data   =================

# Merge biometric with metadata
# In previous seasons, removed rows with no weights, but in 2023 birds were handled / measured multiple times to check feather growth, but weights not always taken (only wing measurements)
# dt_all <- merge(dt_meta, dt_biometric %>% filter(!is.na(weight)), by = c("ring", "year", "flag_id"))
dt_all <- merge(dt_meta, dt_biometric, by = c("ring", "year", "flag_id"))


dt <- dt_all %>% 
  mutate(cohort_num = as.factor(cohort_num)) %>% 
  filter(!is.na(cohort_num))

dt <- dt %>% 
  mutate(date = as.POSIXct(strptime(paste(day, month, year, sep="/"), format = "%d/%m/%Y", tz="UTC")))%>%
  mutate(release_date = as.POSIXct(strptime(release_date, format = "%d/%m/%Y", tz="UTC"))) %>% 
  mutate(tagged_date = as.POSIXct(strptime(tagged_date, format = "%d/%m/%Y", tz="UTC")))


# ----- Age released  -----

# create an age_released dataset with 'H' records only (adding an age_released column as below doesn't work when there are NAs in days_age, even if you use an ifelse statement - all 'H' records should have a days age attached)
age_released <- dt %>% 
  filter(type %in% "H") %>% 
  mutate(age_released = ifelse(is.na(days_age), NA, as.integer(days_age + (release_date - date)))) %>% 
  dplyr::select(year, ring, flag_id, year_flag, sex, cohort_num, tag_gps_radio_none, release_location, release_date, age_released)



# ----- Create output for DemOn data entry  -----

dt_biometric_ringing <- dt_biometric %>% filter(type %in% "H")

# Merge metadata with biometric ringing event (type = H, as per DemOn code H)
# dt_all <- merge(dt_biometric_ringing, dt_meta, by = c("flag_id", "year", "ring"))
dt_all <- dt_biometric %>%
  right_join(dt_meta, by = c("flag_id", "year", "ring"))

# Output easily readable data for DemOn data entry
dt_easy_demon <- dt_all %>%
  filter(year %in% current_year) %>%
  filter(type %in% "H") %>%
  filter(release_location != "not released") %>%
  filter(entered_demon != "Y") %>%
  arrange(ring) %>%
  mutate(scheme = "GBT",
         species = "Curlew",
         sexing_method = "D",
         capture_time = "12:00",
         condition = "H",
         capture_method = "H",
         metal_mark_info = "N",
         colour_mark_info = "U",
         LB = "")

# Code all 'type' and 'condition' = H
# Code locations
# Code habitat_1 codes for each location: KH-pen-01 = C6, all Sandringham pens = E2
dt_easy_demon <- dt_easy_demon %>%
  rename(visit_date = release_date,
         location = release_location,
         extra_text = tag_gps_radio_none) %>%
  # mutate(location = ifelse(location %in% "Ken Hill 1", "KH-pen-01", ifelse(location %in% "Ken Hill 2", "KH-pen-02", "SH-pen-02"))) # 2024
  mutate(location = ifelse(location %in% "Ken Hill 1", "KH-pen-01", "SH-pen-03")) %>% # 2025
  mutate(habitat_1 = ifelse(location %in% "KH-pen-01", "C6", "E2"))


# Modify all GPS / radio tagged birds to type and condition = M add add extra text field with comments containing tag serial number
# for demon bulk upload:
# add field warning_fc_rehabilitated for record/condition = H, comment can be same as in 'extra_text' field
# add warning_fc_special_method for record/condition = M - populate this field with code 'GPSH' for GPS logger (harness), and keep the comment 'headstarted, tag serial # etc' in the extra_text field
dt_easy_demon <- dt_easy_demon %>% 
  mutate(type = ifelse(extra_text %in% c("gps", "radio"), "M", type)) %>% 
  mutate(condition = ifelse(type %in% "M", "M", condition)) %>% 
  mutate(extra_text = ifelse(extra_text %in% "gps", "headstarted, gps tag serial", ifelse(extra_text %in% "radio", "radio tag serial", ""))) %>% 
  mutate(extra_text = ifelse(extra_text %in% "", "", paste(extra_text, tag_serial, sep = " = "))) %>% 
  mutate(extra_text = ifelse(extra_text %in% "", "headstarted", extra_text)) %>% 
  mutate(LA = str_replace(LA, fixed(")O"), "),O")) %>% 
  rename(record_type = type,
         ring_no = ring,
         location_code = location,
         ringer_initials = wing_initials,
         left_leg_below = LB,
         right_leg_below = RB,
         left_leg_above = LA,
         right_leg_above = RA) %>% 
  dplyr::select(-tag_serial) %>% 
  mutate(warning_fc_special_method = ifelse(record_type == "M" & condition == "M", "GPSH", "")) %>% 
  mutate(warning_fc_rehabilitated = "headstarted")

if (all(is.na(dt_easy_demon$LB))) dt_easy_demon <- dt_easy_demon %>% mutate(LB = "")

dt_easy_demon <-  dt_easy_demon %>% 
  dplyr::select(record_type, ring_no, scheme, species, age, sex, sexing_method, visit_date, capture_time, location_code, habitat_1, condition, capture_method, metal_mark_info, ringer_initials, colour_mark_info, left_leg_below, right_leg_below, left_leg_above, right_leg_above, extra_text, warning_fc_rehabilitated, warning_fc_special_method)

head(dt_easy_demon)

# Output csv file
write.csv(dt_easy_demon, file.path(outputwd, "headstart_curlew_easy_demon_data_entry_2025.csv"), row.names = FALSE)


# ----- Merge dt_easy_demon with dt_biometric  -----

dt_fill_gps_deploy <- dt_meta %>% 
  filter(year %in% 2024) %>% 
  filter(tag_gps_radio_none %in% "gps") %>% 
  left_join(., dt_biometric %>% filter(type %in% "R"), by = c("ring", "flag_id")) %>% 
  dplyr::select(flag_id, ring, tagged_date, release_location, weight, wing, tarsus_toe, day, month)

# write.csv(dt_fill_gps_deploy, file.path(outputwd, "easy_fill_gps_deploy_data_entry_2024.csv"), row.names = FALSE)


# ----- Merge tag deployment data with dt_biometric  -----

# choose latest biometrics date for each individual
dt_easy_tag <- dt_biometric %>% 
  filter(type %in% "R") %>% 
  group_by(ring) %>% 
  summarise(max_date = max(new_date), weight, wing, tarsus_toe) %>% 
  right_join(dt_deploy_gps, by = c("ring")) %>% 
  filter(year %in% current_year) %>% 
  dplyr::select(ring, flag_id,tagged_date, release_location, weight.x, wing.x, tarsus_toe) %>% 
  as.data.frame() %>% 
  arrange(flag_id)

# write.csv(dt_easy_tag, file.path(outputwd, "easy_gps_sm_reporting_2024.csv"), row.names = FALSE)


# ----- playing around  -----


# dt_all <- dt_biometric %>%
#   right_join(dt_meta, by = c("flag_id",  "ring"))
# 
# dt_all %>% 
#   filter(tag_gps_radio_none %in% "gps") %>% 
#   filter(type %in% "R") %>% 
#   filter(year.x %in% "2022") %>% 
#   dplyr::select(flag_id, tagged_date, days_age, weight, wing) %>% 
#   group_by(tagged_date) %>% 
#   summarise(mean_age = mean(days_age), sd_age = sd(days_age), min_age = min(days_age), max_age = max(days_age), mean_weight = mean(weight), mean_wing = mean(wing)) %>% 
#   arrange(tagged_date)


# ----- Merge individual metadata with egg & rearing data (released birds only)  ------

dt_meta_rear <- dt_meta %>% 
  right_join(dt_rearing, by = c("flag_id", "year")) %>% 
  mutate(release_date = strptime(release_date, format = "%d/%m/%Y")) %>% 
  mutate(date_hatched = strptime(date_hatched, format = "%d/%m/%Y")) %>% 
  mutate(release_age = release_date - date_hatched)

# Extract data for Europe headstarting review ##########

# mean release age
dt_meta_rear %>%
  # filter(year %in% 2023) %>% 
  dplyr::select(year, flag_id, ring, sex, date_hatched, release_location, release_date, release_age) %>% 
  group_by(year) %>% 
  summarise(mean_release_age = mean(release_age, na.rm=TRUE), 
            min_release_age = min(release_age, na.rm=TRUE),
            max_release_age = max(release_age, na.rm=TRUE),
            min_release_date = min(release_date, na.rm=TRUE),
            max_release_date = max(release_date, na.rm=TRUE)
          )

# KMB adding a write out here in case useful for the moment. Sam - you can remove this is you want!
# write.csv(dt_meta_rear, file.path(outputwd, "easy_release_age_2024.csv"), row.names = FALSE)


# dt_meta_rear_egg <- dt_meta_rear %>% 
#   right_join(dt_eggs, by = c("year", "clutch_num"))
# 
# # tally of released birds only by airfield
# dt_meta_rear_egg %>% 
#   group_by(year, airfield_name) %>% 
#   tally()

