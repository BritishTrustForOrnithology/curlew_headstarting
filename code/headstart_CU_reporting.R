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

project_details <- list(project_name="curlew", output_version_date="2023_headstarting", workspace_version_date="2023_headstarting")
package_details <- c("sf","tidyverse","patchwork","move","moveVis","RColorBrewer","viridisLite","rcartocolor","lubridate", "nlme", "lme4", "ggeffects", "broom.mixed", "patchwork")
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

current_year <- 2023


# =======================    Load data   =================

today_date <- format(Sys.Date(), "%d-%b-%Y")

# Load data
source(file.path("code", "headstart_CU_database.R"))


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
  mutate(tagged_date = as.POSIXct(strptime(tagged_date, format = "%d/%m/%Y", tz="UTC"))) %>% 
  mutate(age_released = as.integer(days_age + (release_date - date)))

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
         sexing_method = "DNA",
         capture_time = "12:00",
         condition = "H",
         capture_method = "H",
         metal_mark_info = "N",
         colour_mark_info = "U"
         ) %>%
  dplyr::select(entered_demon, type, ring, scheme, species, age, sex, sexing_method, release_date, capture_time, release_location, condition, capture_method, metal_mark_info, wing_initials, colour_mark_info, LB, RB, LA, RA, tag_gps_radio_none, tag_serial)

# Code all 'type' and 'condition' = H
dt_easy_demon <- dt_easy_demon %>%
  rename(vist_date = release_date,
         location = release_location,
         extra_text = tag_gps_radio_none) %>%
  mutate(location = ifelse(location %in% "Ken Hill", "KH-pen-01", ifelse(location %in% "Ken Hill 2", "KH-pen-02", "SH-pen-02")))

# Modify all GPS / radio tagged birds to type and condition = M
dt_easy_demon <- dt_easy_demon %>% 
  mutate(type = ifelse(extra_text %in% c("gps", "radio"), "M", type)) %>% 
  mutate(condition = ifelse(type %in% "M", "M", condition)) %>% 
  mutate(extra_text = ifelse(extra_text %in% "gps", "headstarted, gps tag serial", ifelse(extra_text %in% "radio", "radio tag serial", ""))) %>% 
  mutate(extra_text = ifelse(extra_text %in% "", "", paste(extra_text, tag_serial, sep = " = "))) %>% 
  mutate(LA = str_replace(LA, fixed(")O"), "),O")) %>% 
  dplyr::select(-tag_serial)

if (all(is.na(dt_easy_demon$LB))) dt_easy_demon <- dt_easy_demon %>% mutate(LB = "")

dt_easy_demon

# Output csv file
write.csv(dt_easy_demon, file.path(outputwd, "easy_demon_data_entry_2023.csv"), row.names = FALSE)


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

write.csv(dt_easy_tag, file.path(outputwd, "easy_gps_sm_reporting_2023.csv"), row.names = FALSE)


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
  right_join(dt_rearing, by = c("flag_id", "year"))

dt_meta_rear_egg <- dt_meta_rear %>% 
  right_join(dt_eggs, by = c("year", "clutch_num"))

# tally of released birds only by airfield
dt_meta_rear_egg %>% 
  group_by(year, airfield_name) %>% 
  tally()
