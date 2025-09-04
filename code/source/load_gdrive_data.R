

# SOURCE CODE to load files from Google Drive

# Load individual metadata
dt_meta <- read.csv(file.path(datawd, "headstart_curlew_individual_metadata.csv"), header = TRUE, stringsAsFactors = FALSE)

# Load biometric data
dt_biometric <- read.csv(file.path(datawd, "headstart_curlew_biometric_data.csv"), header = TRUE, stringsAsFactors = FALSE) %>% dplyr::select(1:7, 9:10, 12, 15:16, 18:22, 36:42)

# Load gps tag data
dt_deploy_gps <- read.csv(file.path(datawd, "headstart_curlew_tag_deployment_data_gps.csv"), header = TRUE, stringsAsFactors = FALSE)

# Load radio tag data
dt_deploy_radio <- read.csv(file.path(datawd, "headstart_curlew_tag_deployment_data_radio.csv"), header = TRUE, stringsAsFactors = FALSE)

# Load resighting data
dt_resighting <- read.csv(file.path(datawd, "headstart_curlew_resighting_data.csv"), header = TRUE, stringsAsFactors = FALSE)

# Load egg collection data
dt_eggs <- read.csv(file.path(datawd, "headstart_curlew_egg_collection_data.csv"), header = TRUE, stringsAsFactors = FALSE)

# Load rearing data
dt_rearing <- read.csv(file.path(datawd, "headstart_curlew_rearing_data.csv"), header = TRUE, stringsAsFactors = FALSE)

# Load study month data
dt_study_month <- read.csv(file.path(datawd, "headstart_curlew_study_month.csv"), header = TRUE, stringsAsFactors = FALSE)

# ----- Create output for DemOn data entry  -----

# Rename biometric fields
biometric_field_names <- c(
  "entered_demon",
  "urn",
  "day",
  "month",
  "year",
  "site_code",
  "county_code",
  "ring",
  "type",
  "age",
  "wing_initials",
  "wing",
  "tarsus_toe",
  "bill",
  "total_head",
  "weight",
  "weight_time",
  "days_age",
  "flag_id",
  "LA",
  "LB",
  "RA",
  "RB",
  "comments"
)

names(dt_biometric) <- biometric_field_names

# add date to biometric data
dt_biometric <- dt_biometric %>% 
  filter(!is.na(urn)) %>% 
  mutate(across(.cols = c("day", "month", "year", "LB"), .fns = as.character)) %>% 
  mutate(date = ifelse(is.na(year), NA, paste(day, month, year, sep="/"))) %>% 
  mutate(new_date = as.Date(date, tz = "BST", format = c("%d/%m/%Y"))) %>% 
  mutate(across(.cols = c("day", "month", "year"), .fns = as.integer))