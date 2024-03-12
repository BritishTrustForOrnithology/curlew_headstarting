#### NE103 -- Curlew headstarting -- post-release tracking

## Original code (2021 and 2022) by Gary Clewley
## Updated for 2023 analysis by Hannah Hereward, Katharine Bowgen, Sam Franks (2023-)


#### NOTES ####
# Analysis of 2023 deployments and any remaining individuals from 2021 and 2022 still transmitting in 2023

# Check 2022 report for example of previous analytical structure to follow
# Note requirement to possibly extract 'potential breeding season' data for separate analysis for 2021/2022 deployments - to discuss with KMB and RCT a suitable date range

# Tide data from Oceanwise - Port-log.net reports
# go to https://thewash.port-log.net/live/History.php?Site=49&Dataset=Tides
# click Download > Advanced
# select yearly data or required range of data
# Now saved in googledrive folder: 3.Data --< bulldog_bcn_ ... 


#### SETUP AND CLEAN DATA ####

## Checkpoint??


## LOAD PACKAGES
load_pkg <- rlang::quos(tidyverse,BTOTrackingTools, here, sp, leaflet, terra)  # quos() function to be lazy on "" around each package
lapply(lapply(load_pkg, rlang::quo_name), library, character.only = TRUE)





#### LOAD MOVEBANK DATA

# Set login credentials #HH NB: if this is the first time you're downloading data then need to download the data on the movebank webpage to accept the licencing agreement. Then this should work.
login<-move::movebankLogin()

# Ongoing issues using BTOTT to load and clean data. Workaround to use move:: directly
data <- move::getMovebankLocationData(study="BTO-NE-Pensthorpe - Eurasian Curlews - headstarted",
                                      sensorID=653, login = login)

## Make parsable as a Track object by BTOTT package
names(data)[names(data)=="individual.local.identifier"]<-"TagID"   # Some tags redeployed multiple times
names(data)[names(data)=="timestamp"]<-"DateTime"
names(data)[names(data)=="location.long"]<-"longitude"
names(data)[names(data)=="location.lat"]<-"latitude"
names(data)[names(data)=="gps.satellite.count"]<-"satellites_used"







## Match tide data - categorical 2 hours either side of high/low (from Port-log.net reports)
# Load data and set Datetime class
tide_dat_21 <- read_csv(here("data/Wash_tide_data_Bulldog_July_November_2021.csv"), 
                        col_types = cols(Observed_DateTime = col_datetime(format = "%d/%m/%Y %H:%M"), Predicted_DateTime = col_datetime(format = "%d/%m/%Y %H:%M")))
summary(tide_dat_21)

tide_dat_22 <- read_csv(here("data/Wash_tide_data_Bulldog_July_December_2022.csv"), 
                     col_types = cols(Observed_DateTime = col_datetime(format = "%d/%m/%Y %H:%M"), Predicted_DateTime = col_datetime(format = "%d/%m/%Y %H:%M")))
summary(tide_dat_22)

#2023 tide data 1) - HH NB - Previous code uses the 'reports'--> 'daily high and low' from Port-log.net NOT the raw data
  #To use the daily highs and lows have to download a pdf of each month of interest. HH (2024) copied the data into an excel spreadsheet sifted the excess headings away, combined the date and time columns together and saved it as a csv. There should technically be a way of converting pdf to csv... but this was easier for me (can use tinywow to convert pdf to excel but this created a tab per page of pdf so copying was easier). Finally can directly use the read in using 2022 code

tide_dat_23 <- read_csv(here("data/Wash_tide_data_Bulldog_July_December_2023.csv"), 
                     col_types = cols(Observed_DateTime = col_datetime(format = "%d/%m/%Y %H:%M"), Predicted_DateTime = col_datetime(format = "%d/%m/%Y %H:%M")))
summary(tide_dat_23)



#bind all 3 years of tide data together
tide_dat <- rbind(tide_dat_21, tide_dat_22, tide_dat_23[c(1:6)])
summary(tide_dat)


#save out this combined 3 year tide data
#write_csv(tide_dat, "data/Wash_tide_data_Bulldog_July_Nov2021_July_December_20222023.csv")


tide_dat <- read_csv(here("data/Wash_tide_data_Bulldog_July_Nov2021_July_December_20222023.csv")) 
summary(tide_dat)

#filter out the 2 NAs
tide_dat <- tide_dat %>% filter(!is.na(tide_dat$Observed_Height))


#----- tangent away from Gary's original code (HH 2024)
#ALTERNATIVELY: 2023 tide data 2) - could  use raw data and extract the high and low water times - below is the start of code to do this, left here just in case needed in future
#clean up tide data first:
#tide_dat_23 <- read.csv(here("data/Tides_Bulldog_Bcn_20230101_20231231_0.csv")) 
#need to keep these columns: "Station_ID"         "Observed_DateTime"  "Observed_Height"    "Tide"   "Predicted_DateTime" "Predicted_Height"  

#tide_dat_23_sub <- tide_dat_23[c(1,2,3,4,5,12)] #keep site ID and name, date.time, tide level, predicted tide level and quality flag - to remove NAs

#colnames(tide_dat_23_sub) <- c("SiteID", "SiteName", "Observed_DateTime", "Observed_Height", "Predicted_Height", "Quality_Flag")

#remove NAs
#tide_dat_23_sub <- tide_dat_23_sub %>% filter(!is.na(tide_dat_23_sub$Quality_Flag))

#update the date time column to make sure the format is correct
#library("lubridate")

#tide_dat_23_sub$Observed_DateTime <- as.POSIXct.default(tide_dat_23_sub$Observed_DateTime, format = "%Y-%m-%d %H:%M:%S", usetz=T, tz="UTC")

#filter so that it is JUST July-December

#tide_dat_23_sub$keep <- ifelse(tide_dat_23_sub$Observed_DateTime >= "2023-07-01 01:00:00", T, F)

#tide_dat_23_sub_July_Dec <- tide_dat_23_sub %>% filter(tide_dat_23_sub$keep==T)

#min(tide_dat_23_sub_July_Dec$Observed_DateTime)
#max(tide_dat_23_sub_July_Dec$Observed_DateTime)

#tide_dat_23_sub_July_Dec$date <- as.Date(tide_dat_23_sub_July_Dec$Observed_DateTime, tz = "UTC")
#min(tide_dat_23_sub_July_Dec$date)
#max(tide_dat_23_sub_July_Dec$date)


#create a new table with high and low for each date
#tide_dat_23_sub_July_Dec_HWLW <- tide_dat_23_sub_July_Dec %>%
#  group_by(SiteID, SiteName, date) %>%
#  summarize(LW = min(Observed_Height), HW = max(Observed_Height))


#write as csv and read back in:
#write.csv(tide_dat_23_sub, "./data/XXXX", row.names = F)

#----- back to Gary's original code

# Find closest match to trk timestamp (package MALDIquant)
closest<-MALDIquant::match.closest(data$DateTime, tide_dat$Observed_DateTime)

# Extract nearest tide time, high/low, height 
data$tide_time<-tide_dat$Observed_DateTime[closest]; data$tide_diff<-difftime(data$DateTime, data$tide_time)
data$tide<-tide_dat$Tide[closest];data$tide_height<-tide_dat$Observed_Height[closest]

# Create categorical tide factor with desired threshold; below annotated fixes 2 hour either side of high or low tide
data$tide<-as.factor(ifelse(data$tide_diff<7201 & data$tide_diff>-7201,data$tide, "NA"))





# Coerce required trip and gap columns for later functions (not running trip definition for this project as not central place)
data$tripNo<-1; data$gap<-0; data$gapsec<-1


#check summary of data
summary(data)
summary(as.factor(data$tide_diff))


#save data out so I do keep having to read it all back in and do the tide stuff
saveRDS(data, here("data/data_with_tide.rds"))


#HH Updated code to use an external csv to apply the cohort identifier and release site identifier (created in code: 'headstart_curlew_gps_movements.R' ~ line 76 to get the cohort identifier and site identifiers)
dt_meta <- readRDS(here("data/dt_meta_metadata_allyears.rds"))
head(dt_meta)

#filter for just GPS birds
dt_meta_gsp <- dt_meta %>% filter(dt_meta$tag_gps_radio_none =="gps")


#table for cohort
#cohort <- data.frame(FlagID = dt_meta_gsp$flag_id, cohort_no = dt_meta_gsp$cohort_num, release_location = dt_meta_gsp$release_location)

#manual list of each bird and which release site and flag ID they have 
dt_meta_gsp$release_site <- c("Sandringham", "Sandringham", "Ken Hill", "Sandringham","Ken Hill", "Sandringham","Ken Hill", "Sandringham","Ken Hill", "Sandringham","Sandringham", "Sandringham", "Sandringham","Ken Hill","Ken Hill","Ken Hill","Sandringham","Ken Hill","Ken Hill","Sandringham","Ken Hill",
                         "Sandringham","Sandringham", "Sandringham", "Sandringham", "Sandringham", "Ken Hill","Ken Hill","Ken Hill",
                         "Ken Hill","Ken Hill","Ken Hill","Sandringham","Ken Hill","Ken Hill","Sandringham")

TagID <- data.frame(TagID = unique(data$TagID))

TagID$flag_id <- c("0E","3A", "0J" , "3K", "6X", "7E","7U" ,"6Y", "7K" , "7Y" ,"8E", "8K", "8L" ,"8X","9L" ,"9J" , "XJ", "XL","XH",
                "YK","XA", "XE","YJ","XP","XU","XT" ,"YH" ,"XX","LP" ,"LJ" ,"YX","LA","LV" ,"LU"  ,"YU", "YN"  )

#left_join the two metadata tables together
dt_meta_gsp_TagID <- dt_meta_gsp %>% left_join(TagID, by=join_by(flag_id))


#final release site column here - to keep the two Sandringham sites separate because the habitat is so different but merge the Ken Hill sites as their habitat is so similar
dt_meta_gsp_TagID$release_site_final <- ifelse(dt_meta_gsp_TagID$release_location =="Sandringham 1", "Sandringham 1",
                                          ifelse(dt_meta_gsp_TagID$release_location=="Sandringham 2", "Sandringham 2", "Ken Hill"))



#***** HH NB add in here two final columns
#cohort needs to be added in here. decide for 2023 how many cohorts we want = first cohort seperate and the rest clumped together
dt_meta_gsp_TagID$cohort_analysis <- ifelse(dt_meta_gsp_TagID$cohort_num > 1, 2, 1)

dt_meta_gsp_TagID$cohort_analysis <- as.factor(dt_meta_gsp_TagID$cohort_analysis)

summary(dt_meta_gsp_TagID)


#add in 1 day, 1 week, 2 weeks, 6 week dates post release
#create a as.posixct datetime column
dt_meta_gsp_TagID$release_date_time <- as.POSIXct(dt_meta_gsp_TagID$release_date, format = "%d/%m/%Y", usetz=T, tz="UTC")

#one day according to Garry's 2022 code. Was to the end of the next day up until midnight = one full day after release e.g. release date 20/07 : 21/07 at 23:59:59
dt_meta_gsp_TagID$Data_1d <- dt_meta_gsp_TagID$release_date_time + (86400+86399)


#one week in my mind means 7 full days after release  e.g.release date 20/07 : 27/07 at 23:59:59
dt_meta_gsp_TagID$Data_1w <- dt_meta_gsp_TagID$release_date_time + ((86400*7)+86399)

#two weeks in my mind means 14 full days after release 
dt_meta_gsp_TagID$Data_2w <- dt_meta_gsp_TagID$release_date_time + ((86400*14)+86399)

#six weeks in my mind means 42 full days after release 
dt_meta_gsp_TagID$Data_6w <- dt_meta_gsp_TagID$release_date_time + ((86400*42)+86399)




#Then do a left_join on the dataset using the TagID are the join_by so that the meta data is populated for each GPS fix
data <- data %>% left_join(dt_meta_gsp_TagID, by=join_by(TagID))

summary(data)
 

#---------#
#HH NB - this is Gary's long hand code to set the cohort identifier and site identifer - but HH has left_joined the meta data to data so the hashtagged stages below are not needed

# Add cohort identifier
#c1<-c("Yf(6X)O/-:Y/m_KenHill","Yf(6Y)O/-:Y/m_Sandringham","Yf(7E)O/-:Y/m_KenHill","Yf(7K)O/-:Y/m_Sandringham","Yf(7U)O/-:Y/m_KenHill","Yf(7Y)O/-:Y/m_Sandringham")
#c2<-c("Yf(8E)O/-:Y/m_Sandringham","Yf(8K)O/-:Y/m_Sandringham","Yf(8L)O/-:Y/m_Sandringham")
#c3<-c("Yf(8X)O/-:Y/m_KenHill", "Yf(9J)O/-:Y/m_KenHill","Yf(9L)O/-:Y/m_KenHill")

#data<- mutate(data, cohort = factor(case_when(TagID %in% c1 ~ "1",
#                                              TagID %in% c2 ~ "2",
#                                              TagID %in% c3 ~ "3",
#                                              TRUE~ NA_character_)))




# Add release site identifier
#c1<-c("Yf(6X)O/-:Y/m_KenHill","Yf(7E)O/-:Y/m_KenHill","Yf(7U)O/-:Y/m_KenHill","Yf(8X)O/-:Y/m_KenHill", "Yf(9J)O/-:Y/m_KenHill","Yf(9L)O/-:Y/m_KenHill")
#c2<-c("Yf(8E)O/-:Y/m_Sandringham","Yf(8K)O/-:Y/m_Sandringham","Yf(8L)O/-:Y/m_Sandringham","Yf(6Y)O/-:Y/m_Sandringham","Yf(7K)O/-:Y/m_Sandringham","Yf(7Y)O/-:Y/m_Sandringham")

#data<- mutate(data, release = factor(case_when(TagID %in% c1 ~ "Ken",
#                                              TagID %in% c2 ~ "Sand",
#                                             TRUE~ NA_character_)))

#-------#


# Tidy surplus columns from move:: direct loading
drop_cols<-c("event.id", "visible", "individual.id", "deployment.id", "tag.id", "study.id", "sensor.type.id", "tag.local.identifier", "individual.taxon.canonical.name", "acceleration.raw.x", "acceleration.raw.y", "acceleration.raw.z", "barometric.height", "battery.charge.percent", "battery.charging.current", "gps.hdop", "gps.time.to.fix", "heading", "import.marked.outlier", "light.level", "magnetic.field.raw.x", "magnetic.field.raw.y", "magnetic.field.raw.z", "ornitela.transmission.protocol", "study.name", "sensor.type")
data<- data %>% select(-!!drop_cols)



#save data out 
#saveRDS(data, here("data/data_withcohorts_release_sites.rds"))


#SHOULD BE ABLE TO START FROM HERE FOR ANALYSIS NOW DATA DONWLOADED, CLEANED AND SAVED
#read back in - can start from here now ####
#data <- readRDS(here("data/data_withcohorts_release_sites.rds"))

#summary(data)



## Convert to BTOTT Track object
data_tt<-Track(data) 
data_tt<-clean_GPS(data_tt, drop_sats = 3, Thres = 30, GAP = 28800)
#HH NB: now warning messages about "flt_switch" for each bird - an error from the Track(data) function with is a BTOTT function
  #See messages from Chris T about this but the summary is it is a flag option for clean_GPS - when importing data into movebank you can add a 
      #column to tell you if it is dodgy data or not and then add a second column to clean it/remove it... as the dataset I am working with doesn't have this column
      # I can disregard this warning

# Set ID factor
data_tt$TagID<-as.factor(as.character(data_tt$TagID)) 


#---------#
#HH NB --- HH alternative code to filter the data for ANY GPS data for the WHOLE of 2023: splitting it by the cohorts released in 2023 and previous cohorts still recording in 2023
data_tt<-data_tt %>% filter(DateTime >= "2023-01-01") %>% droplevels()
summary(data_tt$year)


plot(data_tt$longitude, data_tt$latitude)


# Then Filter the data for each ID based on the staggered deployments for cohort released in 2023
data_tt_2023<-data_tt %>% filter(year == "2023") %>% droplevels()
summary(data_tt_2023$year)
summary(data_tt_2023$DateTime)

#this uses ifelse to filter out each unique tag and release date into the different categories
data_tt_2023$Period <- ifelse(data_tt_2023$DateTime < data_tt_2023$release_date_time, "Pre Release",
                                     ifelse(data_tt_2023$DateTime< data_tt_2023$Data_1d, "1 One Day",
                                            ifelse(data_tt_2023$DateTime < data_tt_2023$Data_1w, "2 One Week",
                                                   ifelse(data_tt_2023$DateTime < data_tt_2023$Data_2w, "3 Two Weeks",
                                                          ifelse(data_tt_2023$DateTime < data_tt_2023$Data_6w, "4 Six Weeks", 
                                                                 ifelse(data_tt_2023$DateTime < as.POSIXct("2023-12-31 23:59:59", tz="UTC"), "5 End of December", "Post December"))))))

summary(as.factor(data_tt_2023$Period))


#A final set of decisions needs to be made to decide whether some individuals need to be taken out of some categories 
  # due to the number of days the remained alive
#create a dead_date_time column
dt_meta_gsp_TagID$dead_date_time <- as.POSIXct(dt_meta_gsp_TagID$dead_date, format = "%d/%m/%Y", usetz=T, tz="UTC")

#minus the dead date time from the release date time
dt_meta_gsp_TagID$daysalive <- as.POSIXct(dt_meta_gsp_TagID$dead_date_time,  tz="UTC") - as.POSIXct(dt_meta_gsp_TagID$release_date_time, tz="UTC")

#seperate table of just dead curlew from all years
deadcurlew <- dt_meta_gsp_TagID[!is.na(dt_meta_gsp_TagID$daysalive),]

#filter for 2023
deadcurlew <- deadcurlew[deadcurlew$year=="2023",]


#save the meta data out:
write.csv(dt_meta_gsp_TagID, here("data/metadata_TagID.csv"), row.names = F)


#also check the last datetime of transmission
currcohorts <- data_tt_2023 %>%
  group_by(year, TagID, sex, release_date, cohort_analysis) %>%
  summarize(max_date_time = max(DateTime))

currcohorts$release_date <- as.POSIXct(currcohorts$release_date, format="%d/%m/%Y", tz="UTC")

currcohorts$datediff <- as.POSIXct(currcohorts$max_date_time, tz="UTC") - as.POSIXct(currcohorts$release_date, format="%Y-%m-%d", tz="UTC")

write.csv(currcohorts, here("data/current_cohort_maxdatetime.csv"), row.names = F)


#THEN need to take these into account in the filtering below:

#subset the data based on these Periods of time - BUT to keep the first day, first week, first two weeks in the subsequent ones they need to be combined together
data_1d <- data_tt_2023 %>% filter(data_tt_2023$Period == "1 One Day")
data_1d <- data_1d %>% filter(data_1d$TagID != "Yf(YN)O/-:Y/m_KenHill") %>% droplevels()
data_1d <- data_1d %>% filter(data_1d$TagID != "Yf(YU)O/-:Y/m_KenHill") %>% droplevels()

summary(as.factor(data_1d$Period))

data_1d$Period <- "1 One Day"

summary(as.factor(data_1d$Period))
summary(as.factor(data_1d$TagID))
unique(data_1d$TagID) #18


data_1w <- data_tt_2023 %>% filter(data_tt_2023$Period == "1 One Day" | data_tt_2023$Period == "2 One Week")
data_1w <- data_1w %>% filter(data_1w$TagID != "Yf(YN)O/-:Y/m_KenHill") %>% droplevels()
data_1w <- data_1w %>% filter(data_1w$TagID != "Yf(YU)O/-:Y/m_KenHill") %>% droplevels()
data_1w <- data_1w %>% filter(data_1w$TagID != "Yf(XT)O/-:Y/m_KenHill") %>% droplevels()


summary(as.factor(data_1w$Period))

data_1w$Period <- "2 One Week"

summary(as.factor(data_1w$Period))
summary(as.factor(data_1w$TagID))
unique(data_1w$TagID) #17


data_2 <- data_tt_2023 %>% filter(data_tt_2023$Period == "1 One Day" | data_tt_2023$Period == "2 One Week" | data_tt_2023$Period == "3 Two Weeks")
data_2 <- data_2 %>% filter(data_2$TagID != "Yf(YN)O/-:Y/m_KenHill") %>% droplevels()
data_2 <- data_2 %>% filter(data_2$TagID != "Yf(YU)O/-:Y/m_KenHill") %>% droplevels()
data_2 <- data_2 %>% filter(data_2$TagID != "Yf(LA)O/-:Y/m_Sandringham") %>% droplevels()
data_2 <- data_2 %>% filter(data_2$TagID != "Yf(XT)O/-:Y/m_KenHill") %>% droplevels()


summary(as.factor(data_2$Period))

data_2$Period <- "3 Two Weeks"

summary(as.factor(data_2$Period))
summary(as.factor(data_2$TagID))
unique(data_2$TagID) #16


data_6 <- data_tt_2023 %>% filter(data_tt_2023$Period == "1 One Day" | data_tt_2023$Period == "2 One Week" | data_tt_2023$Period == "3 Two Weeks" | data_tt_2023$Period == "4 Six Weeks")
data_6 <- data_6 %>% filter(data_6$TagID != "Yf(YN)O/-:Y/m_KenHill") %>% droplevels()
data_6 <- data_6 %>% filter(data_6$TagID != "Yf(YU)O/-:Y/m_KenHill") %>% droplevels()
data_6 <- data_6 %>% filter(data_6$TagID != "Yf(LA)O/-:Y/m_Sandringham") %>% droplevels()
data_6 <- data_6 %>% filter(data_6$TagID != "Yf(LP)O/-:Y/m_KenHill") %>% droplevels()
data_6 <- data_6 %>% filter(data_6$TagID != "Yf(XT)O/-:Y/m_KenHill") %>% droplevels()

summary(as.factor(data_6$Period))

data_6$Period <- "4 Six Weeks"

summary(as.factor(data_6$Period))
summary(as.factor(data_6$TagID))
unique(data_6$TagID) #15


data_all <- data_tt_2023 %>% filter(data_tt_2023$Period == "1 One Day" | data_tt_2023$Period == "2 One Week" | 
                                      data_tt_2023$Period == "3 Two Weeks" | data_tt_2023$Period == "4 Six Weeks" | 
                                      data_tt_2023$Period == "5 End of December")
data_all <- data_all %>% filter(data_all$TagID != "Yf(YN)O/-:Y/m_KenHill") %>% droplevels()
data_all <- data_all %>% filter(data_all$TagID != "Yf(YU)O/-:Y/m_KenHill") %>% droplevels()
data_all <- data_all %>% filter(data_all$TagID != "Yf(LA)O/-:Y/m_Sandringham") %>% droplevels()
data_all <- data_all %>% filter(data_all$TagID != "Yf(LP)O/-:Y/m_KenHill") %>% droplevels()
data_all <- data_all %>% filter(data_all$TagID != "Yf(YX)O/-:Y/m_Sandringham") %>% droplevels()
data_all <- data_all %>% filter(data_all$TagID != "Yf(XU)O/-:Y/m_KenHill") %>% droplevels()
data_all <- data_all %>% filter(data_all$TagID != "Yf(XT)O/-:Y/m_KenHill") %>% droplevels()

summary(as.factor(data_all$Period))

data_all$Period <- "5 End of December"

summary(data_all$DateTime)
summary(as.factor(data_all$Period))
summary(as.factor(data_all$TagID))
unique(data_all$TagID) #13

# Final merge 
#data for the 2023 cohort #####
data_2023cohort<-Track2TrackMultiStack(rbind(data_1d, data_1w, data_2, data_6, data_all), by=c("TagID", "Period"))
data_2023cohort

# Save
setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data") #HH laptop
save(data_2023cohort, file="NE103_2023 report_clean tracking data for 2023 cohort.RData")


# Load
setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data") #HH laptop
load("NE103_2023 report_clean tracking data for 2023 cohort.RData")




#for previous cohorts:
data_tt_21_22<-data_tt %>% filter(year != "2023") %>% droplevels()
summary(data_tt_21_22$year)
summary(data_tt_21_22$DateTime)

prevcohorts <- data_tt_21_22 %>%
  group_by(year, TagID, sex, cohort_analysis) %>%
  summarize(max_date_time = max(DateTime))


#split by sex
#Female
data_tt_21_22_F<-data_tt_21_22 %>% filter(sex == "F")
summary(as.factor(data_tt_21_22_F$sex))


data_tt_21_22_F$Period <- ifelse(data_tt_21_22_F$DateTime < as.POSIXct("2023-02-28 23:59:59", tz="UTC"), "6 Winter pre-breeding",
                              ifelse(data_tt_21_22_F$DateTime< as.POSIXct("2023-03-31 23:59:59", tz="UTC"), "7 Spring fuzzy",
                                     ifelse(data_tt_21_22_F$DateTime[data_tt_21_22_F$sex=="F"] < as.POSIXct("2023-06-15 23:59:59", tz="UTC"), "8a Female Breeding Season",
                                                ifelse(data_tt_21_22_F$DateTime < as.POSIXct("2023-09-15 23:59:59", tz="UTC"), "9a Female Autumn fuzzy", 
                                                          ifelse(data_tt_21_22_F$DateTime < as.POSIXct("2023-12-31 23:59:59", tz="UTC"), "10 End of December - Winter", "Post December")))))

summary(as.factor(data_tt_21_22_F$Period))

summary(data_tt_21_22_F$DateTime[data_tt_21_22_F$Period=="8a Female Breeding Season"])

#Subset based on the different categories in period
Data_W_PreB_F <- data_tt_21_22_F %>% filter(data_tt_21_22_F$Period == "6 Winter pre-breeding")
Data_W_PreB_F <- Data_W_PreB_F %>% filter(Data_W_PreB_F$TagID != "Yf(0E)O/-:Y/m") %>% droplevels() #removed because battery must have not charged enough to record during winter? Bird from 2021
summary(as.factor(Data_W_PreB_F$Period))
summary(as.factor(Data_W_PreB_F$TagID))

Data_SF_F <- data_tt_21_22_F %>% filter(data_tt_21_22_F$Period == "7 Spring fuzzy")
summary(as.factor(Data_SF_F$Period))
summary(as.factor(Data_SF_F$TagID))

Data_Breed_F <- data_tt_21_22_F %>% filter(data_tt_21_22_F$Period == "8a Female Breeding Season")
summary(as.factor(Data_Breed_F$Period))

Data_AF_F <- data_tt_21_22_F %>% filter(data_tt_21_22_F$Period == "9a Female Autumn fuzzy")
Data_AF_F <- Data_AF_F %>% filter(Data_AF_F$TagID != "Yf(0E)O/-:Y/m") %>% droplevels()
summary(as.factor(Data_AF_F$Period))
summary(as.factor(Data_AF_F$TagID))

Data_W_AfterB_F <- data_tt_21_22_F %>% filter(data_tt_21_22_F$Period == "10 End of December - Winter")
Data_W_AfterB_F <- Data_W_AfterB_F %>% filter(Data_W_AfterB_F$TagID != "Yf(0E)O/-:Y/m") %>% droplevels()
summary(as.factor(Data_W_AfterB_F$Period))
summary(as.factor(Data_W_AfterB_F$TagID))


#male
data_tt_21_22_M<-data_tt_21_22 %>% filter(sex == "M")
summary(as.factor(data_tt_21_22_M$sex))
summary(data_tt_21_22_M$DateTime)


data_tt_21_22_M$Period <- ifelse(data_tt_21_22_M$DateTime < as.POSIXct("2023-02-28 23:59:59", tz="UTC"), "6 Winter pre-breeding",
                                 ifelse(data_tt_21_22_M$DateTime< as.POSIXct("2023-03-31 23:59:59", tz="UTC"), "7 Spring fuzzy",
                                               ifelse(data_tt_21_22_M$DateTime[data_tt_21_22_M$sex=="M"] < as.POSIXct("2023-07-06 23:59:59", tz="UTC"), "8b Male Breeding Season",
                                                      ifelse(data_tt_21_22_M$DateTime < as.POSIXct("2023-09-15 23:59:59", tz="UTC"), "9b Male Autumn fuzzy", 
                                                             ifelse(data_tt_21_22_M$DateTime < as.POSIXct("2023-12-31 23:59:59", tz="UTC"), "10 End of December - Winter", "Post December")))))

summary(as.factor(data_tt_21_22_M$Period))

summary(data_tt_21_22_M$DateTime[data_tt_21_22_M$sex=="M" & data_tt_21_22_M$Period=="8b Male Breeding Season"])


#Subset based on the different categories in period. Added extract removal of certain tag numbers based on when the data stopped transmitting from them
Data_W_PreB_M <- data_tt_21_22_M %>% filter(data_tt_21_22_M$Period == "6 Winter pre-breeding")
summary(as.factor(Data_W_PreB_M$Period))

Data_SF_M <- data_tt_21_22_M %>% filter(data_tt_21_22_M$Period == "7 Spring fuzzy")
summary(as.factor(Data_SF_M$Period))

Data_Breed_M <- data_tt_21_22_M %>% filter(data_tt_21_22_M$Period == "8b Male Breeding Season")
summary(as.factor(Data_Breed_M$Period))
summary(as.factor(Data_Breed_M$TagID))

Data_AF_M <- data_tt_21_22_M %>% filter(data_tt_21_22_M$Period == "9b Male Autumn fuzzy")
Data_AF_M <- Data_AF_M %>% filter(Data_AF_M$TagID != "Yf(6Y)O/-:Y/m_Sandringham") %>% droplevels()
summary(as.factor(Data_AF_M$Period))
summary(as.factor(Data_AF_M$TagID))

Data_W_AfterB_M <- data_tt_21_22_M %>% filter(data_tt_21_22_M$Period == "10 End of December - Winter")
Data_W_AfterB_M <- Data_W_AfterB_M %>% filter(Data_W_AfterB_M$TagID != "Yf(6Y)O/-:Y/m_Sandringham") %>% droplevels()
Data_W_AfterB_M <- Data_W_AfterB_M %>% filter(Data_W_AfterB_M$TagID != "Yf(6X)O/-:Y/m_KenHill") %>% droplevels()
summary(as.factor(Data_W_AfterB_M$Period))
summary(as.factor(Data_W_AfterB_M$TagID))


#Bind some together keeping the breeding season and post breeding season fuzzy separate
Data_W_PreB <- rbind(Data_W_PreB_M,Data_W_PreB_F)
summary(Data_W_PreB$DateTime)

summary(as.factor(Data_W_PreB$TagID))

Data_SF <- rbind(Data_SF_M,Data_SF_F)
summary(Data_SF$DateTime)
summary(as.factor(Data_SF$TagID))

#Data_Breed_M, Data_Breed_F,Data_AF_M ,Data_AF_F

summary(as.factor(Data_Breed_M$TagID))
summary(as.factor(Data_Breed_F$TagID))
summary(as.factor(Data_AF_M$TagID))
summary(as.factor(Data_AF_F$TagID))

Data_W_AfterB <- rbind(Data_W_AfterB_M, Data_W_AfterB_F)
summary(Data_W_AfterB$DateTime)
summary(as.factor(Data_W_AfterB$TagID))


#data for previous cohorts still recording in 2023 #####
data_21_22cohort <- Track2TrackMultiStack(rbind(Data_W_PreB, Data_SF, Data_Breed_M, Data_Breed_F, Data_AF_M, Data_AF_F, Data_W_AfterB), by=c("TagID", "Period"))

data_21_22cohort


# Save
setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data") #HH laptop
save(data_21_22cohort, file="NE103_2023 report_clean tracking data for 2021 2022 cohorts.RData")


# Load
setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data") #HH laptop
load("NE103_2023 report_clean tracking data for 2021 2022 cohorts.RData")



#rbind both together so I just have the 12 combined together
data_2023 <- Track2TrackMultiStack(rbind(data_1d, data_1w, data_2, data_6, data_all, Data_W_PreB, Data_SF, Data_Breed_M, Data_Breed_F, Data_AF_M, Data_AF_F, Data_W_AfterB), by=c("TagID", "Period"))

data_2023
summary(data_2023)

# Save
setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data") ##HH laptop
save(data_2023, file="NE103_2023 report_clean tracking data for all 2023 data.RData")


# Load
setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data") #HH laptop
load("NE103_2023 report_clean tracking data for all 2023 data.RData")



#---------#
#HH NB---- This is Gary's original code to manually filter the data for 2022 NOTE that these actually filter:
    #1 week = 1 day, 2 week = 7-8 days, 6 week = 35-37 days. This is different from the requirement in the report methods which state:
      # the of year cohort analysis should be split into "four discrete time periods: one week, two weeks and six weeks post-release, and all data collected up until the end of December 2022"
#SO HH hashed this out and instead use code above #

# Remove 2021 deployments with no 2022 data 
#data_tt<-data_tt %>% filter(TagID!="Yf(0J)O/-:Y/m" & TagID!="Yf(3A)O/-:Y/m" & TagID!="Yf(3K)O/-:Y/m") %>% droplevels()



# Filter work around for each ID to select time period based on staggered deployments

# One week
#dat1<- data_tt %>% filter(TagID=="Yf(6X)O/-:Y/m_KenHill"|TagID=="Yf(6Y)O/-:Y/m_Sandringham"|TagID=="Yf(7E)O/-:Y/m_KenHill"|TagID=="Yf(7K)O/-:Y/m_Sandringham"|TagID=="Yf(7U)O/-:Y/m_KenHill"|TagID=="Yf(7Y)O/-:Y/m_Sandringham")  %>%
#  filter(DateTime<"2022-07-21 23:59:59")
#dat2<- data_tt %>% filter(TagID=="Yf(8E)O/-:Y/m_Sandringham"|TagID=="Yf(8K)O/-:Y/m_Sandringham"|TagID=="Yf(8L)O/-:Y/m_Sandringham")  %>%
#  filter(DateTime<"2022-08-10 23:59:59")
#dat3<- data_tt %>% filter(TagID=="Yf(8X)O/-:Y/m_KenHill") %>% 
#  filter(DateTime<"2022-08-16 23:59:59")
#dat4<- data_tt %>% filter(TagID=="Yf(9J)O/-:Y/m_KenHill"|TagID=="Yf(9L)O/-:Y/m_KenHill")  %>% 
#  filter(DateTime<"2022-08-23 23:59:59")

#data_1<-rbind(dat1, dat2, dat3, dat4); data_1$Period<-"one week"


# Two weeks
#dat1<- data_tt %>% filter(TagID=="Yf(6X)O/-:Y/m_KenHill"|TagID=="Yf(6Y)O/-:Y/m_Sandringham"|TagID=="Yf(7E)O/-:Y/m_KenHill"|TagID=="Yf(7K)O/-:Y/m_Sandringham"|TagID=="Yf(7U)O/-:Y/m_KenHill"|TagID=="Yf(7Y)O/-:Y/m_Sandringham")  %>%
#  filter(DateTime<"2022-07-28 23:59:59")
#dat2<- data_tt %>% filter(TagID=="Yf(8E)O/-:Y/m_Sandringham"|TagID=="Yf(8K)O/-:Y/m_Sandringham"|TagID=="Yf(8L)O/-:Y/m_Sandringham")  %>%
#  filter(DateTime<"2022-08-17 23:59:59")
#dat3<- data_tt %>% filter(TagID=="Yf(8X)O/-:Y/m_KenHill") %>% 
##  filter(DateTime<"2022-08-23 23:59:59")
#dat4<- data_tt %>% filter(TagID=="Yf(9J)O/-:Y/m_KenHill"|TagID=="Yf(9L)O/-:Y/m_KenHill")  %>% 
# filter(DateTime<"2022-08-30 23:59:59")

#data_2<-rbind(dat1, dat2, dat3, dat4); data_2$Period<-"two weeks"


# Six weeks
#dat1<- data_tt %>% filter(TagID=="Yf(6X)O/-:Y/m_KenHill"|TagID=="Yf(6Y)O/-:Y/m_Sandringham"|TagID=="Yf(7E)O/-:Y/m_KenHill"|TagID=="Yf(7K)O/-:Y/m_Sandringham"|TagID=="Yf(7U)O/-:Y/m_KenHill"|TagID=="Yf(7Y)O/-:Y/m_Sandringham")  %>%
#  filter(DateTime<"2022-08-25 23:59:59")
#dat2<- data_tt %>% filter(TagID=="Yf(8E)O/-:Y/m_Sandringham"|TagID=="Yf(8K)O/-:Y/m_Sandringham"|TagID=="Yf(8L)O/-:Y/m_Sandringham")  %>%
#  filter(DateTime<"2022-09-14 23:59:59")
#dat3<- data_tt %>% filter(TagID=="Yf(8X)O/-:Y/m_KenHill") %>% 
#  filter(DateTime<"2022-09-20 23:59:59")
#dat4<- data_tt %>% filter(TagID=="Yf(9J)O/-:Y/m_KenHill"|TagID=="Yf(9L)O/-:Y/m_KenHill")  %>% 
#  filter(DateTime<"2022-09-27 23:59:59")

#data_6<-rbind(dat1, dat2, dat3, dat4); data_6$Period<-"six weeks"


# End of calendar year (2022 deployments only)
#data_all<- data_tt %>%  filter(TagID!="Yf(0E)O/-:Y/m" & DateTime<"2022-12-31 23:59:59")
#data_all$Period<-"all"


# 2021 deployment for 2022 
#data_21<- data_tt %>% filter(TagID=="Yf(0E)O/-:Y/m") %>% filter(DateTime>"2022-01-01 00:00:00")
#data_21$Period<-"21_dep_all"



# Final merge
#data<-Track2TrackMultiStack(rbind(data_1, data_2, data_6, data_all, data_21), by=c("TagID", "Period"))

#trialrundata<-data

# Save
#setwd("C:/Users/gary.clewley/Desktop/BTO - GDC/2019- Wetland and Marine Team/_NE103 -- Headstarted Curlew tracking/Data/")
#save(data, file="NE103_2022 report_clean tracking data.RData")


# Load
#setwd("C:/Users/gary.clewley/Desktop/BTO - GDC/2019- Wetland and Marine Team/_NE103 -- Headstarted Curlew tracking/Data/")
#setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data") #HH laptop
#load("NE103_2022 report_clean tracking data.RData")

#----- #
# HH NB - back into main code

#Analysis begins#####

#If need the clean but raw data per row can read in this data:
#data <- readRDS(here("data/data_withcohorts_release_sites.rds"))

#summary(data)


#read in the meta data just in case useful:
dt_meta_gsp_TagID <- read.csv(here("data/metadata_TagID.csv"), header=T)

# Load - NB load automatically loads the data back in as the same name it was saved out as 
setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data") #HH laptop
load("NE103_2023 report_clean tracking data for all 2023 data.RData")

#loaded as data_2023. rename it:
data <- data_2023


# Set time period of interest going forward for TIA or remove TrackMultiStack and group_by for habitat work
#data_all<-data_2023[["all"]] # ...etc


#List in order of the data held in the data_2023 list
#1= "1 One Day" 
#3= "2 One Week" 
#4= "3 Two Weeks" 
#5= "4 Six Weeks" 
#6= "5 End of December" 
#7= "6 Winter pre-breeding" 
#8= "7 Spring fuzzy" 
#9= "8a Female Breeding Season" 
#10= "8b Male Breeding Season" 
#11= "9a Female Autumn fuzzy" 
#12= "9b Male Autumn fuzzy" 
#2="10 End of December - Winter" 

#HH NB - useful if going to use a loop
#datasplit <- c("1 One Day" ,"2 One Week" ,"3 Two Weeks" , "4 Six Weeks" ,"5 End of December" ,
 #                "6 Winter pre-breeding" , "7 Spring fuzzy" , "8a Female Breeding Season" , "8b Male Breeding Season" ,
  #               "9a Female Autumn fuzzy","9b Male Autumn fuzzy", "10 End of December - Winter" )


#### TIME IN AREA #HH NB - Leaflet plots ####
# Using BTOTT::

#extract vidiridis for 18 - max number of different birds 
scales::viridis_pal()(18) # "#440154FF" "#481769FF" "#472A7AFF" "#433D84FF" "#3D4E8AFF" "#355E8DFF" 
                                    # "#2E6D8EFF" "#297B8EFF" "#23898EFF" "#1F978BFF" "#21A585FF" "#2EB37CFF" 
                                        # "#46C06FFF" "#65CB5EFF" "#89D548FF" "#B0DD2FFF" "#D8E219FF" "#FDE725FF"

# Basic visualisation of data
BTOTrackingTools::plot_leaflet(data[[1]], lines=FALSE, col=c("#440154FF", "#481769FF", "#472A7AFF" ,"#433D84FF", "#3D4E8AFF", "#355E8DFF" ,
                                            "#2E6D8EFF", "#297B8EFF" ,"#23898EFF", "#1F978BFF", "#21A585FF", "#2EB37CFF" ,
                                            "#46C06FFF", "#65CB5EFF", "#89D548FF", "#B0DD2FFF", "#D8E219FF" ,"#FDE725FF")) #code update - now "plot_leaflet" not "plot_leaflet_dev"
plot_leaflet(data[[3]], lines=FALSE, col=c("#440154FF", "#481769FF", "#472A7AFF" ,"#433D84FF", "#3D4E8AFF", "#355E8DFF" ,
                                           "#2E6D8EFF", "#297B8EFF" ,"#23898EFF", "#1F978BFF", "#21A585FF" ,
                                           "#46C06FFF", "#65CB5EFF", "#89D548FF", "#B0DD2FFF", "#D8E219FF" ,"#FDE725FF"))
plot_leaflet(data[[4]], lines=FALSE, col=c("#481769FF", "#472A7AFF" ,"#433D84FF", "#3D4E8AFF", "#355E8DFF" ,
                                           "#2E6D8EFF", "#297B8EFF" ,"#23898EFF", "#1F978BFF", "#21A585FF",  
                                           "#46C06FFF", "#65CB5EFF", "#89D548FF", "#B0DD2FFF", "#D8E219FF" ,"#FDE725FF"))
plot_leaflet(data[[5]], lines=FALSE, col=c("#481769FF", "#433D84FF", "#3D4E8AFF", "#355E8DFF" ,
                                           "#2E6D8EFF", "#297B8EFF" ,"#23898EFF", "#1F978BFF", "#21A585FF", 
                                           "#46C06FFF", "#65CB5EFF", "#89D548FF", "#B0DD2FFF", "#D8E219FF" ,"#FDE725FF"))
plot_leaflet(data[[6]], lines=FALSE, col=c("#481769FF", "#433D84FF", "#3D4E8AFF", "#355E8DFF" ,
                                           "#2E6D8EFF", "#297B8EFF" ,"#23898EFF", "#1F978BFF", "#21A585FF", 
                                           "#65CB5EFF", "#89D548FF", "#B0DD2FFF", "#D8E219FF" )) 
  

scales::viridis_pal()(8) #"#440154FF" "#46337EFF" "#365C8DFF" "#277F8EFF" "#1FA187FF" "#4AC16DFF" "#9FDA3AFF" "#FDE725FF"
scales::viridis_pal()(4) #"#440154FF" "#31688EFF" "#35B779FF" "#FDE725FF"
scales::viridis_pal()(3) #"#440154FF" ,"#21908CFF" ,"#FDE725FF"
scales::viridis_pal()(5) # "#440154FF", "#3B528BFF", "#21908CFF" ,"#5DC863FF", "#FDE725FF"

plot_leaflet(data[[7]], lines=FALSE, col=c("#440154FF", "#46337EFF", "#365C8DFF" ,"#277F8EFF", "#1FA187FF", "#4AC16DFF" ,"#9FDA3AFF" ))
plot_leaflet(data[[8]], lines=FALSE, col=c("#440154FF", "#46337EFF", "#365C8DFF" ,"#277F8EFF", "#1FA187FF", "#4AC16DFF" ,"#9FDA3AFF", "#FDE725FF"))
plot_leaflet(data[[9]], lines=FALSE, col=c("#440154FF", "#31688EFF" ,"#35B779FF", "#FDE725FF"))
plot_leaflet(data[[10]], lines=FALSE, col=c("#440154FF", "#31688EFF" ,"#35B779FF", "#FDE725FF"))
plot_leaflet(data[[11]], lines=FALSE, col=c("#440154FF" ,"#21908CFF" ,"#FDE725FF"))
plot_leaflet(data[[12]], lines=FALSE, col=c("#440154FF" ,"#21908CFF" ,"#FDE725FF"))
plot_leaflet(data[[2]], lines=FALSE, col = c("#440154FF", "#3B528BFF", "#21908CFF" ,"#5DC863FF", "#FDE725FF"))

#plot_leaflet(data[[9]], lines=FALSE) 

#use this to save out a screenshoot - HH NB this is code from KMB, HH hasn't tried it but Chris T reckons it could work
#mapview::mapshot(leaflet_map, file = paste0("./output/AnimationTests/Rplot",i,".png"))

# Interactive plot with tide data for output
data_tide<-BTOTrackingTools::TrackStack2Track(data[[1]])
data_tide<-TrackStack2Track(data[[3]])
data_tide<-TrackStack2Track(data[[4]])
data_tide<-TrackStack2Track(data[[5]])
data_tide<-TrackStack2Track(data[[6]])

data_tide<-TrackStack2Track(data[[7]]) # no data
data_tide<-TrackStack2Track(data[[8]]) # no data
data_tide<-TrackStack2Track(data[[9]]) # no data
data_tide<-TrackStack2Track(data[[10]])
data_tide<-TrackStack2Track(data[[11]])
data_tide<-TrackStack2Track(data[[12]])
data_tide<-TrackStack2Track(data[[2]])



data_tide<-data_tide %>% filter(tide!="NA")
data_tide$Tide<-as.character(fct_recode(data_tide$tide, "High tide" = "HW", "Low tide" = "LW") )
plot_leaflet(data_tide, plotby="Tide", lines=FALSE, col=c("#31688EFF","#35B779FF")) #code update - now "plot_leaflet" not "plot_leaflet_dev"




#HH addition - save per release site for 1day post release
scales::viridis_pal()(9)
#"#440154FF", "#482878FF" ,"#3E4A89FF" ,"#31688EFF", "#26828EFF", "#1F9E89FF", "#35B779FF", "#6DCD59FF", "#B4DE2CFF" ,"#FDE725FF"
#"#440154FF" ,"#472D7BFF", "#3B528BFF" ,"#2C728EFF" ,"#21908CFF", "#27AD81FF", "#5DC863FF" ,"#AADC32FF", "#FDE725FF"

data_site <- TrackStack2Track(data[[1]])

data_site_ken <- droplevels(data_site %>% filter(data_site$release_site_final=="Ken Hill"))
unique(data_site_ken$TagID) #9
plot_leaflet(data_site_ken, lines=FALSE, col=c("#440154FF", "#482878FF" ,"#3E4A89FF" ,"#31688EFF", "#26828EFF", "#1F9E89FF", "#35B779FF", "#6DCD59FF", "#B4DE2CFF" ))
plot_leaflet(data_site_ken, lines=FALSE, col=viridis_pal()(9)) # KMB, new colour scheme, needs libraries Viridis and ViridisLite

data_site_san <- droplevels(data_site %>% filter(data_site$release_site_final=="Sandringham 2"))
unique(data_site_san$TagID) #9
plot_leaflet(data_site_san, lines=FALSE, col=c("#440154FF" ,"#472D7BFF", "#3B528BFF" ,"#2C728EFF" ,"#21908CFF", "#27AD81FF", "#5DC863FF" ,"#AADC32FF", "#FDE725FF"))
plot_leaflet(data_site_san, lines=FALSE, col=viridis_pal()(9))# KMB, new colour scheme, needs libraries Viridis and ViridisLite



#HH addition - save per release site for 1wk post release

data_site <- TrackStack2Track(data[[3]])
  
data_site_ken <- droplevels(data_site %>% filter(data_site$release_site_final=="Ken Hill"))
unique(data_site_ken$TagID) #8
plot_leaflet(data_site_ken, lines=FALSE, col=c("#440154FF", "#46337EFF", "#365C8DFF" ,"#277F8EFF", "#1FA187FF", "#4AC16DFF" ,"#9FDA3AFF", "#FDE725FF"))
plot_leaflet(data_site_ken, lines=FALSE, col=viridis_pal()(8)) # KMB, new colour scheme, needs libraries Viridis and ViridisLite

data_site_san <- droplevels(data_site %>% filter(data_site$release_site_final=="Sandringham 2"))
unique(data_site_san$TagID) #9 (HH had 6 but 9 here for KMB)
plot_leaflet(data_site_san, lines=FALSE, col=c("#440154FF", "#472D7BFF", "#3B528BFF", "#2C728EFF", "#21908CFF" ,"#27AD81FF" ,"#5DC863FF", "#AADC32FF" ,"#FDE725FF"))
plot_leaflet(data_site_san, lines=FALSE, col=viridis_pal()(9)) # KMB, new colour scheme, needs libraries Viridis and ViridisLite



# basic colour mark sightings plot using leaflet:: directly #HH NB - for the fieldwork year 2023-24 KMB going to do this bit
#col_data<-read_csv("data/NE103_2022 colour ring sighting map locations.csv") # for 2022 csv: ID 8 long had a gap before the -3. Removed
#m<-leaflet(col_data) %>% addTiles()  %>%
#   addCircleMarkers(col_data$longitude, col_data$latitude,radius=3, fillOpacity = 1, opacity = 1)





#### TIME IN AREA -- AREA USE UTILISATION DISTRIBUTIONS #HH NB - this essentially creates a grided version of a KDE
library(sf)
library(tidyterra)
library(ggplot2)

# Set arbitrary 'Colony' location to facilitate later functions. Using central Snettisham location here (HH NB Wild Ken Hill release pen 1 site)
# but not used to define trips away from central place for Curlew
ColLon = 0.50
ColLat = 52.87

# Set projection with 00 on the "colony"
p4 <- sp::CRS(paste("+proj=laea +lon_0=", ColLon," +lat_0=", ColLat, " +units=m", sep=""))

# read in simple UK shapefile map from btotrackingtools and re-project it
ukmap <- BTOTrackingTools::ukmap
ukmap <- sf::st_transform(ukmap,p4) #HH NB: ukmap is an sf not an sp. So changed code here from sp::spTransform to sf::st_transform

# reproject ukmap - HH NB redundant code now as reprojected in line above
#ukmap <- project_points(ukmap, p4s = p4) #HH NB: project_points doesn't work on sf so using st_transform instead
#ukmap <- sf::st_transform(ukmap, crs = st_crs(p4))



# Set time period         ##### UPDATE MANUALLY
#tia_dat<-data[["all"]]

tia_dat<-data[[1]]
tia_dat<-data[[3]]
tia_dat<-data[[4]]
tia_dat<-data[[5]]
tia_dat<-data[[6]]

tia_dat<-data[[7]]
tia_dat<-data[[8]] #error in .local cannot derive coordinates from non-numeric matrix error only for "Yf(0E)O/-:Y/m" and there are only 2 GPS fixes so that is likely to be the issue
tia_dat<-data[[9]]
tia_dat<-data[[10]]
tia_dat<-data[[11]]
tia_dat<-data[[12]]
tia_dat<-data[[2]]

summary(tia_dat)

#HH NB - these lines below then take each section of the data:
    #and 1) find the boundary for the grid, 2) then create a grid with 500 as the cellsize, 
          #3) calculate the amount of time each bird spends in each cell for a) whole population and b) individual birds 
# get bounds for the grid 
llyrb = get_bounds(tia_dat, p4s=p4) # Defaults to UK BNG p4s = sp::CRS("+init=epsg:27700")

# run TIA (trial and error on suitable cell size) # grid of cells. HH NB _ FYI - some of these have 'trips' removed. Chris T reckons this is because of the extra filtering and so for some points there will not be enough to do the amount of time in cell count and so they are removed
indata_grd <- get_TIA_grd(tia_dat, xRa=llyrb$xRa, yRa=llyrb$yRa, cellsize = 500, p4s=p4) # Laptop will not process next step if smaller grid size #Gary's code = cellsize=500

# rank the time cumulatively for plotting for each bird. #ranks the time spent in each cell
grd_rank_all<- rank_time(indata_grd, population = TRUE) # Population level
grd_rank_birds<- rank_time(indata_grd, population = FALSE) # Individual level



# PLOTTING  ##HH  NB - Utilisation Distribution TIA plots - THE WASH ####

# Set axes limit (units m here) - trial and error to set suitable bounds. centered on the colony. units m
xRa<-c(-35000,28000)
yRa<-c(-15000,25000)

####--- this is setting up new units to put lat long onto the map without re-projecting the data... not ideal but it's what Garry did before ... 
# prepare new axes in lat/long - 
earth <- 6378.137
m <- (1 / ((2 * pi / 360) * earth)) /1000

new_lat_lower <- round(ColLat + (min(yRa) * m),1)     ## multiply xyRa by 100 if working in p4 units km
new_lat_upper <- round(ColLat + (max(yRa) * m),1)
new_long_lower <- round(ColLon + (min(xRa) * m) / cos(ColLat * (pi / 180)),1)
new_long_upper <- round(ColLon + (max(xRa) * m) / cos(ColLat * (pi / 180)),1)	

lab_long<-seq(new_long_lower, new_long_upper,length.out=length(seq(min(xRa), max(xRa),by=5000)))
lab_lat<-seq(new_lat_lower, new_lat_upper,length.out=length(seq(min(yRa), max(yRa),by=5000)))


# Get colours
# Get hex colours from viridis (colour blind friendly)
#scales::viridis_pal()(3)
#   "#440154FF" "#21908CFF" "#FDE725FF"                 # For GPS plots
#   "#440154FF" "#31688EFF" "#35B779FF" "#FDE725FF"     # For TIA plots #four categories of the "Bins" = cut offs of the distribution, 50%, 75%, 95%, 100%




# Set directory (outside of Github here)
#dir<-"C:/Users/gary.clewley/Desktop/BTO - GDC/2019- Wetland and Marine Team/_NE103 -- Headstarted Curlew tracking/Outputs/"
dir <- "C:/Users/hannah.hereward/Documents/Projects/2024_curlewheadstarting/curlew_headstarting/output/Figures/" #HH laptop directory
plot_name<-"NE103_Headstart CURLEW_TIA_2023_PREVIOUSCOHORT_10_WinterPostBreed.tiff"



# Set plot device (saving hi-res base R maps)
tiff(paste0(dir,plot_name), width=25, height=23, units="cm", pointsize=18, res=600, compression ="lzw")

#HH NB: updated as sp package was discontinued. terra used now according to plot_TIA
  #removed: 
terra::plot(ukmap$geometry, xlim=xRa, ylim=yRa,col="grey80",border="grey80", axes=T, yaxt="n",  #need to specify here ukmap$geometry
         xaxt="n", xlab="Longitude", ylab="Latitude",
         main="Winter - post-breeding")# UPDATE MANUALLY                     
#axis(1)
#axis(2)
axis(1, at=seq(min(xRa), max(xRa),by=5000), labels=round(lab_long,2)) 
axis(2, at=seq(min(yRa), max(yRa),by=5000), labels=round(lab_lat,2))


#HH NB. had error for this plot about memory. UPDATE: needed to specify the ukmap$geometry in the terra::plot above and now it works
# UPDATE INDIVIDUAL BETWEEN PLOTS
plot_TIA(data=grd_rank_all,Add=TRUE,                    # UPDATE ID SELECTION. grd_rank_birds OR grd_rank_all ??
         xra=xRa, yra=yRa,
         g_levs = c(1,0.95,0.75,0.5),
         c_levs = c(0.95,0.75,0.5),
         col_ramp_grd =c("#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF"), #TIA colours for the 50%, 75%, 95% and 100%
         #col_ramp_con =c("#31688EFF", "#35B779FF", "#FDE725FF"), #colours for the countour lines rather than grided 
         cont_typ=1) # if this is 4 you can plot it outside the function and it returns an object in R 

#HH NB. dev.off needs running to 'close' the tiff and save it - without running this bit it won't save !
dev.off()




# PLOTTING  ##HH  NB - Utilisation Distribution TIA plots - THE UK and SE for 2021 & 2022 cohorts ####

#This is to assess if the older cohort birds are using other parts of the UK aside from the Wash which are accounted for in the avalible/used panel but not in the TIA
#2023 - only the Breeding season (Female) had one - Rotchester

#Use same colony lat and long as for the wash

# Set axes limit (units m here) - trial and error to set suitable bounds. centered on the colony. units m

#whole of UK
xRa<-c(-622435.4,224987.6)
yRa<-c(-414008.8,886812.5)


#slight zoom in:
xRa<-c(-622435.4,124987.6)
yRa<-c(-314008.8,686812.5)


#southeast of the UK
xRa<-c(-122435.4,24987.6)
yRa<-c(-214008.8,86812.5)

####--- this is setting up new units to put lat long onto the map without re-projecting the data... not ideal but it's what Garry did before ... 
# prepare new axes in lat/long - 
earth <- 6378.137
m <- (1 / ((2 * pi / 360) * earth)) /1000

new_lat_lower <- round(ColLat + (min(yRa) * m),1)     ## multiply xyRa by 100 if working in p4 units km
new_lat_upper <- round(ColLat + (max(yRa) * m),1)
new_long_lower <- round(ColLon + (min(xRa) * m) / cos(ColLat * (pi / 180)),1)
new_long_upper <- round(ColLon + (max(xRa) * m) / cos(ColLat * (pi / 180)),1)	

lab_long<-seq(new_long_lower, new_long_upper,length.out=length(seq(min(xRa), max(xRa),by=5000)))
lab_lat<-seq(new_lat_lower, new_lat_upper,length.out=length(seq(min(yRa), max(yRa),by=5000)))


# Get colours
# Get hex colours from viridis (colour blind friendly)
#scales::viridis_pal()(3)
#   "#440154FF" "#21908CFF" "#FDE725FF"                 # For GPS plots
#   "#440154FF" "#31688EFF" "#35B779FF" "#FDE725FF"     # For TIA plots #four categories of the "Bins" = cut offs of the distribution, 50%, 75%, 95%, 100%




# Set directory (outside of Github here)
#dir<-"C:/Users/gary.clewley/Desktop/BTO - GDC/2019- Wetland and Marine Team/_NE103 -- Headstarted Curlew tracking/Outputs/"
dir <- "C:/Users/hannah.hereward/Documents/Projects/2024_curlewheadstarting/curlew_headstarting/output/Figures/" #HH laptop directory
plot_name<-"NE103_Headstart CURLEW_TIA_2023_PREVIOUSCOHORT_10_Winter_PostBreed_UK_SE.tiff"



# Set plot device (saving hi-res base R maps)
tiff(paste0(dir,plot_name), width=25, height=23, units="cm", pointsize=18, res=600, compression ="lzw")

#HH NB: updated as sp package was discontinued. terra used now according to plot_TIA
#removed: 
terra::plot(ukmap$geometry, xlim=xRa, ylim=yRa,col="grey80",border="grey80", axes=T, yaxt="n",  #need to specify here ukmap$geometry
            xaxt="n", xlab="Longitude", ylab="Latitude",
            main="Winter post-breeding")# UPDATE MANUALLY                     
#axis(1)
#axis(2)
axis(1, at=seq(min(xRa), max(xRa),by=5000), labels=round(lab_long,2)) 
axis(2, at=seq(min(yRa), max(yRa),by=5000), labels=round(lab_lat,2))


#HH NB. had error for this plot about memory. UPDATE: needed to specify the ukmap$geometry in the terra::plot above and now it works
# UPDATE INDIVIDUAL BETWEEN PLOTS
plot_TIA(data=grd_rank_all,Add=TRUE,                    # UPDATE ID SELECTION. grd_rank_birds OR grd_rank_all ??
         xra=xRa, yra=yRa,
         g_levs = c(1,0.95,0.75,0.5),
         c_levs = c(0.95,0.75,0.5),
         col_ramp_grd =c("#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF"), #TIA colours for the 50%, 75%, 95% and 100%
         #col_ramp_con =c("#31688EFF", "#35B779FF", "#FDE725FF"), #colours for the countour lines rather than grided 
         cont_typ=1) # if this is 4 you can plot it outside the function and it returns an object in R 

#HH NB. dev.off needs running to 'close' the tiff and save it - without running this bit it won't save !
dev.off()







#### HABITAT SELECTION ####

#### Data prep 
# Load amt:: package
library(amt) 


## Load Land Cover Map 2021 25m Raster
landuse <- raster::raster(here("data","NE103_LCM2021","LCM.tif"))
#landuse <- terra::rast(here("data","NE103_LCM2021","LCM.tif")) #terra might be needed if raster package 
#HH NB - landuse already in the same projection as in line below so can hash tag out this line
#landuse <- raster::projectRaster(landuse, crs =("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"), method = "ngb") # method nearest neighbour for categorical raster values (opposed to bilinear interpolation)




## ## ## ## ## ## ##
# Unlist and SET TIME PERIOD
#trk_dat<-TrackStack2Track(data[["all"]]) # Redo manually for all time periods

trk_dat<-TrackStack2Track(data[[1]])
trk_dat<-TrackStack2Track(data[[3]])
trk_dat<-TrackStack2Track(data[[4]])
trk_dat<-TrackStack2Track(data[[5]])
trk_dat<-TrackStack2Track(data[[6]])

trk_dat<-TrackStack2Track(data[[7]])
trk_dat<-TrackStack2Track(data[[8]])  # Yf(0E)O/-:Y/m only has two fixes in this time period and so is removed for this part of the analysis
trk_dat<-TrackStack2Track(data[[9]])
trk_dat<-TrackStack2Track(data[[10]])
trk_dat<-TrackStack2Track(data[[11]])
trk_dat<-TrackStack2Track(data[[12]])
trk_dat<-TrackStack2Track(data[[2]])


#HH NB added in this code to 'droplevels' of the non-relevant TagID per time period
trk_dat<- trk_dat %>% droplevels()

# Convert to 'amt' track (using BTOTT headers) #HH NB - release col name updated - use release_site_final = combined Ken Hill, seperate Sandringham. Cohort col name updated - cohort_analysis - this uses the first cohorts = 1, and remaining cohorts = 2 
trk <- make_track(trk_dat, .x = longitude, .y = latitude, .t = DateTime, id = TagID, tide = tide, release=release_site_final, cohort=cohort_analysis, speed=ground.speed, crs = "epsg:4326")


# Transform to BNG
trk <- transform_coords(trk, "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs")


# Extract LCM variables
trk <- trk %>% 
  extract_covariates(landuse)

#HH NB ADDED IN A FILTER HERE to remove NAs (equates to marine and non-UK fixes) SO THAT THE RANDOM POINTS (further down) ARE ONLY GENERATED IN THE UK. Therefore, this code is filtering the LCM results in trk to remove all NAs.
    #NOTE this will remove some 'at sea' around the UK NAs as well as non-UK fixes 
trk <- trk %>% 
  filter(!is.na(layer))


# Check sampling rate
summarize_sampling_rate_many(trk, cols="id")


###--- HH NB didn't do this hashed out bit as hash tagged out in 2021 and 2022 
# Standardise sampling rate 

#(nesting used on NE86 not functional by ID here - need to look into new syntax - returns as NULL list)
# Workaround to use unnest_legacy()


# potentially causing issues further on - causes random_points() to fail - luckily sampling consistent anyway in this case
#trk <- trk %>% nest(data=-"id") %>% 
#  mutate(data = map(data, ~ .x %>% 
#    track_resample(rate = minutes(30), tolerance = minutes(3))))  %>%
#    unnest_legacy(cols=c(data))

# Revert to class as lost during nesting
#trk <- make_track(trk, .x = x_, .y = y_, .t = t_, id = id, tide = tide, release=release, cohort=cohort, speed=ground.speed, burst = burst_, crs = "epsg:4326")

###---



# Speed filter to remove likely flight/commuting fixes - using ground.speed from Movebank (not amt calculation)
trk<-trk %>% filter(speed<4)



# Create random points for each individual (As above nesting issue)

#HH NB - for Yf(0E)O/-:Y/m only has two fixes in this time period and so is removed for this part of the analysis
#ONLY USE THIS CODE FOR '8' = Spring Transition:  trk <- trk %>% filter(trk$id!= "Yf(0E)O/-:Y/m")


avail.pts <- trk %>%  nest(data=-c("id", "tide", "cohort", "release")) %>% 
  mutate(rnd_pts = map(data, ~ random_points(., factor = 20, type="random"))) %>% 	
  select(id, tide,cohort,release, rnd_pts) %>%  # you don't want to have the original point twice, hence drop data
  unnest_legacy(cols=c(rnd_pts))


# Assign class as lost during nesting
class(avail.pts) <- c("random_points", class(avail.pts))


# Extract LCM values for random points
avail.pts <- avail.pts %>% 
  extract_covariates(landuse)


# Make factor plotting friendly
avail.pts$used<-as.factor(avail.pts$case_)
avail.pts$used<-fct_recode(avail.pts$used, "Available" = "FALSE", "Used" = "TRUE")

#for Appendix save out a example plot of observed vrs random points - using 8 as only two birds so not too confusing with overlapping convex polygons
#create the file 
#jpeg(file="C:/Users/hannah.hereward/Documents/Projects/2024_curlewheadstarting/curlew_headstarting/output/Figures/NE103_Headstart CURLE_APPENDIXplot_8a_Breeding_F.jpg", width=15, height=15, units="cm", res=300)
#run the plot
#plot(avail.pts[avail.pts$id=="Yf(9J)O/-:Y/m_KenHill",])
#close the file
#dev.off() 


# Tidy LCM variable  # variable name 'layer' with new landuse data for 2022. HH NB - note that this produces warning messages presumably because some of the habitat numbers don't feature in each extraction
rsfdat <- avail.pts %>%  mutate(
  layer = as.character(layer), 
  layer = fct_collapse(layer,
                     "Arable" = c("3"),
                     "Grassland" = c("4","5","6","7"),
                     "Coastal rock" = c("15", "17"),
                     "Coastal sediment" = c("16", "18"),
                     "Saltmarsh" = c("19"),
                     "Other" = c("1", "2","8", "9", "10", "11", "12","13","14", "20", "21")))


# Reorder factor level
rsfdat$layer<- factor(rsfdat$layer, levels=c("Coastal sediment","Saltmarsh","Coastal Rock","Arable","Grassland","Other"))


# set response to numeric
rsfdat <- rsfdat %>% mutate(case_ = as.numeric(case_))


# Weight available data 
rsfdat$w <- ifelse(rsfdat$case_ == 1, 1, 5000)


# Set individual habitat factors (pooling all other habitats into single reference level)
rsfdat$Coastal <- ifelse(rsfdat$layer == "Coastal sediment", 1, 0)
rsfdat$Saltmarsh <- ifelse(rsfdat$layer == "Saltmarsh", 1, 0)
rsfdat$Arable <- ifelse(rsfdat$layer == "Arable", 1, 0)
rsfdat$Grassland <- ifelse(rsfdat$layer == "Grassland", 1, 0)
rsfdat$Other <- ifelse(rsfdat$layer == "Other", 1, 0)




#rename the RSF files based on the timeframe - 1wk,2wks,6wks,all Jul-Dec
#HH NB - some of the categories will have NAs some of these are because they are too far out into the sea for the LCM map to categories them BUT others are outside of the UK and therefore outside of the UK LCM!
rsfdat

#List in order of the data held in the data_2023 list
#1= "1 One Day" 
#3= "2 One Week" 
#4= "3 Two Weeks" 
#5= "4 Six Weeks" 
#6= "5 End of December" 
#7= "6 Winter pre-breeding" 
#8= "7 Spring fuzzy" 
#9= "8a Female Breeding Season" 
#10= "8b Male Breeding Season" 
#11= "9a Female Autumn fuzzy" 
#12= "9b Male Autumn fuzzy" 
#2="10 End of December - Winter" 

# Save rsfdat for each time period
#setwd("C:/Users/gary.clewley/Desktop/BTO - GDC/2019- Wetland and Marine Team/_NE103 -- Headstarted Curlew tracking/Data/")
setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data/") #HH laptop

save(rsfdat, file="NE103_2023 report_RSF_data_cohort2023_OneDay.RData") #1
save(rsfdat, file="NE103_2023 report_RSF_data_cohort2023_OneWeek.RData") #3
save(rsfdat, file="NE103_2023 report_RSF_data_cohort2023_TwoWeeks.RData") #4
save(rsfdat, file="NE103_2023 report_RSF_data_cohort2023_SixWeeks.RData") #5
save(rsfdat, file="NE103_2023 report_RSF_data_cohort2023_July_Dec2023.RData") #6

save(rsfdat, file="NE103_2023 report_RSF_data_cohort2122_Winter_PreBreed.RData") #7
save(rsfdat, file="NE103_2023 report_RSF_data_data_cohort2122_Spring_T.RData") #8
save(rsfdat, file="NE103_2023 report_RSF_data_cohort2122_Breeding_F.RData") #9
save(rsfdat, file="NE103_2023 report_RSF_data_cohort2122_Breeding_M.RData") #10
save(rsfdat, file="NE103_2023 report_RSF_data_cohort2122_Autumn_T_F.RData") #11
save(rsfdat, file="NE103_2023 report_RSF_data_cohort2122_Autumn_T_M.RData") #12
save(rsfdat, file="NE103_2023 report_RSF_data_cohort2122_Winter_PostBreed.RData") #2






#### RSF plotting ##HH NB - Utilisation Distribution plots, habitat avalibility ####

# Load rsfdat for each time period
#setwd("C:/Users/gary.clewley/Desktop/BTO - GDC/2019- Wetland and Marine Team/_NE103 -- Headstarted Curlew tracking/Data/")
setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data/") #HH laptop

load("NE103_2023 report_RSF_data_cohort2023_OneDay.RData")
load("NE103_2023 report_RSF_data_cohort2023_OneWeek.RData")
load("NE103_2023 report_RSF_data_cohort2023_TwoWeeks.RData")
load("NE103_2023 report_RSF_data_cohort2023_SixWeeks.RData")
load("NE103_2023 report_RSF_data_cohort2023_July_Dec2023.RData")

load("NE103_2023 report_RSF_data_cohort2122_Winter_PreBreed.RData")
load("NE103_2023 report_RSF_data_data_cohort2122_Spring_T.RData")
load("NE103_2023 report_RSF_data_cohort2122_Breeding_F.RData")
load("NE103_2023 report_RSF_data_cohort2122_Breeding_M.RData")
load("NE103_2023 report_RSF_data_cohort2122_Autumn_T_F.RData")
load("NE103_2023 report_RSF_data_cohort2122_Autumn_T_M.RData")
load("NE103_2023 report_RSF_data_cohort2122_Winter_PostBreed.RData")


#FILTER ONLY TO BE USED FOR TIDE TO REMOVE THE NAs
#rsfdat <- rsfdat %>% filter(tide != "NA")

## Available/Used Plot 
na.omit(rsfdat) %>% #filter(id=="Yf(0E)O/-:Y/m") %>%	                          	# Update period or ID
 #filter(tide != "NA") %>%                                                          #FILTER ONLY TO BE USED FOR TIDE TO REMOVE THE NAs
  ggplot(.,  aes(x=layer,group=used))                                       +	      # select data and variables - using na.omit() here to exclude random points offshore outside LCM area. HH NB - LCM = layer in 2022 and 2023 LCM data so need to change this to layer
  geom_bar(position=position_dodge(), aes(y=after_stat(prop), fill = used),
           stat="count", colour="black")                                +       # select barplot of proportions presented side by side with black outline
  scale_fill_manual(values=c("grey70", "grey20"))                       + 		  # define colours, plenty of good built in palettes if colour can be used
  labs(y = "Proportion of fixes\n", fill="used", x="\nHabitat")         + 			# labels, \n indicates space between line and text
  theme_classic()                                                       +       # remove default grid lines and grey background
  theme(legend.title=element_blank(),  																	 			  # remove legend title
        legend.position = c(0.9,0.8),                                           # specify legend position inside plot area
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))   +       # rotated x axis labels for individual plots
  #scale_y_continuous(expand = expansion(mult = c(0, .1)))               +       # remove gap between bars and axis lines
  #ylim(c(0,0.6)) +                                                              #HH NB - use ylim 2023 = c(0,0.6) and scale_y_continuous for 2021&2022 
  scale_y_continuous(breaks = seq(0,1,by=0.2), limits =c(0,1)) +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14)) +
  ggtitle("Winter - post-breeding")   #    +
  #facet_grid(rows=vars(release)) # if by tide:  facet_grid(rows=vars(tide)) or release site:   facet_grid(rows=vars(release)) or cohort:  facet_grid(rows=vars(cohort))



# Save plot (outside of Github)
#setwd("C:/Users/gary.clewley/Desktop/BTO - GDC/2019- Wetland and Marine Team/_NE103 -- Headstarted Curlew tracking/Outputs/")
setwd("C:/Users/hannah.hereward/Documents/Projects/2024_curlewheadstarting/curlew_headstarting/output/Figures/Habitat_prop/")
ggsave("NE103_2023_Headstart CURLE_RSF plot_10_Winter_postbreed.jpg", width=15, height=15, units="cm", dpi=300)  ## UPDATE FILENAME




#### --- HH NB this set of code doesn't quite all work and it is not clear what it is doing. So as the following code creates the graphs needed this set of code has been ignored EXCEPT FOR: 
# THE top two x lines because they feed into the next bit below. copied below to make this make more sense!

## ## ## ## ## ## ##
# Calculate error bars (in progress - clunky)
#x<-as.data.frame(rsfdat %>% with(table(id,used,layer))) #HH NB - LCM = layer in 2022 and 2023 LCM data so need to change this to layer
#x<-x %>% filter (layer!="Coastal Rock") # HH NB - Gary's code here removed the 2021 bird and coastal rock layer. May or may not be needed for each time period

#x1<-x %>% filter(used=="Used") %>% group_by(layer) %>% mutate(prop=Freq/y$Used)# HH NB - the dataframe y is in the line just below - need to run these first and then run these two lines
#x2<-x %>% filter(used=="Available") %>% group_by(layer) %>% mutate(prop=Freq/y$Available)


## work out denominator for each individual
#y<-tapply(x$Freq, list(x$id, x$used), sum)
#y<-as.data.frame(y)
#y<-as.data.frame(y[-1,]) #*****HH NB - this for some reason removed the first row of data... not sure why... 

#note differs from flexible plots combining all birds above)
#prop_u<-x1 %>% group_by(LCM) %>% mutate(mean_prop=mean(prop)) %>% select(LCM, mean_prop) %>% distinct() #HH NB - LCM = layer in 2022 and 2023 LCM data so need to change this to layer
#prop_a<-x2 %>% group_by(LCM) %>% mutate(mean_prop=mean(prop)) %>% select(LCM, mean_prop) %>% distinct() #HH NB - LCM = layer in 2022 and 2023 LCM data so need to change this to layer

#HH NB - use the y table to manually count and update the sqrt number
#sd_u<-x1 %>% group_by(layer) %>% mutate(sd=round(sd(prop)/sqrt(18),2)) %>% select(layer, sd) %>% distinct() #HH NB - LCM = layer in 2022 and 2023 LCM data so need to change this to layer
#sd_a<-x2 %>% group_by(layer) %>% mutate(sd=round(sd(prop)/sqrt(18),2)) %>% select(layer, sd) %>% distinct() #HH NB - LCM = layer in 2022 and 2023 LCM data so need to change this to layer

##### Need to sense check if best to use these proportions for plotting (done by individual rather than combined)
# then create new simple plot dataframe e.g. below (from NE86) rather than plotting code used above
# with two rows for each habitat alternating between used and available as per plot


#*****------HH NB - for some reason 'prop' isn't here... so can't do this figure...
#fig_3_newdf<-as.data.frame(cbind(prop, conf.low, conf.high, se))
#fig_3_newdf$used<-as.factor(rep(c("Available", "Used"), 7))
#fig_3_newdf$lcm_mod<-as.factor(as.character(c("coastal", "coastal", "agriculture","agriculture", "mussel","mussel", "marine", "marine","urban", "urban","other", "other","landfill","landfill")))
#fig_3_newdf$lcm_mod<- factor(fig_3_newdf$lcm_mod, levels=c("coastal", "agriculture", "mussel", "marine", "urban", "other", "landfill"))
## ## ## ## ## ## ##


####### --- HH NB - return to code 




#### RSS models and plotting ## HH NB - Relative Selection Strength graphs ####
## Relative Selection Strength models (ran manually for time period and habitat for now)
# exp(estimate) for Relative Selection Strength from RSF models



# Select time period
#rsfdat<-rsfdat_6 #*****HH NB - change this for each time period

# Load rsfdat for each time period
#setwd("C:/Users/gary.clewley/Desktop/BTO - GDC/2019- Wetland and Marine Team/_NE103 -- Headstarted Curlew tracking/Data/")
setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data/") #HH laptop

load("NE103_2023 report_RSF_data_cohort2023_OneDay.RData")
load("NE103_2023 report_RSF_data_cohort2023_OneWeek.RData")
load("NE103_2023 report_RSF_data_cohort2023_TwoWeeks.RData")
load("NE103_2023 report_RSF_data_cohort2023_SixWeeks.RData")
load("NE103_2023 report_RSF_data_cohort2023_July_Dec2023.RData")

load("NE103_2023 report_RSF_data_cohort2122_Winter_PreBreed.RData")
load("NE103_2023 report_RSF_data_data_cohort2122_Spring_T.RData")
load("NE103_2023 report_RSF_data_cohort2122_Breeding_F.RData")
load("NE103_2023 report_RSF_data_cohort2122_Breeding_M.RData")
load("NE103_2023 report_RSF_data_cohort2122_Autumn_T_F.RData")
load("NE103_2023 report_RSF_data_cohort2122_Autumn_T_M.RData")
load("NE103_2023 report_RSF_data_cohort2122_Winter_PostBreed.RData")


#HH NB - code copied down from above - this seems to be the only code needed from the hashtagged out code above 
## Calculate error bars (in progress - clunky)
x<-as.data.frame(rsfdat %>% with(table(id,used,layer))) #HH NB - LCM = layer in 2022 and 2023 LCM data so need to change this to layer
x<-x %>% filter (layer!="Coastal Rock") # HH NB - Gary's code here removed the 2021 bird and coastal rock layer. May or may not be needed for each time period


####

#### HH NB - Gary's code - HH alternative code below ####
#HH NB: list of separate columns need to be run in line below: Coastal,Grassland, Saltmarsh, Arable, Other
# Fit habitat model for each habitat to each individual #HH NB this has to be manually changed for each time period AND each habitat per time period
rsffits <- rsfdat  %>% nest(data=-"id") %>%   mutate(mod = map(data, function(x) glm(case_ ~ Coastal, data = x, weight=w,family = binomial)))
rsffits <- rsfdat  %>% nest(data=-"id") %>%   mutate(mod = map(data, function(x) glm(case_ ~ Grassland, data = x, weight=w,family = binomial)))
rsffits <- rsfdat  %>% nest(data=-"id") %>%   mutate(mod = map(data, function(x) glm(case_ ~ Saltmarsh, data = x, weight=w,family = binomial)))
rsffits <- rsfdat  %>% nest(data=-"id") %>%   mutate(mod = map(data, function(x) glm(case_ ~ Arable, data = x, weight=w,family = binomial)))
rsffits <- rsfdat  %>% nest(data=-"id") %>%   mutate(mod = map(data, function(x) glm(case_ ~ Other, data = x, weight=w,family = binomial)))


## Gary's note: DO NOT open rsffits object from R Studio panel view(rsffits) - keep crashing on 3.6.1 ##HH NB: opens fine in R 4.2.2

# Check goodness of fit #HH NB this is an appendix table
# Running issues originally due to na.action = argument not set. Ran later and daved manually (writeClipboard(as.character(rsf_gof$auc_test)))
# Could integrate into tidy output if needed

#rsf_gof <- rsfdat  %>% nest(data=-"id") %>%   mutate(auc_test = map(data, function(x) pROC::auc(pROC::roc(x$case_~(predict(glm(case_ ~ Arable, data = x, weight=w,family = binomial, na.action=na.exclude), type=c("response"))))))) #edit response var

rsf_gof <- rsfdat  %>% nest(data=-"id") %>%   mutate(auc_test = map(data, function(x) pROC::auc(pROC::roc(x$case_~(predict(glm(case_ ~ Coastal, data = x, weight=w,family = binomial, na.action=na.exclude), type=c("response"))))))) #edit response var
rsf_gof <- rsfdat  %>% nest(data=-"id") %>%   mutate(auc_test = map(data, function(x) pROC::auc(pROC::roc(x$case_~(predict(glm(case_ ~ Grassland, data = x, weight=w,family = binomial, na.action=na.exclude), type=c("response"))))))) #edit response var
rsf_gof <- rsfdat  %>% nest(data=-"id") %>%   mutate(auc_test = map(data, function(x) pROC::auc(pROC::roc(x$case_~(predict(glm(case_ ~ Saltmarsh, data = x, weight=w,family = binomial, na.action=na.exclude), type=c("response"))))))) #edit response var
rsf_gof <- rsfdat  %>% nest(data=-"id") %>%   mutate(auc_test = map(data, function(x) pROC::auc(pROC::roc(x$case_~(predict(glm(case_ ~ Arable, data = x, weight=w,family = binomial, na.action=na.exclude), type=c("response"))))))) #edit response var
rsf_gof <- rsfdat  %>% nest(data=-"id") %>%   mutate(auc_test = map(data, function(x) pROC::auc(pROC::roc(x$case_~(predict(glm(case_ ~ Other, data = x, weight=w,family = binomial, na.action=na.exclude), type=c("response"))))))) #edit response var

rsf_gof$auc_test # viewing this as a tab gets slower

#copy the AUC for each bird to clipboard
writeClipboard(as.character(rsf_gof$auc_test))

###beta_sub[beta_sub$id==rsf_gof$id, beta_sub$'Coastal.sediment']

# tidy model outputs
rsffits <- rsffits %>%
   dplyr::mutate(tidy = purrr::map(mod, broom::tidy),
                 n = purrr::map(data, nrow))

# Unnest and tidy outputs
rsf_coefs<-rsffits %>% unnest(c(tidy)) %>%  
       select(-(std.error:p.value))

# rm data from coefs object for efficiency
rsf_coefs<-within(rsf_coefs, rm(data))
rsf_coefs<-within(rsf_coefs, rm(mod))


#HH NB: list of separate columns need to be run in line below: Coastal,Grassland, Saltmarsh, Arable, Other
# Name for habitat and ***** repeat code above******* #HH NB - have to update this manually!
rsf_coefs_coast<-rsf_coefs
rsf_coefs_grass<-rsf_coefs
rsf_coefs_salt<-rsf_coefs
rsf_coefs_arable<-rsf_coefs
rsf_coefs_other<-rsf_coefs 


# Combine and save RSF model outputs
rsf_coefs_hab<-bind_rows(rsf_coefs_coast, rsf_coefs_grass, rsf_coefs_salt, rsf_coefs_arable, rsf_coefs_other)

##HH NB - ONCE bound, remove the files from the environment to avoid accidentally including it in the next one
rm(rsf_coefs_coast, rsf_coefs_grass, rsf_coefs_salt, rsf_coefs_arable, rsf_coefs_other)


#setwd("C:/Users/gary.clewley/Desktop/BTO - GDC/2019- Wetland and Marine Team/_NE103 -- Headstarted Curlew tracking/Data/")
setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data/") #HH laptop

save(rsf_coefs_hab, file="NE103_2023 report_RSF_models_cohort2023_OneDay.RData")
save(rsf_coefs_hab, file="NE103_2023 report_RSF_models_cohort2023_OneWeek.RData")
save(rsf_coefs_hab, file="NE103_2023 report_RSF_models_cohort2023_TwoWeeks.RData")
save(rsf_coefs_hab, file="NE103_2023 report_RSF_models_cohort2023_SixWeeks.RData")
save(rsf_coefs_hab, file="NE103_2023 report_RSF_models_cohort2023_July_Dec2023.RData")

save(rsf_coefs_hab, file="NE103_2023 report_RSF_models_cohort2122_Winter_PreBreed.RData")
save(rsf_coefs_hab, file="NE103_2023 report_RSF_models_cohort2122_Spring_T.RData")
save(rsf_coefs_hab, file="NE103_2023 report_RSF_models_cohort2122_Breeding_F.RData")
save(rsf_coefs_hab, file="NE103_2023 report_RSF_models_cohort2122_Breeding_M.RData")
save(rsf_coefs_hab, file="NE103_2023 report_RSF_models_cohort2122_Autumn_T_F.RData")
save(rsf_coefs_hab, file="NE103_2023 report_RSF_models_cohort2122_Autumn_T_M.RData")
save(rsf_coefs_hab, file="NE103_2023 report_RSF_models_cohort2122_Winter_PostBreed.RData")



#instead of the code above - set up to run individually per habitat: ####

### coastal
#HH NB: list of separate columns need to be run in line below: Coastal,Grassland, Saltmarsh, Arable, Other
# Fit habitat model for each habitat to each individual #HH NB this has to be manually changed for each time period AND each habitat per time period
rsffits <- rsfdat  %>% nest(data=-"id") %>%   mutate(mod = map(data, function(x) glm(case_ ~ Coastal, data = x, weight=w,family = binomial)))


# Check goodness of fit #HH NB this is an appendix table
# Running issues originally due to na.action = argument not set. Ran later and daved manually (writeClipboard(as.character(rsf_gof$auc_test)))
rsf_gof <- rsfdat  %>% nest(data=-"id") %>%   mutate(auc_test = map(data, function(x) pROC::auc(pROC::roc(x$case_~(predict(glm(case_ ~ Coastal, data = x, weight=w,family = binomial, na.action=na.exclude), type=c("response"))))))) #edit response var


rsf_gof$auc_test # viewing this as a tab gets slower

#copy the AUC for each bird to clipboard
writeClipboard(as.character(rsf_gof$auc_test))



# tidy model outputs
rsffits <- rsffits %>%
  dplyr::mutate(tidy = purrr::map(mod, broom::tidy),
                n = purrr::map(data, nrow))

# Unnest and tidy outputs
rsf_coefs<-rsffits %>% unnest(c(tidy)) %>%  
  select(-(std.error:p.value))

# rm data from coefs object for efficiency
rsf_coefs<-within(rsf_coefs, rm(data))
rsf_coefs<-within(rsf_coefs, rm(mod))


#HH NB: list of separate columns need to be run in line below: Coastal,Grassland, Saltmarsh, Arable, Other
# Name for habitat and ***** repeat code above******* #HH NB - have to update this manually!
rsf_coefs_coast<-rsf_coefs


### Grassland
#HH NB: list of separate columns need to be run in line below: Coastal,Grassland, Saltmarsh, Arable, Other
# Fit habitat model for each habitat to each individual #HH NB this has to be manually changed for each time period AND each habitat per time period
rsffits <- rsfdat  %>% nest(data=-"id") %>%   mutate(mod = map(data, function(x) glm(case_ ~ Grassland, data = x, weight=w,family = binomial)))


## Gary's note: DO NOT open rsffits object from R Studio panel view(rsffits) - keep crashing on 3.6.1 ##HH NB: opens fine in R 4.2.2

# Check goodness of fit #HH NB this is an appendix table
# Running issues originally due to na.action = argument not set. Ran later and daved manually (writeClipboard(as.character(rsf_gof$auc_test)))
# Could integrate into tidy output if needed

rsf_gof <- rsfdat  %>% nest(data=-"id") %>%   mutate(auc_test = map(data, function(x) pROC::auc(pROC::roc(x$case_~(predict(glm(case_ ~ Grassland, data = x, weight=w,family = binomial, na.action=na.exclude), type=c("response"))))))) #edit response var

rsf_gof$auc_test # viewing this as a tab gets slower

#copy the AUC for each bird to clipboard
writeClipboard(as.character(rsf_gof$auc_test))

###beta_sub[beta_sub$id==rsf_gof$id, beta_sub$'Coastal.sediment']

# tidy model outputs
rsffits <- rsffits %>%
  dplyr::mutate(tidy = purrr::map(mod, broom::tidy),
                n = purrr::map(data, nrow))

# Unnest and tidy outputs
rsf_coefs<-rsffits %>% unnest(c(tidy)) %>%  
  select(-(std.error:p.value))

# rm data from coefs object for efficiency
rsf_coefs<-within(rsf_coefs, rm(data))
rsf_coefs<-within(rsf_coefs, rm(mod))


#HH NB: list of separate columns need to be run in line below: Coastal,Grassland, Saltmarsh, Arable, Other
# Name for habitat and ***** repeat code above******* #HH NB - have to update this manually!
rsf_coefs_grass<-rsf_coefs


### Saltmarsh
#HH NB: list of separate columns need to be run in line below: Coastal,Grassland, Saltmarsh, Arable, Other
# Fit habitat model for each habitat to each individual #HH NB this has to be manually changed for each time period AND each habitat per time period
rsffits <- rsfdat  %>% nest(data=-"id") %>%   mutate(mod = map(data, function(x) glm(case_ ~ Saltmarsh, data = x, weight=w,family = binomial)))


## Gary's note: DO NOT open rsffits object from R Studio panel view(rsffits) - keep crashing on 3.6.1 ##HH NB: opens fine in R 4.2.2

# Check goodness of fit #HH NB this is an appendix table
# Running issues originally due to na.action = argument not set. Ran later and daved manually (writeClipboard(as.character(rsf_gof$auc_test)))
# Could integrate into tidy output if needed
rsf_gof <- rsfdat  %>% nest(data=-"id") %>%   mutate(auc_test = map(data, function(x) pROC::auc(pROC::roc(x$case_~(predict(glm(case_ ~ Saltmarsh, data = x, weight=w,family = binomial, na.action=na.exclude), type=c("response"))))))) #edit response var


rsf_gof$auc_test # viewing this as a tab gets slower

#copy the AUC for each bird to clipboard
writeClipboard(as.character(rsf_gof$auc_test))

###beta_sub[beta_sub$id==rsf_gof$id, beta_sub$'Coastal.sediment']

# tidy model outputs
rsffits <- rsffits %>%
  dplyr::mutate(tidy = purrr::map(mod, broom::tidy),
                n = purrr::map(data, nrow))

# Unnest and tidy outputs
rsf_coefs<-rsffits %>% unnest(c(tidy)) %>%  
  select(-(std.error:p.value))

# rm data from coefs object for efficiency
rsf_coefs<-within(rsf_coefs, rm(data))
rsf_coefs<-within(rsf_coefs, rm(mod))

#HH NB: list of separate columns need to be run in line below: Coastal,Grassland, Saltmarsh, Arable, Other
# Name for habitat and ***** repeat code above******* #HH NB - have to update this manually!
rsf_coefs_salt<-rsf_coefs


### Arable
#HH NB: list of separate columns need to be run in line below: Coastal,Grassland, Saltmarsh, Arable, Other
# Fit habitat model for each habitat to each individual #HH NB this has to be manually changed for each time period AND each habitat per time period
rsffits <- rsfdat  %>% nest(data=-"id") %>%   mutate(mod = map(data, function(x) glm(case_ ~ Arable, data = x, weight=w,family = binomial)))


## Gary's note: DO NOT open rsffits object from R Studio panel view(rsffits) - keep crashing on 3.6.1 ##HH NB: opens fine in R 4.2.2

# Check goodness of fit #HH NB this is an appendix table
# Running issues originally due to na.action = argument not set. Ran later and daved manually (writeClipboard(as.character(rsf_gof$auc_test)))
# Could integrate into tidy output if needed
rsf_gof <- rsfdat  %>% nest(data=-"id") %>%   mutate(auc_test = map(data, function(x) pROC::auc(pROC::roc(x$case_~(predict(glm(case_ ~ Arable, data = x, weight=w,family = binomial, na.action=na.exclude), type=c("response"))))))) #edit response var

rsf_gof$auc_test # viewing this as a tab gets slower

#copy the AUC for each bird to clipboard
writeClipboard(as.character(rsf_gof$auc_test))

###beta_sub[beta_sub$id==rsf_gof$id, beta_sub$'Coastal.sediment']

# tidy model outputs
rsffits <- rsffits %>%
  dplyr::mutate(tidy = purrr::map(mod, broom::tidy),
                n = purrr::map(data, nrow))

# Unnest and tidy outputs
rsf_coefs<-rsffits %>% unnest(c(tidy)) %>%  
  select(-(std.error:p.value))

# rm data from coefs object for efficiency
rsf_coefs<-within(rsf_coefs, rm(data))
rsf_coefs<-within(rsf_coefs, rm(mod))

#HH NB: list of separate columns need to be run in line below: Coastal,Grassland, Saltmarsh, Arable, Other
# Name for habitat and ***** repeat code above******* #HH NB - have to update this manually!
rsf_coefs_arable<-rsf_coefs


### Other
#HH NB: list of separate columns need to be run in line below: Coastal,Grassland, Saltmarsh, Arable, Other
# Fit habitat model for each habitat to each individual #HH NB this has to be manually changed for each time period AND each habitat per time period
rsffits <- rsfdat  %>% nest(data=-"id") %>%   mutate(mod = map(data, function(x) glm(case_ ~ Other, data = x, weight=w,family = binomial)))



## Gary's note: DO NOT open rsffits object from R Studio panel view(rsffits) - keep crashing on 3.6.1 ##HH NB: opens fine in R 4.2.2

# Check goodness of fit #HH NB this is an appendix table
# Running issues originally due to na.action = argument not set. Ran later and daved manually (writeClipboard(as.character(rsf_gof$auc_test)))
# Could integrate into tidy output if needed
rsf_gof <- rsfdat  %>% nest(data=-"id") %>%   mutate(auc_test = map(data, function(x) pROC::auc(pROC::roc(x$case_~(predict(glm(case_ ~ Other, data = x, weight=w,family = binomial, na.action=na.exclude), type=c("response"))))))) #edit response var

rsf_gof$auc_test # viewing this as a tab gets slower

#copy the AUC for each bird to clipboard
writeClipboard(as.character(rsf_gof$auc_test))

###beta_sub[beta_sub$id==rsf_gof$id, beta_sub$'Coastal.sediment']

# tidy model outputs
rsffits <- rsffits %>%
  dplyr::mutate(tidy = purrr::map(mod, broom::tidy),
                n = purrr::map(data, nrow))

# Unnest and tidy outputs
rsf_coefs<-rsffits %>% unnest(c(tidy)) %>%  
  select(-(std.error:p.value))

# rm data from coefs object for efficiency
rsf_coefs<-within(rsf_coefs, rm(data))
rsf_coefs<-within(rsf_coefs, rm(mod))

#HH NB: list of separate columns need to be run in line below: Coastal,Grassland, Saltmarsh, Arable, Other
# Name for habitat and ***** repeat code above******* #HH NB - have to update this manually!
rsf_coefs_other<-rsf_coefs 


###
# Combine and save RSF model outputs
rsf_coefs_hab<-bind_rows(rsf_coefs_coast, rsf_coefs_grass, rsf_coefs_salt, rsf_coefs_arable, rsf_coefs_other)

##HH NB - ONCE bound, remove the files from the environment to avoid accidentally including it in the next one
rm(rsf_coefs_coast, rsf_coefs_grass, rsf_coefs_salt, rsf_coefs_arable, rsf_coefs_other)


#setwd("C:/Users/gary.clewley/Desktop/BTO - GDC/2019- Wetland and Marine Team/_NE103 -- Headstarted Curlew tracking/Data/")
setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data/") #HH laptop

save(rsf_coefs_hab, file="NE103_2023 report_RSF_models_cohort2023_OneDay.RData")
save(rsf_coefs_hab, file="NE103_2023 report_RSF_models_cohort2023_OneWeek.RData")
save(rsf_coefs_hab, file="NE103_2023 report_RSF_models_cohort2023_TwoWeeks.RData")
save(rsf_coefs_hab, file="NE103_2023 report_RSF_models_cohort2023_SixWeeks.RData")
save(rsf_coefs_hab, file="NE103_2023 report_RSF_models_cohort2023_July_Dec2023.RData")

save(rsf_coefs_hab, file="NE103_2023 report_RSF_models_cohort2122_Winter_PreBreed.RData")
save(rsf_coefs_hab, file="NE103_2023 report_RSF_models_cohort2122_Spring_T.RData")
save(rsf_coefs_hab, file="NE103_2023 report_RSF_models_cohort2122_Breeding_F.RData")
save(rsf_coefs_hab, file="NE103_2023 report_RSF_models_cohort2122_Breeding_M.RData")
save(rsf_coefs_hab, file="NE103_2023 report_RSF_models_cohort2122_Autumn_T_F.RData")
save(rsf_coefs_hab, file="NE103_2023 report_RSF_models_cohort2122_Autumn_T_M.RData")
save(rsf_coefs_hab, file="NE103_2023 report_RSF_models_cohort2122_Winter_PostBreed.RData")




#### Stat testing RSF - two-stage modeling ####

# Load RSF models
#setwd("C:/Users/gary.clewley/Desktop/BTO - GDC/2019- Wetland and Marine Team/_NE103 -- Headstarted Curlew tracking/Data/")
setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data/") #HH laptop

#load("NE103_2022 report_RSF_models_all.RData")
#load("NE103_2022 report_RSF_models_six weeks.RData")
#load("NE103_2022 report_RSF_models_two weeks.RData")
#load("NE103_2022 report_RSF_models_one week.RData")
#load("NE103_2022 report_RSF_models_0E_22.RData")

load("NE103_2023 report_RSF_models_cohort2023_OneDay.RData")
load("NE103_2023 report_RSF_models_cohort2023_OneWeek.RData")
load("NE103_2023 report_RSF_models_cohort2023_TwoWeeks.RData")
load("NE103_2023 report_RSF_models_cohort2023_SixWeeks.RData")
load("NE103_2023 report_RSF_models_cohort2023_July_Dec2023.RData")

load("NE103_2023 report_RSF_models_cohort2122_Winter_PreBreed.RData")
load("NE103_2023 report_RSF_models_cohort2122_Spring_T.RData")
load("NE103_2023 report_RSF_models_cohort2122_Breeding_F.RData")
load("NE103_2023 report_RSF_models_cohort2122_Breeding_M.RData")
load("NE103_2023 report_RSF_models_cohort2122_Autumn_T_F.RData")
load("NE103_2023 report_RSF_models_cohort2122_Autumn_T_M.RData")
load("NE103_2023 report_RSF_models_cohort2122_Winter_PostBreed.RData")





#### Stat testing RSF - two-stage modeling

library(emmeans)

# Set data
x<-rsf_coefs_hab

# Set factor
x$term<-as.factor(as.character(x$term))

# Filter intercept   #Use the 'estimate' per bird and habitat in x in the appendix table for the beta column
x<- x %>% filter(term!="(Intercept)")

#read out as csv to make it easier to put into appendix table
x_out <- data.frame(x)
library(reshape2)
x_out_id <- unique(x_out$id) 
x_out_wide <- dcast(x_out, id  ~ term, value.var="estimate")
x_out_wide$id<- factor(x_out_wide$id, levels= x_out_id)
x_out_wide <- x_out_wide[,c(1,3,6,4,2,5)]
write_csv(x_out_wide, here("output/myestimates.csv")) # this allows you to read out the output data as a csv for easiest copying to the report


# Run linear model
m1<-lm(x$estimate~x$term)
summary(m1)

# Check contrasts
mytest <- emmeans(m1, ~ term)
mytest.contrast <- contrast(regrid(mytest))
print(mytest.contrast)
write.csv(mytest.contrast, here("output/mytest.csv"), row.names=F) # this allows you to read out the output data as a csv for easiest copying to the report

## Gary's Note - following advice from Fieberg RSF/amt course (2019) but needs considering how to better
# carry out testing when many model fits are poor (and coefficients then include outliers)






## RSS PLOTS
## Set data
#rsf_coefs_hab<-rsf_coefs_hab_6


#### HH NB - Gary's original code
# remove outliers (tbc...) 
# One week
#rsf_coefs_hab<- rsf_coefs_hab %>% filter(id!="Yf(6Y)O/-:Y/m_Sandringham"&id!="Yf(7Y)O/-:Y/m_Sandringham"&id!="Yf(8L)O/-:Y/m_Sandringham")
# Two weeks
#rsf_coefs_hab<- rsf_coefs_hab %>% filter(id!="Yf(8E)O/-:Y/m_Sandringham")
# Six weeks
#rsf_coefs_hab<- rsf_coefs_hab %>% filter(id!="Yf(8X)O/-:Y/m_KenHill"&id!="Yf(8L)O/-:Y/m_Sandringham")
# all data
#rsf_coefs_hab<- rsf_coefs_hab %>% filter(id!="Yf(7K)O/-:Y/m_Sandringham"&id!="Yf(8L)O/-:Y/m_Sandringham"&id!="Yf(8X)O/-:Y/m_KenHill")


####

#HH NB - OUTLIER CHECK AND VIEW IN PLOT CONSTRICTED TO ZOOM IN ON THE OTHER PART OF GRAPH. NOT removed from dataset or model:
#HH trial and final removing of outliers 
#One day - 
    #MAJOR outliers for birds: "Yf(XL)O/-:Y/m_Sandringham", "Yf(YK)O/-:Y/m_Sandringham" , "Yf(XE)O/-:Y/m_Sandringham"
      ##"Yf(YX)O/-:Y/m_Sandringham", "Yf(LU)O/-:Y/m_Sandringham"

#test to filter out for JUST these birds to look at the proportion of fixes in other habitats using rsfdat and rsf_coefs_habs
#These birds spent over 80% of fixes in their first day in arable. So going to remove as outliers
#test <- rsf_coefs_hab %>% filter(id=="Yf(XL)O/-:Y/m_Sandringham" | id=="Yf(YK)O/-:Y/m_Sandringham" | id=="Yf(XE)O/-:Y/m_Sandringham" |
#id=="Yf(YX)O/-:Y/m_Sandringham" | id=="f(LU)O/-:Y/m_Sandringham")

#rsf_coefs_hab<- rsf_coefs_hab %>% filter(id!="Yf(XL)O/-:Y/m_Sandringham" & id!="Yf(YK)O/-:Y/m_Sandringham" & id!="Yf(XE)O/-:Y/m_Sandringham" &
 #                                          id!="Yf(YX)O/-:Y/m_Sandringham" & id!="Yf(LU)O/-:Y/m_Sandringham")


#One week - MAJOR outliers for birds: "Yf(YX)O/-:Y/m_Sandringham"

#test <- rsf_coefs_hab %>% filter(id=="Yf(YX)O/-:Y/m_Sandringham")
#test <- rsfdat %>% filter(id=="Yf(YX)O/-:Y/m_Sandringham") #almost 100% fixes in arable so removed as an outlier

#rsf_coefs_hab<- rsf_coefs_hab %>% filter(id!="Yf(YX)O/-:Y/m_Sandringham")


#Two weeks - MAJOR outliers for birds: "Yf(YX)O/-:Y/m_Sandringham"

#test <- rsf_coefs_hab %>% filter(id=="Yf(YX)O/-:Y/m_Sandringham")
#test <- rsfdat %>% filter(id=="Yf(YX)O/-:Y/m_Sandringham") # 100% fixes in arable so removed as an outlier - notes say this bird was found dead 22/09/2023. (36 days after release). This habitat use suggests that it could have been dead before this? But found in diff habiats in 6wks so maybe not

#rsf_coefs_hab<- rsf_coefs_hab %>% filter(id!="Yf(YX)O/-:Y/m_Sandringham")


#Six weeks - "Yf(YX)O/-:Y/m_Sandringham" left in as the prop of fixes used in different habitats varied not just arable

#test <- rsf_coefs_hab %>% filter(id=="Yf(YX)O/-:Y/m_Sandringham")
#test <- rsfdat %>% filter(id=="Yf(YX)O/-:Y/m_Sandringham") # only 50% fixes in arable so kept in - notes say this bird was found dead 22/09/2023 (36 days after release).


#Winter post-breeding - "Yf(7K)O/-:Y/m_Sandringham"

#test <- rsf_coefs_hab %>% filter(id=="Yf(7K)O/-:Y/m_Sandringham")
#test <- rsfdat %>% filter(id=="Yf(7K)O/-:Y/m_Sandringham") # almost 100% fixes in coastal sediment. Remove trialed:

#rsf_coefs_hab<- rsf_coefs_hab %>% filter(id!="Yf(7K)O/-:Y/m_Sandringham")



###


# Reorder factor levels
rsf_coefs_hab$term<- factor(rsf_coefs_hab$term, levels=c("Saltmarsh", "Coastal", "Arable", "Grassland", "Other"))




# Set mean and sd around individual selection coefficients
d2a <- na.omit(rsf_coefs_hab) %>%
  filter(term!="(Intercept)") %>% 
  mutate(id = factor(id)) %>% group_by(term) %>%
  summarize(
    mean = mean(exp(estimate)),
    ymin = mean - 1.96 * sd(exp(estimate)),
    ymax = mean + 1.96 * sd(exp(estimate)))

# Add column for number of factors in plot
d2a$x <- 1:nrow(d2a) 







options(scipen=3000000)
# Plot (habitat factor listed under term from model outputs)
rsf_coefs_hab %>% filter(term!="(Intercept)") %>%	ggplot(., ) +
  geom_point(aes(x = term, y = exp(estimate), group = id, col = id),
             position = position_dodge(width = 0.7))+
  scale_colour_viridis_d() +
  geom_rect(mapping = aes(xmin = x - .4, xmax = x + .4, ymin = ymin,
                          ymax = ymax), data = d2a, inherit.aes = FALSE,
            fill = "grey90", alpha=0.5) +
  geom_segment(mapping = aes(x = x - .4, xend = x + .4,
                             y = mean, yend = mean), data = d2a, inherit.aes = FALSE,
               linewidth = 1) +
  geom_hline(yintercept = 0, lty = 2) +
  labs(x = "Habitat", y = "Relative Selection Strength") +
  coord_cartesian(ylim=c(-10,30)) +                                         #keep this line in ONLY when need to constrain the y axis
  theme(legend.position="none") +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14),
        strip.text.x = element_text(size = 12, face="bold")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("Six weeks post-release (all birds)")   ## UPDATE MANUALLY #HH NB if cropping to ignore outliers add in (cropped - one outlier outside view)




# Save plot
#setwd("C:/Users/gary.clewley/Desktop/BTO - GDC/2019- Wetland and Marine Team/_NE103 -- Headstarted Curlew tracking/Outputs/")
setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/output/Figures/RSS/") #HH NB laptop
ggsave("NE103_2023_Headtsart CURLE_RSS plot_4_SixWeeks_postrelease_all.jpg", width=15, height=15, units="cm", dpi=300)  ## UPDATE FILENAME








#### Misc plotting ####

#### Colour ring plots (saved from interactive plot)

data <- read.csv("C:/Users/gary.clewley/Desktop/BTO - GDC/2019- Wetland and Marine Team/_NE103 -- Headstarted Curlew tracking/Data/NE103_2022 colour ring sighting map locations.csv")


leaflet(data) %>% addTiles() %>%
       addCircleMarkers(lng = ~longitude, lat = ~latitude, radius =1, opacity=1)










#### UNUSED/TEST CODE ####


# Convert DateTime class
data$DateTime<-as.POSIXct(data$DateTime, format="%Y-%m-%d %H:%M:%S" )





# Issues cleaning data when loaded through BTOTT - loading below works OK but workaround using move:: used for now

# Set repository
repo<-"BTO/NE/Pensthorpe/WWT - Eurasian Curlews - headstarted"
# TagID<-c("Yf(0E)O/-:Y/m", "Yf(3A)O/-:Y/m") # update to individual 

# Set dates manually - error if one NULL and other defined - logged issue in Git
start<-c("2022-07-14 00:00:00") # First release data

##UPDATE TO INDIVIDUAL DATES
#end_2<-c("2021-07-17 23:59:59", "2021-07-31 23:59:59") # First 2 weeks post-release
#end_6<-c("2021-08-14 23:59:59", "2021-08-28 23:59:59") # First 6 weeks post-release

end_all<-c("2022-12-31 23:59:59") # Arbitrary end of calendar year

# Loads for all time period
data_all <- read_track_MB(TagID=TagID,repo=repo,start=start,end=end_all) 
#data_2 <- read_track_MB(TagID=TagID,repo=repo,start=start,end=end_2)
#data_6 <- read_track_MB(TagID=TagID,repo=repo,start=start,end=end_6)

## loads OK but now clean_GPS issue to solve
# data<- read_track_MB(TagID=TagID,repo=repo,start=start,end=end) %>% clean_GPS() # workaround below



















# Set 'Other' habitat as reference level for all habitat model - difference reference levels had large effect on results
# so focal hab model against all other habitats pooled as reference used to determine selection
rsfdat <- within(rsfdat, LCM <- relevel(LCM, ref = "Other"))

m1<-glm(case_ ~ LCM, data = rsfdat %>% filter(id=="Yf(0E)O/-:Y/m"), weight=w,family = binomial)






# Dummy reference variable?  - Not informative 'used' still comes out as positive and messes with weighting in model
dummy<-rsfdat[1:400,]
dummy$id<-c(rep(levels(dummy$id)[1],200), rep(levels(dummy$id)[2],200))
dummy$case_<-c(rep(FALSE, 100), rep(TRUE, 100), rep(FALSE, 100), rep(TRUE, 100))
dummy$x_<-555555
dummy$y_<-333333
dummy$LCM<-"Dummy"
dummy$used<-c(rep(levels(dummy$used)[1],100), rep(levels(dummy$used)[2],100),rep(levels(dummy$used)[1],100), rep(levels(dummy$used)[2],100))

  




## Issues with GOF test for RSF
# Tried extracting individual model tests

## individual

dat<-rsfdat %>% filter(id=="Yf(9J)O/-:Y/m_KenHill")
pROC::auc(pROC::roc(dat$case_~(predict(glm(case_ ~ Coastal, data = dat, weight=w,family = binomial), type=c("response")))))
pROC::auc(pROC::roc(dat$case_~(predict(glm(case_ ~ Arable, data = dat, weight=w,family = binomial), type=c("response")))))
pROC::auc(pROC::roc(dat$case_~(predict(glm(case_ ~ Saltmarsh, data = dat, weight=w,family = binomial), type=c("response")))))
pROC::auc(pROC::roc(dat$case_~(predict(glm(case_ ~ Grassland, data = dat, weight=w,family = binomial), type=c("response")))))
pROC::auc(pROC::roc(dat$case_~(predict(glm(case_ ~ Other, data = dat, weight=w,family = binomial), type=c("response")))))


## Resolved by adding na.action=na.exclude to the rsf_gof<- line within the GLM to ensure predict() output was same length as data













