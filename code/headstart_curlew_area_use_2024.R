#### NE103 -- Curlew headstarting -- post-release tracking

## Original code (2021 and 2022) by Gary Clewley
## Updated for 2023 analysis by Hannah Hereward, Katharine Bowgen, Sam Franks (2023-)
## NEW SCRIPT copying relevant code from the 2023 analysis - Hannah Hereward, Katharine Bowgen, Sam Franks (2023-)


# NOTES ####
# Analysis of 2024 deployments and any remaining individuals from 2021, 2022 and 2023 still transmitting in 2024

# Check previous reports for example of previous analytical structure to follow
# Now also includes extracting 'potential breeding season' data for separate analysis for 2021/2022/2023 deployments using appropriate ranges 

# Tide data from Oceanwise - Port-log.net reports
# go to https://thewash.port-log.net/live/History.php?Site=49&Dataset=Tides
# click Reports > Daily High and Low
# select the monthly periods and download all required (typically July-December inclusive) 
# save these as PDFs and then copy them over to an excel spreadsheet and using 'text to columns' to deliminate out the columns.
# Save the csv and store a copy on googledrive (2023: 3.Data --< bulldog_bcn_ ...)


# SETUP AND CLEAN DATA ####

## LOAD PACKAGES
load_pkg <- rlang::quos(tidyverse,BTOTrackingTools, here, sp, leaflet, terra)  # quos() function to be lazy on "" around each package
lapply(lapply(load_pkg, rlang::quo_name), library, character.only = TRUE)


## LOAD MOVEBANK DATA

# Set login credentials #HH NB: if this is the first time you're downloading data then need to download the data on the movebank webpage to accept the licencing agreement. Then this should work.
login<-move::movebankLogin() #note you need to fill in your username and password in the console)


# (note from Gary) Ongoing issues using BTOTT to load and clean data. Workaround to use move:: directly
data <- move::getMovebankLocationData(study="BTO-NE-Pensthorpe - Eurasian Curlews - headstarted",
                                      sensorID=653, login = login)

## Make parsable as a Track object by BTOTT package
names(data)[names(data)=="individual.local.identifier"]<-"TagID"   # Some tags redeployed multiple times
names(data)[names(data)=="timestamp"]<-"DateTime"
names(data)[names(data)=="location.long"]<-"longitude"
names(data)[names(data)=="location.lat"]<-"latitude"
names(data)[names(data)=="gps.satellite.count"]<-"satellites_used"


summary(data)
head(data)


## Match tide data - categorical 2 hours either side of high/low (from Port-log.net reports) ####

### Load data and set Datetime class ####

#NOTE HH has added in December 2021 which was missing in the original data 
tide_dat_21 <- read_csv(here("data/Wash_tide_data_Bulldog_July_December_2021.csv"), 
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


#2024 #currently only to the end of 7th December 2024. Missing rest of December 2024! 
tide_dat_24 <- read_csv(here("data/Wash_tide_data_Bulldog_July_December_2024.csv"), 
                        col_types = cols(Observed_DateTime = col_datetime(format = "%d/%m/%Y %H:%M"), Predicted_DateTime = col_datetime(format = "%d/%m/%Y %H:%M")))
summary(tide_dat_24)



#bind all 4 years of tide data together
tide_dat <- rbind(tide_dat_21, tide_dat_22, tide_dat_23[c(1:6)], tide_dat_24[c(1:6)])
summary(tide_dat)


#save out this combined 4 year tide data
#write_csv(tide_dat, "data/Wash_tide_data_Bulldog_July_Nov2021_July_December_2021_2024.csv")


tide_dat <- read_csv(here("data/Wash_tide_data_Bulldog_July_Nov2021_July_December_2021_2024.csv")) 
summary(tide_dat)

#filter out NAs
tide_dat <- tide_dat %>% filter(!is.na(tide_dat$Observed_Height))



### Find closest match to trk timestamp (package MALDIquant) ####
closest<-MALDIquant::match.closest(data$DateTime, tide_dat$Observed_DateTime)

# Extract nearest tide time, high/low, height 
data$tide_time<-tide_dat$Observed_DateTime[closest]; data$tide_diff<-difftime(data$DateTime, data$tide_time)
data$tide<-tide_dat$Tide[closest];data$tide_height<-tide_dat$Observed_Height[closest]

# Create categorical tide factor with desired threshold; below annotated fixes 2 hour either side of high or low tide
data$tide<-as.factor(ifelse(data$tide_diff<7201 & data$tide_diff>-7201,data$tide, "NA"))



### Coerce required trip and gap columns for later functions (not running trip definition for this project as not central place) ####
data$tripNo<-1; data$gap<-0; data$gapsec<-1


## Check summary of data ####
summary(data)
summary(as.factor(data$tide_diff))


##save data out so I don't keep having to read it all back in and do the tide stuff####
saveRDS(data, here("data/data_with_tide_2021_2024.rds"))




#Extract curlew metadata #####

##Read in the summary metadata table from googledrive folder 3:Data####
dt_meta <- as_tibble(read.csv(here("data/headstart_curlew_individual_metadata.csv")))

#filter for just GPS birds
dt_meta_gsp <- dt_meta %>% filter(dt_meta$tag_gps_radio_none =="gps")


#create a 'site' column by removing the number from the release_location using 'gsub'
dt_meta_gsp$release_site <- gsub(" 1","", as.character(dt_meta_gsp$release_location))
dt_meta_gsp$release_site <- gsub(" 2","", as.character(dt_meta_gsp$release_site))



##Next in order to be able to left join 'data' and 'dt_meta_gsp' a column needs to be created which matches both together.####

#first extract the tag idea from 'data'
TagID <- data.frame(TagID = unique(data$TagID))

#from this extract out the unique two letter/number code using str_sub
TagID$flag_id <- str_sub(TagID$TagID, 4,5)

#left_join the two metadata tables together
dt_meta_gsp_TagID <- dt_meta_gsp %>% left_join(TagID, by=join_by(flag_id))

#final release site column here - to keep the two Sandringham sites separate because the habitat is so different but merge the Ken Hill sites as their habitat is so similar
dt_meta_gsp_TagID$release_site_final <- ifelse(dt_meta_gsp_TagID$release_location =="Sandringham 1", "Sandringham 1",
                                               ifelse(dt_meta_gsp_TagID$release_location=="Sandringham 2", "Sandringham 2", "Ken Hill"))




##Add in here two final columns for cohorts and number of days post release####

#cohort - work out how many cohorts there were and which ones need combining together 
    #2021 = 
    #2022 = 
    #2023 = cohort 1 separate, then combine remaining cohorts
    #2024 = 

table(dt_meta_gsp_TagID$year, dt_meta_gsp_TagID$cohort_num)

dt_meta_gsp_TagID$cohort_analysis[dt_meta_gsp_TagID$year==2021] <- dt_meta_gsp_TagID$cohort_num[dt_meta_gsp_TagID$year==2021]
dt_meta_gsp_TagID$cohort_analysis[dt_meta_gsp_TagID$year==2022] <- dt_meta_gsp_TagID$cohort_num[dt_meta_gsp_TagID$year==2022]
dt_meta_gsp_TagID$cohort_analysis[dt_meta_gsp_TagID$year==2023] <- ifelse(dt_meta_gsp_TagID$cohort_num[dt_meta_gsp_TagID$year==2023] == 1, 1, 2)
dt_meta_gsp_TagID$cohort_analysis[dt_meta_gsp_TagID$year==2024] <- ifelse(dt_meta_gsp_TagID$cohort_num[dt_meta_gsp_TagID$year==2024] == 4, 4, 
                                                                          ifelse(dt_meta_gsp_TagID$cohort_num[dt_meta_gsp_TagID$year==2024] == 5, 4, dt_meta_gsp_TagID$cohort_num[dt_meta_gsp_TagID$year==2024]))

#make it into a factor rather than a number
dt_meta_gsp_TagID$cohort_analysis <- as.factor(dt_meta_gsp_TagID$cohort_analysis)

summary(dt_meta_gsp_TagID)



#add in 1 day, 1 week, 2 weeks, 6 week dates post release
#create a as.posixct datetime column
dt_meta_gsp_TagID$release_date_time <- as.POSIXct(dt_meta_gsp_TagID$release_date, format = "%d/%m/%Y", usetz=T, tz="UTC")

#one day according to Garry's 2022 code. Was to the end of the next day up until midnight = one full day after release e.g. release date 20/07 : 21/07 at 23:59:59
dt_meta_gsp_TagID$Date_1d <- dt_meta_gsp_TagID$release_date_time + (86400+86399)


#one week in my mind means 7 full days after release  e.g.release date 20/07 : 27/07 at 23:59:59
dt_meta_gsp_TagID$Date_1w <- dt_meta_gsp_TagID$release_date_time + ((86400*7)+86399)

#two weeks in my mind means 14 full days after release 
dt_meta_gsp_TagID$Date_2w <- dt_meta_gsp_TagID$release_date_time + ((86400*14)+86399)

#six weeks in my mind means 42 full days after release 
dt_meta_gsp_TagID$Date_6w <- dt_meta_gsp_TagID$release_date_time + ((86400*42)+86399)



## Finally do a left_join on the dataset using the TagID as the join_by so that the meta data is populated for each GPS fix
data <- data %>% left_join(dt_meta_gsp_TagID, by=join_by(TagID))

summary(data)


# Tidy surplus columns from move:: direct loading
drop_cols<-c("event.id", "visible", "individual.id", "deployment.id", "tag.id", "study.id", "sensor.type.id", "tag.local.identifier", "individual.taxon.canonical.name", "acceleration.raw.x", "acceleration.raw.y", "acceleration.raw.z", "barometric.height", "battery.charge.percent", "battery.charging.current", "gps.hdop", "gps.time.to.fix", "heading", "import.marked.outlier", "light.level", "magnetic.field.raw.x", "magnetic.field.raw.y", "magnetic.field.raw.z", "ornitela.transmission.protocol", "study.name", "sensor.type")
data<- data %>% select(-!!drop_cols)



##save data out ####
#saveRDS(data, here("data/data_withcohorts_release_sites_2021_2024.rds"))



####.####

#START HERE - Once all data is correct and cleaned and combined above you can start from here ####

#read back in ####
#data <- readRDS(here("data/data_withcohorts_release_sites_2021_2024.rds"))

#summary(data)


## Convert to BTOTT Track object
data_tt<-Track(data) 
data_tt<-clean_GPS(data_tt, drop_sats = 3, Thres = 30, GAP = 28800)
#HH NB: new warning messages about "flt_switch" for each bird - an error from the Track(data) function with is a BTOTT function
#See messages from Chris T about this but the summary is it is a flag option for clean_GPS - when importing data into movebank you can add a 
#column to tell you if it is dodgy data or not and then add a second column to clean it/remove it... as the dataset I am working with doesn't have this column
# I can disregard this warning


# Set ID factor
data_tt$TagID<-as.factor(as.character(data_tt$TagID)) 

#try plotting all the data
plot(data_tt$longitude, data_tt$latitude)


#LINE 285#
