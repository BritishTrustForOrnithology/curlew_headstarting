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





## Match tide data - categorical 2 hours either side of high/low (from Port-log.net reports) ####

### Load data and set Datetime class ####

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


#2024 
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



# INTERUPTION ####
# to extract the cohort identifer and release site info go to this code: 'headstart_curlew_gps_movements.R' ~ line 76 to get the cohort identifier and site identifiers and save it out as a csv

# RETURN ####











