#### NE103 -- Curlew headstarting project -- using The Wash 2022 and 2023 July-December dataset for wild curlew comparision

## Original code (2021 and 2022) by Gary Clewley
## Updated for 2023 analysis by Hannah Hereward, Katharine Bowgen, Sam Franks (2023-)
## NEW SCRIPT copying relevant code from the 2023 analysis - Hannah Hereward, Katharine Bowgen, Sam Franks (2023-)


# NOTES ####
# Analysis of 2022 and 2023 The Wash dataset from 2022 and 2023 July-December using same time periods as the headstarting analysis 

# Check previous reports for example of previous analytical structure to follow
# Now also includes extracting 'potential breeding season' time periods

# LOAD PACKAGES ####
load_pkg <- rlang::quos(tidyverse,BTOTrackingTools, here, sp, leaflet, terra)  # quos() function to be lazy on "" around each package
lapply(lapply(load_pkg, rlang::quo_name), library, character.only = TRUE)



# SETUP AND CLEAN DATA ####

#read in the wash data here
#washdat <- 


## Make parsable as a Track object by BTOTT package
names(washdat)[names(washdat)=="individual.local.identifier"]<-"TagID"   # Some tags redeployed multiple times
names(washdat)[names(washdat)=="timestamp"]<-"DateTime"
names(washdat)[names(washdat)=="location.long"]<-"longitude"
names(washdat)[names(washdat)=="location.lat"]<-"latitude"
names(washdat)[names(washdat)=="gps.satellite.count"]<-"satellites_used"


summary(washdat)
head(washdat)


### Coerce required trip and gap columns for later functions (not running trip definition for this project as not central place) ####
washdat$tripNo<-1; washdat$gap<-0; washdat$gapsec<-1


## Check summary of data ####
summary(washdat)


##save data out so I don't keep having to read it all back in and do the tide stuff####
saveRDS(washdat, here("data/2025 analysis - The Wash/TheWashData.rds"))


#Extract meta data???

#Create a flag ID column

##Next in order to be able to left join 'data' and 'dt_meta_gsp' a column needs to be created which matches both together.####

#first extract the tag idea from 'data'
TagID <- data.frame(TagID = unique(washdat$TagID))

#from this extract out the unique two letter/number code using str_sub
TagID$flag_id <- str_sub(TagID$TagID, 4,5)



#clean up a few last things: #####
###create a dead or stopped transmitting date column to help assess whether some individuals need to be taken out of some categories due to the number of days the remained alive####




# Tidy surplus columns from move:: direct loading #####
drop_cols<-c("event.id", "visible", "individual.id", "deployment.id", "tag.id", "study.id", "sensor.type.id", "tag.local.identifier", "individual.taxon.canonical.name", "acceleration.raw.x", "acceleration.raw.y", "acceleration.raw.z", "barometric.height", "battery.charge.percent", "battery.charging.current", "gps.hdop", "gps.time.to.fix", "heading", "import.marked.outlier", "light.level", "magnetic.field.raw.x", "magnetic.field.raw.y", "magnetic.field.raw.z", "ornitela.transmission.protocol", "study.name", "sensor.type")
washdat<- washdat %>% select(-!!drop_cols)

summary(washdat)

##save data out ####
#saveRDS(washdat, here("data/2025 analysis - The Wash/TheWashData_cleanedup.rds"))

#read back in
#data <- readRDS(here("data/2025 analysis - The Wash/TheWashData_cleanedup.rds"))


####.####


#which birds should stay in which category? ####
#Before we can use the data for analysis it would be useful to add in information to the whole dataset working out which categories the birds best fit into
#this is because some of the birds died within year of release and subsequent years of release

#NOTE HH additions (14/01/2025) - after discussions with Katharine for year of release birds: we will exclude birds if their fixes do not fully fit within the category 
# EXCEPTIONS: for 6weeks post release we included anything above 5weeks (35days). For all data from release to end of Dec we included all data if fixes were beyond 31st October
#this decisions comes to balance wanting to retain as much data as possible but also the coefficent analysis later on comparing individual birds per time period

#use the past cohort data frame to extract the ones needed for this
past_cohort_behavs_thewash <- past_cohort_behavs %>% filter(!pastcohort_behaviours %in% c("6 Winter pre-breeding" ,"7 Spring fuzzy" )) %>% filter(year != 2024)

ifelse(washdat$TimeDate <  paste(as.POSIXct(past_cohort_behavs_thewash$maxdate_pastcohort_behav, tz="UTC")),
       ifelse(washdat$TimeDate <  paste(as.POSIXct(past_cohort_behavs_thewash$maxdate_pastcohort_behav, tz="UTC")),
         
       ))

#year + 1 to get the first year the cohort returns
year_return <- year_released+1

#sex
MF <- dat.in$sex

#added this in to catch the few birds which had an unknown sex # HANNAH TO CHECK WITH KATHARINE whether these should be categorised as M or F for the purposes of the dates
MF <- ifelse(MF == "U", "F", MF)

#filter the past cohort behaviours to year+1 and the sex
past_cohort_filter <- past_cohort_behavs %>% filter(year>year_released & sex == MF)


#run a which query to find the row that is the last row that include the date
dt_meta_gsp_TagID_update$max_category[dt_meta_gsp_TagID_update$flag_id==id] <- past_cohort_filter$label_year[min(which(past_cohort_filter$maxdate_pastcohort_behav > dat.in$max_date_time_transmiss))]

lab <- dt_meta_gsp_TagID_update$max_category[dt_meta_gsp_TagID_update$flag_id==id]

#extracting the date for the respective label
dt_meta_gsp_TagID_update$max_category_date[dt_meta_gsp_TagID_update$flag_id==id] <- paste(as.POSIXct(past_cohort_filter$maxdate_pastcohort_behav[past_cohort_filter$label_year == lab], tz="UTC"))




###Once all the tweaking is done - save this out as the final dataset!  ####
#saveRDS(washdat, (here("data/2025 analysis - The Wash/TheWashData_cleanedup_******.rds")))



####.####


# LOAD PACKAGES ####
load_pkg <- rlang::quos(tidyverse,BTOTrackingTools, here, sp, leaflet, terra)  # quos() function to be lazy on "" around each package
lapply(lapply(load_pkg, rlang::quo_name), library, character.only = TRUE)


#START HERE - TO CREATE THE BTO TRACKS - Once all data is correct and cleaned and combined above you can start from here ####
#read back in the data ####
#washdat <- readRDS(here("data/2025 analysis - The Wash/TheWashData_cleanedup_******.rds"))

summary(washdat)




#NEXT create separate files per year - for cohort released and remaining from previous cohorts, all the split by stage ####

#Using the same name as the code already set up:
data_tt_W <- washdat


nyears_W <- c("2022", "2023")
cohort_periods_W <- c( )


#FEMALE OR UNKNOWN#

#filter the data
dat.keep_F <- dat.in_pastcohort %>% filter(sex == "F"| sex == "U") %>% droplevels()
summary(dat.keep_F)

#filter to past_cohort_behavs dataframe to get the year and sex time periods
past_cohort_behavs_year <- past_cohort_behavs %>% filter(sex=="F" & year== nyr)

#use the table above to then create a list of labels
past_cohort_periods_F <- past_cohort_behavs_year$label_year







