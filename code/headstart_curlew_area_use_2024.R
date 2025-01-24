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


# LOAD PACKAGES ####
load_pkg <- rlang::quos(tidyverse,BTOTrackingTools, here, sp, leaflet, terra)  # quos() function to be lazy on "" around each package
lapply(lapply(load_pkg, rlang::quo_name), library, character.only = TRUE)



# SETUP AND CLEAN DATA ####

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

#NOTE HH has added in December 2021 which was missing in the original data - although NOTE that not all of December data is present! 
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


#2024 #currently only to the end of 7th December 2024. Missing rest of December 2024! #UPDATE missing due to the sensor breaking and it not being fixed yet
tide_dat_24 <- read_csv(here("data/Wash_tide_data_Bulldog_July_December_2024.csv"), 
                        col_types = cols(Observed_DateTime = col_datetime(format = "%d/%m/%Y %H:%M"), Predicted_DateTime = col_datetime(format = "%d/%m/%Y %H:%M")))
summary(tide_dat_24)



#bind all 4 years of tide data together
tide_dat <- rbind(tide_dat_21, tide_dat_22, tide_dat_23[c(1:6)], tide_dat_24[c(1:6)])
summary(tide_dat)


#save out this combined 4 year tide data
#write_csv(tide_dat, here("data/2025 analysis/Wash_tide_data_Bulldog_July_Nov2021_July_December_2021_2024.csv"))


tide_dat <- read_csv(here("data/2025 analysis/Wash_tide_data_Bulldog_July_Nov2021_July_December_2021_2024.csv")) 
summary(tide_dat)

#filter out NAs
tide_dat <- tide_dat %>% filter(!is.na(tide_dat$Observed_Height))
summary(tide_dat)


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
saveRDS(data, here("data/2025 analysis/data_with_tide_2021_2024.rds"))




#Extract curlew metadata #####

##Read in the summary metadata table from googledrive folder 3:Data####
dt_meta <- as_tibble(read.csv(here("data/2025 analysis/headstart_curlew_individual_metadata.csv")))

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




##Add in here three final columns for: cohorts and number of days post release and deaddate ####

###cohort - work out how many cohorts there were and which ones need combining together ####
    #2021 = keep with all cohorts in meta data file
    #2022 = keep with all cohorts in meta data file
    #2023 = cohort 1 separate, then combine remaining cohorts
    #2024 =  >7days difference in release for a cohort should be assessed separately whilst <7 days together. This means 1,2,3 separately and 4+5 together

table(dt_meta_gsp_TagID$year, dt_meta_gsp_TagID$cohort_num)

dt_meta_gsp_TagID$cohort_analysis <- NA

dt_meta_gsp_TagID$cohort_analysis[dt_meta_gsp_TagID$year==2021] <- dt_meta_gsp_TagID$cohort_num[dt_meta_gsp_TagID$year==2021]
dt_meta_gsp_TagID$cohort_analysis[dt_meta_gsp_TagID$year==2022] <- dt_meta_gsp_TagID$cohort_num[dt_meta_gsp_TagID$year==2022]
dt_meta_gsp_TagID$cohort_analysis[dt_meta_gsp_TagID$year==2023] <- ifelse(dt_meta_gsp_TagID$cohort_num[dt_meta_gsp_TagID$year==2023] == 1, 1, 2)
dt_meta_gsp_TagID$cohort_analysis[dt_meta_gsp_TagID$year==2024] <- ifelse(dt_meta_gsp_TagID$cohort_num[dt_meta_gsp_TagID$year==2024] == 4, 4, 
                                                                          ifelse(dt_meta_gsp_TagID$cohort_num[dt_meta_gsp_TagID$year==2024] == 5, 4, dt_meta_gsp_TagID$cohort_num[dt_meta_gsp_TagID$year==2024]))

#make it into a factor rather than a number
dt_meta_gsp_TagID$cohort_analysis <- as.factor(dt_meta_gsp_TagID$cohort_analysis)

summary(dt_meta_gsp_TagID)



###number of days post release - add in 1 day, 1 week, 2 weeks, 6 week dates post release####
#create a as.posixct datetime column
dt_meta_gsp_TagID$release_date_time <- as.POSIXct(dt_meta_gsp_TagID$release_date, format = "%d/%m/%Y", usetz=T, tz="UTC")


#one day according to Garry's 2022 code. Was to the end of the next day up until midnight = one full day after release e.g. release date 20/07 : 21/07 at 23:59:59
dt_meta_gsp_TagID$Date_1d <- dt_meta_gsp_TagID$release_date_time + (86400+86399)
#add in extra line to get the hours in this category
dt_meta_gsp_TagID$Date_1d_time <- difftime(dt_meta_gsp_TagID$Date_1d,dt_meta_gsp_TagID$release_date_time, units="days") 


#one week in my mind means 7 full days after release  e.g.release date 20/07 : 27/07 at 23:59:59
dt_meta_gsp_TagID$Date_1w <- dt_meta_gsp_TagID$release_date_time + ((86400*7)+86399)
#add in extra line to get the hours in this category
dt_meta_gsp_TagID$Date_1w_time <- difftime(dt_meta_gsp_TagID$Date_1w,dt_meta_gsp_TagID$release_date_time, units="days") 


#two weeks in my mind means 14 full days after release 
dt_meta_gsp_TagID$Date_2w <- dt_meta_gsp_TagID$release_date_time + ((86400*14)+86399)
#add in extra line to get the hours in this category
dt_meta_gsp_TagID$Date_2w_time <- difftime(dt_meta_gsp_TagID$Date_2w,dt_meta_gsp_TagID$release_date_time, units="days") 


#six weeks in my mind means 42 full days after release 
dt_meta_gsp_TagID$Date_6w <- dt_meta_gsp_TagID$release_date_time + ((86400*42)+86399)
#add in extra line to get the hours in this category
dt_meta_gsp_TagID$Date_6w_time <- difftime(dt_meta_gsp_TagID$Date_6w,dt_meta_gsp_TagID$release_date_time, units="days") 


#July-Dec - release date to the year decemeber date
dt_meta_gsp_TagID$Date_J_D_time <- difftime(as.POSIXct(ifelse(dt_meta_gsp_TagID$year == 2021, "2021-12-31 23:59:59",
                              ifelse(dt_meta_gsp_TagID$year == 2022, "2022-12-31 23:59:59", 
                                     ifelse(dt_meta_gsp_TagID$year == 2023, "2023-12-31 23:59:59", "2024-12-31 23:59:59"))), tz="UTC") ,dt_meta_gsp_TagID$release_date_time, units="days") 



###create a dead or stopped transmitting date column to help assess whether some individuals need to be taken out of some categories due to the number of days the remained alive####
#copy the 'state' column into a new column due to one having an unknown death date
dt_meta_gsp_TagID$dead_date <- dt_meta_gsp_TagID$state

#update the one cell that has 'Autumn 2024' based on the notes from the meta data table 
dt_meta_gsp_TagID$dead_date[dt_meta_gsp_TagID$flag_id=="LU"] <- "26/10/2024"

#create a separate last transmission column from the ones in the Fate column that are 'unknown' with no date in 'state'
#dt_meta_gsp_TagID$last_transmiss <- NA
dt_meta_gsp_TagID$last_transmiss[dt_meta_gsp_TagID$flag_id == "0E"] <- "10/06/2023"
dt_meta_gsp_TagID$last_transmiss[dt_meta_gsp_TagID$flag_id == "3K"] <- "28/07/2021"
dt_meta_gsp_TagID$last_transmiss[dt_meta_gsp_TagID$flag_id == "7Y"] <-  "14/12/2022" #updated from dataset, last transmission in metatable: "22/02/2023"
dt_meta_gsp_TagID$last_transmiss[dt_meta_gsp_TagID$flag_id == "7E"] <- "29/10/2022" #updated from dataset, last transmission in metatable: "16/04/2023"
dt_meta_gsp_TagID$last_transmiss[dt_meta_gsp_TagID$flag_id == "8E"] <- "15/12/2022"
dt_meta_gsp_TagID$last_transmiss[dt_meta_gsp_TagID$flag_id == "7U"] <- "24/09/2022"



#create a dead_date_no_t_time column combining both dead date and last transmission
dt_meta_gsp_TagID$dead_no_t_date_time <- paste(dt_meta_gsp_TagID$dead_date, dt_meta_gsp_TagID$last_transmiss)

dt_meta_gsp_TagID$dead_no_t_date_time <- as.POSIXct(dt_meta_gsp_TagID$dead_no_t_date_time, format = "%d/%m/%Y", usetz=T, tz="UTC")

#minus the dead date time from the release date time to get a days alive 
dt_meta_gsp_TagID$daysalive <- difftime(as.POSIXct(dt_meta_gsp_TagID$dead_no_t_date_time,  tz="UTC"), as.POSIXct(dt_meta_gsp_TagID$release_date_time, tz="UTC"), units = "days")


#save the meta data out:
#write.csv(dt_meta_gsp_TagID, here("data/2025 analysis/metadata_TagID_deaddates_notransmision_2021_2024.csv"), row.names = F)

#dt_meta_gsp_TagID<-read.csv(here("data/2025 analysis/metadata_TagID_deaddates_notransmision_2021_2024.csv"), header=T)


## Finally do a left_join on the dataset using the TagID as the join_by so that the meta data is populated for each GPS fix ####

#read data back in
data <- readRDS(here("data/2025 analysis/data_with_tide_2021_2024.rds"))


#left join
data <- data %>% left_join(dt_meta_gsp_TagID, by=join_by(TagID))

summary(data)


# Tidy surplus columns from move:: direct loading
drop_cols<-c("event.id", "visible", "individual.id", "deployment.id", "tag.id", "study.id", "sensor.type.id", "tag.local.identifier", "individual.taxon.canonical.name", "acceleration.raw.x", "acceleration.raw.y", "acceleration.raw.z", "barometric.height", "battery.charge.percent", "battery.charging.current", "gps.hdop", "gps.time.to.fix", "heading", "import.marked.outlier", "light.level", "magnetic.field.raw.x", "magnetic.field.raw.y", "magnetic.field.raw.z", "ornitela.transmission.protocol", "study.name", "sensor.type")
data<- data %>% select(-!!drop_cols)

summary(data)

##save data out ####
#saveRDS(data, here("data/2025 analysis/data_withcohorts_release_sites_2021_2024.rds"))

#read back in
#data <- readRDS(here("data/2025 analysis/data_withcohorts_release_sites_2021_2024.rds"))


####.####


#which birds should stay in which category? ####
#Before we can use the data for analysis it would be useful to add in information to the whole dataset working out which categories the birds best fit into
  #this is because some of the birds died within year of release and subsequent years of release

  #NOTE HH additions (14/01/2025) - after discussions with Katharine for year of release birds: we will exclude birds if their fixes do not fully fit within the category 
            # EXCEPTIONS: for 6weeks post release we included anything above 5weeks (35days). For all data from release to end of Dec we included all data if fixes were beyond 31st October
            #this decisions comes to balance wanting to retain as much data as possible but also the coefficent analysis later on comparing individual birds per time period


#using the last datetime of transmission
currcohorts <- data %>%
  group_by(year, TagID, flag_id, sex, release_date, cohort_analysis, daysalive) %>%
  summarize(max_date_time_transmiss = max(DateTime))

currcohorts$release_date_posi <- as.POSIXct(currcohorts$release_date, format="%d/%m/%Y", tz="UTC")

currcohorts$datediff <- as.POSIXct(currcohorts$max_date_time_transmiss, tz="UTC") - as.POSIXct(currcohorts$release_date_posi, format="%Y-%m-%d", tz="UTC")


#inspect any differences between daysalive and datediff and updated accordingly above in "dt_meta_gsp_TagID"
#combine daysalive and datediff 
currcohorts$daysalive_transmit <- ifelse(!is.na(currcohorts$daysalive),currcohorts$daysalive,currcohorts$datediff )


#final corrections to be made are for ones that have died but continued transmitting beyond the known death date:

#re run the as.posixct to set it back into the correct datetime format
dt_meta_gsp_TagID$dead_no_t_date_time <- as.POSIXct(dt_meta_gsp_TagID$dead_no_t_date_time, format = "%Y-%m-%d", usetz=T, tz="UTC")

currcohorts$max_date_time_transmiss[currcohorts$flag_id=="JJ"] <- dt_meta_gsp_TagID$dead_no_t_date_time[dt_meta_gsp_TagID$flag_id=="JJ"]+86399
currcohorts$max_date_time_transmiss[currcohorts$flag_id=="NH"] <- dt_meta_gsp_TagID$dead_no_t_date_time[dt_meta_gsp_TagID$flag_id=="NH"]+86399
currcohorts$max_date_time_transmiss[currcohorts$flag_id=="NT"] <- dt_meta_gsp_TagID$dead_no_t_date_time[dt_meta_gsp_TagID$flag_id=="NT"]+86399
currcohorts$max_date_time_transmiss[currcohorts$flag_id=="PU"] <- dt_meta_gsp_TagID$dead_no_t_date_time[dt_meta_gsp_TagID$flag_id=="PU"]+86399


#Save it 
#write.csv(currcohorts, here("data/2025 analysis/current_cohort_maxdatetime_2021_2024.csv"), row.names = F)

#full join the currcohorts to dt_meta_gsp_TagID
dt_meta_gsp_TagID_update <- dt_meta_gsp_TagID %>% full_join(currcohorts, by=join_by(year, flag_id, sex, release_date, cohort_analysis, daysalive))
summary(dt_meta_gsp_TagID_update)
head(dt_meta_gsp_TagID_update)


#find the last category the bird has data in ####

#read in csv which summaries all the categories for past cohort behaviours and max dates
past_cohort_behavs <- read.csv(here("data/pastcohort_behaviours_maxdates.csv"), header=T)

#add in a unique label ID
past_cohort_behavs$label_year <- paste(past_cohort_behavs$pastcohort_behaviours, past_cohort_behavs$year)

#make the date time column a posi
past_cohort_behavs$maxdate_pastcohort_behav <- as.POSIXct(past_cohort_behavs$maxdate_pastcohort_behav, format = "%d/%m/%Y %H:%M:%S", tz="UTC")




###LOOP 1 combined with LOOP 2 - uses max category, so would include birds even if their fixes stop before the max end of that time period  & creates MIN category- so doesn't include birds if their fixes stop before the end of the max end of that time period ####

#additional data frame to summarises the number of days per time period - if it's useful to check the number of days per time period
#time_period_1d_JD <-data.frame(rowID = c(1:6), period = c("0 Days", "1 One Day"  ,  "2 One Week"  , "3 Two Weeks" ,  "4 Six Weeks"   , "5 End of December" ), time_days = c("0", unique(dt_meta_gsp_TagID_update$Date_1d_time), unique(dt_meta_gsp_TagID_update$Date_1w_time), unique(dt_meta_gsp_TagID_update$Date_2w_time), unique(dt_meta_gsp_TagID_update$Date_6w_time), "100"))

#add a blank column into the metadata dataset for category name and associated date so that the results can be read in to it for both max category and min category:
dt_meta_gsp_TagID_update$max_category <- NA
dt_meta_gsp_TagID_update$max_category_date <- NA

dt_meta_gsp_TagID_update$min_category <- NA
dt_meta_gsp_TagID_update$min_category_date <- NA


#set up a loop to run through each of the tagged birds in turn. First to check if they fit into the first year categories. 
        #Then how many further year categories did their transmissions remain in
for(i in 1:nrow(dt_meta_gsp_TagID_update)){
  
  id <- dt_meta_gsp_TagID_update$flag_id[i]
  
  dat.in <- dt_meta_gsp_TagID_update[i,]
  
    #year released
  year_released <- dat.in$year
  
  #december date according to the year of release
  decdate <-  as.POSIXct(ifelse(year_released == 2021, "2021-12-31 23:59:59",
                 ifelse(year_released == 2022, "2022-12-31 23:59:59", 
                ifelse(year_released == 2023, "2023-12-31 23:59:59", "2024-12-31 23:59:59"))), tz="UTC")
  
  octdate <- as.POSIXct(ifelse(year_released == 2021, "2021-10-31 23:59:59",
                               ifelse(year_released == 2022, "2022-10-31 23:59:59", 
                                      ifelse(year_released == 2023, "2023-10-31 23:59:59", "2024-10-31 23:59:59"))), tz="UTC")
  
  
  
  #a set of ifelse's to run through the dates based on the 1day, 1week, 2weeks, 6weeks and end of December post release
  dt_meta_gsp_TagID_update$max_category[dt_meta_gsp_TagID_update$flag_id==id] <-  ifelse(dat.in$max_date_time_transmiss <= dat.in$release_date_time, "Pre Release", 
         ifelse(dat.in$max_date_time_transmiss<= dat.in$Date_1d, "1 One Day",
                ifelse(dat.in$max_date_time_transmiss <= dat.in$Date_1w, "2 One Week",
                       ifelse(dat.in$max_date_time_transmiss <= dat.in$Date_2w, "3 Two Weeks",
                              ifelse(dat.in$max_date_time_transmiss <= dat.in$Date_6w, "4 Six Weeks", 
                                     ifelse(dat.in$max_date_time_transmiss <= decdate, "5 End of December", "Post December"))))))
  
  dt_meta_gsp_TagID_update$max_category_date[dt_meta_gsp_TagID_update$flag_id==id] <-  ifelse(dat.in$max_date_time_transmiss <= dat.in$release_date, NA, 
                                                                                         ifelse(dat.in$max_date_time_transmiss<= dat.in$Date_1d, paste(as.POSIXct(dat.in$Date_1d, tz="UTC")),
                                                                                                ifelse(dat.in$max_date_time_transmiss <= dat.in$Date_1w, paste(as.POSIXct(dat.in$Date_1w, tz="UTC")),
                                                                                                       ifelse(dat.in$max_date_time_transmiss <= dat.in$Date_2w, paste(as.POSIXct(dat.in$Date_2w, tz="UTC")),
                                                                                                              ifelse(dat.in$max_date_time_transmiss <= dat.in$Date_6w, paste(as.POSIXct(dat.in$Date_6w,  tz="UTC")),
                                                                                                                     ifelse(dat.in$max_date_time_transmiss <= decdate, paste(as.POSIXct(decdate, tz="UTC")), "Post December"))))))
  
  
  
  #Then this if loop is in the check if the bird survived to 'post december' then runs through more code to extract the appropriate rows in the past cohort behaviour csv 
    #then uses a which query to find the date closest to the last date of transmission
  if(dt_meta_gsp_TagID_update$max_category[dt_meta_gsp_TagID_update$flag_id==id] == "Post December"){
    
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
    
    
  }

  
  
  
  #LOOP 2 for min category:
  
  #a set of ifelse's to run through the dates based on the 1day, 1week, 2weeks, 6weeks and end of December post release
  dt_meta_gsp_TagID_update$min_category[dt_meta_gsp_TagID_update$flag_id==id] <-  ifelse(dat.in$max_date_time_transmiss <= dat.in$release_date_time, "Pre Release", 
                                                                                         ifelse(dat.in$max_date_time_transmiss<= dat.in$Date_1d, "0 Days",
                                                                                                ifelse(dat.in$max_date_time_transmiss<= dat.in$Date_1w, "1 One Day",
                                                                                                       ifelse(dat.in$max_date_time_transmiss <= dat.in$Date_2w, "2 One Week",
                                                                                                              ifelse(dat.in$datediff < 35 & dat.in$max_date_time_transmiss <= dat.in$Date_6w,  "3 Two Weeks",
                                                                                                                     ifelse(dat.in$datediff >= 35 & dat.in$max_date_time_transmiss <= dat.in$Date_6w, "4 Six Weeks",
                                                                                                                            
                                                                                                                            ifelse(dat.in$max_date_time_transmiss <= octdate, "4 Six Weeks",  
                                                                                                                                   ifelse(dat.in$max_date_time_transmiss <= decdate, "5 End of December", "Post December"))))))))
  
  dt_meta_gsp_TagID_update$min_category_date[dt_meta_gsp_TagID_update$flag_id==id] <-  ifelse(dat.in$max_date_time_transmiss <= dat.in$release_date, NA, 
                                                                                              ifelse(dat.in$max_date_time_transmiss<= dat.in$Date_1d, NA,
                                                                                                     ifelse(dat.in$max_date_time_transmiss<= dat.in$Date_1w, paste(as.POSIXct(dat.in$Date_1d, tz="UTC")),
                                                                                                            ifelse(dat.in$max_date_time_transmiss <= dat.in$Date_2w, paste(as.POSIXct(dat.in$Date_1w, tz="UTC")),
                                                                                                                   
                                                                                                                   ifelse(dat.in$datediff < 35 & dat.in$max_date_time_transmiss <= dat.in$Date_6w, paste(as.POSIXct(dat.in$Date_2w, tz="UTC")),
                                                                                                                          ifelse(dat.in$datediff >= 35 & dat.in$max_date_time_transmiss <= dat.in$Date_6w, paste(as.POSIXct(dat.in$Date_6w, tz="UTC")),
                                                                                                                                 ifelse(dat.in$max_date_time_transmiss <= octdate, paste(as.POSIXct(dat.in$Date_6w, tz="UTC")),
                                                                                                                                        ifelse(dat.in$max_date_time_transmiss <= decdate, paste(as.POSIXct(decdate, tz="UTC")), "Post December"))))))))
  
  
  
  #Then this if loop is in the check if the bird survived to 'post december' then runs through more code to extract the appropriate rows in the past cohort behaviour csv 
  #then uses a which query to find the date closest to the last date of transmission
  if(dt_meta_gsp_TagID_update$min_category[dt_meta_gsp_TagID_update$flag_id==id] == "Post December"){
    
    #year + 1 to get the first year the cohort returns
    year_return <- year_released+1
    
    #sex
    MF <- dat.in$sex
    
    #added this in to catch the few birds which had an unknown sex # HANNAH TO CHECK WITH KATHARINE whether these should be categorised as M or F for the purposes of the dates
    MF <- ifelse(MF == "U", "F", MF)
    
    #filter the past cohort behaviours to year+1 and the sex
    past_cohort_filter <- past_cohort_behavs %>% filter(year>year_released & sex == MF)
    
    
    #run a which query to find the row that is the last row that include the date
    dt_meta_gsp_TagID_update$min_category[dt_meta_gsp_TagID_update$flag_id==id] <- past_cohort_filter$label_year[min(which(past_cohort_filter$maxdate_pastcohort_behav > dat.in$max_date_time_transmiss))]
    
    lab <- dt_meta_gsp_TagID_update$min_category[dt_meta_gsp_TagID_update$flag_id==id]
    
    #extracting the date for the respective label
    dt_meta_gsp_TagID_update$min_category_date[dt_meta_gsp_TagID_update$flag_id==id] <- paste(as.POSIXct(past_cohort_filter$maxdate_pastcohort_behav[past_cohort_filter$label_year == lab], tz="UTC"))
    
    
  }
 
  
}


#NB - in the MAX category - birds that died the day of release they ARE included in the 1 day post release 
#NB - in the MIN category - birds that died the day of release they are NOT included in the 1 day post release 


#save the meta data out:
#write.csv(dt_meta_gsp_TagID_update, here("data/2025 analysis/metadata_TagID_deaddates_notransmision_behaviourdates_2021_2024.csv"), row.names = F)

#read it back in if needed:
#dt_meta_gsp_TagID_update <- read.csv(here("data/2025 analysis/metadata_TagID_deaddates_notransmision_behaviourdates_2021_2024.csv"), header = T)

#create a sub-table to check we're happy with the final categories:
#colnames(dt_meta_gsp_TagID_update)
#checktable <- dt_meta_gsp_TagID_update[,c(2,3,5,7,12,13,15,16,17,21,22,23,34,38,41,42,43,44,45)]
colnames(checktable)
#[1] "year"                    "flag_id"                 "sex"                     "cohort_num"              "release_location"        "release_date"           
#[7] "Fate"                    "state"                   "breeding_status"         "release_site_final"      "cohort_analysis"         "release_date_time"      
#[13] "last_transmiss"          "max_date_time_transmiss" "daysalive_transmit"      "max_category"            "max_category_date"       "min_category"           
#[19] "min_category_date"     
#write.csv(checktable, here("data/2025 analysis/tabletocheck.csv"), row.names=F)


####.####

#update the column so that they are both datetime
data$dead_no_t_date_time <- as.POSIXct(data$dead_no_t_date_time, format = "%Y-%m-%d", usetz=T, tz="UTC")
dt_meta_gsp_TagID_update$dead_no_t_date_time <- as.POSIXct(dt_meta_gsp_TagID_update$dead_no_t_date_time, format = "%Y-%m-%d", usetz=T, tz="UTC")


# full join to the data_tt file
colna <- colnames(data[c(17:47)])
data_update <- data %>% full_join(dt_meta_gsp_TagID_update, by=join_by(  "urn"  ,  "year", "flag_id", "ring", "sex" , "name" ,              
                                                                         "cohort_num" , "tag_gps_radio_none" , "tag_serial", "radio_tag_freq" ,"tagged_date" ,"release_location"   ,
                                                                         "release_date" , "migration_date", "Fate" , "state" ,  "breeding_status" , "comments" ,          
                                                                         "release_site", "release_site_final" , "cohort_analysis" ,"release_date_time"  , "Date_1d" ,"Date_1d_time", "Date_1w"    ,        
                                                                         "Date_1w_time", "Date_2w", "Date_2w_time" , "Date_6w" , "Date_6w_time", "Date_J_D_time", "dead_date" , "last_transmiss", "dead_no_t_date_time", "daysalive" ))
colnames(data_update)

tail(data_update)

#tidy up some columns so that we don't have replicates
data_final <- data_update[,c(1:51,54:61)]
colnames(data_final)
tail(data_final)
summary(data_final)




#### Some final bits of cleaning up based on preliminary analysis further down FOR THE PAST COHORT DATA ONLY (because loop 1 & 2 sort out the issues of the current cohort).####

#NOTE this code has been added in back up here retrospectively so you need to run down to get data as a track object before you can run these bits of code:

#NOTES from messages to-from Chris Thaxter. It is possible to filter a Track() dataset by using BTOTrackingTools::subset_TMS(data, TagIDs = c("a","b")). 
#BUT you have to put in ALL the tag numbers you want to keep and there is no "!" short cut. 

#INSTEAD you could convert the lists (if one list: data = TrackStack2Track(data). If two lists: data = TrackMultiStack2data(data))
# you can then convert it to a tibble and filter out what you don't want. E.g.: tibble(data) %>% filter(TagID != "a") etc, or data[data$TagID != "a",]
# And then you would convert it back to a Track using: Track() 
# THEN you can convert it back to a stacked list (if one list:  Track2TrackStack(). If two lists: data = Track2TrackMultiStack())

#Alternatively you just filter out the data you're not wanting at this point before the dataset gets converted to a Track()

#test tables using the chris T method to extract out the stacked rows and summarise to check if any birds need filtering from certain sections

#From this and from the Utilisation Distribution TIA plot section 'get_TIA_grd' you can see which birds cause an error (notes on why are in that section below)

###2022 year data ####
#"0E"  'End of December 2022' winter post breeding - seven fixes need removing

#exclude0E_22 <- (data$`10 End of December - Winter 2022`$`Yf(0E)O/-:Y/m`$DateTime)

#check these are definitely all LU timesdates
#data_final_final %>% filter(data_final_final$DateTime %in% exclude0E_22)



###2023 year data####
#list of birds and time periods: 
    #"0E"  'spring fuzz' - two fixes need removing


#data_2023 <- data
#testtable_2023 <- TrackMultiStack2Track(data_2023)

#testtable_2023_2 <- data.frame(testtable_2023) %>% 
#group_by(flag_id, period) %>%  
#  count() 

#save as csv to look at with Katharine and Sam
#write.csv(testtable_2023_2, here("data/2025 analysis/table_to_check_fixes_per_period_2023.csv"), row.names = F)


#"0E": 'spring fuzz' - two fixes need removing
#exclude0E <- (data$`7 Spring fuzzy 2023`$`Yf(0E)O/-:Y/m`$DateTime)

#check these are definitely all LU timesdates
#data_final %>% filter(data_final$DateTime %in% exclude0E)



###2024 year data ####
#list of birds and time periods: 
#"LU": has two fixes in winter pre-breeding  2024 and 5 fives in spring fuzzy 2024 and two fixes 8a female breeding season 2024 - need removing
#"7K"  15 tracks for in 6 Winter pre-breeding 2024 that need excluding
#LV": 5 fixes in winter pre-breeding  2024 and 5 fixes in spring fuzzy 2024

#"9L": spent the first couple of years in France - only one data point is in the UK for spring fuzzy 2024 - added an if loop at the point needed in the code 
      #BUT COULD COMPLETELY REMOVE IT FROM THIS TIME PERIOD INSTEAD????


#create some test tables to check the gaps
#testtable_LU <- data.frame("LU" = data_final_final$DateTime[data_final_final$flag_id=="LU"])
#testtable_LJ <- data.frame("LU" = data_final_final$DateTime[data_final_final$flag_id=="LJ"])
#testtable_LV <- data.frame("LU" = data_final_final$DateTime[data_final_final$flag_id=="LV"])
#testtable_XJ <- data.frame("LU" = data_final_final$DateTime[data_final_final$flag_id=="XJ"])


#data_2024 <- data
#testtable_2024 <- TrackMultiStack2Track(data_2024)

#table out 
#testtable_2024_2 <- data.frame(testtable_2024) %>% 
#group_by(flag_id, period) %>%
#  count() 

#save as csv to look at with Katharine and Sam
#write.csv(testtable_2024_2, here("data/2025 analysis/table_to_check_fixes_per_period_2024.csv"), row.names = F)


#"LU": has two fixes in winter pre-breeding  2024 and 5 fives in spring fuzzy 2024 and two fixes 8a female breeding season 2024 both need removing
#excludeLU <- c(data$`6 Winter pre-breeding 2024`$`Yf(LU)O/-:Y/m_Sandringham`$DateTime, data$`7 Spring fuzzy 2024`$`Yf(LU)O/-:Y/m_Sandringham`$DateTime, data$`8a Female Breeding Season 2024`$`Yf(LU)O/-:Y/m_Sandringham`$DateTime)

#check these are definitely all LU timesdates
#data_final %>% filter(data_final$DateTime %in% excludeLU)



#"7K": excluding 15 tracks for in 6 Winter pre-breeding 2024
#extracting out a list of datetime from the track:
#exclude7k <- (data$`6 Winter pre-breeding 2024`$`Yf(7K)O/-:Y/m_Sandringham`$DateTime)

#use this datetime list to check them in data_final
#data_final %>% filter(data_final$DateTime %in% exclude7k )


#LV": 5 fixes in winter pre-breeding  2024 and 5 fixes in spring fuzzy 2024
#extracting out a list of datetime from the track:
#excludeLV <- c(data$`6 Winter pre-breeding 2024`$`Yf(LV)O/-:Y/m_KenHill`$DateTime, data$`7 Spring fuzzy 2024`$`Yf(LV)O/-:Y/m_KenHill`$DateTime)

#use this datetime list to check them in data_final
#data_final %>% filter(data_final$DateTime %in% excludeLV )



#save the whole list of dates to be filtered in case I need it again:
#allexclude <- c(exclude0E, excludeLU, exclude7k, excludeLV)
#saveRDS(allexclude, here("data/2025 analysis/listofdatestoexclude_2023_2024_anlaysis.rds"))

#allexclude <- readRDS(here("data/2025 analysis/listofdatestoexclude_2023_2024_anlaysis.rds"))



#add in 9J for winter post breeding 2024 
#one more bird has come up (22/01/2025) that needs removing for End of Decemter 2023 winter - "9J" 
#testtable_2023_3 <- data.frame(Data_W_AfterB_M) %>% 
#  group_by(flag_id) %>% 
#  count()

#exclude9J <- (Data_W_AfterB_M$DateTime[Data_W_AfterB_M$flag_id=="9J"])

#check these are definitely all 9J timesdates
#data_final_final %>% filter(data_final_final$DateTime %in% exclude9J)




#allexclude <- c(allexclude, exclude9J)


#saveRDS(allexclude, here("data/2025 analysis/listofdatestoexclude_2023_2024_anlaysis.rds"))

#allexclude <- readRDS(here("data/2025 analysis/listofdatestoexclude_2023_2024_anlaysis.rds"))


#add in 0E winter post breeding 2022 
#allexclude <- c(allexclude, exclude0E_22)

#saveRDS(allexclude, here("data/2025 analysis/listofdatestoexclude_2023_2024_anlaysis.rds"))

allexclude <- readRDS(here("data/2025 analysis/listofdatestoexclude_2023_2024_anlaysis.rds"))




###final removal of all 'exclude' fixes:####
#exclude the rows for "OE" and "LU", "7k"
data_final_final <- data_final %>% filter(! data_final$DateTime %in% allexclude)


### final checks on how many fixes there were in Dec 2024 after 7th because this is when the sensor stopped working on the tide bouy ####
#after discussion with Katharine - happy for me to ignore these dates for the tide related analysis but keep them in for the remaining analysis
#NOTE there are 131 fixes - this isn't enough to get a good idea of tide change from 8th Dec-31st Dec. 

#dec2024fixes <- data_final_final %>% filter(DateTime > "2024-12-07" )

#dec2024fixes %>%
#  group_by(flag_id) %>% count()


###.####


###Once all the tweaking is done - save this out as the final dataset!  ####
#saveRDS(data_final_final, (here("data/2025 analysis/data_withcohorts_release_sites_tagduration_2021_2024.rds")))



####.####


# LOAD PACKAGES ####
load_pkg <- rlang::quos(tidyverse,BTOTrackingTools, here, sp, leaflet, terra)  # quos() function to be lazy on "" around each package
lapply(lapply(load_pkg, rlang::quo_name), library, character.only = TRUE)

#START HERE - TO CREATE THE BTO TRACKS - Once all data is correct and cleaned and combined above you can start from here ####

#read back in the data ####
data_final_final <- readRDS(here("data/2025 analysis/data_withcohorts_release_sites_tagduration_2021_2024.rds"))

summary(data_final_final)

#update all the date time columns for 1 day, 1 week, 2 weeks, 6 weeks, end of Dec
data_final_final$Date_1d <- as.POSIXct(data_final_final$Date_1d, format = "%Y-%m-%d %H:%M:%S", tz="UTC")
data_final_final$Date_1w <- as.POSIXct(data_final_final$Date_1w, format = "%Y-%m-%d %H:%M:%S", tz="UTC")
data_final_final$Date_2w <- as.POSIXct(data_final_final$Date_2w, format = "%Y-%m-%d %H:%M:%S", tz="UTC")
data_final_final$Date_6w <- as.POSIXct(data_final_final$Date_6w, format = "%Y-%m-%d %H:%M:%S", tz="UTC")
data_final_final$release_date_posi <- as.POSIXct(data_final_final$release_date_posi, format = "%Y-%m-%d", tz="UTC")



#read in csv which summaries all the categories for past cohort behaviours and max dates
past_cohort_behavs <- read.csv(here("data/pastcohort_behaviours_maxdates.csv"), header=T)

#add in a unique label ID
past_cohort_behavs$label_year <- paste(past_cohort_behavs$pastcohort_behaviours, past_cohort_behavs$year)

#make the date time column a posi
past_cohort_behavs$maxdate_pastcohort_behav <- as.POSIXct(past_cohort_behavs$maxdate_pastcohort_behav, format = "%d/%m/%Y %H:%M:%S", tz="UTC")




# Convert to BTOTT Track object ####
#HH UPDATE NOTE 21/01/2025 for some reason when it is converted to tracks it looses the as.POSIXct on the Date_1d, Date_1w etc... so moving this into the loop

#data_tt<-Track(data_final_final) 
#data_tt<-clean_GPS(data_tt, drop_sats = 3, Thres = 30, GAP = 28800) #HH NEW NOTE 2024 analysis: GAP here constrains the function to not interpolate time in between this gap of 28800 seconds = 8 hours (chat message from Chris T 17/01/2025)

#HH NB 2023 analysis: new warning messages about "flt_switch" for each bird - an error from the Track(data) function with is a BTOTT function
#See messages from Chris T about this but the summary is it is a flag option for clean_GPS - when importing data into movebank you can add a 
#column to tell you if it is dodgy data or not and then add a second column to clean it/remove it... as the dataset I am working with doesn't have this column
# I can disregard this warning


# Set ID factor
#data_tt$TagID<-as.factor(as.character(data_tt$TagID)) 

#try plotting all the data
#plot(data_tt$longitude, data_tt$latitude)



#NEXT create separate files per year - for cohort released and remaining from previous cohorts, all the split by stage ####

#Using the same name as the code already set up:
data_tt <- data_final_final

#update all the date time columns for 1 day, 1 week, 2 weeks, 6 weeks, end of Dec
data_tt$Date_1d <- as.POSIXct(data_tt$Date_1d, format = "%Y-%m-%d %H:%M:%S", tz="UTC")
data_tt$Date_1w <- as.POSIXct(data_tt$Date_1w, format = "%Y-%m-%d %H:%M:%S", tz="UTC")
data_tt$Date_2w <- as.POSIXct(data_tt$Date_2w, format = "%Y-%m-%d %H:%M:%S", tz="UTC")
data_tt$Date_6w <- as.POSIXct(data_tt$Date_6w, format = "%Y-%m-%d %H:%M:%S", tz="UTC")
data_tt$release_date_posi <- as.POSIXct(data_tt$release_date_posi, format = "%Y-%m-%d", tz="UTC")




nyears <- c("2021", "2022", "2023", "2024")
cohort_periods <- c("1 One Day"  ,  "2 One Week"  , "3 Two Weeks" ,  "4 Six Weeks"   , "5 End of December" )

#summary_dat <- read.csv(here("data/summary_curlewcount_per_timeperiod.csv"), header=T) 

#add in a unique label ID
summary_dat$label_year <- paste(summary_dat$pastcohort_behaviours, summary_dat$year)

summary_dat$number_birds <- NA




for(y in 1:length(nyears)){

  #first pick out the selected year
  nyr <- nyears[y]
  
  #pick out the correct december and january date for the year 
  decdate <-  as.POSIXct(ifelse(nyr == 2021, "2021-12-31 23:59:59",
                                ifelse(nyr == 2022, "2022-12-31 23:59:59", 
                                       ifelse(nyr == 2023, "2023-12-31 23:59:59", "2024-12-31 23:59:59"))), tz="UTC")
  
  
  jandate <-  as.POSIXct(ifelse(nyr == 2021, "2021-01-01 00:00:00",
                                ifelse(nyr == 2022, "2022-01-01 00:00:00", 
                                       ifelse(nyr == 2023, "2023-01-01 00:00:00", "2024-01-01 00:00:00"))), tz="UTC")
  
  
  #create a subset of data based on the date and time - IE keeping the data recording dates to the year selected 
  dat.in_c <-data_tt %>% filter(DateTime >= jandate & DateTime <= decdate ) %>% droplevels()
  
  #this checks that there are still the different year cohorts
  summary(dat.in_c$year)
  
  #this checks the min and max datetime
  summary(dat.in_c$DateTime)
  
  #check the plot
  plot(dat.in_c$longitude, dat.in_c$latitude)
  
  
  #Then two sub sets of analysis:
    #create a sub dataset for cohort released in nyr
  dat.in_cohort <-  dat.in_c %>% 
    filter(year == nyr) %>% 
    droplevels()
  
  #checks that only the cohort year is retained
  summary(dat.in_cohort$year)
  
  #checks the categories remaining
  summary(as.factor(dat.in_cohort$min_category))
  
  #checks the total number of data rows per min_category
  table(dat.in_cohort$min_category)
  
  
  
  #then create an individual dataset filtered for each of the time periods: 
  
  #one day - Filters to keep it just to one day for all data
  data_1d <-  dat.in_cohort %>% 
    filter(dat.in_cohort$DateTime >= dat.in_cohort$release_date_posi & dat.in_cohort$DateTime <= dat.in_cohort$Date_1d) %>% 
    filter(min_category !=  "0 Days") %>%
    droplevels() 
  
    summary(as.factor(data_1d$min_category))
    summary(data_1d$DateTime)
    
   #check max number of hours is less than 48hrs
    max(data_1d$DateTime - data_1d$release_date_posi)
    
    
    #This is a VERY key line to help Track2TrackMultiStack stack the tags in the correct stack! 
    data_1d$period <- "1 One Day"
    
    #add the number of birds into the summary table
    summary_dat$number_birds[summary_dat$year == nyr & summary_dat$pastcohort_behaviours == "1 One Day"] <- length(unique(data_1d$TagID))
    
    
    #turn it into a Track using BTOTT
    data_1d_tt <-Track(data_1d) 
    data_1d_tt<-clean_GPS(data_1d_tt, drop_sats = 3, Thres = 30, GAP = 28800) #HH NEW NOTE 2024 analysis: GAP here constrains the function to not interpolate time in between this gap of 28800 seconds = 8 hours (chat message from Chris T 17/01/2025)
    
    # Set ID factor
    data_1d_tt$TagID<-as.factor(as.character(data_1d_tt$TagID)) 
    
    
    
  #one week - filters data to just one week after release but removing the birds that were only in one day category
  data_1w <- dat.in_cohort %>%
    filter(dat.in_cohort$DateTime >= dat.in_cohort$release_date_posi & dat.in_cohort$DateTime <= dat.in_cohort$Date_1w) %>%
    filter(min_category !=  "0 Days") %>%
    filter(min_category !=  "1 One Day") %>% 
    droplevels() 
    
     
 
  summary(as.factor(data_1w$min_category))
  summary(data_1w$DateTime)
  #check max number of hours is less than 192hrs (1 week + 24hrs)
  max(data_1w$DateTime - data_1w$release_date_posi)
  
  #This is a VERY key line to help Track2TrackMultiStack stack the tags in the correct stack! 
  data_1w$period <- "2 One Week"
  
  
  #add the number of birds into the summary table
  summary_dat$number_birds[summary_dat$year == nyr & summary_dat$pastcohort_behaviours == "2 One Week"] <- length(unique(data_1w$TagID))
  
  
  #turn it into a Track using BTOTT
  data_1w_tt <-Track(data_1w) 
  data_1w_tt<-clean_GPS(data_1w_tt, drop_sats = 3, Thres = 30, GAP = 28800) #HH NEW NOTE 2024 analysis: GAP here constrains the function to not interpolate time in between this gap of 28800 seconds = 8 hours (chat message from Chris T 17/01/2025)
  
  # Set ID factor
  data_1w_tt$TagID<-as.factor(as.character(data_1w_tt$TagID)) 
  
  
  
  #two weeks - filters data to just two weeks after release but removes the birds that were only in one day and one week categories
  data_2 <- dat.in_cohort %>% 
    filter(DateTime >= release_date_posi & DateTime <= Date_2w )  %>% 
    filter(min_category !=  "0 Days") %>%
    filter(min_category !=  "1 One Day") %>%
    filter(min_category !=  "2 One Week")%>% 
    droplevels()
  
  
  summary(data_2$DateTime)
  summary(as.factor(data_2$min_category))
  #check max number of hours is less than 390hrs
  max(data_2$DateTime - data_2$release_date_posi)
  
  
  #This is a VERY key line to help Track2TrackMultiStack stack the tags in the correct stack! 
  data_2$period <- "3 Two Weeks"
  
  
  #add the number of birds into the summary table
  summary_dat$number_birds[summary_dat$year == nyr & summary_dat$pastcohort_behaviours == "3 Two Weeks"] <- length(unique(data_2$TagID))
  
  
  
  #turn it into a Track using BTOTT
  data_2_tt <-Track(data_2) 
  data_2_tt<-clean_GPS(data_2_tt, drop_sats = 3, Thres = 30, GAP = 28800) #HH NEW NOTE 2024 analysis: GAP here constrains the function to not interpolate time in between this gap of 28800 seconds = 8 hours (chat message from Chris T 17/01/2025)
  
  # Set ID factor
  data_2_tt$TagID<-as.factor(as.character(data_2_tt$TagID)) 
  
  
  
  #six weeks - filters data to six weeks post release, but removes the birds that were only in one day and one week and two week categories
  data_6 <- dat.in_cohort %>% 
    filter(DateTime >= release_date_posi & DateTime <= Date_6w)  %>% 
    filter(min_category !=  "0 Days") %>%
    filter(min_category !=  "1 One Day") %>%
    filter(min_category !=  "2 One Week") %>%
    filter(min_category !=  "3 Two Weeks")%>% 
    droplevels()
    
  summary(data_6$DateTime)
  summary(as.factor(data_6$min_category))
  #check max number of hours is less than 1032hrs
  max(data_6$DateTime - data_6$release_date_posi)
  
  
  #This is a VERY key line to help Track2TrackMultiStack stack the tags in the correct stack! 
  data_6$period <- "4 Six Weeks"
  

  #add the number of birds into the summary table
  summary_dat$number_birds[summary_dat$year == nyr & summary_dat$pastcohort_behaviours == "4 Six Weeks"] <- length(unique(data_6$TagID))
  
  
  
  #turn it into a Track using BTOTT
  data_6_tt <-Track(data_6) 
  data_6_tt<-clean_GPS(data_6_tt, drop_sats = 3, Thres = 30, GAP = 28800) #HH NEW NOTE 2024 analysis: GAP here constrains the function to not interpolate time in between this gap of 28800 seconds = 8 hours (chat message from Chris T 17/01/2025)
  
  # Set ID factor
  data_6_tt$TagID<-as.factor(as.character(data_6_tt$TagID)) 
  
  
  
  
  #This combines all data up until the end of December- HH to check this is correct - data regardless of when the data stopped = YES
  data_all <- dat.in_cohort %>%
    filter(min_category !=  "0 Days") %>%
    filter(min_category !=  "1 One Day") %>%
    filter(min_category !=  "2 One Week") %>%
    filter(min_category !=  "3 Two Weeks") %>%
    filter(min_category !=  "4 Six Weeks") %>% 
    droplevels()
    
  data_all$period <- "5 End of December"
  
  summary(data_all$DateTime)
  summary(as.factor(data_all$min_category))

  
  #NOTE THIS IS HASH TAGGED OUT FOR NOW - because all data July-Dec only includes birds which have data beyond November of that year. This could be brought back in again to include ALL data if needed
  #data_all <- rbind(data_all, data_1d[data_1d$min_category=="1 One Day",], data_1w[data_1w$min_category=="2 One Week",], data_2[data_2$min_category=="3 Two Weeks",], data_6[data_6$min_category=="4 Six Weeks",] )
  #summary(data_all$DateTime)
  #summary(as.factor(data_all$min_category))
  
  
  #This is a VERY key line to help Track2TrackMultiStack stack the tags in the correct stack! 
  data_all$period <- "5 End of December"
  

  #add the number of birds into the summary table
  summary_dat$number_birds[summary_dat$year == nyr & summary_dat$pastcohort_behaviours == "5 End of December"] <- length(unique(data_all$TagID))
  
  
  
  #turn it into a Track using BTOTT
  data_all_tt <-Track(data_all) 
  data_all_tt<-clean_GPS(data_all_tt, drop_sats = 3, Thres = 30, GAP = 28800) #HH NEW NOTE 2024 analysis: GAP here constrains the function to not interpolate time in between this gap of 28800 seconds = 8 hours (chat message from Chris T 17/01/2025)

  # Set ID factor
  data_all_tt$TagID<-as.factor(as.character(data_all_tt$TagID)) 
  
  
  
  
  
  # Final merge for the current year cohort #####
  data_cohort<-Track2TrackMultiStack(rbind(data_1d_tt, data_1w_tt, data_2_tt, data_6_tt, data_all_tt), by=c("TagID", "period"))
  data_cohort
  
  
  
  # Save
  setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data/2025 analysis") #HH laptop
  save(data_cohort, file=paste0("NE103_",nyr," report_clean tracking data for ",nyr," cohort.RData"))
  
  
  
  
  ####then create a sub dataset for past cohorts still recording in nyr
  #Need to do something a bit different for the past cohorts
  
    dat.in_pastcohort <-  dat.in_c %>% 
    filter(year != nyr) %>% 
    droplevels()
  
  summary(dat.in_pastcohort$year)
  summary(dat.in_pastcohort$DateTime)
  table(unique(dat.in_pastcohort$TagID))
  
  
  #put in a loop catch for 2021 which doesn't have any past cohort data!
  if(nyr==2021){
    
    print(2021)
    
    # Final merge for the whole of the year #####
    data_year<-Track2TrackMultiStack(rbind(data_1d_tt, data_1w_tt, data_2_tt, data_6_tt, data_all_tt), by=c("TagID", "period"))
    data_year
    
    
    # Save
    setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data/2025 analysis") #HH laptop
    save(data_year, file=paste0("NE103_",nyr," report_clean tracking data for all ",nyr," data.RData"))
    
    
    
  } else if(nyr == 2022 ){     
    
    print(2022)
    
    #FEMALE OR UNKNOWN#
    
    #filter the data
    dat.keep_F <- dat.in_pastcohort %>% filter(sex == "F"| sex == "U") %>% droplevels()
      summary(dat.keep_F)
      
    #filter to past_cohort_behavs dataframe to get the year and sex time periods
    past_cohort_behavs_year <- past_cohort_behavs %>% filter(sex=="F" & year== nyr)
    
    #use the table above to then create a list of labels
    past_cohort_periods_F <- past_cohort_behavs_year$label_year
      
    
    
        #winter pre breeding
    #because it didn't quite work to have it in a loop the periods_F <- line is really important to loop over the different labels 
      periods_F <- past_cohort_periods_F[1]
      
    
      Data_W_PreB_F <- dat.keep_F %>% 
        filter(DateTime >= jandate  & DateTime <= past_cohort_behavs_year$maxdate_pastcohort_behav[past_cohort_behavs_year$label_year==periods_F]) %>%
        droplevels()
      
      summary(Data_W_PreB_F$DateTime)
      summary(Data_W_PreB_F)
    
      #This is a VERY key line to help Track2TrackMultiStack stack the tags in the correct stack! 
      Data_W_PreB_F$period <- periods_F
      
      #add the number of birds into the summary table
      summary_dat$number_birds[summary_dat$sex == "F" & summary_dat$label_year == periods_F] <- length(unique(Data_W_PreB_F$TagID))
      
      
      #turn it into a Track using BTOTT
      Data_W_PreB_F_tt <-Track(Data_W_PreB_F) 
      Data_W_PreB_F_tt<-clean_GPS(Data_W_PreB_F_tt, drop_sats = 3, Thres = 30, GAP = 28800) #HH NEW NOTE 2024 analysis: GAP here constrains the function to not interpolate time in between this gap of 28800 seconds = 8 hours (chat message from Chris T 17/01/2025)
      
      # Set ID factor
      Data_W_PreB_F_tt$TagID<-as.factor(as.character(Data_W_PreB_F_tt$TagID)) 
      
      
      
      #spring fuzz
      periods_F_before <- past_cohort_periods_F[1]
      periods_F <- past_cohort_periods_F[2]
      
      
      Data_SF_F <- dat.keep_F %>% 
        filter(DateTime >=  past_cohort_behavs_year$maxdate_pastcohort_behav[past_cohort_behavs_year$label_year==periods_F_before]  & DateTime <= past_cohort_behavs$maxdate_pastcohort_behav[past_cohort_behavs$sex=="F" & past_cohort_behavs$label_year==periods_F]) %>%
        droplevels()
      
      summary(Data_SF_F$DateTime)
      summary(Data_SF_F)
      
      #This is a VERY key line to help Track2TrackMultiStack stack the tags in the correct stack! 
      Data_SF_F$period <- periods_F
      
      
      #add the number of birds into the summary table
      summary_dat$number_birds[summary_dat$sex == "F" & summary_dat$label_year == periods_F] <- length(unique(Data_SF_F$TagID))
      
      
      #turn it into a Track using BTOTT
      Data_SF_F_tt <-Track(Data_SF_F) 
      Data_SF_F_tt<-clean_GPS(Data_SF_F_tt, drop_sats = 3, Thres = 30, GAP = 28800) #HH NEW NOTE 2024 analysis: GAP here constrains the function to not interpolate time in between this gap of 28800 seconds = 8 hours (chat message from Chris T 17/01/2025)
      
      # Set ID factor
      Data_SF_F_tt$TagID<-as.factor(as.character(Data_SF_F_tt$TagID)) 
      
      
      
      
      #Female breeding
      periods_F_before <- past_cohort_periods_F[2]
      periods_F <- past_cohort_periods_F[3]
      
      
      Data_Breed_F <- dat.keep_F %>% 
        filter(DateTime >=  past_cohort_behavs_year$maxdate_pastcohort_behav[past_cohort_behavs_year$label_year==periods_F_before]  & DateTime <= past_cohort_behavs$maxdate_pastcohort_behav[past_cohort_behavs$sex=="F" & past_cohort_behavs$label_year==periods_F]) %>%
        droplevels()
      
      summary(Data_Breed_F$DateTime)
      summary(Data_Breed_F)
      
      #This is a VERY key line to help Track2TrackMultiStack stack the tags in the correct stack! 
      Data_Breed_F$period <- periods_F
      
      
      #add the number of birds into the summary table
      summary_dat$number_birds[summary_dat$sex == "F" & summary_dat$label_year == periods_F] <- length(unique(Data_Breed_F$TagID))
      
      
      #turn it into a Track using BTOTT
      Data_Breed_F_tt <-Track(Data_Breed_F) 
      Data_Breed_F_tt<-clean_GPS(Data_Breed_F_tt, drop_sats = 3, Thres = 30, GAP = 28800) #HH NEW NOTE 2024 analysis: GAP here constrains the function to not interpolate time in between this gap of 28800 seconds = 8 hours (chat message from Chris T 17/01/2025)
      
      # Set ID factor
      Data_Breed_F_tt$TagID<-as.factor(as.character(Data_Breed_F_tt$TagID)) 
      
      
      
      
      #Female autumn fuzz
      periods_F_before <- past_cohort_periods_F[3]
      periods_F <- past_cohort_periods_F[4]
      
      
      Data_AF_F <- dat.keep_F %>% 
        filter(DateTime >=  past_cohort_behavs_year$maxdate_pastcohort_behav[past_cohort_behavs_year$label_year==periods_F_before]  & DateTime <= past_cohort_behavs$maxdate_pastcohort_behav[past_cohort_behavs$sex=="F" & past_cohort_behavs$label_year==periods_F]) %>%
        droplevels()
      
      summary(Data_AF_F$DateTime)
      summary(Data_AF_F)
      
      #This is a VERY key line to help Track2TrackMultiStack stack the tags in the correct stack! 
      Data_AF_F$period <- periods_F
      
      
      #add the number of birds into the summary table
      summary_dat$number_birds[summary_dat$sex == "F" & summary_dat$label_year == periods_F] <- length(unique(Data_AF_F$TagID))
      
      
      #turn it into a Track using BTOTT
      Data_AF_F_tt <-Track(Data_AF_F) 
      Data_AF_F_tt<-clean_GPS(Data_AF_F_tt, drop_sats = 3, Thres = 30, GAP = 28800) #HH NEW NOTE 2024 analysis: GAP here constrains the function to not interpolate time in between this gap of 28800 seconds = 8 hours (chat message from Chris T 17/01/2025)
      
      # Set ID factor
      Data_AF_F_tt$TagID<-as.factor(as.character(Data_AF_F_tt$TagID)) 
      
      
      
      
      
      
      # Final merge for the cohort #####
      data_past_F <-Track2TrackMultiStack(rbind(Data_W_PreB_F_tt, Data_SF_F_tt, Data_Breed_F_tt, Data_AF_F_tt), by=c("TagID", "period"))
      data_past_F
      
      
      # Save
      setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data/2025 analysis") #HH laptop
      save(data_cohort, file=paste0("NE103_",nyr," report_clean tracking data for ",nyr," past cohorts_Female.RData"))
      
      #this catches the 2022 year where there were no male past cohorts
        
        #Final merge all together:
        
        # Final merge for the whole of the year #####
        data_year<-Track2TrackMultiStack(rbind(data_1d_tt, data_1w_tt, data_2_tt, data_6_tt, data_all_tt, Data_W_PreB_F_tt, Data_SF_F_tt, Data_Breed_F_tt, Data_AF_F_tt), by=c("TagID", "period"))
        data_year
        
        
        # Save
        setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data/2025 analysis") #HH laptop
        save(data_year, file=paste0("NE103_",nyr," report_clean tracking data for all ",nyr," data.RData"))
        
        
        
      }else  {
        
        print("2023-2024")
        
        #FEMALE OR UNKNOWN#
        
        #filter the data
        dat.keep_F <- dat.in_pastcohort %>% filter(sex == "F"| sex == "U") %>% droplevels()
        summary(dat.keep_F)
        
        #filter to past_cohort_behavs dataframe to get the year and sex time periods
        past_cohort_behavs_year <- past_cohort_behavs %>% filter(sex=="F" & year== nyr)
        
        #use the table above to then create a list of labels
        past_cohort_periods_F <- past_cohort_behavs_year$label_year
        
        
        
        #winter pre breeding
        #because it didn't quite work to have it in a loop the periods_F <- line is really important to loop over the different labels 
        periods_F <- past_cohort_periods_F[1]
        
        
        Data_W_PreB_F <- dat.keep_F %>% 
          filter(DateTime >= jandate  & DateTime <= past_cohort_behavs_year$maxdate_pastcohort_behav[past_cohort_behavs_year$label_year==periods_F]) %>%
          droplevels()
        
        summary(Data_W_PreB_F$DateTime)
        summary(Data_W_PreB_F)
        
        #This is a VERY key line to help Track2TrackMultiStack stack the tags in the correct stack! 
        Data_W_PreB_F$period <- periods_F
        
        #add the number of birds into the summary table
        summary_dat$number_birds[summary_dat$sex == "F" & summary_dat$label_year == periods_F] <- length(unique(Data_W_PreB_F$TagID))
        
        
        #turn it into a Track using BTOTT
        Data_W_PreB_F_tt <-Track(Data_W_PreB_F) 
        Data_W_PreB_F_tt<-clean_GPS(Data_W_PreB_F_tt, drop_sats = 3, Thres = 30, GAP = 28800) #HH NEW NOTE 2024 analysis: GAP here constrains the function to not interpolate time in between this gap of 28800 seconds = 8 hours (chat message from Chris T 17/01/2025)
        
        # Set ID factor
        Data_W_PreB_F_tt$TagID<-as.factor(as.character(Data_W_PreB_F_tt$TagID)) 
        
        
        
        #spring fuzz
        periods_F_before <- past_cohort_periods_F[1]
        periods_F <- past_cohort_periods_F[2]
        
        
        Data_SF_F <- dat.keep_F %>% 
          filter(DateTime >=  past_cohort_behavs_year$maxdate_pastcohort_behav[past_cohort_behavs_year$label_year==periods_F_before]  & DateTime <= past_cohort_behavs$maxdate_pastcohort_behav[past_cohort_behavs$sex=="F" & past_cohort_behavs$label_year==periods_F]) %>%
          droplevels()
        
        summary(Data_SF_F$DateTime)
        summary(Data_SF_F)
        
        #This is a VERY key line to help Track2TrackMultiStack stack the tags in the correct stack! 
        Data_SF_F$period <- periods_F
        
        
        #add the number of birds into the summary table
        summary_dat$number_birds[summary_dat$sex == "F" & summary_dat$label_year == periods_F] <- length(unique(Data_SF_F$TagID))
        
        
        #turn it into a Track using BTOTT
        Data_SF_F_tt <-Track(Data_SF_F) 
        Data_SF_F_tt<-clean_GPS(Data_SF_F_tt, drop_sats = 3, Thres = 30, GAP = 28800) #HH NEW NOTE 2024 analysis: GAP here constrains the function to not interpolate time in between this gap of 28800 seconds = 8 hours (chat message from Chris T 17/01/2025)
        
        # Set ID factor
        Data_SF_F_tt$TagID<-as.factor(as.character(Data_SF_F_tt$TagID)) 
        
        
        
        
        #Female breeding
        periods_F_before <- past_cohort_periods_F[2]
        periods_F <- past_cohort_periods_F[3]
        
        
        Data_Breed_F <- dat.keep_F %>% 
          filter(DateTime >=  past_cohort_behavs_year$maxdate_pastcohort_behav[past_cohort_behavs_year$label_year==periods_F_before]  & DateTime <= past_cohort_behavs$maxdate_pastcohort_behav[past_cohort_behavs$sex=="F" & past_cohort_behavs$label_year==periods_F]) %>%
          droplevels()
        
        summary(Data_Breed_F$DateTime)
        summary(Data_Breed_F)
        
        #This is a VERY key line to help Track2TrackMultiStack stack the tags in the correct stack! 
        Data_Breed_F$period <- periods_F
        
        
        #add the number of birds into the summary table
        summary_dat$number_birds[summary_dat$sex == "F" & summary_dat$label_year == periods_F] <- length(unique(Data_Breed_F$TagID))
        
        
        #turn it into a Track using BTOTT
        Data_Breed_F_tt <-Track(Data_Breed_F) 
        Data_Breed_F_tt<-clean_GPS(Data_Breed_F_tt, drop_sats = 3, Thres = 30, GAP = 28800) #HH NEW NOTE 2024 analysis: GAP here constrains the function to not interpolate time in between this gap of 28800 seconds = 8 hours (chat message from Chris T 17/01/2025)
        
        # Set ID factor
        Data_Breed_F_tt$TagID<-as.factor(as.character(Data_Breed_F_tt$TagID)) 
        
        
        
        
        #Female autumn fuzz
        periods_F_before <- past_cohort_periods_F[3]
        periods_F <- past_cohort_periods_F[4]
        
        
        Data_AF_F <- dat.keep_F %>% 
          filter(DateTime >=  past_cohort_behavs_year$maxdate_pastcohort_behav[past_cohort_behavs_year$label_year==periods_F_before]  & DateTime <= past_cohort_behavs$maxdate_pastcohort_behav[past_cohort_behavs$sex=="F" & past_cohort_behavs$label_year==periods_F]) %>%
          droplevels()
        
        summary(Data_AF_F$DateTime)
        summary(Data_AF_F)
        
        #This is a VERY key line to help Track2TrackMultiStack stack the tags in the correct stack! 
        Data_AF_F$period <- periods_F
        
        
        #add the number of birds into the summary table
        summary_dat$number_birds[summary_dat$sex == "F" & summary_dat$label_year == periods_F] <- length(unique(Data_AF_F$TagID))
        
        
        #turn it into a Track using BTOTT
        Data_AF_F_tt <-Track(Data_AF_F) 
        Data_AF_F_tt<-clean_GPS(Data_AF_F_tt, drop_sats = 3, Thres = 30, GAP = 28800) #HH NEW NOTE 2024 analysis: GAP here constrains the function to not interpolate time in between this gap of 28800 seconds = 8 hours (chat message from Chris T 17/01/2025)
        
        # Set ID factor
        Data_AF_F_tt$TagID<-as.factor(as.character(Data_AF_F_tt$TagID)) 
        
        
        
        
        #Female winter after breeding
        periods_F_before <- past_cohort_periods_F[4]
        periods_F <- past_cohort_periods_F[5]
        
        
        Data_W_AfterB_F <- dat.keep_F %>% 
          filter(DateTime >=  past_cohort_behavs_year$maxdate_pastcohort_behav[past_cohort_behavs_year$label_year==periods_F_before]  & DateTime <= past_cohort_behavs$maxdate_pastcohort_behav[past_cohort_behavs$sex=="F" & past_cohort_behavs$label_year==periods_F]) %>%
          droplevels()
        
        summary(Data_W_AfterB_F$DateTime)
        summary(Data_W_AfterB_F)
        
        #This is a VERY key line to help Track2TrackMultiStack stack the tags in the correct stack! 
        Data_W_AfterB_F$period <- periods_F
        
        
        #add the number of birds into the summary table
        summary_dat$number_birds[summary_dat$sex == "F" & summary_dat$label_year == periods_F] <- length(unique(Data_W_AfterB_F$TagID))
        
        
        #turn it into a Track using BTOTT
        Data_W_AfterB_F_tt <-Track(Data_W_AfterB_F) 
        Data_W_AfterB_F_tt<-clean_GPS(Data_W_AfterB_F_tt, drop_sats = 3, Thres = 30, GAP = 28800) #HH NEW NOTE 2024 analysis: GAP here constrains the function to not interpolate time in between this gap of 28800 seconds = 8 hours (chat message from Chris T 17/01/2025)
        
        # Set ID factor
        Data_W_AfterB_F_tt$TagID<-as.factor(as.character(Data_W_AfterB_F_tt$TagID)) 
        
        
        
        
        # Final merge for the cohort #####
        data_past_F <-Track2TrackMultiStack(rbind(Data_W_PreB_F_tt, Data_SF_F_tt, Data_Breed_F_tt, Data_AF_F_tt, Data_W_AfterB_F_tt), by=c("TagID", "period"))
        data_past_F
        
        
        # Save
        setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data/2025 analysis") #HH laptop
        save(data_cohort, file=paste0("NE103_",nyr," report_clean tracking data for ",nyr," past cohorts_Female.RData"))
        
        
        
      #MALE#
      #filter the data
      dat.keep_M <- dat.in_pastcohort %>% filter(sex == "M") %>% droplevels()
      summary(dat.keep_M)
      
      #filter to past_cohort_behavs dataframe to get the year and sex time periods
      past_cohort_behavs_year_M <- past_cohort_behavs %>% filter(sex=="M" & year== nyr)
      
      #use the table above to then create a list of labels
      past_cohort_periods_M <- past_cohort_behavs_year_M$label_year
      
      
      
      
      #winter pre breeding
      #because it didn't quite work to have it in a loop the periods_F <- line is really important to loop over the different labels 
      periods_M <- past_cohort_periods_M[1]
      
      
      Data_W_PreB_M <- dat.keep_M %>% 
        filter(DateTime >= jandate  & DateTime <= past_cohort_behavs_year$maxdate_pastcohort_behav[past_cohort_behavs_year$label_year==periods_M]) %>%
        droplevels()
      
      summary(Data_W_PreB_M$DateTime)
      summary(Data_W_PreB_M)
      
      #This is a VERY key line to help Track2TrackMultiStack stack the tags in the correct stack! 
      Data_W_PreB_M$period <- periods_M
      
      #add the number of birds into the summary table
      summary_dat$number_birds[summary_dat$sex == "M" & summary_dat$label_year == periods_M] <- length(unique(Data_W_PreB_M$TagID))
      
      
      #turn it into a Track using BTOTT
      Data_W_PreB_M_tt <-Track(Data_W_PreB_M) 
      Data_W_PreB_M_tt<-clean_GPS(Data_W_PreB_M_tt, drop_sats = 3, Thres = 30, GAP = 28800) #HH NEW NOTE 2024 analysis: GAP here constrains the function to not interpolate time in between this gap of 28800 seconds = 8 hours (chat message from Chris T 17/01/2025)
      
      # Set ID factor
      Data_W_PreB_M_tt$TagID<-as.factor(as.character(Data_W_PreB_M_tt$TagID)) 
      
      
      
      
    
      #spring fuzz
      periods_M_before <- past_cohort_periods_M[1]
      periods_M <- past_cohort_periods_M[2]
      
      
      Data_SF_M <- dat.keep_M %>% 
        filter(DateTime >=  past_cohort_behavs_year_M$maxdate_pastcohort_behav[past_cohort_behavs_year_M$label_year==periods_M_before]  & DateTime <= past_cohort_behavs_year_M$maxdate_pastcohort_behav[past_cohort_behavs_year_M$label_year==periods_M]) %>%
        droplevels()
      
      summary(Data_SF_M$DateTime)
      summary(Data_SF_M)
      
      #This is a VERY key line to help Track2TrackMultiStack stack the tags in the correct stack! 
      Data_SF_M$period <- periods_M
      
      
      #add the number of birds into the summary table
      summary_dat$number_birds[summary_dat$sex == "M" & summary_dat$label_year == periods_M] <- length(unique(Data_SF_M$TagID))
      
      #turn it into a Track using BTOTT
      Data_SF_M_tt <-Track(Data_SF_M) 
      Data_SF_M_tt<-clean_GPS(Data_SF_M_tt, drop_sats = 3, Thres = 30, GAP = 28800) #HH NEW NOTE 2024 analysis: GAP here constrains the function to not interpolate time in between this gap of 28800 seconds = 8 hours (chat message from Chris T 17/01/2025)
      
      # Set ID factor
      Data_SF_M_tt$TagID<-as.factor(as.character(Data_SF_M_tt$TagID)) 
      
      
      
      
      #Male breeding season
      periods_M_before <- past_cohort_periods_M[2]
      periods_M <- past_cohort_periods_M[3]
      
      
      Data_Breed_M <- dat.keep_M %>% 
        filter(DateTime >=  past_cohort_behavs_year_M$maxdate_pastcohort_behav[past_cohort_behavs_year_M$label_year==periods_M_before]  & DateTime <= past_cohort_behavs_year_M$maxdate_pastcohort_behav[past_cohort_behavs_year_M$label_year==periods_M]) %>%
        droplevels()
      
      summary(Data_Breed_M$DateTime)
      summary(Data_Breed_M)
      
      #This is a VERY key line to help Track2TrackMultiStack stack the tags in the correct stack! 
      Data_Breed_M$period <- periods_M
      
      
      #add the number of birds into the summary table
      summary_dat$number_birds[summary_dat$sex == "M" & summary_dat$label_year == periods_M] <- length(unique(Data_Breed_M$TagID))
      
      #turn it into a Track using BTOTT
      Data_Breed_M_tt <-Track(Data_Breed_M) 
      Data_Breed_M_tt<-clean_GPS(Data_Breed_M_tt, drop_sats = 3, Thres = 30, GAP = 28800) #HH NEW NOTE 2024 analysis: GAP here constrains the function to not interpolate time in between this gap of 28800 seconds = 8 hours (chat message from Chris T 17/01/2025)
      
      # Set ID factor
      Data_Breed_M_tt$TagID<-as.factor(as.character(Data_Breed_M_tt$TagID)) 
      
      
      
      
      #Male autumn fuzzy
      periods_M_before <- past_cohort_periods_M[3]
      periods_M <- past_cohort_periods_M[4]
      
      
      Data_AF_M <- dat.keep_M %>% 
        filter(DateTime >=  past_cohort_behavs_year_M$maxdate_pastcohort_behav[past_cohort_behavs_year_M$label_year==periods_M_before]  & DateTime <= past_cohort_behavs_year_M$maxdate_pastcohort_behav[past_cohort_behavs_year_M$label_year==periods_M]) %>%
        droplevels()
      
      summary(Data_AF_M$DateTime)
      summary(Data_AF_M)
      
      #This is a VERY key line to help Track2TrackMultiStack stack the tags in the correct stack! 
      Data_AF_M$period <- periods_M
      
      
      #add the number of birds into the summary table
      summary_dat$number_birds[summary_dat$sex == "M" & summary_dat$label_year == periods_M] <- length(unique(Data_AF_M$TagID))
      
      #turn it into a Track using BTOTT
      Data_AF_M_tt <-Track(Data_AF_M) 
      Data_AF_M_tt<-clean_GPS(Data_AF_M_tt, drop_sats = 3, Thres = 30, GAP = 28800) #HH NEW NOTE 2024 analysis: GAP here constrains the function to not interpolate time in between this gap of 28800 seconds = 8 hours (chat message from Chris T 17/01/2025)
      
      # Set ID factor
      Data_AF_M_tt$TagID<-as.factor(as.character(Data_AF_M_tt$TagID)) 
      
      
      
      
      #Male winter post breeding
      periods_M_before <- past_cohort_periods_M[4]
      periods_M <- past_cohort_periods_M[5]
      
      
      Data_W_AfterB_M <- dat.keep_M %>% 
        filter(DateTime >=  past_cohort_behavs_year_M$maxdate_pastcohort_behav[past_cohort_behavs_year_M$label_year==periods_M_before]  & DateTime <= past_cohort_behavs_year_M$maxdate_pastcohort_behav[past_cohort_behavs_year_M$label_year==periods_M]) %>%
        droplevels()
      
      summary(Data_W_AfterB_M$DateTime)
      summary(Data_W_AfterB_M)
      
      #This is a VERY key line to help Track2TrackMultiStack stack the tags in the correct stack! 
      Data_W_AfterB_M$period <- periods_M
      
      
      #add the number of birds into the summary table
      summary_dat$number_birds[summary_dat$sex == "M" & summary_dat$label_year == periods_M] <- length(unique(Data_W_AfterB_M$TagID))
      
  
      #turn it into a Track using BTOTT
      Data_W_AfterB_M_tt <-Track(Data_W_AfterB_M) 
      Data_W_AfterB_M_tt<-clean_GPS(Data_W_AfterB_M_tt, drop_sats = 3, Thres = 30, GAP = 28800) #HH NEW NOTE 2024 analysis: GAP here constrains the function to not interpolate time in between this gap of 28800 seconds = 8 hours (chat message from Chris T 17/01/2025)
      
      # Set ID factor
      Data_W_AfterB_M_tt$TagID<-as.factor(as.character(Data_W_AfterB_M_tt$TagID)) 
      
      
  
    
      
      # Final merge for the cohort #####
      data_past_M <-Track2TrackMultiStack(rbind(Data_W_PreB_M_tt, Data_SF_M_tt, Data_Breed_M_tt, Data_AF_M_tt, Data_W_AfterB_M_tt), by=c("TagID", "period"))
      data_past_M
      
      
      # Save
      setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data/2025 analysis") #HH laptop
      save(data_cohort, file=paste0("NE103_",nyr," report_clean tracking data for ",nyr," past cohorts_Male.RData"))
      
      
    
  
  
  #Final merge all together:
  
  # Final merge for the whole of the year #####
  data_year<-Track2TrackMultiStack(rbind(data_1d_tt, data_1w_tt, data_2_tt, data_6_tt, data_all_tt, Data_W_PreB_F_tt, Data_SF_F_tt, Data_Breed_F_tt, Data_AF_F_tt, Data_W_AfterB_F_tt, Data_W_PreB_M_tt, Data_SF_M_tt, Data_Breed_M_tt, Data_AF_M_tt, Data_W_AfterB_M_tt ), by=c("TagID", "period"))
  data_year
  
  
   # Save
  setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data/2025 analysis") #HH laptop
  save(data_year, file=paste0("NE103_",nyr," report_clean tracking data for all ",nyr," data.RData"))
  
      }
  }

  
  
#save the summary_dat out
#write.csv(summary_dat, here("output/Tables 2025/summarycountbirds_pertimeperiod_peryear.csv"), row.names = F)
 

####.####
# LOAD PACKAGES ####
load_pkg <- rlang::quos(tidyverse,BTOTrackingTools, here, sp, leaflet, terra)  # quos() function to be lazy on "" around each package
lapply(lapply(load_pkg, rlang::quo_name), library, character.only = TRUE)


#ANALYSIS BEGIGNS! #####

## read things in ####
#If need the clean but raw data per row can read in this data:
#data_final_final <- readRDS(here("data/2025 analysis/data_withcohorts_release_sites_tagduration_2021_2024.rds"))

#summary(data_final_final)


#read in the meta data just in case useful:
dt_meta_gsp_TagID_update <- read.csv(here("data/2025 analysis/metadata_TagID_deaddates_notransmision_behaviourdates_2021_2024.csv"), header=T)
head(dt_meta_gsp_TagID_update)


#looooop set up ####
#Previous code read in at this point one item from the list each time and then had to run through it. Here I'm hopefully going to set up a loop
  #to avoid the human error that this could potentially cause! 


# Load - NB load automatically loads the data back in as the same name it was saved out as. this year it reads back in as "data_year" 
nyears <- c("2021", "2022", "2023", "2024")

nyr <- nyears[4]

setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data/2025 analysis") #HH laptop
load(file=paste0("NE103_",nyr," report_clean tracking data for all ",nyr," data.RData"))


#loaded as data_year. rename it:
data <- data_year


#HH NB - useful if going to use a loop
datasplit <- c("1 One Day" ,"2 One Week" ,"3 Two Weeks" , "4 Six Weeks" ,"5 End of December" ,
                "6 Winter pre-breeding" , "7 Spring fuzzy" , "8a Female Breeding Season" , "8b Male Breeding Season" ,
               "9a Female Autumn fuzzy","9b Male Autumn fuzzy", "10 End of December - Winter" )


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


# TIME IN AREA #HH NB - Leaflet plots ####
library(viridisLite)
library(viridis)



# Using BTOTT::
#extract vidiridis for 18 - max number of different birds 


## Basic visualisation of data - uses BTO plot_leaflet ####
BTOTrackingTools::plot_leaflet(data[[1]], lines=FALSE, col=c(scales::viridis_pal()(length(unique(data$`1 One Day`))))) #code update - now "plot_leaflet" not "plot_leaflet_dev"
plot_leaflet(data[[3]], lines=FALSE, col=c(scales::viridis_pal()(length(data[[3]]))))
plot_leaflet(data[[4]], lines=FALSE, col=c(scales::viridis_pal()(length(data[[4]]))))
plot_leaflet(data[[5]], lines=FALSE, col=c(scales::viridis_pal()(length(data[[5]]))))
plot_leaflet(data[[6]], lines=FALSE, col=c(scales::viridis_pal()(length(data[[6]])))) 


plot_leaflet(data[[7]], lines=FALSE, col=c(scales::viridis_pal()(length(data[[7]]))))
plot_leaflet(data[[8]], lines=FALSE, col=c(scales::viridis_pal()(length(data[[8]]))))
plot_leaflet(data[[9]], lines=FALSE, col=c(scales::viridis_pal()(length(data[[9]]))))
plot_leaflet(data[[10]], lines=FALSE, col=c(scales::viridis_pal()(length(data[[10]]))))
plot_leaflet(data[[11]], lines=FALSE, col=c(scales::viridis_pal()(length(data[[11]]))))
plot_leaflet(data[[12]], lines=FALSE, col=c(scales::viridis_pal()(length(data[[12]]))))
plot_leaflet(data[[2]], lines=FALSE, col=c(scales::viridis_pal()(length(data[[2]]))))

  

## by tide Interactive plot with tide data for output #####

#read one of these in at a time and then the rows below
#data_tide<-BTOTrackingTools::TrackStack2Track(data[[1]])
#data_tide<-TrackStack2Track(data[[3]])
#data_tide<-TrackStack2Track(data[[4]])
#data_tide<-TrackStack2Track(data[[5]])


#data_tide<-TrackStack2Track(data[[7]]) 
#data_tide<-TrackStack2Track(data[[8]])
#data_tide<-TrackStack2Track(data[[9]]) 
#data_tide<-TrackStack2Track(data[[10]])
#data_tide<-TrackStack2Track(data[[11]])
#data_tide<-TrackStack2Track(data[[12]])
#data_tide<-TrackStack2Track(data[[2]])


#only need this plot for all data up to December
data_tide<-TrackStack2Track(data[[6]])

#Filter data_tide for 2024 because the bouy sensor stopped recording so we only have high and low tided data up to 7th Dec 2024 (inclusive) 
data_tide <- data_tide %>% filter(DateTime < "2024-12-08")


data_tide<-data_tide %>% filter(tide!="NA")
data_tide$Tide<-as.character(fct_recode(data_tide$tide, "High tide" = "HW", "Low tide" = "LW") )
plot_leaflet(data_tide, plotby="Tide", lines=FALSE, col=c("#31688EFF","#35B779FF")) #code update - now "plot_leaflet" not "plot_leaflet_dev"



## save per release site for 1day post release ####
scales::viridis_pal()(9)
#"#440154FF", "#482878FF" ,"#3E4A89FF" ,"#31688EFF", "#26828EFF", "#1F9E89FF", "#35B779FF", "#6DCD59FF", "#B4DE2CFF" ,"#FDE725FF"
#"#440154FF" ,"#472D7BFF", "#3B528BFF" ,"#2C728EFF" ,"#21908CFF", "#27AD81FF", "#5DC863FF" ,"#AADC32FF", "#FDE725FF"

data_site <- TrackStack2Track(data[[1]])

data_site_ken <- droplevels(data_site %>% filter(data_site$release_site_final=="Ken Hill"))
unique(data_site_ken$TagID) #5
plot_leaflet(data_site_ken, lines=FALSE, col=viridis_pal()(length(unique(data_site_ken$TagID)))) # KMB, new colour scheme, needs libraries Viridis and ViridisLite

data_site_san <- droplevels(data_site %>% filter(data_site$release_site_final=="Sandringham 2"))
unique(data_site_san$TagID) #5
plot_leaflet(data_site_san, lines=FALSE, col=viridis_pal()(length(unique(data_site_san$TagID))))# KMB, new colour scheme, needs libraries Viridis and ViridisLite


## save per release site for 1wk post release ####

data_site <- TrackStack2Track(data[[3]])

data_site_ken <- droplevels(data_site %>% filter(data_site$release_site_final=="Ken Hill"))
unique(data_site_ken$TagID) #8
plot_leaflet(data_site_ken, lines=FALSE, col=viridis_pal()(length(unique(data_site_ken$TagID)))) # KMB, new colour scheme, needs libraries Viridis and ViridisLite

data_site_san <- droplevels(data_site %>% filter(data_site$release_site_final=="Sandringham 2"))
unique(data_site_san$TagID) #9 (HH had 6 but 9 here for KMB)
plot_leaflet(data_site_san, lines=FALSE, col=viridis_pal()(length(unique(data_site_san$TagID)))) # KMB, new colour scheme, needs libraries Viridis and ViridisLite


# basic colour mark sightings plot using leaflet:: directly #HH NB - for the fieldwork year 2023-24 KMB going to do this bit ####CHECK THIS FOR THIS YEAR!!


####.####

#MAIN LOOP FOR ANALYSIS, FIGURES ETC... ####

## TIME IN AREA -- AREA USE UTILISATION DISTRIBUTIONS #HH NB - this essentially creates a grided version of a KDE ####
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



## PLOTTING  ##HH  NB - Utilisation Distribution TIA plots - THE WASH ####

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


#Habitat Selection ##
# Load amt:: package
library(amt) 

## Load Land Cover Map 2021 25m Raster
#NOTE THIS IS THE WASH ONLY!!!
#landuse <- raster::raster(here("data","NE103_LCM2021","LCM.tif"))

#this is a new raster
landuse <- raster::raster(here("data","NE103_LCM2021_UK","gblcm25m2021_UK.tif"))
 

#RSF model set up ##
#have to run this long hand so no loops over the five categories 

#create a data frame that can be rbound to for the AUC outputs:
AUC_dat <- data.frame()

#and one for the beta data
beta_dat <- data.frame()


#and one for the my tests outputs
mytest.contrast_dat <- data.frame()


# Stat testing RSF - two-stage modeling ##
library(emmeans)




#loop to run Utilisation Distribution TIA plots & habitat selection - HH ####

#set up a loop here to run through all the years and all the categories:

#HH NB - useful if going to use a loop
datasplit <- c("1 One Day" ,"2 One Week" ,"3 Two Weeks" , "4 Six Weeks" ,"5 End of December" ,
               "6 Winter pre-breeding" , "7 Spring fuzzy" , "8a Female Breeding Season" , "8b Male Breeding Season" ,
               "9a Female Autumn fuzzy","9b Male Autumn fuzzy", "10 End of December - Winter" )


plotlabels <- c("One day post-release" ,"One week post-release" ,"Two weeks post-release" , "Six weeks post-release" ,"July-December" ,
             "Winter - pre-breeding" , "Spring transition" , "Breeding season - Female" , "Breeding season - Male" ,
             "Autumn transition - Female","Autumn transition - Male", "Winter - post-breeding")

filelabels <- c("1_OneDay" ,"2_OneWeek" ,"3_TwoWeeks" , "4_SixWeeks" ,"5_July_December" ,
               "6_WinterPreBreed" , "7_Spring_transition" , "8a_Breeding_female" , "8b_Breeding_male" ,
               "9a_Autumn_transition_female","9b_Autumn_transition_male", "10_WinterPostBreed" )


nyears <- c("2021", "2022", "2023", "2024")


#SET SEED - THEN run through the loop ######
set.seed(c(1,2,3,4))

for(y in 1:length(nyears)){

nyr <- nyears[y]

setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data/2025 analysis") #HH laptop
load(file=paste0("NE103_",nyr," report_clean tracking data for all ",nyr," data.RData"))


#loaded as data_year. rename it:
data <- data_year


if(nyr == "2021"){
  
  datasplit <- c("1 One Day" ,"2 One Week" ,"3 Two Weeks" , "4 Six Weeks" ,"5 End of December")
  
  plotlabels <- c("One day post-release" ,"One week post-release" ,"Two weeks post-release" , "Six weeks post-release" ,"July-December" )
  
  filelabels <- c("1_OneDay" ,"2_OneWeek" ,"3_TwoWeeks" , "4_SixWeeks" ,"5_July_December" )
  
  
  
}else if(nyr == "2022"){
  
  datasplit <- c("1 One Day" ,"2 One Week" ,"3 Two Weeks" , "4 Six Weeks" ,"5 End of December" ,
                 "6 Winter pre-breeding" , "7 Spring fuzzy" , "8a Female Breeding Season" , 
                 "9a Female Autumn fuzzy" ) #winter post breeding removed because only 7 fixes
  
  plotlabels <- c("One day post-release" ,"One week post-release" ,"Two weeks post-release" , "Six weeks post-release" ,"July-December" ,
                  "Winter - pre-breeding" , "Spring transition" , "Breeding season - Female" , 
                  "Autumn transition - Female")
  
  filelabels <- c("1_OneDay" ,"2_OneWeek" ,"3_TwoWeeks" , "4_SixWeeks" ,"5_July_December" ,
                  "6_WinterPreBreed" , "7_Spring_transition" , "8a_Breeding_female" ,
                  "9a_Autumn_transition_female" )
  
  
  
  
} else {
  
  datasplit <- c("1 One Day" ,"2 One Week" ,"3 Two Weeks" , "4 Six Weeks" ,"5 End of December" ,
                 "6 Winter pre-breeding" , "7 Spring fuzzy" , "8a Female Breeding Season" , "8b Male Breeding Season" ,
                 "9a Female Autumn fuzzy","9b Male Autumn fuzzy", "10 End of December - Winter" )
  
  plotlabels <- c("One day post-release" ,"One week post-release" ,"Two weeks post-release" , "Six weeks post-release" ,"July-December" ,
                  "Winter - pre-breeding" , "Spring transition" , "Breeding season - Female" , "Breeding season - Male" ,
                  "Autumn transition - Female","Autumn transition - Male", "Winter - post-breeding")
  
  filelabels <- c("1_OneDay" ,"2_OneWeek" ,"3_TwoWeeks" , "4_SixWeeks" ,"5_July_December" ,
                  "6_WinterPreBreed" , "7_Spring_transition" , "8a_Breeding_female" , "8b_Breeding_male" ,
                  "9a_Autumn_transition_female","9b_Autumn_transition_male", "10_WinterPostBreed" )
  
  
  
}



  for(p in 1:length(datasplit)){
    
    TP <- datasplit[p] 
    
    #add this if loop in for data split label to include the correct year
    if(p > 5){
      TP <- paste0("",TP," ",nyr,"")
    }
    
    #TIA analysis: select the specific list from the 'data' set
    tia_dat<-data[[TP]]
    
     
    #add this in so that we can add it in as a label in the file name below
    cohort <- ifelse(p > 5, "PASTCOHORT", "COHORT")
    
    #extract out the file label
    filelab <- filelabels[p]
    
    #add in plot label
    plotlab <- plotlabels[p]
    
    
    #add this if loop in for plot label for july-december to include the correct year
    if(p >4){
      plotlab <- paste0("",plotlab," ",nyr,"")
    }
    
    
    ## Utilisation Distribution TIA plots####
    #HH NB - these lines below then take each section of the data:
    #and 1) find the boundary for the grid, 2) then create a grid with 500 as the cellsize, 
    #3) calculate the amount of time each bird spends in each cell for a) whole population and b) individual birds 
    # get bounds for the grid 
    llyrb = get_bounds(tia_dat, p4s=p4) # Defaults to UK BNG p4s = sp::CRS("+init=epsg:27700") 
    ##CHECK THAT I ACTUALLY STILL NEED p4 HERE!!!!!!!!
    
    # run TIA (trial and error on suitable cell size) # grid of cells. HH NB _ FYI - some of these have 'trips' removed. Chris T reckons this is because of the extra filtering and so for some points there will not be enough to do the amount of time in cell count and so they are removed
    indata_grd <- get_TIA_grd(tia_dat, xRa=llyrb$xRa, yRa=llyrb$yRa, cellsize = 500, p4s=p4) # Laptop will not process next step if smaller grid size #Gary's code = cellsize=500
    
    #NOTE there are various warnings where a trip has too few data so it removed. 
      #NOTE Error thrown up for 2023 Spring transition 2023: #error in .local cannot derive coordinates from non-numeric matrix error only for "Yf(0E)O/-:Y/m" and there are only 2 GPS fixes so that is likely to be the issue - these are now removed from data_final_final and code works
      #NOTE Error thrown up for 2024 Winter post breeding 2024: Error in `$<-.data.frame`(`*tmp*`, "dum", value = 1) :     replacement has 1 row, data has 0.
          ##this is for tag: "LU" but I suspect: "LJ", "LV" and "XJ" will also throw this error as they have less than 10 tracks too
    
    #NOTE UPDATE (17/01/2025 - chat message from Chris T). Some birds cause an error at this point, there is as two fold reason for this
      #1 when the data was cleaned up ("clean_GPS") 'GAP' is used to restrain the analysis to not interpolate time in between 'gaps', this becomes important when the data is gappy anyway (e.g. winter when tags aren't recording as much),
            #the default 'gap' of 8hrs is used in this analysis. This will isolate data points when the data is already gappy.
      #2 once filtered by this 'gap' then the data is put into 'trips' which needs a minimum of three fixes per trip for data resolution. 
      #because of both of these filters it means that some birds end up with no trips (due to limited fixes to start with)
      #manual updating of the code is needed higher up to filter these out (ideally this would be able to be done in the function but it is not possible yet)
    
    
    # rank the time cumulatively for plotting for each bird. #ranks the time spent in each cell
    grd_rank_all<- rank_time(indata_grd, population = TRUE) # Population level
    
    #grd_rank_birds<- rank_time(indata_grd, population = FALSE) # Individual level - currently not used
    
  
  
    # Set directory (outside of Github here)
    dir <- "C:/Users/hannah.hereward/Documents/Projects/2024_curlewheadstarting/curlew_headstarting/output/Figures 2025/TIA/" #HH laptop directory
    plot_name<-paste0("NE103_Headstart CURLE_TIA_",nyr,"_",cohort,"_",filelab,".tiff")
    
    
    # Set plot device (saving hi-res base R maps)
    tiff(paste0(dir,plot_name), width=25, height=23, units="cm", pointsize=18, res=600, compression ="lzw")
    
    #HH NB: updated as sp package was discontinued. terra used now according to plot_TIA
    #removed: 
    terra::plot(ukmap$geometry, xlim=xRa, ylim=yRa,col="grey80",border="grey80", axes=T, yaxt="n",  #need to specify here ukmap$geometry
                xaxt="n", xlab="Longitude", ylab="Latitude",
                main=paste0(plotlab))# UPDATE MANUALLY                     
    #axis(1)
    #axis(2)
    axis(1, at=seq(min(xRa), max(xRa),by=5000), labels=round(lab_long,2)) 
    axis(2, at=seq(min(yRa), max(yRa),by=5000), labels=round(lab_lat,2))
    
    
    #HH NB. had error for this plot about memory. UPDATE: needed to specify the ukmap$geometry in the terra::plot above and now it works
    # UPDATE INDIVIDUAL BETWEEN PLOTS
    plot_TIA(data=grd_rank_all,Add=TRUE,                    # UPDATE ID SELECTION. grd_rank_all is the only one that was used by Gary before - one could put in "grd_rank_birds". BUT NOT DONE FOR THIS ANALYSIS!
             xra=xRa, yra=yRa,
             g_levs = c(1,0.95,0.75,0.5),
             c_levs = c(0.95,0.75,0.5),
             col_ramp_grd =c("#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF"), #TIA colours for the 50%, 75%, 95% and 100%
             #col_ramp_con =c("#31688EFF", "#35B779FF", "#FDE725FF"), #colours for the countour lines rather than grided 
             cont_typ=1) # if this is 4 you can plot it outside the function and it returns an object in R 
    
    #HH NB. dev.off needs running to 'close' the tiff and save it - without running this bit it won't save !
    dev.off()
    
    
    #adding mini loop to extract UK wide maps:
    if(nyr==2024 & p > 5){
      
      #Use same colony lat and long as for the wash
      
      # Set axes limit (units m here) - trial and error to set suitable bounds. centered on the colony. units m
      
      #whole of UK
      xRa_UK<-c(-622435.4,224987.6)
      yRa_UK<-c(-414008.8,886812.5)
      
      
      new_lat_lower_UK <- round(ColLat + (min(yRa_UK) * m),1)     ## multiply xyRa by 100 if working in p4 units km
      new_lat_upper_UK <- round(ColLat + (max(yRa_UK) * m),1)
      new_long_lower_UK <- round(ColLon + (min(xRa_UK) * m) / cos(ColLat * (pi / 180)),1)
      new_long_upper_UK <- round(ColLon + (max(xRa_UK) * m) / cos(ColLat * (pi / 180)),1)	
      
      lab_long_UK<-seq(new_long_lower_UK, new_long_upper_UK,length.out=length(seq(min(xRa_UK), max(xRa_UK),by=5000)))
      lab_lat_UK<-seq(new_lat_lower_UK, new_lat_upper_UK,length.out=length(seq(min(yRa_UK), max(yRa_UK),by=5000)))
      
      
      
      
      
      # Set directory (outside of Github here)
      dir <- "C:/Users/hannah.hereward/Documents/Projects/2024_curlewheadstarting/curlew_headstarting/output/Figures 2025/TIA/" #HH laptop directory
      plot_name<-paste0("NE103_Headstart CURLE_TIA_",nyr,"_",cohort,"_",filelab,"_UK.tiff")
      
      
      # Set plot device (saving hi-res base R maps)
      tiff(paste0(dir,plot_name), width=25, height=23, units="cm", pointsize=18, res=600, compression ="lzw")
      
      #HH NB: updated as sp package was discontinued. terra used now according to plot_TIA
      #removed: 
      terra::plot(ukmap$geometry, xlim=xRa_UK, ylim=yRa_UK,col="grey80",border="grey80", axes=T, yaxt="n",  #need to specify here ukmap$geometry
                  xaxt="n", xlab="Longitude", ylab="Latitude",
                  main=paste0(plotlab))# UPDATE MANUALLY                     
      #axis(1)
      #axis(2)
      axis(1, at=seq(min(xRa_UK), max(xRa_UK),by=5000), labels=round(lab_long_UK,2)) 
      axis(2, at=seq(min(yRa_UK), max(yRa_UK),by=5000), labels=round(lab_lat_UK,2))
      
      
      #HH NB. had error for this plot about memory. UPDATE: needed to specify the ukmap$geometry in the terra::plot above and now it works
      # UPDATE INDIVIDUAL BETWEEN PLOTS
      plot_TIA(data=grd_rank_all,Add=TRUE,                    # UPDATE ID SELECTION. grd_rank_all is the only one that was used by Gary before - one could put in "grd_rank_birds". BUT NOT DONE FOR THIS ANALYSIS!
               xra=xRa_UK, yra=yRa_UK,
               g_levs = c(1,0.95,0.75,0.5),
               c_levs = c(0.95,0.75,0.5),
               col_ramp_grd =c("#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF"), #TIA colours for the 50%, 75%, 95% and 100%
               #col_ramp_con =c("#31688EFF", "#35B779FF", "#FDE725FF"), #colours for the countour lines rather than grided 
               cont_typ=1) # if this is 4 you can plot it outside the function and it returns an object in R 
      
      #HH NB. dev.off needs running to 'close' the tiff and save it - without running this bit it won't save !
      dev.off()
      
      
    #and one more additional loop for east England zoom
   
      #Use same colony lat and long as for the wash
      
      # Set axes limit (units m here) - trial and error to set suitable bounds. centered on the colony. units m
      
      #whole of UK
      #southeast of the UK
      xRa_SE<-c(-122435.4,24987.6)
      yRa_SE<-c(-214008.8,86812.5)
      
      
      new_lat_lower_SE <- round(ColLat + (min(yRa_SE) * m),1)     ## multiply xyRa by 100 if working in p4 units km
      new_lat_upper_SE <- round(ColLat + (max(yRa_SE) * m),1)
      new_long_lower_SE <- round(ColLon + (min(xRa_SE) * m) / cos(ColLat * (pi / 180)),1)
      new_long_upper_SE <- round(ColLon + (max(xRa_SE) * m) / cos(ColLat * (pi / 180)),1)	
      
      lab_long_SE<-seq(new_long_lower_SE, new_long_upper_SE,length.out=length(seq(min(xRa_SE), max(xRa_SE),by=5000)))
      lab_lat_SE<-seq(new_lat_lower_SE, new_lat_upper_SE,length.out=length(seq(min(yRa_SE), max(yRa_SE),by=5000)))
      
      
      
      
      
      # Set directory (outside of Github here)
      dir <- "C:/Users/hannah.hereward/Documents/Projects/2024_curlewheadstarting/curlew_headstarting/output/Figures 2025/TIA/" #HH laptop directory
      plot_name<-paste0("NE103_Headstart CURLE_TIA_",nyr,"_",cohort,"_",filelab,"_SouthEastEngland.tiff")
      
      
      # Set plot device (saving hi-res base R maps)
      tiff(paste0(dir,plot_name), width=25, height=23, units="cm", pointsize=18, res=600, compression ="lzw")
      
      #HH NB: updated as sp package was discontinued. terra used now according to plot_TIA
      #removed: 
      terra::plot(ukmap$geometry, xlim=xRa_SE, ylim=yRa_SE,col="grey80",border="grey80", axes=T, yaxt="n",  #need to specify here ukmap$geometry
                  xaxt="n", xlab="Longitude", ylab="Latitude",
                  main=paste0(plotlab))# UPDATE MANUALLY                     
      #axis(1)
      #axis(2)
      axis(1, at=seq(min(xRa_SE), max(xRa_SE),by=5000), labels=round(lab_long_SE,2)) 
      axis(2, at=seq(min(yRa_SE), max(yRa_SE),by=5000), labels=round(lab_lat_SE,2))
      
      
      #HH NB. had error for this plot about memory. UPDATE: needed to specify the ukmap$geometry in the terra::plot above and now it works
      # UPDATE INDIVIDUAL BETWEEN PLOTS
      plot_TIA(data=grd_rank_all,Add=TRUE,                    # UPDATE ID SELECTION. grd_rank_all is the only one that was used by Gary before - one could put in "grd_rank_birds". BUT NOT DONE FOR THIS ANALYSIS!
               xra=xRa_SE, yra=yRa_SE,
               g_levs = c(1,0.95,0.75,0.5),
               c_levs = c(0.95,0.75,0.5),
               col_ramp_grd =c("#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF"), #TIA colours for the 50%, 75%, 95% and 100%
               #col_ramp_con =c("#31688EFF", "#35B779FF", "#FDE725FF"), #colours for the countour lines rather than grided 
               cont_typ=1) # if this is 4 you can plot it outside the function and it returns an object in R 
      
      #HH NB. dev.off needs running to 'close' the tiff and save it - without running this bit it won't save !
      dev.off()
      
      
      
    }
    
    
    
    
    ## HABITAT SELECTION ####
    #Habitat selection
    trk_dat<-TrackStack2Track(data[[TP]])
   
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
    summary(trk)
    
    trk <- trk %>% 
      filter(!is.na(layer))
    
    #table to check the count of fixes 
    test <- trk %>% group_by(id) %>% count()
    
    #if loop added because "9L" is mostly in France but has one fix in the UK so has one habitat point (Female spring fuzzy 2024). Because of having a lot of other fixes it has not been filtered and remains in the dataset
      #INSTEAD I COULD FILTER IT FROM THE DATA SET??????
    if(min(test$n) <2){
      
      trk <- trk %>% filter(! trk$id == test$id[test$n<2])
      
    }
    
    #table to check the count of fixes 
    test <- trk %>% group_by(id) %>% count()
    
    # Check sampling rate
    summarize_sampling_rate_many(trk, cols="id")
    
    
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
    
    
    if(nyr==2024 & TP == "8b Male Breeding Season 2024"){
    #for Appendix save out a example plot of observed vrs random points - using 8 as only two birds so not too confusing with overlapping convex polygons
    #create the file 
    jpeg(file=paste0("C:/Users/hannah.hereward/Documents/Projects/2024_curlewheadstarting/curlew_headstarting/output/Figures 2025/NE103_Headstart CURLEW_",nyr,"_APPENDIXplot_8b_Breeding_M.jpg"), width=15, height=15, units="cm", res=300)
    #run the plot
    plot(avail.pts[avail.pts$id=="Yf(9J)O/-:Y/m_KenHill",])
    #close the file
    dev.off() 
    }
    
    
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
    #might get a warning about unknown levels - this is because they're not in the datafile
    
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
    
    
    # Save rsfdat for each time period
    setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data/2025 analysis/") #HH laptop
    save(rsfdat, file=paste0("NE103_",nyr," report_RSF_data_cohort_",nyr,"_",filelab,".RData")) 
    
    
    
    ## RSF plotting ##HH NB - Utilisation Distribution plots, habitat avalibility ####
    
    ## Available/Used Plot 
    na.omit(rsfdat) %>% #filter(id=="Yf(0E)O/-:Y/m") %>%	                          	# Update period or ID
      ggplot(.,  aes(x=layer,group=used))                                       +	      # select data and variables - using na.omit() here to exclude random points offshore outside LCM area. HH NB - LCM = layer in 2022 and 2023 LCM data so need to change this to layer
      geom_bar(position=position_dodge(), aes(y=after_stat(prop), fill = used),
               stat="count", colour="black")                                +       # select barplot of proportions presented side by side with black outline
      scale_fill_manual(values=c("grey70", "grey20"))                       + 		  # define colours, plenty of good built in palettes if colour can be used
      labs(y = "Proportion of fixes\n", fill="used", x="\nHabitat")         + 			# labels, \n indicates space between line and text
      theme_classic()                                                       +       # remove default grid lines and grey background
      theme(legend.title=element_blank(),  																	 			  # remove legend title
            legend.position = c(0.9,0.9),                                           # specify legend position inside plot area
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))   +       # rotated x axis labels for individual plots
      #scale_y_continuous(expand = expansion(mult = c(0, .1)))               +       # remove gap between bars and axis lines
      #ylim(c(0,0.6)) +                                                              #HH NB - use ylim 2023 = c(0,0.6) and scale_y_continuous for 2021&2022 
      scale_y_continuous(breaks = seq(0,1,by=0.2), limits =c(0,1)) +
      theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14)) +
      ggtitle(paste0("",plotlab,""))   #    +
    #facet_grid(rows=vars(release)) # if by tide:  facet_grid(rows=vars(tide)) or release site:   facet_grid(rows=vars(release)) or cohort:  facet_grid(rows=vars(cohort))
    
    
    # Save plot (outside of Github)
    setwd("C:/Users/hannah.hereward/Documents/Projects/2024_curlewheadstarting/curlew_headstarting/output/Figures 2025/Habitat_prop/")
    ggsave(paste0("NE103_",nyr,"_Headstart CURLE_RSF plot_",filelab,".jpg"), width=15, height=15, units="cm", dpi=300)  ## UPDATE FILENAME
    
    
    
    #for cohorts
    if(p<6){
      
      
      ## Available/Used Plot 
      na.omit(rsfdat) %>%                               	                          	# Update period or ID
         ggplot(.,  aes(x=layer,group=used))                                       +	      # select data and variables - using na.omit() here to exclude random points offshore outside LCM area. HH NB - LCM = layer in 2022 and 2023 LCM data so need to change this to layer
        geom_bar(position=position_dodge(), aes(y=after_stat(prop), fill = used),
                 stat="count", colour="black")                                +       # select barplot of proportions presented side by side with black outline
        scale_fill_manual(values=c("grey70", "grey20"))                       + 		  # define colours, plenty of good built in palettes if colour can be used
        labs(y = "Proportion of fixes\n", fill="used", x="\nHabitat")         + 			# labels, \n indicates space between line and text
        theme_classic()                                                       +       # remove default grid lines and grey background
        theme(legend.title=element_blank(),  																	 			  # remove legend title
              legend.position = c(0.9,0.9),                                           # specify legend position inside plot area
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))   +       # rotated x axis labels for individual plots
        #scale_y_continuous(expand = expansion(mult = c(0, .1)))               +       # remove gap between bars and axis lines
        #ylim(c(0,0.6)) +                                                              #HH NB - use ylim 2023 = c(0,0.6) and scale_y_continuous for 2021&2022 
        scale_y_continuous(breaks = seq(0,1,by=0.2), limits =c(0,1)) +
        theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
              axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14)) +
        ggtitle(paste0("",plotlab,""))     +
        facet_wrap(facets=vars(cohort), ncol=1, strip.position = "right") 
      
      
      # Save plot (outside of Github)
      setwd("C:/Users/hannah.hereward/Documents/Projects/2024_curlewheadstarting/curlew_headstarting/output/Figures 2025/Habitat_prop/")
      ggsave(paste0("NE103_",nyr,"_Headstart CURLE_RSF plot_",filelab,"_COHORT.jpg"), width=15, height=15, units="cm", dpi=300)  ## UPDATE FILENAME
      
    }
    
    
    
    #for release site
    if(p<6){
      
      
      ## Available/Used Plot 
      na.omit(rsfdat) %>%                               	                          	# Update period or ID
        ggplot(.,  aes(x=layer,group=used))                                       +	      # select data and variables - using na.omit() here to exclude random points offshore outside LCM area. HH NB - LCM = layer in 2022 and 2023 LCM data so need to change this to layer
        geom_bar(position=position_dodge(), aes(y=after_stat(prop), fill = used),
                 stat="count", colour="black")                                +       # select barplot of proportions presented side by side with black outline
        scale_fill_manual(values=c("grey70", "grey20"))                       + 		  # define colours, plenty of good built in palettes if colour can be used
        labs(y = "Proportion of fixes\n", fill="used", x="\nHabitat")         + 			# labels, \n indicates space between line and text
        theme_classic()                                                       +       # remove default grid lines and grey background
        theme(legend.title=element_blank(),  																	 			  # remove legend title
              legend.position = c(0.9,0.9),                                           # specify legend position inside plot area
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))   +       # rotated x axis labels for individual plots
        #scale_y_continuous(expand = expansion(mult = c(0, .1)))               +       # remove gap between bars and axis lines
        #ylim(c(0,0.6)) +                                                              #HH NB - use ylim 2023 = c(0,0.6) and scale_y_continuous for 2021&2022 
        scale_y_continuous(breaks = seq(0,1,by=0.2), limits =c(0,1)) +
        theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
              axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14)) +
        ggtitle(paste0("",plotlab,""))     +
        facet_wrap(facets=vars(release), ncol=1, strip.position = "right") 
      
      
      # Save plot (outside of Github)
      setwd("C:/Users/hannah.hereward/Documents/Projects/2024_curlewheadstarting/curlew_headstarting/output/Figures 2025/Habitat_prop/")
      ggsave(paste0("NE103_",nyr,"_Headstart CURLEW_RSF plot_",filelab,"_SITE.jpg"), width=15, height=15, units="cm", dpi=300)  ## UPDATE FILENAME
      
    }
    
    
    
    #for the july-dec tide. NOTE for using this for 2021 data need to cap this at November because the tide data in Dec 2021 is patchy
    if(p==5){
      
      if(nyr==2024){ #added this loop in because in 2024 the tide data stopped after the 7th Dec.
      
        
        #need to filter out the data points that are beyond the 7th Dec 2024 #39 points removed
        trk_filter<-trk %>% filter(! t_ > "2024-12-07")
        
        
        
        avail.pts_filter <- trk_filter %>%  nest(data=-c("id", "tide", "cohort", "release")) %>% 
          mutate(rnd_pts = map(data, ~ random_points(., factor = 20, type="random"))) %>% 	
          select(id, tide,cohort,release, rnd_pts) %>%  # you don't want to have the original point twice, hence drop data
          unnest_legacy(cols=c(rnd_pts))
        
        
        # Assign class as lost during nesting
        class(avail.pts_filter) <- c("random_points", class(avail.pts_filter))
        
        
        # Extract LCM values for random points
        avail.pts_filter <- avail.pts_filter %>% 
          extract_covariates(landuse)
        
        
        # Make factor plotting friendly
        avail.pts_filter$used<-as.factor(avail.pts_filter$case_)
        avail.pts_filter$used<-fct_recode(avail.pts_filter$used, "Available" = "FALSE", "Used" = "TRUE")
        
        # Tidy LCM variable  # variable name 'layer' with new landuse data for 2022. HH NB - note that this produces warning messages presumably because some of the habitat numbers don't feature in each extraction
        rsfdat_filter <- avail.pts_filter %>%  mutate(
          layer = as.character(layer), 
          layer = fct_collapse(layer,
                               "Arable" = c("3"),
                               "Grassland" = c("4","5","6","7"),
                               "Coastal rock" = c("15", "17"),
                               "Coastal sediment" = c("16", "18"),
                               "Saltmarsh" = c("19"),
                               "Other" = c("1", "2","8", "9", "10", "11", "12","13","14", "20", "21")))
        #might get a warning about unknown levels - this is because they're not in the datafile
        
        # Reorder factor level
        rsfdat_filter$layer<- factor(rsfdat_filter$layer, levels=c("Coastal sediment","Saltmarsh","Coastal Rock","Arable","Grassland","Other"))
        
        
        # set response to numeric
        rsfdat_filter <- rsfdat_filter %>% mutate(case_ = as.numeric(case_))
        
        
        # Weight available data 
        rsfdat_filter$w <- ifelse(rsfdat_filter$case_ == 1, 1, 5000)
        
        
        # Set individual habitat factors (pooling all other habitats into single reference level)
        rsfdat_filter$Coastal <- ifelse(rsfdat_filter$layer == "Coastal sediment", 1, 0)
        rsfdat_filter$Saltmarsh <- ifelse(rsfdat_filter$layer == "Saltmarsh", 1, 0)
        rsfdat_filter$Arable <- ifelse(rsfdat_filter$layer == "Arable", 1, 0)
        rsfdat_filter$Grassland <- ifelse(rsfdat_filter$layer == "Grassland", 1, 0)
        rsfdat_filter$Other <- ifelse(rsfdat_filter$layer == "Other", 1, 0)
        
        
        
        #rename the RSF files based on the timeframe - 1wk,2wks,6wks,all Jul-Dec
        #HH NB - some of the categories will have NAs some of these are because they are too far out into the sea for the LCM map to categories them BUT others are outside of the UK and therefore outside of the UK LCM!
        rsfdat_filter
        
        
        
        
        
      ## Available/Used Plot 
      na.omit(rsfdat_filter) %>%                               	                          	# Update period or ID
        filter(tide != "NA") %>%                                                          #FILTER ONLY TO BE USED FOR TIDE TO REMOVE THE NAs
        ggplot(.,  aes(x=layer,group=used))                                       +	      # select data and variables - using na.omit() here to exclude random points offshore outside LCM area. HH NB - LCM = layer in 2022 and 2023 LCM data so need to change this to layer
        geom_bar(position=position_dodge(), aes(y=after_stat(prop), fill = used),
                 stat="count", colour="black")                                +       # select barplot of proportions presented side by side with black outline
        scale_fill_manual(values=c("grey70", "grey20"))                       + 		  # define colours, plenty of good built in palettes if colour can be used
        labs(y = "Proportion of fixes\n", fill="used", x="\nHabitat")         + 			# labels, \n indicates space between line and text
        theme_classic()                                                       +       # remove default grid lines and grey background
        theme(legend.title=element_blank(),  																	 			  # remove legend title
              legend.position = c(0.9,0.9),                                           # specify legend position inside plot area
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))   +       # rotated x axis labels for individual plots
        #scale_y_continuous(expand = expansion(mult = c(0, .1)))               +       # remove gap between bars and axis lines
        #ylim(c(0,0.6)) +                                                              #HH NB - use ylim 2023 = c(0,0.6) and scale_y_continuous for 2021&2022 
        scale_y_continuous(breaks = seq(0,1,by=0.2), limits =c(0,1)) +
        theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
              axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14)) +
        ggtitle(paste0("",plotlab,""))     +
      facet_wrap(facets=vars(tide), ncol=1, strip.position = "right") 
      
      
      # Save plot (outside of Github)
      setwd("C:/Users/hannah.hereward/Documents/Projects/2024_curlewheadstarting/curlew_headstarting/output/Figures 2025/Habitat_prop/")
      ggsave(paste0("NE103_",nyr,"_Headstart CURLEW_RSF plot_",filelab,"_TIDE.jpg"), width=15, height=15, units="cm", dpi=300)  ## UPDATE FILENAME
      
      
      
      
      
      
      
      
      
      }else{
        
        ## Available/Used Plot 
        na.omit(rsfdat) %>%                               	                          	# Update period or ID
          filter(tide != "NA") %>%                                                          #FILTER ONLY TO BE USED FOR TIDE TO REMOVE THE NAs
          ggplot(.,  aes(x=layer,group=used))                                       +	      # select data and variables - using na.omit() here to exclude random points offshore outside LCM area. HH NB - LCM = layer in 2022 and 2023 LCM data so need to change this to layer
          geom_bar(position=position_dodge(), aes(y=after_stat(prop), fill = used),
                   stat="count", colour="black")                                +       # select barplot of proportions presented side by side with black outline
          scale_fill_manual(values=c("grey70", "grey20"))                       + 		  # define colours, plenty of good built in palettes if colour can be used
          labs(y = "Proportion of fixes\n", fill="used", x="\nHabitat")         + 			# labels, \n indicates space between line and text
          theme_classic()                                                       +       # remove default grid lines and grey background
          theme(legend.title=element_blank(),  																	 			  # remove legend title
                legend.position = c(0.9,0.9),                                           # specify legend position inside plot area
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))   +       # rotated x axis labels for individual plots
          #scale_y_continuous(expand = expansion(mult = c(0, .1)))               +       # remove gap between bars and axis lines
          #ylim(c(0,0.6)) +                                                              #HH NB - use ylim 2023 = c(0,0.6) and scale_y_continuous for 2021&2022 
          scale_y_continuous(breaks = seq(0,1,by=0.2), limits =c(0,1)) +
          theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
                axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14)) +
          ggtitle(paste0("",plotlab,""))     +
          facet_wrap(facets=vars(tide), ncol=1, strip.position = "right") 
        
        
        # Save plot (outside of Github)
        setwd("C:/Users/hannah.hereward/Documents/Projects/2024_curlewheadstarting/curlew_headstarting/output/Figures 2025/Habitat_prop/")
        ggsave(paste0("NE103_",nyr,"_Headstart CURLEW_RSF plot_",filelab,"_TIDE.jpg"), width=15, height=15, units="cm", dpi=300)  ## UPDATE FILENAME
        
        
        
        
      }
      
      
    }
    
    
    
    ## RSS models and plotting ## HH NB - Relative Selection Strength graphs ####
    ## Relative Selection Strength models (ran manually for time period and habitat for now)
    # exp(estimate) for Relative Selection Strength from RSF models
    
    #need rsfdat
    
    #HH NB - code copied down from above - this seems to be the only code needed from the hashtagged out code above 
    ## Calculate error bars (in progress - clunky)
    x<-as.data.frame(rsfdat %>% with(table(id,used,layer))) #HH NB - LCM = layer in 2022 and 2023 LCM data so need to change this to layer
    x<-x %>% filter (layer!="Coastal Rock") # HH NB - Gary's code here removed the 2021 bird and coastal rock layer. May or may not be needed for each time period
    
    
    
    #UPDATE - can't loop because the habitat categories in the rsfdat file are column headings and the column selection isn't working 
            #need another for loop here to loop over the five habitat categories: Coastal,Grassland, Saltmarsh, Arable, Other
    #So instead copying the same code from last year that I updated long hand 
    
    ### coastal
    #HH NB: list of separate columns need to be run in line below: Coastal,Grassland, Saltmarsh, Arable, Other
    # Fit habitat model for each habitat to each individual #HH NB this has to be manually changed for each time period AND each habitat per time period
    rsffits <- rsfdat  %>% nest(data=-"id") %>%   mutate(mod = map(data, function(x) glm(case_ ~ Coastal, data = x, weight=w,family = binomial)))
    
    
    # Check goodness of fit #HH NB this is an appendix table
    # Running issues originally due to na.action = argument not set. Ran later and daved manually (writeClipboard(as.character(rsf_gof$auc_test)))
    rsf_gof <- rsfdat  %>% nest(data=-"id") %>%   mutate(auc_test = map(data, function(x) pROC::auc(pROC::roc(x$case_~(predict(glm(case_ ~ Coastal, data = x, weight=w,family = binomial, na.action=na.exclude), type=c("response"))))))) #edit response var
    
    
    rsf_gof$auc_test # viewing this as a tab gets slower
    
    #copy the AUC for each bird to clipboard
    #writeClipboard(as.character(rsf_gof$auc_test))
    
    #Instead of copying the AUC to the clipboard save it as a dataframe. 
    auc.out <- data.frame(year= nyr, cohort = cohort, Period = TP, Habitat = "Coastal", TagID = rsf_gof$id, AUC = as.character(rsf_gof$auc_test))
    
    AUC_dat <- rbind(AUC_dat, auc.out)
    
    
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
    #writeClipboard(as.character(rsf_gof$auc_test))
    
    #Instead of copying the AUC to the clipboard save it as a dataframe. 
    auc.out <- data.frame(year= nyr, cohort = cohort, Period = TP, Habitat = "Grassland", TagID = rsf_gof$id, AUC = as.character(rsf_gof$auc_test))
    
    AUC_dat <- rbind(AUC_dat, auc.out)
    
    
    
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
    #writeClipboard(as.character(rsf_gof$auc_test))
    
    #Instead of copying the AUC to the clipboard save it as a dataframe. 
    auc.out <- data.frame(year= nyr, cohort = cohort, Period = TP, Habitat = "Saltmarsh", TagID = rsf_gof$id, AUC = as.character(rsf_gof$auc_test))
    
    AUC_dat <- rbind(AUC_dat, auc.out)
    
    
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
    #writeClipboard(as.character(rsf_gof$auc_test))
    
    #Instead of copying the AUC to the clipboard save it as a dataframe. 
    auc.out <- data.frame(year= nyr, cohort = cohort, Period = TP, Habitat = "Arable", TagID = rsf_gof$id, AUC = as.character(rsf_gof$auc_test))
    
    AUC_dat <- rbind(AUC_dat, auc.out)
    
    
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
    #writeClipboard(as.character(rsf_gof$auc_test))
    
    #Instead of copying the AUC to the clipboard save it as a dataframe. 
    auc.out <- data.frame(year= nyr, cohort = cohort, Period = TP, Habitat = "Other", TagID = rsf_gof$id, AUC = as.character(rsf_gof$auc_test))
    
    AUC_dat <- rbind(AUC_dat, auc.out)
    
    
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
    
    
    
    # Save data 
    setwd("C:/Users/hannah.hereward/Documents/Projects/2024_curlewheadstarting/curlew_headstarting/data/2025 analysis/")
    save(rsf_coefs_hab, file=paste0("NE103_",nyr," report_RSF_models_",cohort,"_",filelab,".RData"))
      
    
    ## Stat testing RSF - two-stage modeling ####
    
    # Set data
    x<-rsf_coefs_hab
    
    # Set factor
    x$term<-as.factor(as.character(x$term))
    
    # Filter intercept   #Use the 'estimate' per bird and habitat in x in the appendix table for the beta column
    x<- x %>% filter(term!="(Intercept)")
    
    #read out as csv to make it easier to put into appendix table
    x_out <- data.frame(x)
    x_out$year <- nyr
    x_out$Period <- TP
    x_out$cohort <- cohort
    x_out$n <- as.character(x_out$n) #have to turn "n" a character as it is currently a list and it won't read out as a csv if it is still a list
    colnames(x_out)[1] <- "TagID"
    colnames(x_out)[2] <- "Habitat"
    colnames(x_out)[3] <- "Beta"
    
    
    #rbind the beta data to the beta_dat data frame
    beta_dat <- rbind(beta_dat, x_out)
    
    
    # Run linear model
    m1<-lm(x$estimate~x$term)
    summary(m1)
    
    #added in these two lines to extract out the overall F value, the two DFs and the associated P value - these all are used in text in the report
    fstat <- summary(m1)$fstatistic
    fp <- pf(summary(m1)$fstatistic[[1]], summary(m1)$fstatistic[[2]], summary(m1)$fstatistic[[3]], lower.tail = F)
    
    
    # Check contrasts
    mytest <- emmeans(m1, ~ term)
    mytest.contrast <- contrast(regrid(mytest))
    print(mytest.contrast)
     
    
    #turn it into a dataframe
    mytest.contrast <- data.frame(mytest.contrast)
    
    #add some extra columns
    mytest.contrast$year <- nyr
    mytest.contrast$Period <- TP
    mytest.contrast$cohort <- cohort
    mytest.contrast$fstat <- fstat[1]
    mytest.contrast$fstat_df1 <- fstat[2]
    mytest.contrast$fstat_df2 <- fstat[3]
    mytest.contrast$fstat_p <- fp
    
    #re order the contrast rows:
    mytest.contrast <- mytest.contrast[c(1,2,3,5,4),]
    
     
   
    #rbind mytest.constrast to mytest.contrast_dat 
    mytest.contrast_dat <- rbind(mytest.contrast_dat, mytest.contrast)
    
    
    ## Gary's Note - following advice from Fieberg RSF/amt course (2019) but needs considering how to better
    # carry out testing when many model fits are poor (and coefficients then include outliers)
    
    ## RSS PLOTS #####
    ## Set data - need: rsf_coefs_hab
    #read data back in
    
    # Save data 
    #setwd("C:/Users/hannah.hereward/Documents/Projects/2024_curlewheadstarting/curlew_headstarting/data/2025 analysis/")
    #load(file=paste0("NE103_",nyr," report_RSF_models_",cohort,"_",filelab,".RData"))
    
    
    
    ##NOTE from 2023 analysis - need to manually check for outliers and do a scale restriction if there are so there are two graphs - one with the outliers and one without.
    
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
   plot <- rsf_coefs_hab %>% filter(term!="(Intercept)") %>%	ggplot(., ) +
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
      #coord_cartesian(ylim=c(-10,30)) +                                         #keep this line in ONLY when need to constrain the y axis
      theme(legend.position="none") +
      theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14),
            strip.text.x = element_text(size = 12, face="bold")) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))+
      ggtitle(paste0("",plotlab," (all birds)"))   ## UPDATE MANUALLY #HH NB if cropping to ignore outliers add in (cropped - one outlier outside view)
    
    
    # Save plot
    setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/output/Figures 2025/RSS/") #HH NB laptop
    ggsave(plot, file=paste0("NE103_",nyr,"_Headtsart CURLE_RSS plot_",filelab,".png"), width=15, height=15, units="cm", dpi=300)  ## UPDATE FILENAME
    
    print(plot)
   
   
    options(scipen=3000000)
    # Plot (habitat factor listed under term from model outputs)
    plot2 <- rsf_coefs_hab %>% filter(term!="(Intercept)") %>%	ggplot(., ) +
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
      ggtitle(paste0("",plotlab," (cropped)"))   ## UPDATE MANUALLY #HH NB if cropping to ignore outliers add in (cropped - one outlier outside view)
    
    print(plot2)
    
    # Save plot
    setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/output/Figures 2025/RSS/") #HH NB laptop
    ggsave(plot2, file=paste0("NE103_",nyr,"_Headtsart CURLE_RSS plot_",filelab,"_CROPPED.png"), width=15, height=15, units="cm", dpi=300)  ## UPDATE FILENAME
    
    
    
    }
    
    
    }





# save the tables out
#AUC_dat
#beta_dat
#mytest.contrast_dat 


write.csv(AUC_dat, here("output/Tables 2025/AUC_outputs.csv"), row.names=F) # this allows you to read out the output data as a csv for easiest copying to the report
write.csv(beta_dat, here("output/Tables 2025/beta_outputs.csv"), row.names=F)  # this allows you to read out the output data as a csv for easiest copying to the report
write.csv(mytest.contrast_dat, here("output/Tables 2025/mytests_outputs.csv"), row.names=F) # this allows you to read out the output data as a csv for easiest copying to the report



#need to reshape the AUC and beta tables for easiesr translation into the report tables
#library(reshape2)

RSF_coef_dat_out <- full_join(beta_dat[c(1:3,5:7)],AUC_dat, by=c("year", "cohort", "Period", "Habitat", "TagID"))

RSF_coef_dat_out_wide <- RSF_coef_dat_out %>% pivot_wider(names_from = Habitat, values_from = c( "Beta" , "AUC"))

#resort it
colnames(RSF_coef_dat_out_wide)
RSF_coef_dat_out_wide <- data.frame(RSF_coef_dat_out_wide[c(1:4,5,10,7,12,6,11,8,13,9,14)])
colnames(RSF_coef_dat_out_wide)

#write_csv(RSF_coef_dat_out_wide, here("output/Tables 2025/Beta_AUC_estimates_2021_2024.csv")) # this allows you to read out the output data as a csv for easiest copying to the report



####.####





####. ####. #### . ####  

# PLOTTING Utilisation Distribution TIA plots - THE UK and SE for 2021 & 2022 cohorts ####
#This is to assess if the older cohort birds are using other parts of the UK aside from the Wash which are accounted for in the avalible/used panel but not in the TIA
#2024 - includes various of the past cohort time periods. Loops now in the main loop to extract a UK wide and a south east zoom for each time period


####.####

# NOTE ADDITIONAL CODE NOT COPIED OVER FROM 2023 headings below ####

#### Misc plotting ####

#### Colour ring plots (saved from interactive plot)


#### UNUSED/TEST CODE ####




