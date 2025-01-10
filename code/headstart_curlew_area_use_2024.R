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




##Add in here three final columns for: cohorts and number of days post release and deaddate ####

###cohort - work out how many cohorts there were and which ones need combining together ####
    #2021 = 
    #2022 = 
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


#one week in my mind means 7 full days after release  e.g.release date 20/07 : 27/07 at 23:59:59
dt_meta_gsp_TagID$Date_1w <- dt_meta_gsp_TagID$release_date_time + ((86400*7)+86399)

#two weeks in my mind means 14 full days after release 
dt_meta_gsp_TagID$Date_2w <- dt_meta_gsp_TagID$release_date_time + ((86400*14)+86399)

#six weeks in my mind means 42 full days after release 
dt_meta_gsp_TagID$Date_6w <- dt_meta_gsp_TagID$release_date_time + ((86400*42)+86399)



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
#write.csv(dt_meta_gsp_TagID, here("data/metadata_TagID_deaddates_notransmision_2021_2024.csv"), row.names = F)




## Finally do a left_join on the dataset using the TagID as the join_by so that the meta data is populated for each GPS fix ####

#read data back in
data <- readRDS(here("data/data_with_tide_2021_2024.rds"))


#left join
data <- data %>% left_join(dt_meta_gsp_TagID, by=join_by(TagID))

summary(data)


# Tidy surplus columns from move:: direct loading
drop_cols<-c("event.id", "visible", "individual.id", "deployment.id", "tag.id", "study.id", "sensor.type.id", "tag.local.identifier", "individual.taxon.canonical.name", "acceleration.raw.x", "acceleration.raw.y", "acceleration.raw.z", "barometric.height", "battery.charge.percent", "battery.charging.current", "gps.hdop", "gps.time.to.fix", "heading", "import.marked.outlier", "light.level", "magnetic.field.raw.x", "magnetic.field.raw.y", "magnetic.field.raw.z", "ornitela.transmission.protocol", "study.name", "sensor.type")
data<- data %>% select(-!!drop_cols)

summary(data)

##save data out ####
#saveRDS(data, here("data/data_withcohorts_release_sites_2021_2024.rds"))



####.####


#which birds should stay in which category? ####
#Before we can use the data for analysis it would be useful to add in information to the whole dataset working out which categories the birds best fit into
  #this is because some of the birds died within year of release and subsequent years of release


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
currcohorts$max_date_time_transmiss[currcohorts$flag_id=="JJ"] <- dt_meta_gsp_TagID$dead_no_t_date_time[dt_meta_gsp_TagID$flag_id=="JJ"]+86399
currcohorts$max_date_time_transmiss[currcohorts$flag_id=="NH"] <- dt_meta_gsp_TagID$dead_no_t_date_time[dt_meta_gsp_TagID$flag_id=="NH"]+86399
currcohorts$max_date_time_transmiss[currcohorts$flag_id=="NT"] <- dt_meta_gsp_TagID$dead_no_t_date_time[dt_meta_gsp_TagID$flag_id=="NT"]+86399
currcohorts$max_date_time_transmiss[currcohorts$flag_id=="PU"] <- dt_meta_gsp_TagID$dead_no_t_date_time[dt_meta_gsp_TagID$flag_id=="PU"]+86399



#Save it 
#write.csv(currcohorts, here("data/current_cohort_maxdatetime_2021_2024.csv"), row.names = F)

#full join the currcohorts to dt_meta_gsp_TagID
dt_meta_gsp_TagID_update <- dt_meta_gsp_TagID %>% full_join(currcohorts, by=join_by(year, flag_id, sex, release_date, cohort_analysis, daysalive))


#find the last category the bird has data in ####

#read in csv which summaries all the categories for past cohort behaviours and max dates
past_cohort_behavs <- read.csv(here("data/pastcohort_behaviours_maxdates.csv"), header=T)

#add in a unique label ID
past_cohort_behavs$label_year <- paste(past_cohort_behavs$pastcohort_behaviours, past_cohort_behavs$year)

#make the date time column a posi
past_cohort_behavs$maxdate_pastcohort_behav <- as.POSIXct(past_cohort_behavs$maxdate_pastcohort_behav, format = "%d/%m/%Y %H:%M", tz="UTC")


#add a blank column into this dataset for category name and associated date so that the results can be read in to it:
dt_meta_gsp_TagID_update$max_category <- NA
dt_meta_gsp_TagID_update$max_category_date <- NA

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
  
  
  #a set of ifelse's to run through the dates based on the 1day, 1week, 2weeks, 6weeks and end of December post release
  dt_meta_gsp_TagID_update$max_category[dt_meta_gsp_TagID_update$flag_id==id] <-  ifelse(dat.in$max_date_time_transmiss <= dat.in$release_date, "Pre Release", 
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

  
 
  
}


#NB - for birds that died the day of release we include them for the 1 day post release 

#save the meta data out:
#write.csv(dt_meta_gsp_TagID_update, here("data/metadata_TagID_deaddates_notransmision_behaviourdates_2021_2024.csv"), row.names = F)


#create a sub-table to check we're happy with the final categories:
#colnames(dt_meta_gsp_TagID_update)
#checktable <- dt_meta_gsp_TagID_update[,c(2,3,5,7,12,13,15,16,17,21,22,23,36,37,38)]
#write.csv(checktable, here("data/tabletocheck.csv"), row.names=F)



# full join to the data_tt file
colna <- colnames(data[c(17:46)])
data_update <- data %>% full_join(dt_meta_gsp_TagID_update, by=join_by(  "urn"  ,  "year", "flag_id", "ring", "sex" , "name" ,              
                                                                         "cohort_num" , "tag_gps_radio_none" , "tag_serial", "radio_tag_freq" ,"tagged_date" ,"release_location"   ,
                                                                         "release_date" , "migration_date", "Fate" , "state" ,  "breeding_status" , "comments" ,          
                                                                         "release_site", "release_site_final" , "cohort_analysis" ,"release_date_time"  , "Date_1d" , "Date_1w"    ,        
                                                                         "Date_2w" , "Date_6w" , "dead_date" , "last_transmiss", "dead_no_t_date_time", "daysalive" ))
colnames(data_update)

tail(data_update)

#tidy up some columns so that we don't have replicates
data_final <- data_update[,c(1:46,49:54)]
colnames(data_final)
tail(data_final)
summary(data_final)

#save this out as the final dataset! 
#saveRDS(data_final, (here("data/data_withcohorts_release_sites_tagduration_2021_2024.rds")))


####.####


# LOAD PACKAGES ####
load_pkg <- rlang::quos(tidyverse,BTOTrackingTools, here, sp, leaflet, terra)  # quos() function to be lazy on "" around each package
lapply(lapply(load_pkg, rlang::quo_name), library, character.only = TRUE)

#START HERE - Once all data is correct and cleaned and combined above you can start from here ####

#read back in the data ####
data_final <- readRDS(here("data/data_withcohorts_release_sites_tagduration_2021_2024.rds"))

summary(data_final)


# Convert to BTOTT Track object ####
data_tt<-Track(data_final) 
data_tt<-clean_GPS(data_tt, drop_sats = 3, Thres = 30, GAP = 28800)
#HH NB: new warning messages about "flt_switch" for each bird - an error from the Track(data) function with is a BTOTT function
#See messages from Chris T about this but the summary is it is a flag option for clean_GPS - when importing data into movebank you can add a 
#column to tell you if it is dodgy data or not and then add a second column to clean it/remove it... as the dataset I am working with doesn't have this column
# I can disregard this warning


# Set ID factor
data_tt$TagID<-as.factor(as.character(data_tt$TagID)) 

#try plotting all the data
plot(data_tt$longitude, data_tt$latitude)



#NEXT create separate files per year - for cohort released and remaining from previous cohorts, all the split by stage ####

nyears <- c("2021", "2022", "2023", "2024")
cohort_periods <- c("1 One Day"  ,  "2 One Week"  , "3 Two Weeks" ,  "4 Six Weeks"   , "5 End of December" )

summary_dat <- read.csv(here("data/summary_curlewcount_per_timeperiod.csv"), header=T) 

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
  summary(as.factor(dat.in_cohort$max_category))
  
  #checks the total number of data rows per max_category
  table(dat.in_cohort$max_category)
  
  
  
  #then create an individual dataset filtered for each of the time periods: 
  
  #one day - Filters to keep it just to one day for all data
  data_1d <-  dat.in_cohort %>% 
    filter(dat.in_cohort$DateTime >= dat.in_cohort$release_date & dat.in_cohort$DateTime <= dat.in_cohort$Date_1d) %>% 
    droplevels() 
  
    summary(as.factor(data_1d$max_category))
    summary(data_1d$DateTime)
   #check max number of hours is less than 48hrs
    max(data_1d$DateTime - data_1d$release_date_posi)
    
    #This is a VERY key line to help Track2TrackMultiStack stack the tags in the correct stack! 
    data_1d$period <- "1 One Day"
    
    #add the number of birds into the summary table
    summary_dat$number_birds[summary_dat$year == nyr & summary_dat$pastcohort_behaviours == "1 One Day"] <- length(levels(data_1d$TagID))
    
    
  #one week - filters data to just one week after release but removing the birds that were only in one day category
  data_1w <- dat.in_cohort %>%
    filter(dat.in_cohort$DateTime >= dat.in_cohort$release_date & dat.in_cohort$DateTime <= dat.in_cohort$Date_1w) %>%
    filter(max_category !=  "1 One Day") %>% 
    droplevels()  
 
   summary(as.factor(data_1w$max_category))
  summary(data_1w$DateTime)
  #check max number of hours is less than 192hrs (1 week + 24hrs)
  max(data_1w$DateTime - data_1w$release_date_posi)
  
  #This is a VERY key line to help Track2TrackMultiStack stack the tags in the correct stack! 
  data_1w$period <- "2 One Week"
  
  
  #add the number of birds into the summary table
  summary_dat$number_birds[summary_dat$year == nyr & summary_dat$pastcohort_behaviours == "2 One Week"] <- length(levels(data_1w$TagID))
  
  
  #two weeks - filters data to just two weeks after release but removes the birds that were only in one day and one week categories
  data_2 <- dat.in_cohort %>% 
    filter(DateTime >= release_date & DateTime <= Date_2w)  %>% 
    filter(max_category !=  "1 One Day") %>%
    filter(max_category !=  "2 One Week")%>% 
    droplevels()
  
  summary(data_2$DateTime)
  summary(as.factor(data_2$max_category))
  #check max number of hours is less than 390hrs
  max(data_2$DateTime - data_2$release_date_posi)
  
  
  #This is a VERY key line to help Track2TrackMultiStack stack the tags in the correct stack! 
  data_2$period <- "3 Two Weeks"
  
  
  #add the number of birds into the summary table
  summary_dat$number_birds[summary_dat$year == nyr & summary_dat$pastcohort_behaviours == "3 Two Weeks"] <- length(levels(data_2$TagID))
  
  
  
  #six weeks - filters data to six weeks post release, but removes the birds that were only in one day and one week and two week categories
  data_6 <- dat.in_cohort %>% 
    filter(DateTime >= release_date & DateTime <= Date_6w)  %>% 
    filter(max_category !=  "1 One Day") %>%
    filter(max_category !=  "2 One Week") %>%
    filter(max_category !=  "3 Two Weeks")%>% 
    droplevels()
    
  summary(data_6$DateTime)
  summary(as.factor(data_6$max_category))
  #check max number of hours is less than 1032hrs
  max(data_6$DateTime - data_6$release_date_posi)
  
  
  #This is a VERY key line to help Track2TrackMultiStack stack the tags in the correct stack! 
  data_6$period <- "4 Six Weeks"
  

  #add the number of birds into the summary table
  summary_dat$number_birds[summary_dat$year == nyr & summary_dat$pastcohort_behaviours == "4 Six Weeks"] <- length(levels(data_6$TagID))
  
  
  #This combines all data up until the end of December- HH to check this is correct - data regardless of when the data stopped??
  data_all <- dat.in_cohort %>%
    filter(max_category !=  "1 One Day") %>%
    filter(max_category !=  "2 One Week") %>%
    filter(max_category !=  "3 Two Weeks") %>%
    filter(max_category !=  "4 Six Weeks") %>% 
    droplevels()
    
  data_all$period <- "5 End of December"
  
  summary(data_all$DateTime)
  summary(as.factor(data_all$max_category))

  data_all <- rbind(data_all, data_1d[data_1d$max_category=="1 One Day",], data_1w[data_1w$max_category=="2 One Week",], data_2[data_2$max_category=="3 Two Weeks",], data_6[data_6$max_category=="4 Six Weeks",] )
 
  summary(data_all$DateTime)
  summary(as.factor(data_all$max_category))
  
  
  #This is a VERY key line to help Track2TrackMultiStack stack the tags in the correct stack! 
  data_all$period <- "5 End of December"
  

  #add the number of birds into the summary table
  summary_dat$number_birds[summary_dat$year == nyr & summary_dat$pastcohort_behaviours == "5 End of December"] <- length(levels(data_all$TagID))
  
  
  
  
  # Final merge for the 2023 cohort #####
  data_cohort<-Track2TrackMultiStack(rbind(data_1d, data_1w, data_2, data_6, data_all), by=c("TagID", "period"))
  data_cohort
  
  
  # Save
  setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data/2025 analysis") #HH laptop
  save(data_cohort, paste0(file="NE103_2024 report_clean tracking data for ",nyr," cohort.RData"))
  
  
  
  
  ####then create a sub dataset for past cohorts still recording in nyr
  #Need to do something a bit different for the past cohorts
  
  
  
  dat.in_pastcohort <-  dat.in_c %>% 
    filter(year != nyr) %>% 
    droplevels()
  
  summary(dat.in_pastcohort$year)
  summary(dat.in_pastcohort$DateTime)
  summary(dat.in_pastcohort$TagID)
  
  #if else loop for male or female
  if(dat.in_pastcohort$sex == "F" | dat.in_pastcohort$sex == "U"){
    
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
      summary_dat$number_birds[summary_dat$sex == "F" & summary_dat$label_year == periods_F] <- length(levels(Data_W_PreB_F$TagID))
      
      
      
      
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
      summary_dat$number_birds[summary_dat$sex == "F" & summary_dat$label_year == periods_F] <- length(levels(Data_SF_F$TagID))
      
      
      
      
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
      summary_dat$number_birds[summary_dat$sex == "F" & summary_dat$label_year == periods_F] <- length(levels(Data_Breed_F$TagID))
      
      
      
      
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
      summary_dat$number_birds[summary_dat$sex == "F" & summary_dat$label_year == periods_F] <- length(levels(Data_AF_F$TagID))
      
      
      
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
      summary_dat$number_birds[summary_dat$sex == "F" & summary_dat$label_year == periods_F] <- length(levels(Data_W_AfterB_F$TagID))
      
      
      
      
      
      # Final merge for the cohort #####
      data_past_F <-Track2TrackMultiStack(rbind(Data_W_PreB_F, Data_SF_F, Data_Breed_F, Data_AF_F, Data_W_AfterB_F), by=c("TagID", "period"))
      data_past_F
      
      
      # Save
      setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data/2025 analysis") #HH laptop
      save(data_cohort, file=paste0("NE103_2024 report_clean tracking data for ",nyr," past cohorts_Female.RData"))
      
      
      
      
      
      
    }else{
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
      summary_dat$number_birds[summary_dat$sex == "M" & summary_dat$label_year == periods_M] <- length(levels(Data_W_PreB_M$TagID))
      
      
      
      
    
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
      summary_dat$number_birds[summary_dat$sex == "M" & summary_dat$label_year == periods_M] <- length(levels(Data_SF_M$TagID))
      
      
      
      
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
      summary_dat$number_birds[summary_dat$sex == "M" & summary_dat$label_year == periods_M] <- length(levels(Data_Breed_M$TagID))
      
      
      
      
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
      summary_dat$number_birds[summary_dat$sex == "M" & summary_dat$label_year == periods_M] <- length(levels(Data_AF_M$TagID))
      
      
      
      
      
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
      summary_dat$number_birds[summary_dat$sex == "M" & summary_dat$label_year == periods_M] <- length(levels(Data_W_AfterB_M$TagID))
      
  
      
  
    
      
      # Final merge for the cohort #####
      data_past_M <-Track2TrackMultiStack(rbind(Data_W_PreB_M, Data_SF_M, Data_Breed_M, Data_AF_M, Data_W_AfterB_M), by=c("TagID", "period"))
      data_past_M
      
      
      # Save
      setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data/2025 analysis") #HH laptop
      save(data_cohort, file=paste0("NE103_2024 report_clean tracking data for ",nyr," past cohorts_Male.RData"))
      
      
      
    }
    
   
  
  
  #Final merge all together:
  
  # Final merge for the whole of the year #####
  data_year<-Track2TrackMultiStack(rbind(data_1d, data_1w, data_2, data_6, data_all, Data_W_PreB_F, Data_SF_F, Data_Breed_F, Data_AF_F, Data_W_AfterB_F, Data_W_PreB_M, Data_SF_M, Data_Breed_M, Data_AF_M, Data_W_AfterB_M ), by=c("TagID", "period"))
  data_year
  
  
  
  
  
  # Save
  setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data/2025 analysis") #HH laptop
  save(data_year, file=paste0("NE103_2024 report_clean tracking data for all ",nyr," data.RData"))
  
    
  }

  
  
  
  
  
  
  
  
  }
  
  
  
}


data_2023cohort




# Save
setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data") #HH laptop
save(data_2023cohort, file="NE103_2023 report_clean tracking data for 2023 cohort.RData")









# extract data for 2023 only ####
#HH NB --- HH alternative code to filter the data for ANY GPS data for the WHOLE of 2023: splitting it by the cohorts released in 2023 and previous cohorts still recording in 2023
data_tt_all23 <-data_tt %>% filter(DateTime >= "2023-01-01") %>% droplevels()

#this checks that there are still the different year cohorts
summary(data_tt_all23$year)

#this checks the min and max datetime
summary(data_tt_all23$DateTime)

#check the plot
plot(data_tt_all23$longitude, data_tt_all23$latitude)


## cohort 2023 release - filter the data for each ID based on the staggered deployments for cohort released in 2023 ####
data_tt_2023<-data_tt_all23 %>% filter(year == "2023") %>% droplevels()
summary(data_tt_2023$year)
summary(data_tt_2023$DateTime) #note this still includes 2024 data but for the 2023 released cohort

