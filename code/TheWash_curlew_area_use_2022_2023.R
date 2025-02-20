#### NE103 -- Curlew headstarting project -- using The Wash 2022 and 2023 July-December dataset for wild curlew comparision

## Original code (2021 and 2022) by Gary Clewley
## Updated for 2023 analysis by Hannah Hereward, Katharine Bowgen, Sam Franks (2023-)
## NEW SCRIPT copying relevant code from the 2023 analysis - Hannah Hereward, Katharine Bowgen, Sam Franks (2023-)


# NOTES ####
# Analysis of 2022 and 2023 The Wash dataset from 2022 and 2023 July-December using same time periods as the headstarting analysis 

#NOTE Sam used the following code to do an initial clean of the dataset:

#all_tags_filtered <- all_tags %>% 
#  filter(gps_satellite_count >= 4) %>% 
#  filter(event_id != 23271192487)

# merge metadata with tag data
#all_tags_filtered <- all_tags_filtered %>% 
#  mutate(new_datetime = as.POSIXct(strptime(timestamp, format = "%Y-%m-%d %H:%M:%S", tz="UTC"))) %>% 
#  mutate(new_datetime_min = format(new_datetime,format='%Y-%m-%d %H:%M'))

# filter tag dates for headstart analysis comparison
#all_tags_jul_dec <- all_tags_filtered %>% 
#  mutate(date_month = lubridate::month(new_datetime,label = TRUE, abbr = TRUE), date_year = lubridate::year(new_datetime)) %>% 
#  filter(date_month %in% c("Jul","Aug","Sep","Oct","Nov","Dec"))




# LOAD PACKAGES ####
load_pkg <- rlang::quos(tidyverse,BTOTrackingTools, here, sp, leaflet, terra)  # quos() function to be lazy on "" around each package
lapply(lapply(load_pkg, rlang::quo_name), library, character.only = TRUE)



# SETUP AND CLEAN DATA ####

#read in the wash data here
washdat <- readRDS(here("data/2025 analysis - The Wash/wwrg_curlew_jul_dec_all_years.rds"))
head(washdat)
colnames(washdat)


## Make parsable as a Track object by BTOTT package
names(washdat)[names(washdat)=="individual_local_identifier"]<-"TagID"   # Some tags redeployed multiple times
names(washdat)[names(washdat)=="timestamp"]<-"DateTime"
names(washdat)[names(washdat)=="location_long"]<-"longitude"
names(washdat)[names(washdat)=="location_lat"]<-"latitude"
names(washdat)[names(washdat)=="gps_satellite_count"]<-"satellites_used"


summary(washdat)
head(washdat)


### Coerce required trip and gap columns for later functions (not running trip definition for this project as not central place) ####
washdat$tripNo<-1; washdat$gap<-0; washdat$gapsec<-1


## Check summary of data ####
summary(washdat)


##save data out so I don't keep having to read it all back in and do the tide stuff####
#saveRDS(washdat, here("data/2025 analysis - The Wash/wwrg_curlew_jul_dec_all_years_cleaned.rds"))



# Tidy surplus columns from move:: direct loading #####
colnames(washdat)

drop_cols<-c("event_id", "visible", "individual_id", "deployment_id", "tag_id", "study_id", "sensor_type_id", "tag_local_identifier", "individual_taxon_canonical_name", "acceleration_raw_x", "acceleration_raw_y", "acceleration_raw_z", "barometric_height", "battery_charge_percent", "battery_charging_current", "gps_hdop", "gps_time_to_fix", "heading", "import_marked_outlier", "light_level", "magnetic_field_raw_x", "magnetic_field_raw_y", "magnetic_field_raw_z", "ornitela_transmission_protocol", "study_name", "sensor_type")
washdat<- washdat %>% select(-!!drop_cols)

summary(washdat)

##save data out ####
#saveRDS(washdat, here("data/2025 analysis - The Wash/wwrg_curlew_jul_dec_all_years_cleaned_colsdropped.rds"))

#read back in
#washdat <- readRDS(here("data/2025 analysis - The Wash/wwrg_curlew_jul_dec_all_years_cleaned_colsdropped.rds"))


####.####

#Extract meta data to check which birds were in which season ####
washdat_meta <- washdat %>% group_by(TagID) %>%
  summarize(start_date = min(DateTime), end_date = max(DateTime))


#which birds should stay in which category? ####
#Before we can use the data for analysis it would be useful to add in information to the whole dataset working out which categories the birds best fit into
#this is because some of the birds died or may not have transmitted across the whole time period

#NOTE HH additions (14/01/2025) - after discussions with Katharine for year of release birds: For all data from release to end of Dec we included all data if fixes were beyond 31st October
#this decisions comes to balance wanting to retain as much data as possible but also the coefficent analysis later on comparing individual birds per time period

#use the specific past cohort data frame for The Wash 
#read in csv which summaries all the categories for past cohort behaviours and max dates
past_cohort_behavs_thewash <- read.csv(here("data/pastcohort_behaviours_maxdates_TheWash.csv"), header=T)

#add in a unique label ID
past_cohort_behavs_thewash$label_year <- paste(past_cohort_behavs_thewash$pastcohort_behaviours, past_cohort_behavs_thewash$year)

#make the date time column a posi
past_cohort_behavs_thewash$maxdate_pastcohort_behav <- as.POSIXct(past_cohort_behavs_thewash$maxdate_pastcohort_behav, format = "%d/%m/%Y %H:%M:%S", tz="UTC")


#add a blank column into the  dataset for category name and associated date so that the results can be read in to it for both max category and min category:
washdat_meta$max_category <- NA
washdat_meta$max_category_date <- NA



for(i in 1:nrow(washdat_meta)){ 
  
  dat.in <- washdat_meta[i,]
  
  
  #run a which query to find the row that is the last row that include the date
  washdat_meta$max_category[i] <- past_cohort_behavs_thewash$label_year[min(which(past_cohort_behavs_thewash$maxdate_pastcohort_behav > dat.in$end_date))]
  
  
  lab <- washdat_meta$max_category[i]
  
  
  #extracting the date for the respective label
  washdat_meta$max_category_date[i] <- paste(as.POSIXct(past_cohort_behavs_thewash$maxdate_pastcohort_behav[past_cohort_behavs_thewash$label_year == lab], tz="UTC"))
  
  
  
 
}


washdat_meta


#full join the main dataset and the start and end dates ####
washdat <- full_join(washdat, washdat_meta)

summary(washdat)




###Once all the tweaking is done - save this out as the final dataset!  ####
#saveRDS(washdat, (here("data/2025 analysis - The Wash/wwrg_curlew_jul_dec_all_years_cleaned_colsdropped_timeperiod.rds")))


#read back in
#washdat <- readRDS(here("data/2025 analysis - The Wash/wwrg_curlew_jul_dec_all_years_cleaned_colsdropped_timeperiod.rds"))





####.####


# LOAD PACKAGES ####
load_pkg <- rlang::quos(tidyverse,BTOTrackingTools, here, sp, leaflet, terra)  # quos() function to be lazy on "" around each package
lapply(lapply(load_pkg, rlang::quo_name), library, character.only = TRUE)


#START HERE - TO CREATE THE BTO TRACKS - Once all data is correct and cleaned and combined above you can start from here ####
#read back in the data ####
#washdat <- readRDS(here("data/2025 analysis - The Wash/wwrg_curlew_jul_dec_all_years_cleaned_colsdropped_timeperiod.rds"))

summary(washdat)




#NEXT create separate files per year - for cohort released and remaining from previous cohorts, all the split by stage ####

#Using the same name as the code already set up:
data_tt_W <- washdat


nyears_wash <- c("2021", "2022", "2023", "2024")

#summary_dat_TheWash <- read.csv(here("data/summary_curlewcount_per_timeperiod_TheWash.csv"), header=T) 

#add in a unique label ID
summary_dat_TheWash$label_year <- paste(summary_dat_TheWash$pastcohort_behaviours, summary_dat_TheWash$year)

summary_dat_TheWash$number_birds <- NA




for(y in 1:length(nyears_wash)){
  
  #first pick out the selected year
  nyr <- nyears_wash[y]
  
  #pick out the correct december and january date for the year 
  decdate <-  as.POSIXct(ifelse(nyr == 2021, "2021-12-31 23:59:59",
                                ifelse(nyr == 2022, "2022-12-31 23:59:59", 
                                       ifelse(nyr == 2023, "2023-12-31 23:59:59", "2024-12-31 23:59:59"))), tz="UTC")
  
  
  jandate <-  as.POSIXct(ifelse(nyr == 2021, "2021-01-01 00:00:00",
                                ifelse(nyr == 2022, "2022-01-01 00:00:00", 
                                       ifelse(nyr == 2023, "2023-01-01 00:00:00", "2024-01-01 00:00:00"))), tz="UTC")
  
  
  
  
  #create a subset of data based on the date and time - IE keeping the data recording dates to the year selected 
  dat.in_c <-data_tt_W %>% filter(DateTime >= jandate & DateTime <= decdate ) %>% droplevels()
  
  #this checks that there are still the different year cohorts
  summary(dat.in_c$date_year)
  
  #this checks the min and max datetime
  summary(dat.in_c$DateTime)
  
  #check the plot
  plot(dat.in_c$longitude, dat.in_c$latitude)
  
  #checks the total number of data rows per min_category
  table(dat.in_c$max_category)
  
  
  
  #then create an individual dataset filtered for each of the time periods: 
  
  #This combines all data up until the end of December- if there is data beyond the autumn fuzzy date
  data_all <- dat.in_c %>%
    droplevels()
  
  data_all$period <- paste0("July-December ",nyr,"")
  
  summary(data_all$DateTime)
  summary(as.factor(data_all$max_category))
  
  
  #add the number of birds into the summary table
  summary_dat_TheWash$number_birds[summary_dat_TheWash$year == nyr & summary_dat_TheWash$pastcohort_behaviours == "5 End of December"] <- length(unique(data_all$TagID))
  

  
  #turn it into a Track using BTOTT
  data_all_tt <-Track(data_all) 
  data_all_tt<-clean_GPS(data_all_tt, drop_sats = 3, Thres = 30, GAP = 28800) #HH NEW NOTE 2024 analysis: GAP here constrains the function to not interpolate time in between this gap of 28800 seconds = 8 hours (chat message from Chris T 17/01/2025)
  
  # Set ID factor
  data_all_tt$TagID<-as.factor(as.character(data_all_tt$TagID)) 
  
  
  
  
  #Female autumn fuzz
  past_cohort_behavs_thewash_filter <- past_cohort_behavs_thewash %>% filter(year == nyr)
  
  periods_F_before <- past_cohort_behavs_thewash_filter$label_year[1]
  periods_F <- past_cohort_behavs_thewash_filter$label_year[2]
  
  
  Data_AF_F <- dat.in_c %>% 
    filter(DateTime >=  past_cohort_behavs_thewash_filter$maxdate_pastcohort_behav[past_cohort_behavs_thewash_filter$label_year==periods_F_before]  & DateTime <= past_cohort_behavs_thewash_filter$maxdate_pastcohort_behav[ past_cohort_behavs_thewash_filter$label_year==periods_F]) %>%
    droplevels()
  
  summary(Data_AF_F$DateTime)
  summary(Data_AF_F)
  
  #This is a VERY key line to help Track2TrackMultiStack stack the tags in the correct stack! 
  Data_AF_F$period <- periods_F
  
   #add the number of birds into the summary table
    summary_dat_TheWash$number_birds[ summary_dat_TheWash$label_year == periods_F] <- length(unique(Data_AF_F$TagID))
  
  if(length(unique(Data_AF_F$TagID)) == 0) {
   
    
    #Female winter after breeding
    periods_F_before <- past_cohort_behavs_thewash_filter$label_year[2]
    periods_F <- past_cohort_behavs_thewash_filter$label_year[3]
    
    
    Data_W_AfterB_F <- dat.in_c %>% 
      filter(DateTime >=  past_cohort_behavs_thewash_filter$maxdate_pastcohort_behav[past_cohort_behavs_thewash_filter$label_year==periods_F_before]  & DateTime <= past_cohort_behavs_thewash_filter$maxdate_pastcohort_behav[past_cohort_behavs_thewash_filter$label_year==periods_F]) %>%
      droplevels()
    
    summary(Data_W_AfterB_F$DateTime)
    summary(Data_W_AfterB_F)
    
    #This is a VERY key line to help Track2TrackMultiStack stack the tags in the correct stack! 
    Data_W_AfterB_F$period <- periods_F
    
    
    #add the number of birds into the summary table
    summary_dat_TheWash$number_birds[summary_dat_TheWash$label_year == periods_F] <- length(unique(Data_W_AfterB_F$TagID))
    
    
    #turn it into a Track using BTOTT
    Data_W_AfterB_F_tt <-Track(Data_W_AfterB_F) 
    Data_W_AfterB_F_tt<-clean_GPS(Data_W_AfterB_F_tt, drop_sats = 3, Thres = 30, GAP = 28800) #HH NEW NOTE 2024 analysis: GAP here constrains the function to not interpolate time in between this gap of 28800 seconds = 8 hours (chat message from Chris T 17/01/2025)
    
    # Set ID factor
    Data_W_AfterB_F_tt$TagID<-as.factor(as.character(Data_W_AfterB_F_tt$TagID)) 
    
    
  
    
    # Final merge for the current year cohort #####
    data_year<-Track2TrackMultiStack(rbind(data_all_tt, Data_W_AfterB_F_tt), by=c("TagID", "period"))
    data_year
    
    
    
    # Save
    setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data/2025 analysis - The Wash") #HH laptop
    save(data_year, file=paste0("NE103_",nyr," report_clean tracking data for ",nyr,"_alldata_TheWash.RData"))
    
    
    
    
    
  }else{
  #turn it into a Track using BTOTT
  Data_AF_F_tt <-Track(Data_AF_F) 
  Data_AF_F_tt<-clean_GPS(Data_AF_F_tt, drop_sats = 3, Thres = 30, GAP = 28800) #HH NEW NOTE 2024 analysis: GAP here constrains the function to not interpolate time in between this gap of 28800 seconds = 8 hours (chat message from Chris T 17/01/2025)
  
  # Set ID factor
  Data_AF_F_tt$TagID<-as.factor(as.character(Data_AF_F_tt$TagID)) 
  
  
  
  
  #Female winter after breeding
  periods_F_before <- past_cohort_behavs_thewash_filter$label_year[2]
  periods_F <- past_cohort_behavs_thewash_filter$label_year[3]
  
  
  Data_W_AfterB_F <- dat.in_c %>% 
    filter(DateTime >=  past_cohort_behavs_thewash_filter$maxdate_pastcohort_behav[past_cohort_behavs_thewash_filter$label_year==periods_F_before]  & DateTime <= past_cohort_behavs_thewash_filter$maxdate_pastcohort_behav[past_cohort_behavs_thewash_filter$label_year==periods_F]) %>%
    droplevels()
  
  summary(Data_W_AfterB_F$DateTime)
  summary(Data_W_AfterB_F)
  
  #This is a VERY key line to help Track2TrackMultiStack stack the tags in the correct stack! 
  Data_W_AfterB_F$period <- periods_F
  
  
  #add the number of birds into the summary table
  summary_dat_TheWash$number_birds[summary_dat_TheWash$label_year == periods_F] <- length(unique(Data_W_AfterB_F$TagID))
  
  
  #turn it into a Track using BTOTT
  Data_W_AfterB_F_tt <-Track(Data_W_AfterB_F) 
  Data_W_AfterB_F_tt<-clean_GPS(Data_W_AfterB_F_tt, drop_sats = 3, Thres = 30, GAP = 28800) #HH NEW NOTE 2024 analysis: GAP here constrains the function to not interpolate time in between this gap of 28800 seconds = 8 hours (chat message from Chris T 17/01/2025)
  
  # Set ID factor
  Data_W_AfterB_F_tt$TagID<-as.factor(as.character(Data_W_AfterB_F_tt$TagID)) 
  
  
  
  
  
  # Final merge for the current year cohort #####
  data_year<-Track2TrackMultiStack(rbind(data_all_tt, Data_AF_F_tt , Data_W_AfterB_F_tt), by=c("TagID", "period"))
  data_year
  
  
  
  # Save
  setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data/2025 analysis - The Wash") #HH laptop
  save(data_year, file=paste0("NE103_",nyr," report_clean tracking data for ",nyr,"_alldata_TheWash.RData"))
  
  
    
  }
}


#save the summary_dat out
write.csv(summary_dat_TheWash , here("output/Tables 2025/The Wash/summarycountbirds_pertimeperiod_peryear_THEWASH_data_only.csv"), row.names = F)

  



#Analysis begins ####
# Set arbitrary 'Colony' location to facilitate later functions. Using central Snettisham location here (HH NB Wild Ken Hill release pen 1 site)
# but not used to define trips away from central place for Curlew
ColLon = 0.50
ColLat = 52.87

# Set projection with 00 on the "colony"
p4 <- sp::CRS(paste("+proj=laea +lon_0=", ColLon," +lat_0=", ColLat, " +units=m", sep=""))

# read in simple UK shapefile map from btotrackingtools and re-project it
ukmap <- BTOTrackingTools::ukmap
ukmap <- sf::st_transform(ukmap,p4) #HH NB: ukmap is an sf not an sp. So changed code here from sp::spTransform to sf::st_transform





#Habitat Selection ##
# Load amt:: package
library(amt) 


## Load Land Cover Map 2021 25m Raster
#NOTE THIS IS THE WASH ONLY!!!
landuse <- raster::raster(here("data","NE103_LCM2021","LCM.tif"))


nas_dat_wash <- data.frame()

nyears_wash <- c("2021", "2022", "2023", "2024")

#SET SEED - THEN run through the loop ######
set.seed(c(1,2,3,4))

for(y in 1:length(nyears_wash)){
  
  nyr <- nyears_wash[y]
  
  setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data/2025 analysis - The Wash") #HH laptop
  load(file=paste0("NE103_",nyr," report_clean tracking data for ",nyr,"_alldata_TheWash.RData"))
  
  
  #loaded as data_year. rename it:
  data <- data_year
  
  
  if(nyr == "2021"){
    
    datasplit <- c("July-December", "10 End of December - Winter" )
    
    plotlabels <- c("July-December" ,  "Winter - post-breeding")
    
    filelabels <- c("5_July_December",  "10_WinterPostBreed" )
    
    
    
  }else{
    
    
    datasplit <- c("July-December", "9a Female Autumn fuzzy", "10 End of December - Winter" )
    
    plotlabels <- c("July-December" , "Autumn transition", "Winter - post-breeding")
    
    filelabels <- c("5_July_December", "9a_Autumn_transition", "10_WinterPostBreed" )
    
  }
  
  for(p in 1:length(datasplit)){
    
    TP <- datasplit[p] 
    
    #add this if loop in for data split label to include the correct year
   
    TP <- paste0("",TP," ",nyr,"")
    
    
    #TIA analysis: select the specific list from the 'data' set
    #tia_dat<-data[[TP]]
    
    #extract out the file label
    filelab <- filelabels[p]
    
    #add in plot label
    plotlab <- plotlabels[p]

    #add the correct year
    plotlab <- paste0("",plotlab," ",nyr,"")
  
    
    ## HABITAT SELECTION ####
    #Habitat selection
    trk_dat<-TrackStack2Track(data[[TP]])
    
    #HH NB added in this code to 'droplevels' of the non-relevant TagID per time period
    trk_dat<- trk_dat %>% droplevels()
    
    
    # Convert to 'amt' track (using BTOTT headers) #HH NB - release col name updated - use release_site_final = combined Ken Hill, seperate Sandringham. Cohort col name updated - cohort_analysis - this uses the first cohorts = 1, and remaining cohorts = 2 
    trk <- make_track(trk_dat, .x = longitude, .y = latitude, .t = DateTime, id = TagID, speed=ground_speed, crs = "epsg:4326")
    
    
    # Transform to BNG
    trk <- transform_coords(trk, "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs")
    
    
    # Extract LCM variables
    trk <- trk %>% 
      extract_covariates(landuse)
    
    #HH NB ADDED IN A FILTER HERE to remove NAs (equates to marine and non-UK fixes) SO THAT THE RANDOM POINTS (further down) ARE ONLY GENERATED IN THE UK. Therefore, this code is filtering the LCM results in trk to remove all NAs.
    #NOTE this will remove some 'at sea' around the UK NAs as well as non-UK fixes 
    summary(trk)
    
    #add in to the nas_dat file the number of NAs
    nas_out <- data.frame(year = nyr, time_period = filelab, LCM_NAs = sum(is.na(trk$layer)) )
    nas_dat_wash <- rbind(nas_dat_wash, nas_out)
    
    #filter the NAs out
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
    
    
    # Create random points for each individual 
    
    #CODE UPDATE HERE HH 2025 - due to 'factor' being deprecated and so needing an alternative way of getting the correct number of random points 
    avail.pts <- trk %>%  tidyr::nest(data=-c("id"))  #-c is a minus because it is what you 'non-nested columns' which is what you are interested in but you don't want nested
    
    
    #new part in here because the original code used factor = 20... which is now deprecated so instead we need to use n = but this is literally the number of points added and so for us we want to use the number of GPS fixes per row * 20. 
    #so create new column to put in the number of rows of data per row
    avail.pts$data_nrow <- NA
    
    #loop this so that we get the individual count per row
    for(t in 1:length(avail.pts$id)){
      
      avail.pts$data_nrow[[t]] <- nrow(avail.pts$data[[t]])
      
    }
    
    
    
    #then carry on in tidyr to mutate etc... NOTE HH update 20/02/2025 random_points still not generating the correct number of points so have to use pmap instead! #  factor appears to have been deprecated in version 0.1.6 (unfortunately not noticed in 2023 analysis) so need to use n = .
    avail.pts <- avail.pts %>%
      mutate(rnd_pts = pmap(., function(id,data,data_nrow){
        random_points(data, n=data_nrow*20, type="random")
      })) %>%
      select(id,  rnd_pts) %>%  # you don't want to have the original point twice, hence drop data
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
    #jpeg(file=paste0("C:/Users/hannah.hereward/Documents/Projects/2024_curlewheadstarting/curlew_headstarting/output/Figures 2025/The Wash/NE103_Headstart CURLEW_",nyr,"_testplot.jpg"), width=15, height=15, units="cm", res=300)
    #run the plot
    #plot(avail.pts[avail.pts$id=="213816_G/WFN(N3)G",])
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
    setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data/2025 analysis - The Wash/") #HH laptop
    save(rsfdat, file=paste0("NE103_",nyr," report_RSF_data_cohort_",nyr,"_",filelab,"_TheWash.RData")) 
    
    
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
    setwd("C:/Users/hannah.hereward/Documents/Projects/2024_curlewheadstarting/curlew_headstarting/output/Figures 2025/The Wash/")
    ggsave(paste0("NE103_",nyr,"_Headstart CURLE_RSF plot_",filelab,"_TheWash.jpg"), width=15, height=15, units="cm", dpi=300)  ## UPDATE FILENAME
    
    
  }
  
}

write.csv(nas_dat, here("output/Tables 2025/The Wash/number_nas_per_year_timeperiod_TheWash.csv"), row.names=F) # this allows you to read out the output data as a csv for easiest copying to the report
