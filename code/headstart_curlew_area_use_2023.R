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

#HH Updated code to use an external csv to apply the cohort identifier and release site identifier (created in code: 'headstart_curlew_gps_movements.R' ~ line 76 to get the cohort identifier and site identifiers)
dt_meta <- readRDS("./data/dt_meta_metadata_allyears.rds")
head(dt_meta)

#filter for just GPS birds
dt_meta_gsp <- dt_meta %>% filter(dt_meta$tag_gps_radio_none =="gps")


#table for cohort
#cohort <- data.frame(FlagID = dt_meta_gsp$flag_id, cohort_no = dt_meta_gsp$cohort_num, release_location = dt_meta_gsp$release_location)

dt_meta_gsp$release_site <- c("Sandringham", "Sandringham", "Ken Hill", "Sandringham","Ken Hill", "Sandringham","Ken Hill", "Sandringham","Ken Hill", "Sandringham","Sandringham", "Sandringham", "Sandringham","Ken Hill","Ken Hill","Ken Hill","Sandringham","Ken Hill","Ken Hill","Sandringham","Ken Hill",
                         "Sandringham","Sandringham", "Sandringham", "Sandringham", "Sandringham", "Ken Hill","Ken Hill","Ken Hill",
                         "Ken Hill","Ken Hill","Ken Hill","Sandringham","Ken Hill","Ken Hill","Sandringham")

TagID <- data.frame(TagID = unique(data$TagID))

TagID$flag_id <- c("0E","3A", "0J" , "3K", "6X", "7E","7U" ,"6Y", "7K" , "7Y" ,"8E", "8K", "8L" ,"8X","9L" ,"9J" , "XJ", "XL","XH",
                "YK","XA", "XE","YJ","XP","XU","XT" ,"YH" ,"XX","LP" ,"LJ" ,"YX","LA","LV" ,"LU"  ,"YU", "YN"  )

#left_join the two metadata tables together
dt_meta_gsp_TagID <- dt_meta_gsp %>% left_join(TagID, by=join_by(flag_id))

#***** HH NB add in here two final columns 
#cohort = decide for 2023 how many cohorts we want - first cohort and the rest clumped together?
#release = double check with the release site - is it useful to keep the Sandringham release sites different?
#KMB says to merge the two 1,2 per site. single merged Sandringham and then merged Ken Hill is appropriate as the habitat management is similar at each site even if the first Sandringham site was slightly poorer. The Ken Hill ones are very close so they are definitely merged.


#Then do a left_join on the dataset using the TagID are the join_by so that the meta data is populated for each GPS fix
data <- data %>% left_join(dt_meta_gsp_TagID, by=join_by(TagID))


 
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
saveRDS(data, here("data/data_withcohorts_release_sites.rds"))

#read back in - can start from here now ####
data <- readRDS("data/data_withcohorts_release_sites.rds")




## Convert to BTOTT Track object
data_tt<-Track(data) 
data_tt<-clean_GPS(data_tt, drop_sats = 3, Thres = 30, GAP = 28800)

# Set ID factor
data_tt$TagID<-as.factor(as.character(data_tt$TagID)) 




# Remove 2021 deployments with no 2022 data #HH UPDATE THIS WITH NEW INFO FROM SEPERATE CSV
data_tt<-data_tt %>% filter(TagID!="Yf(0J)O/-:Y/m" & TagID!="Yf(3A)O/-:Y/m" & TagID!="Yf(3K)O/-:Y/m") %>% droplevels()


# Filter work around for each ID to select time period based on staggered deployments

# One week
dat1<- data_tt %>% filter(TagID=="Yf(6X)O/-:Y/m_KenHill"|TagID=="Yf(6Y)O/-:Y/m_Sandringham"|TagID=="Yf(7E)O/-:Y/m_KenHill"|TagID=="Yf(7K)O/-:Y/m_Sandringham"|TagID=="Yf(7U)O/-:Y/m_KenHill"|TagID=="Yf(7Y)O/-:Y/m_Sandringham")  %>%
  filter(DateTime<"2022-07-21 23:59:59")
dat2<- data_tt %>% filter(TagID=="Yf(8E)O/-:Y/m_Sandringham"|TagID=="Yf(8K)O/-:Y/m_Sandringham"|TagID=="Yf(8L)O/-:Y/m_Sandringham")  %>%
  filter(DateTime<"2022-08-10 23:59:59")
dat3<- data_tt %>% filter(TagID=="Yf(8X)O/-:Y/m_KenHill") %>% 
  filter(DateTime<"2022-08-16 23:59:59")
dat4<- data_tt %>% filter(TagID=="Yf(9J)O/-:Y/m_KenHill"|TagID=="Yf(9L)O/-:Y/m_KenHill")  %>% 
  filter(DateTime<"2022-08-23 23:59:59")

data_1<-rbind(dat1, dat2, dat3, dat4); data_1$Period<-"one week"


# Two weeks
dat1<- data_tt %>% filter(TagID=="Yf(6X)O/-:Y/m_KenHill"|TagID=="Yf(6Y)O/-:Y/m_Sandringham"|TagID=="Yf(7E)O/-:Y/m_KenHill"|TagID=="Yf(7K)O/-:Y/m_Sandringham"|TagID=="Yf(7U)O/-:Y/m_KenHill"|TagID=="Yf(7Y)O/-:Y/m_Sandringham")  %>%
  filter(DateTime<"2022-07-28 23:59:59")
dat2<- data_tt %>% filter(TagID=="Yf(8E)O/-:Y/m_Sandringham"|TagID=="Yf(8K)O/-:Y/m_Sandringham"|TagID=="Yf(8L)O/-:Y/m_Sandringham")  %>%
  filter(DateTime<"2022-08-17 23:59:59")
dat3<- data_tt %>% filter(TagID=="Yf(8X)O/-:Y/m_KenHill") %>% 
  filter(DateTime<"2022-08-23 23:59:59")
dat4<- data_tt %>% filter(TagID=="Yf(9J)O/-:Y/m_KenHill"|TagID=="Yf(9L)O/-:Y/m_KenHill")  %>% 
 filter(DateTime<"2022-08-30 23:59:59")

data_2<-rbind(dat1, dat2, dat3, dat4); data_2$Period<-"two weeks"


# Six weeks
dat1<- data_tt %>% filter(TagID=="Yf(6X)O/-:Y/m_KenHill"|TagID=="Yf(6Y)O/-:Y/m_Sandringham"|TagID=="Yf(7E)O/-:Y/m_KenHill"|TagID=="Yf(7K)O/-:Y/m_Sandringham"|TagID=="Yf(7U)O/-:Y/m_KenHill"|TagID=="Yf(7Y)O/-:Y/m_Sandringham")  %>%
  filter(DateTime<"2022-08-25 23:59:59")
dat2<- data_tt %>% filter(TagID=="Yf(8E)O/-:Y/m_Sandringham"|TagID=="Yf(8K)O/-:Y/m_Sandringham"|TagID=="Yf(8L)O/-:Y/m_Sandringham")  %>%
  filter(DateTime<"2022-09-14 23:59:59")
dat3<- data_tt %>% filter(TagID=="Yf(8X)O/-:Y/m_KenHill") %>% 
  filter(DateTime<"2022-09-20 23:59:59")
dat4<- data_tt %>% filter(TagID=="Yf(9J)O/-:Y/m_KenHill"|TagID=="Yf(9L)O/-:Y/m_KenHill")  %>% 
  filter(DateTime<"2022-09-27 23:59:59")

data_6<-rbind(dat1, dat2, dat3, dat4); data_6$Period<-"six weeks"


# End of calendar year (2022 deployments only)
data_all<- data_tt %>%  filter(TagID!="Yf(0E)O/-:Y/m" & DateTime<"2022-12-31 23:59:59")
data_all$Period<-"all"


# 2021 deployment for 2022 #HH UPDATE THIS FOR ALL PAST DEPLOYMENTS!!!**
data_21<- data_tt %>% filter(TagID=="Yf(0E)O/-:Y/m") %>% filter(DateTime>"2022-01-01 00:00:00")
data_21$Period<-"21_dep_all"



# Final merge
data<-Track2TrackMultiStack(rbind(data_1, data_2, data_6, data_all, data_21), by=c("TagID", "Period"))

trialrundata<-data

# Save
setwd("C:/Users/gary.clewley/Desktop/BTO - GDC/2019- Wetland and Marine Team/_NE103 -- Headstarted Curlew tracking/Data/")
save(data, file="NE103_2022 report_clean tracking data.RData")


# Load
#setwd("C:/Users/gary.clewley/Desktop/BTO - GDC/2019- Wetland and Marine Team/_NE103 -- Headstarted Curlew tracking/Data/")
setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data") #HH laptop
load("NE103_2022 report_clean tracking data.RData")



# Set time period of interest going forward for TIA or remove TrackMultiStack and group_by for habitat work
# data_all<-data[["all"]] # ...etc










#### TIME IN AREA ####
# Using BTOTT::

# Basic visualisation of data
plot_leaflet(data[["all"]], lines=FALSE) #code update - now "plot_leaflet" not "plot_leaflet_dev"
plot_leaflet(data[[1]], lines=FALSE) # 0E plot #HH NB - PAST COHORTS



# Interactive plot with tide data for output
data_tide<-TrackStack2Track(data[["all"]])
data_tide<-data_tide %>% filter(tide!="NA")
data_tide$Tide<-as.character(fct_recode(data_tide$tide, "High tide" = "HW", "Low tide" = "LW") )
plot_leaflet(data_tide, plotby="Tide", lines=FALSE, col=c("#31688EFF","#35B779FF")) #code update - now "plot_leaflet" not "plot_leaflet_dev"





# basic colour mark sightings plot using leaflet:: directly #HH NB - for the fieldwork year 2023-24 KMB going to do this bit
#col_data<-read_csv("data/NE103_2022 colour ring sighting map locations.csv") # for 2022 csv: ID 8 long had a gap before the -3. Removed
#m<-leaflet(col_data) %>% addTiles()  %>%
#   addCircleMarkers(col_data$longitude, col_data$latitude,radius=3, fillOpacity = 1, opacity = 1)





#### TIME IN AREA -- AREA USE UTILISATION DISTRIBUTIONS
library(sf)

# Set arbitrary 'Colony' location to facilitate later functions. Using central Snettisham location here
# but not used to define trips away from central place for Curlew
ColLon = 0.50
ColLat = 52.87

# Set projection
p4 <- sp::CRS(paste("+proj=laea +lon_0=", ColLon," +lat_0=", ColLat, " +units=m", sep=""))

# read in simple UK shapefile map
ukmap <- sf::st_transform(ukmap,p4) #HH NB: ukmap is an sf not an sp. So changed code here from sp::spTransform to sf::st_transform

# reproject ukmap 
#ukmap <- project_points(ukmap, p4s = p4) #HH NB: project_points doesn't work on sf so using st_transform instead
ukmap <- sf::st_transform(ukmap, crs = st_crs(p4))



# Set time period         ##### UPDATE MANUALLY
tia_dat<-data[["all"]]



# get bounds
llyrb = get_bounds(tia_dat, p4s=p4) # Defaults to UK BNG p4s = sp::CRS("+init=epsg:27700")

# run TIA (trial and error on suitable cell size)
indata_grd <- get_TIA_grd(tia_dat, xRa=llyrb$xRa, yRa=llyrb$yRa, cellsize = 500, p4s=p4) # Laptop will not process next step if smaller grid size #Gary's code = cellsize=500

# rank the time cumulatively for plotting for each bird. 
grd_rank_all<- rank_time(indata_grd, population = TRUE) # Population level
grd_rank_birds<- rank_time(indata_grd, population = FALSE) # Individual level



# PLOTTING 

# Set axes limit (units m here) - trial and error to set suitable bounds
xRa<-c(-35000,28000)
yRa<-c(-15000,25000)

# prepare new axes in lat/long
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
#   "#440154FF" "#31688EFF" "#35B779FF" "#FDE725FF"     # For TIA plots




# Set directory (outside of Github here)
#dir<-"C:/Users/gary.clewley/Desktop/BTO - GDC/2019- Wetland and Marine Team/_NE103 -- Headstarted Curlew tracking/Outputs/"
dir <- setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting") #HH laptop directory
plot.name<-"NE103_Headstart CURLE_all data 2022_TIA.tiff"

# Set plot device (saving hi-res base R maps)
tiff(paste0(dir,plot.name), width=25, height=23, units="cm", pointsize=18, res=600, compression ="lzw")

#HH NB: updated as sp package was discontinued. terra used now according to plot_TIA
  #removed: 
terra::plot(ukmap,xlim=xRa, ylim=yRa,col="grey80",border="grey80", axes=T, yaxt="n",
         xaxt="n", xlab="Longitude", ylab="Latitude",
         main="July-December 2022")                     # UPDATE MANUALLY

axis(1, at=seq(min(xRa), max(xRa),by=5000), labels=round(lab_long,2)) 
axis(2, at=seq(min(yRa), max(yRa),by=5000), labels=round(lab_lat,2))


#*****HH NB: THIS PLOT NOT WORKING FOR SOME REASON? SOMETHING TO DO WITH MEMORY
# UPDATE INDIVIDUAL BETWEEN PLOTS
plot_TIA(data=grd_rank_all,Add=TRUE,                    # UPDATE ID SELECTION. HH NB - [c(1:50000),]
         xra=xRa, yra=yRa,
         g_levs = c(1,0.95,0.75,0.5),
         c_levs = c(0.95,0.75,0.5),
         col_ramp_grd =c("#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF"),
         col_ramp_con =c("#31688EFF", "#35B779FF", "#FDE725FF"),
         cont_typ=1)


dev.off()















#### HABITAT SELECTION ####

#### Data prep 
# Load amt:: package
library(amt) 


## Load Land Cover Map 2021 25m Raster
landuse <- raster::raster(here("data","NE103_LCM2021","LCM.tif"))
landuse <- raster::projectRaster(landuse, crs =("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"), method = "ngb") # method nearest neighbour for categorical raster values (opposed to bilinear interpolation)




## ## ## ## ## ## ##
# Unlist and SET TIME PERIOD
trk_dat<-TrackStack2Track(data[["all"]]) # Redo manually for all time periods





# Convert to 'amt' track (using BTOTT headers) #HH NB - release col name updated - could use either release_location or release_site. Cohort col name updated - cohort_num - this could be updated depending on how many cohorts we want to use?
trk <- make_track(trk_dat, .x = longitude, .y = latitude, .t = DateTime, id = TagID, tide = tide, release=release_location, cohort=cohort_num, speed=ground.speed, crs = "epsg:4326")


# Transform to BNG
trk <- transform_coords(trk, "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs")


# Extract LCM variables
trk <- trk %>% 
  extract_covariates(landuse)


# Check sampling rate
summarize_sampling_rate_many(trk, cols="id")


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




# Speed filter to remove likely flight/commuting fixes - using ground.speed from Movebank (not amt calculation)
trk<-trk %>% filter(speed<4)



# Create random points for each individual (As above nesting issue)

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


# Tidy LCM variable  # variable name 'layer' with new landuse data for 2022
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



colnames(rsfdat[rsfdat$LCM,])

#HH NB - MISSING CODE HERE TO EXPORT EACH RSF?? 
#rename the RSF files based on the timeframe - 1wk,2wks,6wks,all Jul-Dec
rsfdat_all <- rsfdat

# Save rsfdat for each time period
setwd("C:/Users/gary.clewley/Desktop/BTO - GDC/2019- Wetland and Marine Team/_NE103 -- Headstarted Curlew tracking/Data/")
save(rsfdat_all, file="NE103_2022 report_RSF_data_all.RData")
save(rsfdat_6, file="NE103_2022 report_RSF_data_six weeks.RData")
save(rsfdat_2, file="NE103_2022 report_RSF_data_two weeks.RData")
save(rsfdat_1, file="NE103_2022 report_RSF_data_one week.RData")
save(rsfdat_0E_22, file="NE103_2022 report_RSF_data_0E_22.RData")


# Load rsfdat for each time period
#setwd("C:/Users/gary.clewley/Desktop/BTO - GDC/2019- Wetland and Marine Team/_NE103 -- Headstarted Curlew tracking/Data/")
setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data") #HH laptop
load("NE103_2022 report_RSF_data_all.RData")
load("NE103_2022 report_RSF_data_six weeks.RData")
load("NE103_2022 report_RSF_data_two weeks.RData")
load("NE103_2022 report_RSF_data_one week.RData")
load("NE103_2022 report_RSF_data_0E_22.RData")








#### RSF plotting ####

## Available/Used Plot 
na.omit(rsfdat_all) %>% #filter(id=="Yf(0E)O/-:Y/m") %>%	                          	# Update period or ID
  ggplot(.,  aes(x=layer,group=used))                                       +	      # select data and variables - using na.omit() here to exclude random points offshore outside LCM area. HH NB - LCM = layer in 2022 and 2023 LCM data so need to change this to layer
  geom_bar(position=position_dodge(), aes(y= ..prop.., fill = used),
           stat="count", colour="black")                                +       # select barplot of proportions presented side by side with black outline
  scale_fill_manual(values=c("grey70", "grey20"))                       + 		  # define colours, plenty of good built in palettes if colour can be used
  labs(y = "Proportion of fixes\n", fill="used", x="\nHabitat")         + 			# labels, \n indicates space between line and text
  theme_classic()                                                       +       # remove default grid lines and grey background
  theme(legend.title=element_blank(),  																	 			  # remove legend title
        legend.position = c(0.9,0.8),                                           # specify legend position inside plot area
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))   +       # rotated x axis labels for individual plots
  scale_y_continuous(expand = expansion(mult = c(0, .1)))               +       # remove gap between bars and axis lines
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14)) +
  ggtitle("Jul-Dec 2022")       # +
# facet_grid(rows=vars(tide)) # if by tide



# Save plot (outside of Github)
#setwd("C:/Users/gary.clewley/Desktop/BTO - GDC/2019- Wetland and Marine Team/_NE103 -- Headstarted Curlew tracking/Outputs/")
setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/output")
ggsave("NE103_Headstart CURLE_RSF plot_all data.jpg", width=15, height=15, units="cm", dpi=300)  ## UPDATE FILENAME



## ## ## ## ## ## ##
# Calculate error bars (in progress - clunky)
x<-as.data.frame(rsfdat_all %>% with(table(id,used,layer))) #HH NB - LCM = layer in 2022 and 2023 LCM data so need to change this to layer
x<-x %>% filter (id!="Yf(0E)O/-:Y/m" & layer!="Coastal Rock")

x1<-x %>% filter(used=="Used") %>% group_by(layer) %>% mutate(prop=Freq/y$Used)# HH NB - the dataframe y is in the line just below - need to run these first and then run these two lines
x2<-x %>% filter(used=="Available") %>% group_by(layer) %>% mutate(prop=Freq/y$Available)


## work out denominator for each individual
y<-tapply(x$Freq, list(x$id, x$used), sum)
y<-as.data.frame(y[-1,])

#note differs from flexible plots combining all birds above)
#prop_u<-x1 %>% group_by(LCM) %>% mutate(mean_prop=mean(prop)) %>% select(LCM, mean_prop) %>% distinct() #HH NB - LCM = layer in 2022 and 2023 LCM data so need to change this to layer
#prop_a<-x2 %>% group_by(LCM) %>% mutate(mean_prop=mean(prop)) %>% select(LCM, mean_prop) %>% distinct() #HH NB - LCM = layer in 2022 and 2023 LCM data so need to change this to layer


sd_u<-x1 %>% group_by(layer) %>% mutate(sd=round(sd(prop)/sqrt(12),2)) %>% select(layer, sd) %>% distinct() #HH NB - LCM = layer in 2022 and 2023 LCM data so need to change this to layer
sd_a<-x2 %>% group_by(layer) %>% mutate(sd=round(sd(prop)/sqrt(12),2)) %>% select(layer, sd) %>% distinct() #HH NB - LCM = layer in 2022 and 2023 LCM data so need to change this to layer

##### Need to sense check if best to use these proportions for plotting (done by individual rather than combined)
# then create new simple plot dataframe e.g. below (from NE86) rather than plotting code used above
# with two rows for each habitat alternating between used and available as per plot



fig_3_newdf<-as.data.frame(cbind(prop, conf.low, conf.high, se))
fig_3_newdf$used<-as.factor(rep(c("Available", "Used"), 7))
fig_3_newdf$lcm_mod<-as.factor(as.character(c("coastal", "coastal", "agriculture","agriculture", "mussel","mussel", "marine", "marine","urban", "urban","other", "other","landfill","landfill")))
fig_3_newdf$lcm_mod<- factor(fig_3_newdf$lcm_mod, levels=c("coastal", "agriculture", "mussel", "marine", "urban", "other", "landfill"))
## ## ## ## ## ## ##












#### RSS models and plotting####
## Relative Selection Strength models (ran manually for time period and habitat for now)
# exp(estimate) for Relative Selection Strength from RSF models



# Select time period
rsfdat<-rsfdat_6

# Fit habitat model for each habitat to each individual
rsffits <- rsfdat  %>% nest(data=-"id") %>%   mutate(mod = map(data, function(x) glm(case_ ~ Coastal, data = x, weight=w,family = binomial)))

## DO NOT open rsffits object from R Studio panel view(rsffits) - keep crashing on 3.6.1



# Check goodness of fit
# Running issues originally due to na.action = argument not set. Ran later and daved manually (writeClipboard(as.character(rsf_gof$auc_test)))
# Could integrate into tidy output if needed

#rsf_gof <- rsfdat  %>% nest(data=-"id") %>%   mutate(auc_test = map(data, function(x) pROC::auc(pROC::roc(x$case_~(predict(glm(case_ ~ Coastal, data = x, weight=w,family = binomial, na.action=na.exclude), type=c("response"))))))) #edit response var



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


# Name for habitat and repeat
rsf_coefs_coast<-rsf_coefs


# Combine and save RSF model outputs
rsf_coefs_hab<-bind_rows(rsf_coefs_coast, rsf_coefs_grass, rsf_coefs_salt, rsf_coefs_arable, rsf_coefs_other)

setwd("C:/Users/gary.clewley/Desktop/BTO - GDC/2019- Wetland and Marine Team/_NE103 -- Headstarted Curlew tracking/Data/")
save(rsf_coefs_hab_all, file="NE103_2022 report_RSF_models_all.RData")
save(rsf_coefs_hab_6, file="NE103_2022 report_RSF_models_six weeks.RData")
save(rsf_coefs_hab_2, file="NE103_2022 report_RSF_models_two weeks.RData")
save(rsf_coefs_hab_1, file="NE103_2022 report_RSF_models_one week.RData")
save(rsf_coefs_hab_0E_22, file="NE103_2022 report_RSF_models_0E_22.RData")


# Load RSF models
setwd("C:/Users/gary.clewley/Desktop/BTO - GDC/2019- Wetland and Marine Team/_NE103 -- Headstarted Curlew tracking/Data/")
load("NE103_2022 report_RSF_models_all.RData")
load("NE103_2022 report_RSF_models_six weeks.RData")
load("NE103_2022 report_RSF_models_two weeks.RData")
load("NE103_2022 report_RSF_models_one week.RData")
load("NE103_2022 report_RSF_models_0E_22.RData")




#### Stat testing RSF - two-stage modeling

library(emmeans)

# Set data
x<-rsf_coefs_hab_all

# Set factor
x$term<-as.factor(as.character(x$term))

# Filter intercept
x<- x %>% filter(term!="(Intercept)")

# Run linear model
m1<-lm(x$estimate~x$term)
summary(m1)

# Check contrasts
mytest <- emmeans(m1, ~ term)
contrast(regrid(mytest))


## Note - following advice from Fieberg RSF/amt course (2019) but needs considering how to better
# carry out testing when many model fits are poor (and coefficients then include outliers)






## RSS PLOTS
## Set data
rsf_coefs_hab<-rsf_coefs_hab_6

# remove outliers (tbc...)

# One week
rsf_coefs_hab<- rsf_coefs_hab %>% filter(id!="Yf(6Y)O/-:Y/m_Sandringham"&id!="Yf(7Y)O/-:Y/m_Sandringham"&id!="Yf(8L)O/-:Y/m_Sandringham")
# Two weeks
rsf_coefs_hab<- rsf_coefs_hab %>% filter(id!="Yf(8E)O/-:Y/m_Sandringham")
# Six weeks
rsf_coefs_hab<- rsf_coefs_hab %>% filter(id!="Yf(8X)O/-:Y/m_KenHill"&id!="Yf(8L)O/-:Y/m_Sandringham")
# all data
rsf_coefs_hab<- rsf_coefs_hab %>% filter(id!="Yf(7K)O/-:Y/m_Sandringham"&id!="Yf(8L)O/-:Y/m_Sandringham"&id!="Yf(8X)O/-:Y/m_KenHill")




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
               size = 1) +
  geom_hline(yintercept = 0, lty = 2) +
  labs(x = "Habitat", y = "Relative Selection Strength") +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14),
        strip.text.x = element_text(size = 12, face="bold")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("One week post-release")   ## UPDATE MANUALLY




# Save plot
setwd("C:/Users/gary.clewley/Desktop/BTO - GDC/2019- Wetland and Marine Team/_NE103 -- Headstarted Curlew tracking/Outputs/")
ggsave("NE103_Headtsart CURLE_RSS plot_one week.jpg", width=15, height=15, units="cm", dpi=300)  ## UPDATE FILENAME








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













