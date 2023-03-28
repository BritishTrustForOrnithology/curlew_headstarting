#### NE103 - Adult Curlew tracking data on The Wash
# Gary Clewley - v1; 28/03/2023


#### NOTES ####

# For comparison with NE103 headstarted data

# _ used as separater not . 

# Extracted from headstart_CU_area_use_2022.R file

# Additional update for tides etc possible if needed





#### DATA PREP ####

## LOAD PACKAGES
load_pkg <- rlang::quos(tidyverse,BTOTrackingTools, here)  # quos() function to be lazy on "" around each package
lapply(lapply(load_pkg, rlang::quo_name), library, character.only = TRUE)



# Load data




# Parse columns names for BTOTT
names(data)[names(data)=="timestamp"]<-"DateTime"
names(data)[names(data)=="individual_local_identifier"]<-"TagID"   # Some tags redeployed multiple times
names(data)[names(data)=="location_long"]<-"longitude"
names(data)[names(data)=="location_lat"]<-"latitude"
names(data)[names(data)=="gps_satellite_count"]<-"satellites_used"


# Coerce required trip and gap columns for later functions (not running trip definition for this project as not central place)
data$tripNo<-1; data$gap<-0; data$gapsec<-1


# Set factors
data$TagID<-as.factor(as.character(data$TagID))


# Set time format
data$DateTime<-as.POSIXct(data$DateTime, format="%Y-%m-%d %H:%M") # no seconds


# Remove extra columns
drop_cols<-c("event_id", "visible", "individual_id", "deployment_id", "tag_id", "study_id", "sensor_type_id", "tag_local_identifier", "individual_taxon_canonical_name", "acceleration_raw_x", "acceleration_raw_y", "acceleration_raw_z", "barometric_height", "battery_charge_percent", 
             "battery_charging_current", "gps_hdop", "gps_time_to_fix", "heading", "import_marked_outlier", "light_level", "magnetic_field_raw_x", "magnetic_field_raw_y", "magnetic_field_raw_z", "ornitela_transmission_protocol", "study_name", "sensor_type",
             "external_temperature","new_datetime","new_datetime_min","date","month", "sunrise", "sunset", "id","in_spa")
data<- data %>% select(-!!drop_cols)


# Set as BTOTT object
data<-Track(data) 



#### TIA PLOTS ####

# Set arbitrary 'Colony' location to facilitate later functions. Using central Snettisham location here
# but not used to define trips away from central place for Curlew
ColLon = 0.50
ColLat = 52.87

# Set projection
p4 <- sp::CRS(paste("+proj=laea +lon_0=", ColLon," +lat_0=", ColLat, " +units=m", sep=""))

# read in simple UK shapefile map
ukmap <- sp::spTransform(ukmap,p4)

# reproject ukmap 
ukmap <- project_points(ukmap, p4s = p4)



# Set time period         ##### UPDATE MANUALLY
tia_dat<- data %>% filter(DateTime>"2021-09-01 00:00:00" & DateTime <"2022-02-28 11:59:59") # Winter 21/22
#tia_dat<- data %>% filter(DateTime>"2022-03-01 00:00:00" & DateTime <"2022-08-31 11:59:59") # Summer 22
#tia_dat<- data %>% filter(DateTime>"2022-09-01 00:00:00" & DateTime <"2023-02-28 11:59:59") # Winter 22/23



# get bounds
llyrb = get_bounds(tia_dat, p4s=p4) # Defaults to UK BNG p4s = sp::CRS("+init=epsg:27700")

# run TIA (trial and error on suitable cell size)
indata_grd <- get_TIA_grd(tia_dat, xRa=llyrb$xRa, yRa=llyrb$yRa, cellsize = 500, p4s=p4) # Laptop will not process next step if smaller grid size

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
dir<-"C:/Users/gary.clewley/Desktop/BTO - GDC/2019- Wetland and Marine Team/_NE103 -- Headstarted Curlew tracking/Outputs/"
plot.name<-"Wash_Adult_CURLE_all_summer22_TIA_n=17.tiff"

# Set plot device (saving hi-res base R maps)
tiff(paste0(dir,plot.name), width=25, height=23, units="cm", pointsize=18, res=600, compression ="lzw")


sp::plot(ukmap,xlim=xRa, ylim=yRa,col="grey80",border="grey80", axes=T, yaxt="n",
         xaxt="n", xlab="Longitude", ylab="Latitude",
         main="Winter 2021/22")                     # UPDATE MANUALLY

axis(1, at=seq(min(xRa), max(xRa),by=5000), labels=round(lab_long,2)) 
axis(2, at=seq(min(yRa), max(yRa),by=5000), labels=round(lab_lat,2))


# UPDATE INDIVIDUAL BETWEEN PLOTS
plot_TIA(data=grd_rank_all,Add=TRUE,                    # UPDATE ID SELECTION
         xra=xRa, yra=yRa,
         g_levs = c(1,0.95,0.75,0.5),
         c_levs = c(0.95,0.75,0.5),
         col_ramp_grd =c("#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF"),
         col_ramp_con =c("#31688EFF", "#35B779FF", "#FDE725FF"),
         cont_typ=1)


dev.off()

