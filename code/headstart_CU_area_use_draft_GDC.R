#### NE106 -- Curlew headstarting -- post-release tracking
## Draft code - work in progress
## Gary Clewley 


#### NOTES ####
#Individuals 0J and 3K excluded from analyses below as only tracked for <1 week. 
#Descriptive details included in report only

# Current plan to re-run different time periods from the beginning for all plots



#### SETUP ####

## Checkpoint??


#### LOAD PACKAGES
load_pkg <- rlang::quos(tidyverse,BTOTrackingTools)  # quos() function to be lazy on "" around each package
lapply(lapply(load_pkg, rlang::quo_name), library, character.only = TRUE)






#### LOAD MOVEBANK DATA

# Set login credentials
login<-move::movebankLogin()

# Set repository
repo<-"BTO/NE/Pensthorpe/WWT - Eurasian Curlews - headstarted 2021"
TagID<-c("Yf(0E)O/-:Y/m", "Yf(3A)O/-:Y/m")
start=NULL
end=NULL

# Loads for all time period
data <- read_track_MB(TagID=TagID,repo=repo,start=start,end=end) 

## loads OK but now clean_GPS issue to solve
 # %>% clean_GPS()





#### BTOTT TASKS ####


## TIA

## Note arbitrary trip, gap and gapsec added for now until clean_gps() function fixed - 
# confirm if any gaps need dealing with before finalising  GDC 12 Feb

# temp gap fudge?
data$`Yf(0E)O/-:Y/m`$gap<-0; data$`Yf(3A)O/-:Y/m`$gap<-0
data$`Yf(0E)O/-:Y/m`$gapsec<-1; data$`Yf(3A)O/-:Y/m`$gapsec<-1

# Coerce required trip column 
data$`Yf(0E)O/-:Y/m`$tripNo<-1; data$`Yf(3A)O/-:Y/m`$tripNo<-1


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

# get bounds
llyrb = get_bounds(data, p4s=p4) # Defaults to UK BNG p4s = sp::CRS("+init=epsg:27700")

# run TIA (trial and error on suitable cell size)
indata_grd <- get_TIA_grd(data, xRa=llyrb$xRa, yRa=llyrb$yRa, cellsize = 250, p4s=p4)

# rank the time cumulatively for plotting for each bird. Not running population TIA for NE103
grd_rank_birds<- rank_time(indata_grd, population = FALSE)



# PLOTTING 

# Set axes limit (units m here) - trial and error to set suitable bounds
xRa<-c(-35000,10000)
yRa<-c(-15000,25000)

# prepare new axes in lat/long
earth <- 6378.137
m <- (1 / ((2 * pi / 360) * earth)) /1000

new_lat_lower <- round(ColLat + (min(yRa) * m),1)     ## multiple xyRa by 100 if working in p4 units km
new_lat_upper <- round(ColLat + (max(yRa) * m),1)
new_long_lower <- round(ColLon + (min(xRa) * m) / cos(ColLat * (pi / 180)),1)
new_long_upper <- round(ColLon + (max(xRa) * m) / cos(ColLat * (pi / 180)),1)	

lab_long<-seq(new_long_lower, new_long_upper,length.out=length(seq(min(xRa), max(xRa),by=5000)))
lab_lat<-seq(new_lat_lower, new_lat_upper,length.out=length(seq(min(yRa), max(yRa),by=5000)))


x11() # opening new plot window for manual saving avoided issues occurring 
# with incorrect aspect ration and x axes displaying in main RStudio panel

sp::plot(ukmap,xlim=xRa, ylim=yRa,col="darkgreen",border="darkgreen", axes=T, yaxt="n",
         xaxt="n", xlab="Longitude", ylab="Latitude",
         main="Curlew -- 0E") # (manually for each individual)e.g. main=paste("May_",names(grd_rank_birds[2]), sep="")

axis(1, at=seq(min(xRa), max(xRa),by=5000), labels=round(lab_long,2)) 
axis(2, at=seq(min(yRa), max(yRa),by=5000), labels=round(lab_lat,2))



# UPDATE INDIVIDUAL BETWEEN PLOTS
plot_TIA(data=grd_rank_birds$`Yf(0E)O/-:Y/m`,Add=TRUE,     
         xra=xRa, yra=yRa,
         g_levs = c(1,0.95,0.75,0.5),
         c_levs = c(0.95,0.75,0.5),
         col_ramp_grd =c("lightblue","blue","yellow","red"),
         col_ramp_con =c("blue","yellow","red"),
         cont_typ=1)


















#### AMT TASKS ####

### bare bones works - needs tidying


library(amt) 

## LCM2015 Raster
setwd("C:/Users/gary.clewley/Desktop/BTO - GDC/2019- Wetland and Marine Team/GIS/NE103_lcm")
landuse <- raster::raster("LCM.tif")
landuse <- raster::projectRaster(landuse, crs =("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"), method = "ngb") # method nearest neighbour for categorical raster values (opposed to bilinear interpolation)


## clean GPS (use BTOTT?)

## Convert to 'amt' track (using Movebank headers)
trk <- make_track(data, .x = location.long, .y = location.lat, .t = timestamp, id = individual.local.identifier, crs = "+init=epsg:4326")


## Filter out two tags with few data
trk<- trk %>% filter(id!="Yf(0J)O/-:Y/m" & id!="Yf(3K)O/-:Y/m")


## Transform to BNG
trk <- transform_coords(trk, "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs")


## Extract LCM variables
trk <- trk %>% 
  extract_covariates(landuse)


## Check sampling rate
summarize_sampling_rate_many(trk, cols="id")


## Standardised sampling rate 


#(nesting used on NE86 not functional by ID here - need to look into - returns as NULL list)
# also below doesn't work if 'raster' loaded

#trk <- trk %>% nest(data=-"id") %>% 
#  mutate(data = map(
#    data, ~ .x %>% 
#      track_resample(rate = minutes(15), tolerance = minutes(3))))  %>%
#  unnest(cols=c(data))


# Temp fix by individual
trk_id1<- trk %>%  filter(id=="Yf(0E)O/-:Y/m") %>% 
  track_resample(rate = minutes(15), tolerance = minutes(3))

trk_id2<- trk %>%  filter(id=="Yf(3A)O/-:Y/m") %>% 
  track_resample(rate = minutes(15), tolerance = minutes(3))


trk<-rbind(trk_id1, trk_id2)



## Habitat selection

## As above nesting issue
#avail.pts <- trk %>%  nest(data=-"id") %>% 
#  mutate(rnd_pts = map(data, ~ random_points(., factor = 20, type="regular"))) %>% 		# can also drop type="regular" for true random
#  select(id, rnd_pts) %>%  # you dont want to have the original point twice, hence drop data
#  unnest(cols=c(data))




## Example with one individual

## Generate pseudo points and extract landuse
avail.pts <- trk_id1 %>% random_points(factor = 20, type="regular")

avail.pts <- avail.pts %>% 
  extract_covariates(landuse)


## Make factor plotting friendly
avail.pts$used<-as.factor(avail.pts$case_)
avail.pts$used<-fct_recode(avail.pts$used, "Available" = "FALSE", "Used" = "TRUE")


## Tidy LCM variable  

            # redo with some aggregating down the line   
#### ALSO VERIFY EXTRACTION ACCURATE....lots of linear, move rock to other?
## check offshore etc - plot all to check



rsfdat <- avail.pts %>%  mutate(
  LCM = as.character(LCM), 
  LCM = fct_collapse(LCM,
                         "Linear features" = c("3"),
                         "Arable" = c("4"),
                         "Open Water" = c("13"),
                         "Inland Rock" = c("16"),
                         "Supralittoral Rock" = c("18"),
                         "Supralittoral Sediment" = c("19"),
                         "Littoral Rock" = c("20"),
                         "Littoral Sediment" = c("21"),
                         "other" = c("1", "2", "5", "6", "7", "8", "9", "10", "11", "12","14","15","17","22")))

rsfdat$LCM<-forcats::fct_explicit_na(rsfdat$LCM, "Offshore")                         
   




## See twitter bookmarks for setting standards preferences in ggplot

## Plot 
ggplot(rsfdat,  aes(x=LCM,group=used))+    																                    		    # select data and variables
  geom_bar(position=position_dodge(), aes(y=..prop.., fill = used), stat="count", colour="black") + 	# select barplot of proportions presented side by side with black outline
  scale_fill_manual(values=c("grey70", "grey20")) +   												   			                # define colours, plenty of good built in palettes if colour can be used
  labs(y = "Proportion of fixes\n", fill="used", x="\nHabitat") +   										          		# labels, \n indicates sapce between line and text
  theme_classic() +    																								                                # remove default grid lines and grey background
  theme(legend.title=element_blank(),  																	 			                        # remove legend title
        legend.position = c(0.9,0.8),                                                                 # specify legend position inside plot area
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +                               # rotated x axis labels for individual plots
  scale_y_continuous(expand = expand_scale(mult = c(0, .1))) +		                                    # remove gap between bars and axis lines
  ggtitle("Curlew -- 0E")
  
  











#### UNUSED/TEST CODE ####


# Initial issues using BTOTT to load data and used move:: directly
## Using Move package 
data <- move::getMovebankLocationData(study="BTO/NE/Pensthorpe/WWT - Eurasian Curlews - headstarted 2021",
                                      sensorID=653, login = login)

## Make parsable as a Track object by BTOTT package
names(data)[names(data)=="individual.local.identifier"]<-"TagID"   # Some tags redeployed multiple times
names(data)[names(data)=="timestamp"]<-"DateTime"
names(data)[names(data)=="location.long"]<-"longitude"
names(data)[names(data)=="location.lat"]<-"latitude"
names(data)[names(data)=="gps.satellite.count"]<-"satellites_used"


## Convert to BTOTT Track object
data_tt<-Track(data)
data_tt<-clean_GPS(data_tt, drop_sats = 3, Thres = 30, GAP = 28800)


data_stack<-Track2TrackStack(data_tt, by="TagID")







#### KDE test
## BELOW ISSUES NOT PLOTTING USING BTOTT BUT;  CLUNKY BUT WORKS OK WOTH OLD NE82 CODE

data_stack<-Track2TrackStack(data_tt, by="TagID")

indata <- track_multi_stack(list(data_stack)) %>% define_trips(method="rect", plot=TRUE, lls = c(0.43, 52.86,0.46, 52.83), p4s = p4)

indata <- track_subsamp(indata, dt = 1800)

k_prep <- kernelUD_prep(data = indata, Choice = 20)

xy <- kernelUD_grid(data = indata,  Choice=20, res=500, ADJVAL = 20000)

oneUD <- adehabitatHR::kernelUD(k_prep,h = "href", grid=xy, same4all = FALSE, kern = "bivnorm")
#oneUD <- minhref(data = k_prep, grid = xy)


ver_1 <- adehabitatHR::getvolumeUD(oneUD)
ver_1_95 <- adehabitatHR::getverticeshr(oneUD, 95)

sp::plot(ver_1[[1]])
sp::plot(ver_1_95[ver_1_95$id == "Yf(0E)O/-:Y/m",],add=TRUE)





## Plots

#scale_x_discrete(labels = c("Coastal", "Agriculture", "Mussel Bed", "Marine", "Urban", "Other", "Landfill")) +    # rename factor levels without editing original data


