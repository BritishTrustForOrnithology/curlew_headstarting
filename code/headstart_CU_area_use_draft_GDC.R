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
load_pkg <- rlang::quos(tidyverse,BTOTrackingTools, here)  # quos() function to be lazy on "" around each package
lapply(lapply(load_pkg, rlang::quo_name), library, character.only = TRUE)






#### LOAD MOVEBANK DATA

# Set login credentials
login<-move::movebankLogin()

# Set repository
repo<-"BTO/NE/Pensthorpe/WWT - Eurasian Curlews - headstarted 2021"
TagID<-c("Yf(0E)O/-:Y/m", "Yf(3A)O/-:Y/m")

# Set dates manually - error if one NULL and other defined - logged issue in Git
start<-c("2021-07-03 10:57:18", "2021-07-17 13:56:04")
end_2<-c("2021-07-17 23:59:59", "2021-07-31 23:59:59") # First 2 weeks post-release
end_6<-c("2021-08-14 23:59:59", "2021-08-28 23:59:59") # First 6 weeks post-release
  
# Loads for all time period
data_all <- read_track_MB(TagID=TagID,repo=repo,start=NULL,end=NULL) 
data_2 <- read_track_MB(TagID=TagID,repo=repo,start=start,end=end_2)
data_6 <- read_track_MB(TagID=TagID,repo=repo,start=start,end=end_6)

## loads OK but now clean_GPS issue to solve
 # data<- read_track_MB(TagID=TagID,repo=repo,start=start,end=end) %>% clean_GPS() # workaround below




###
### clean_gps() workaround...

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



## Match tide data - categorical 2 hours either side of high/low (from Port-log.net reports)

# Load data and set Datetime class
tide_dat <- read_csv("data/Wash_tide_data_Bulldog_July_November_2021.csv", 
                     col_types = cols(Observed_DateTime = col_datetime(format = "%d/%m/%Y %H:%M"), Predicted_DateTime = col_datetime(format = "%d/%m/%Y %H:%M")))


## find closest match to trk timestamp (package MALDIquant)
closest<-MALDIquant::match.closest(data$DateTime, tide_dat$Observed_DateTime)

## extract nearest tide time, high/low, height 
data$tide_time<-tide_dat$Observed_DateTime[closest]; data$tide_diff<-difftime(data$DateTime, data$tide_time)
data$tide<-tide_dat$Tide[closest];data$tide_height<-tide_dat$Observed_Height[closest]

## create categorical tide factor with desired threshold; below annotated fixes 2 hour either side of high or low tide
data$tide<-as.factor(ifelse(data$tide_diff<7201 & data$tide_diff>-7201,data$tide, "NA"))






## Convert to BTOTT Track object
data_tt<-Track(data)
data_tt<-clean_GPS(data_tt, drop_sats = 3, Thres = 30, GAP = 28800)
data_tt<- data_tt %>% filter(TagID=="Yf(0E)O/-:Y/m"|TagID=="Yf(3A)O/-:Y/m") %>% droplevels()


data<-Track2TrackStack(data_tt, by="TagID")

# Coerce required trip column (not running trip definition for this project as not central place)
data$`Yf(0E)O/-:Y/m`$tripNo<-1; data$`Yf(3A)O/-:Y/m`$tripNo<-1

data_all<-data # backup to filter from 


# Filter work around for each ID to select time period
dat1<- TrackStack2Track(data_all) %>%  filter(TagID=="Yf(0E)O/-:Y/m") %>% filter(DateTime<"2021-07-17 23:59:59")
dat2<- TrackStack2Track(data_all) %>%  filter(TagID=="Yf(3A)O/-:Y/m") %>% filter(DateTime<"2021-07-31 23:59:59")
data_2<-Track2TrackStack(rbind(dat1, dat2), by="TagID")

dat1<- TrackStack2Track(data_all) %>%  filter(TagID=="Yf(0E)O/-:Y/m") %>% filter(DateTime<"2021-08-14 23:59:59")
dat2<- TrackStack2Track(data_all) %>%  filter(TagID=="Yf(3A)O/-:Y/m") %>% filter(DateTime<"2021-08-28 23:59:59")
data_6<-Track2TrackStack(rbind(dat1, dat2), by="TagID")




# Set time period of interest going forward
#data<-data_all # all
#data<-data_2   # 2 week post-release
#data<-data_6   # 6 week post-release








#### BTOTT TASKS ####

# Basic visualisation of data
plot_leaflet(data)

# Interactive plot with tide data for output
data$Tide<-as.character(fct_recode(data$tide, "High tide" = "HW", "Low tide" = "LW") )
plot_leaflet_dev(data, TagID = "Yf(3A)O/-:Y/m", plotby="Tide") 




#### TIME IN AREA -- AREA USE UTILISATION DISTRIBUTIONS

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


setwd(here("outputs","figures")) # setwd() to save figures

# Set plot device so daving hi-res base R maps
jpeg("Plot.jpeg", width = 15, height = 15, units = 'cm', res = 300) # UPDATE FILENAME



sp::plot(ukmap,xlim=xRa, ylim=yRa,col="darkgreen",border="darkgreen", axes=T, yaxt="n",
         xaxt="n", xlab="Longitude", ylab="Latitude",
         main="Curlew -- 0E") # (manually for each individual)e.g. main=paste("May_",names(grd_rank_birds[2]), sep="")

axis(1, at=seq(min(xRa), max(xRa),by=5000), labels=round(lab_long,2)) 
axis(2, at=seq(min(yRa), max(yRa),by=5000), labels=round(lab_lat,2))



# UPDATE INDIVIDUAL BETWEEN PLOTS
plot_TIA(data=grd_rank_birds$`Yf(0E)O/-:Y/m`,Add=TRUE,                    # UPDATE ID SELECTION
         xra=xRa, yra=yRa,
         g_levs = c(1,0.95,0.75,0.5),
         c_levs = c(0.95,0.75,0.5),
         col_ramp_grd =c("lightblue","blue","yellow","red"),
         col_ramp_con =c("blue","yellow","red"),
         cont_typ=1)


dev.off()















#### AMT TASKS ####
#### Habitat selection 

library(amt) 

## Land Cover Map 2020 25m Raster
landuse <- raster::raster(here("data","NE103_lcm","LCM.tif"))
landuse <- raster::projectRaster(landuse, crs =("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"), method = "ngb") # method nearest neighbour for categorical raster values (opposed to bilinear interpolation)



## clean GPS (use BTOTT?)



## Convert to 'amt' track (using BTOTT headers)
data<-TrackStack2Track(data) # unlist
trk <- make_track(data, .x = longitude, .y = latitude, .t = DateTime, id = TagID, tide = tide, crs = "epsg:4326")



## Transform to BNG
trk <- transform_coords(trk, "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs")


## Extract LCM variables
trk <- trk %>% 
  extract_covariates(landuse)


## Check sampling rate
summarize_sampling_rate_many(trk, cols="id")


## Standardised sampling rate 

#(nesting used on NE86 not functional by ID here - need to look into new syntax - returns as NULL list)
# Workaround to use unnest_legacy()


# potentially causing issues further on - causes random_points() to fail - luckily sampling consistent anyway in this case
#trk <- trk %>% nest(data=-"id") %>% 
#  mutate(data = map(data, ~ .x %>% 
#    track_resample(rate = minutes(15), tolerance = minutes(3))))  %>%
#    unnest_legacy(cols=c(data))

# Revert to class as lost during nesting
#trk <- make_track(trk, .x = x_, .y = y_, .t = t_, id = id, tide = tide, burst = burst_, crs = "epsg:4326")





## Habitat selection

## As above nesting issue
avail.pts <- trk %>%  nest(data=-"id") %>% 
        mutate(rnd_pts = map(data, ~ random_points(., factor = 20, type="random"))) %>% 	
        select(id, rnd_pts) %>%  # you don't want to have the original point twice, hence drop data
        unnest_legacy(cols=c(rnd_pts))

# If also sampling by tide
#avail.pts <- trk %>%  nest(data=-c("id", "tide")) %>% 
#  mutate(rnd_pts = map(data, ~ random_points(., factor = 20, type="random"))) %>% 	
#  select(id, tide, rnd_pts) %>%  # you don't want to have the original point twice, hence drop data
#  unnest_legacy(cols=c(rnd_pts))


# Assign class as lost during nesting
class(avail.pts) <- c("random_points", class(avail.pts))





## Extract LCM values for random points
avail.pts <- avail.pts %>% 
  extract_covariates(landuse)


## Make factor plotting friendly
avail.pts$used<-as.factor(avail.pts$case_)
avail.pts$used<-fct_recode(avail.pts$used, "Available" = "FALSE", "Used" = "TRUE")


## Tidy LCM variable  
rsfdat <- avail.pts %>%  mutate(
  LCM = as.character(LCM), 
  LCM = fct_collapse(LCM,
                     "Arable" = c("3"),
                     "Grassland" = c("4","5","6","7"),
                     "Coastal rock" = c("15", "17"),
                     "Coastal sediment" = c("16", "18"),
                     "Saltmarsh" = c("19"),
                     "Other" = c("1", "2","8", "9", "10", "11", "12","13","14", "20", "21")))


# Reorder factor level
rsfdat$LCM<- factor(rsfdat$LCM, levels=c("Coastal sediment","Saltmarsh","Coastal Rock","Arable","Grassland","Other"))




## RSF models (ran manually for each individual/time period for now)

# set response to numeric
rsfdat <- rsfdat %>% mutate(case_ = as.numeric(case_))

# Weight available data 
rsfdat$w <- ifelse(rsfdat$case_ == 1, 1, 5000)

# Set individual habitat factors (pooling all other habitats into single reference level)
rsfdat$Coastal <- ifelse(rsfdat$LCM == "Coastal sediment", 1, 0)
rsfdat$Saltmarsh <- ifelse(rsfdat$LCM == "Saltmarsh", 1, 0)
rsfdat$Arable <- ifelse(rsfdat$LCM == "Arable", 1, 0)
rsfdat$Grassland <- ifelse(rsfdat$LCM == "Grassland", 1, 0)
rsfdat$Other <- ifelse(rsfdat$LCM == "Other", 1, 0)


# Fit habitat specific models
m1<-glm(case_ ~ Coastal, data = na.omit(rsfdat %>% filter(id=="Yf(0E)O/-:Y/m")), weight=w,family = binomial)
m2<-glm(case_ ~ Coastal, data = na.omit(rsfdat %>% filter(id=="Yf(3A)O/-:Y/m")), weight=w,family = binomial)

m3<-glm(case_ ~ Saltmarsh, data = na.omit(rsfdat %>% filter(id=="Yf(0E)O/-:Y/m")), weight=w,family = binomial)
m4<-glm(case_ ~ Saltmarsh, data = na.omit(rsfdat %>% filter(id=="Yf(3A)O/-:Y/m")), weight=w,family = binomial)

m5<-glm(case_ ~ Arable, data = na.omit(rsfdat %>% filter(id=="Yf(0E)O/-:Y/m")), weight=w,family = binomial)
m6<-glm(case_ ~ Arable, data = na.omit(rsfdat %>% filter(id=="Yf(3A)O/-:Y/m")), weight=w,family = binomial)

m7<-glm(case_ ~ Grassland, data = na.omit(rsfdat %>% filter(id=="Yf(0E)O/-:Y/m")), weight=w,family = binomial)
m8<-glm(case_ ~ Grassland, data = na.omit(rsfdat %>% filter(id=="Yf(3A)O/-:Y/m")), weight=w,family = binomial)

m9<-glm(case_ ~ Other, data = na.omit(rsfdat %>% filter(id=="Yf(0E)O/-:Y/m")), weight=w,family = binomial)
m10<-glm(case_ ~ Other, data = na.omit(rsfdat %>% filter(id=="Yf(3A)O/-:Y/m")), weight=w,family = binomial)



# Check goodness of fit (requires na to be removed)
LogisticDx::gof(m1)

# Needed to work out long hand for some
# rsfdat_0E<-rsfdat %>% filter(id=="Yf(0E)O/-:Y/m") %>% na.omit()
# pROC::auc(pROC::roc(rsfdat_0E$case_~(predict(glm(case_ ~ Grassland, data = na.omit(rsfdat %>% filter(id=="Yf(0E)O/-:Y/m")), weight=w,family = binomial), type=c("response")))))
#.... extracted manually for each and put into report appendix


# Confidence intervals
c1<-confint(m1); c2<-confint(m2);c3<-confint(m3); c4<-confint(m4);c5<-confint(m5);
c6<-confint(m6); c7<-confint(m7);c8<-confint(m8); c9<-confint(m9);c10<-confint(m10)

# Manually pull out coefficients and confidence intervals for RSS plot
id<-c(rep("0E",5), rep("3A",5))
habitat<-c(rep(c("Coastal sediment", "Saltmarsh", "Arable", "Grassland", "Other"),2))
conf.low<-c(c1[2], c3[2], c5[2], c7[2], c9[2], c2[2], c4[2], c6[2], c8[2], c10[2])
conf.high<-c(c1[4], c3[4], c5[4], c7[4], c9[4], c2[4], c4[4], c6[4], c8[4], c10[4])
coef<-c(coef(m1)[2], coef(m3)[2], coef(m5)[2], coef(m7)[2], coef(m9)[2],
        coef(m2)[2], coef(m4)[2], coef(m6)[2], coef(m8)[2], coef(m10)[2])
rss_dat<-data.frame(id, habitat, conf.low, conf.high, coef)




## Available/Used Plot 
na.omit(rsfdat) %>% filter(id=="Yf(0E)O/-:Y/m") %>%	                          	# filter ID
ggplot(.,  aes(x=LCM,group=used))                                       +	      # select data and variables - using na.omit() here to exlcude random points offshore outside LCM area
  geom_bar(position=position_dodge(), aes(y=..prop.., fill = used),
           stat="count", colour="black")                                +       # select barplot of proportions presented side by side with black outline
  scale_fill_manual(values=c("grey70", "grey20"))                       + 		  # define colours, plenty of good built in palettes if colour can be used
  labs(y = "Proportion of fixes\n", fill="used", x="\nHabitat")         + 			# labels, \n indicates sapce between line and text
  theme_classic()                                                       +       # remove default grid lines and grey background
  theme(legend.title=element_blank(),  																	 			  # remove legend title
        legend.position = c(0.9,0.8),                                           # specify legend position inside plot area
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))   +       # rotated x axis labels for individual plots
  scale_y_continuous(expand = expansion(mult = c(0, .1)))               +       # remove gap between bars and axis lines
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14)) +
  ggtitle("0E - 6 weeks post-release")       

# facet_grid(rows=vars(id)) # if by individual



# Save plot
#setwd()
ggsave("plot.jpg", width=15, height=15, units="cm", dpi=300)  ## UPDATE FILENAME





# Resource selection strength plot
ggplot(rss_dat, 	aes(x=habitat)) + 
  geom_point(position=position_dodge(width=0.5), stat="identity", aes(y=exp(coef))) +
  geom_hline(yintercept=1, linetype="dashed") +
  geom_errorbar(aes(ymin=exp(conf.low), ymax=exp(conf.high)), colour="black", width=.1) +
  labs(y = "Resource Selection Strength", x="\nHabitat") +								
  theme_classic() +    		
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14),
        strip.text.x = element_text(size = 12, face="bold")) + # update facet label
  facet_grid(cols=vars(id)) +
  ggtitle("All data") 

# Save plot
#setwd()
ggsave("plot.jpg", width=15, height=15, units="cm", dpi=300)  ## UPDATE FILENAME





#### UNUSED/TEST CODE ####


## Note arbitrary trip, gap and gapsec added for now until clean_gps() function fixed - 
# No significat gaps in data - treat as whole

# temp gap fudge -- ONLY needed if loading through BTOTT and not cleaning
#data$`Yf(0E)O/-:Y/m`$gap<-0; data$`Yf(3A)O/-:Y/m`$gap<-0
#data$`Yf(0E)O/-:Y/m`$gapsec<-1; data$`Yf(3A)O/-:Y/m`$gapsec<-1






# Set 'Other' habitat as reference level for all habitat model - difference reference levels had large effect on results
# so focal hab model against all other habitats pooled as reference used to determine selection
rsfdat <- within(rsfdat, LCM <- relevel(LCM, ref = "Other"))

m1<-glm(case_ ~ LCM, data = rsfdat %>% filter(id=="Yf(0E)O/-:Y/m"), weight=w,family = binomial)




x11() # opening new plot window for manual saving avoided issues occurring 
# with incorrect aspect ration and x axes displaying in main RStudio panel

# Can open new plot window for manual saving for better aspect:ratio but less control over quality



# Dummy reference variable?  - Not informative 'used' still comes out as positive and messes with weighting in model
dummy<-rsfdat[1:400,]
dummy$id<-c(rep(levels(dummy$id)[1],200), rep(levels(dummy$id)[2],200))
dummy$case_<-c(rep(FALSE, 100), rep(TRUE, 100), rep(FALSE, 100), rep(TRUE, 100))
dummy$x_<-555555
dummy$y_<-333333
dummy$LCM<-"Dummy"
dummy$used<-c(rep(levels(dummy$used)[1],100), rep(levels(dummy$used)[2],100),rep(levels(dummy$used)[1],100), rep(levels(dummy$used)[2],100))

  



####

# Convert to 'amt' track (using Movebank headers)
trk <- make_track(data, .x = location.long, .y = location.lat, .t = timestamp, id = individual.local.identifier, crs = "+init=epsg:4326")




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





# Temp nesting fix by individual before using unnest_legacy
trk_id1<- trk %>%  filter(id=="Yf(0E)O/-:Y/m") %>% 
  track_resample(rate = minutes(15), tolerance = minutes(3))

trk_id2<- trk %>%  filter(id=="Yf(3A)O/-:Y/m") %>% 
  track_resample(rate = minutes(15), tolerance = minutes(3))


trk<-rbind(trk_id1, trk_id2)




avail.pts <- trk_id2%>% random_points(factor = 20, type="random")

