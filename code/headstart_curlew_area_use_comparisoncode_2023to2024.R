#Code to compare data from data from the 2024 report and the updated code for 2025


#2023 report data
data2023report <- readRDS(here("data/data_withcohorts_release_sites.rds"))

#summary(data2023report)
colnames(data2023report)

data2023report <- data2023report %>% filter(data2023report$DateTime > as.POSIXct("2022-12-31 23:59:59", tz="UTC") & data2023report$DateTime  <= as.POSIXct("2023-12-31 23:59:59", tz="UTC") )

data2023report <- data2023report %>% filter(data2023report$year == "2023")

summary(data2023report$DateTime)
summary(data2023report$year)


#updated meta data table from 2024
dt_meta_gsp_TagID_update <- read.csv(here("data/2025 analysis/metadata_TagID_deaddates_notransmision_behaviourdates_2021_2024.csv"), header=T)
head(dt_meta_gsp_TagID_update)
colnames(dt_meta_gsp_TagID_update)

dt_meta_gsp_TagID_update$tag_serial <- as.character(dt_meta_gsp_TagID_update$tag_serial)
dt_meta_gsp_TagID_update$cohort_analysis <- as.factor(dt_meta_gsp_TagID_update$cohort_analysis)
dt_meta_gsp_TagID_update$cohort_analysis <- as.factor(dt_meta_gsp_TagID_update$cohort_analysis)
dt_meta_gsp_TagID_update$release_date_time <-  as.POSIXct(dt_meta_gsp_TagID_update$release_date, format = "%d/%m/%Y", usetz=T, tz="UTC")


dt_meta_gsp_TagID_update_filter <- dt_meta_gsp_TagID_update %>% filter(dt_meta_gsp_TagID_update$year == "2023")


#full join both tables together
trial <- full_join(data2023report,dt_meta_gsp_TagID_update_filter )

summary(trial$DateTime)
summary(trial$year)



table(trial$flag_id, trial$min_category)




#------
 
#create a table of differences between the final data used in analysis from 2023 code compared to 2024

#2024 report analysis 
# Load - NB load automatically loads the data back in as the same name it was saved out as 
setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data") #HH laptop
load("NE103_2023 report_clean tracking data for all 2023 data.RData")

#loaded as data_2023. rename it:
data_2023an <- data_2023



#2025 report analysis
nyr <- "2023"

setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/data/2025 analysis") #HH laptop
load(file=paste0("NE103_",nyr," report_clean tracking data for all ",nyr," data.RData"))


#loaded as data_year. rename it:
data_2024an <- data_year



#------

#trial to plot in ggplot

ukmap <- BTOTrackingTools::ukmap
ukmap <- sf::st_transform(ukmap,p4) #HH NB: ukmap is an sf not an sp. So changed code here from sp::spTransform to sf::st_transform


trial <- trk %>% 
  filter(is.na(layer))

ggplot()+ geom_sf(data=ukmap) + geom_point(data=trk[trk$id=="Yf(9L)O/-:Y/m_KenHill",], aes(x=x_, y=y_)) + theme_classic()

ggplot()+ geom_sf(data=ukmap) + geom_point(data=avail.pts[avail.pts$id=="Yf(9L)O/-:Y/m_KenHill",], aes(x=x_, y=y_, col=case_)) + theme_classic()





t <- trk %>%  tidyr::nest(data=-c("id", "tide", "cohort", "release")) %>% #-c is a minus because it is what you 'non-nested columns' which is what you are interested in but you don't want nested
  mutate(rnd_pts = map(data, ~ random_points(., n = nrow(trial$data[[1]])*20, type="random"))) %>% #creating 20 random points per -c("id", "tide", "cohort", "release")	
  select(id, tide,cohort,release, rnd_pts) %>%  # you don't want to have the original point twice, hence drop data
  unnest_legacy(cols=c(rnd_pts))

 #factor doesn't exist (anymore?)
nrow(trial$data[[1]])*20




avail.pts <- trk %>%  tidyr::nest(data=-c("id", "tide", "cohort", "release"))  #-c is a minus because it is what you 'non-nested columns' which is what you are interested in but you don't want nested


#new part in here because the original code used factor = 20... which is now deprecated so instead we need to use n = but this is literally the number of points added and so for us we want to use the number of GPS fixes per row * 20. 

avail.pts$data_nrow <- NA

for(t in 1:length(unique(trk$id))){
  
  avail.pts$data_nrow[[t]] <- nrow(avail.pts$data[[t]])
  
}

avail.pts <- avail.pts %>%
 mutate(rnd_pts = map(data, ~ random_points(., n= data_nrow*20 , type="random"))) %>% #  factor appears to have been deprecated in version 0.1.6 (unfortunately not noticed in 2023 analysis) so need to use n = .
  select(id, tide,cohort,release, rnd_pts) %>%  # you don't want to have the original point twice, hence drop data
  unnest_legacy(cols=c(rnd_pts))
 






#clip the 

#random points are created within a minimum covex polygon... 
    #took the defined space, clipped to the landuse map, then you would predicted within the polygon, defined around to predict within is the polygon that has been clipped 




#checking how many data points are not in the Wash
trial_dat <-TrackStack2Track(data)

#HH NB added in this code to 'droplevels' of the non-relevant TagID per time period
trk_dat<- trk_dat %>% droplevels()


# Convert to 'amt' track (using BTOTT headers) #HH NB - release col name updated - use release_site_final = combined Ken Hill, seperate Sandringham. Cohort col name updated - cohort_analysis - this uses the first cohorts = 1, and remaining cohorts = 2 
trk <- make_track(trk_dat, .x = longitude, .y = latitude, .t = DateTime, id = TagID, tide = tide, release=release_site_final, cohort=cohort_analysis, speed=ground.speed, crs = "epsg:4326")


# Transform to BNG
trk <- transform_coords(trk, "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs")


data 

data %>% 
  extract_covariates(landuse)



#---
# a small additional loop to run to extract out how many and for which habitat there are outliers for the RSS ####

#load in rsf_coefs_hab files to check the individuals that are outliers on the RSS figures

nyears <- c("2021", "2022", "2023", "2024")

#y<-4

#nyr <- nyears[y]


datasplit <- c("1 One Day" ,"2 One Week" ,"3 Two Weeks" , "4 Six Weeks" ,"5 End of December" ,
               "6 Winter pre-breeding" , "7 Spring fuzzy" , "8a Female Breeding Season" , "8b Male Breeding Season" ,
               "9a Female Autumn fuzzy","9b Male Autumn fuzzy", "10 End of December - Winter" )



filelabels <- c("1_OneDay" ,"2_OneWeek" ,"3_TwoWeeks" , "4_SixWeeks" ,"5_July_December" ,
                "6_WinterPreBreed" , "7_Spring_transition" , "8a_Breeding_female" , "8b_Breeding_male" ,
                "9a_Autumn_transition_female","9b_Autumn_transition_male", "10_WinterPostBreed" )



#keep_tagID
keep_tagID_all <- data.frame()



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
  
  
  
  #add this if loop in for plot label for july-december to include the correct year
  if(p >4){
    plotlab <- paste0("",plotlab," ",nyr,"")
  }
  


setwd("C:/Users/hannah.hereward/Documents/Projects/2024_curlewheadstarting/curlew_headstarting/data/2025 analysis/")
load(file=paste0("NE103_",nyr," report_RSF_models_",cohort,"_",filelab,".RData"))



# Reorder factor levels
rsf_coefs_hab$term<- factor(rsf_coefs_hab$term, levels=c("Saltmarsh", "Coastal", "Arable", "Grassland", "Other"))

trial <- data.frame(rsf_coefs_hab)

trial <- trial %>% filter(!is.na(trial$term))

trial$exp <- exp(trial$estimate)

max_exp <- max(trial$exp, na.rm = T)
  
if(max_exp > 29){

keep_tagID <- trial %>% filter(trial$exp > 29)
keep_tagID$period <- filelab
  

keep_tagID_all <- rbind(keep_tagID_all, keep_tagID)
  
  
}
}






#2024.
#one day = 2 outliers
#


