#### NE103 -- Curlew headstarting -- survival analysis


## NEW SCRIPT - Hannah Hereward, Katharine Bowgen, Sam Franks (2024-)

##Using this website as a tester for how to analysis survival data: https://www.montana.edu/rotella/documents/502/lab03RMark.html 

#Survival analysis using RMark (need to download Mark as a computer application and then install RMark on R)

#Analysis here uses CJS models which are Cormack-Jolly-Seber Models - these are Mark-Recapture models that allow for different time periods of marking but assume that if it is a 0 this could be because it has died or because it has emigrated 

#NOTES
  #Phi = survival
  #p = detection rate (resighting or recapture etc...) 
  #time = is included so that it can vary over time
  # . or dot = constrained so time is constant across the years




####.####.####

#TRIAL of survival analysis with the CJS model for data on male European dippers ####


#import libraries
library(knitr)
library(RMark)



#trial data read in
edm <- convert.inp("http://www.montana.edu/rotella/documents/502/ed-males.inp",
                   group.df = NULL)
head(edm)


# process the data
edm.pr <- process.data(edm, model = "CJS")

# Setup model structures for each parameter
Phi.dot = list(formula = ~ 1)

# force use of an identity matrix by putting '-1' in formula
Phi.time = list(formula = ~ -1 + time) 


p.dot = list(formula = ~ 1)

# force use of an identity matrix by putting '-1' in formula
p.time = list(formula = ~ -1 + time) 



Phi.dot.p.dot = mark(edm.pr, 
                     model.parameters = list(Phi = Phi.dot, p = p.dot))



Phi.time.p.dot = mark(edm.pr, 
                      model.parameters = list(Phi = Phi.time, p = p.dot))


Phi.dot.p.time = mark(edm.pr, 
                      model.parameters = list(Phi = Phi.dot, p = p.time))


Phi.time.p.time = mark(edm.pr, 
                       model.parameters = list(Phi = Phi.time, p = p.time))


edm.results <- model.table(model.list = c("Phi.dot.p.dot", 
                                          "Phi.dot.p.time",
                                          "Phi.time.p.dot",
                                          "Phi.time.p.time"),
                           model.name = FALSE)

kable(edm.results, digits = 3)


#trial using the dipper data by this website instead:#### 
    #https://jamesepaterson.github.io/jamespatersonblog/2020-04-26_introduction_to_CJS.html 

#process data (and set grouping variables)
#dipper.proc <- process.data(edm)


#make design data (from processed data)
#dipper.ddl <- make.design.data(dipper.proc)



#trial using the dipper data by this webpage: ####
    #https://www.montana.edu/screel/teaching/bioe-440r-521/course-outline/BIOE_440_CJS_dipper_example.html

#process data (and set grouping variables)
dipper.processed <- process.data(edm)


#make design data (from processed data)
dipper.ddl <- make.design.data(dipper.processed)


#define the compeitiong models for survival (Phi)
Phidot <- list(formula = ~1)

Phitime <- list(formula = ~time)


#then define the competing models for detection (p)
pdot <- list(formula = ~1)

ptime <- list(formula = ~time)


#write out the individual models
dipper.phitime.ptime <- mark(dipper.processed, dipper.ddl,
                             model.parameters = list(Phi = Phitime, p=ptime))


dipper.phitime.pdot <- mark(dipper.processed, dipper.ddl,
                             model.parameters = list(Phi = Phitime, p=pdot))

dipper.phidot.ptime <- mark(dipper.processed, dipper.ddl,
                             model.parameters = list(Phi = Phidot, p=ptime))


dipper.phidot.pdot <- mark(dipper.processed, dipper.ddl,
                            model.parameters = list(Phi = Phidot, p=pdot))


#create an AIC table
dipper.table <- collect.models(type = "CJS")

dipper.table


####.####.####


#NE103 -- Curlew headstarting -- survival analysis ####

##Using this website as the basis for how to analysis survival data for the headstarted curlew: https://www.montana.edu/rotella/documents/502/lab03RMark.html 

# this website has more explination of the background to CJS which was useful: https://jamesepaterson.github.io/jamespatersonblog/2020-04-26_introduction_to_CJS.html

#This is possibly the older webpage from montana again but again gives more explanations: https://www.montana.edu/screel/teaching/bioe-440r-521/course-outline/BIOE_440_CJS_dipper_example.html



#NOTES
#Phi = survival
#p = detection rate (resighting or recapture etc...) 
#time = is included so that it can vary over time
# . or dot = constrained so time is constant across the years

#Here we are going to run four models for the headstarted curlew - for resightings and GPS seperately (so 8 models in total)
#These four models per data set allow the survival rate (phi) and the detection rate (p) either vary by time (t) or constrained to be constant ( . or dot)

#model adjustments might be needed I DON'T UNDERSTAND THIS PART OF THE WEBPAGE BLURB



## LOAD PACKAGES ####
load_pkg <- rlang::quos(tidyverse,BTOTrackingTools, here, sp, leaflet, terra)  # quos() function to be lazy on "" around each package
lapply(lapply(load_pkg, rlang::quo_name), library, character.only = TRUE)

library(knitr)
library(RMark)


library(dplyr)
library(ggplot2)
library(viridisLite)
library(viridis)

##read in and manipulate the data into the correct format ####

#correct format is a 0 or 1:
  #Bird classed as 1 in year released
  #Bird classed as 1 in year N if tag transmitted and any tag data (GPS, accelerometer, temperature) indicated bird was alive at the time OR resighted at any time in the year
  #Bird classed as 0 in year N if tag did not transmit (for whatever reason) OR were not resighted at any time in that year
  #Bird classed as 0 in year N if tag transmitted but tag data indicated 1) bird was not alive at the time or 2) tag was not attached to the bird OR bird was found not alive at any time in that year
  #and for GPS data set: Bird classed as 0 in year N even if known to be alive by other means (captured, resighted, etc)
  #and for resighting data set: Bird classed as 0 in year N even if known to be alive by other means (GPS etc)


#resightings dataset - downloaded by KMB on 06/02/2025 from Deomon - by list of ring numbers of headstarted birds
resight <- read.csv(here("data/2025 analysis/Curlew_BTO_DemOn_HS.csv"), header=T)

#extract out the observation year
resight$year_obs <- str_sub(resight$visit_date, 7,10)

#Resightings - group by year observation to get a count per year of obs
resight_RM <- resight %>%
  group_by(hs_year, ring_no, year_obs) %>%
  count()

resight_RM <- data.frame(resight_RM)

#make this a wide table so each column is the year obs 
resight_RM_wide <- resight_RM %>% 
  pivot_wider(names_from = year_obs, values_from = c( "n"))

resight_RM_wide <- data.frame(resight_RM_wide)

#turn the NAs to 0
resight_RM_wide$X2021 <- ifelse(is.na(resight_RM_wide$X2021), 0, resight_RM_wide$X2021)
resight_RM_wide$X2022 <- ifelse(is.na(resight_RM_wide$X2022), 0, resight_RM_wide$X2022)
resight_RM_wide$X2023 <- ifelse(is.na(resight_RM_wide$X2023), 0, resight_RM_wide$X2023)
resight_RM_wide$X2024 <- ifelse(is.na(resight_RM_wide$X2024), 0, resight_RM_wide$X2024)
resight_RM_wide$X2025 <- ifelse(is.na(resight_RM_wide$X2025), 0, resight_RM_wide$X2025)



#turn the > 1 numbers to 1s
resight_RM_wide$X2021 <- ifelse(resight_RM_wide$X2021 > 1, 1, resight_RM_wide$X2021)
resight_RM_wide$X2022 <- ifelse(resight_RM_wide$X2022 > 1, 1, resight_RM_wide$X2022)
resight_RM_wide$X2023 <- ifelse(resight_RM_wide$X2023 > 1, 1, resight_RM_wide$X2023)
resight_RM_wide$X2024 <- ifelse(resight_RM_wide$X2024 > 1, 1, resight_RM_wide$X2024)
resight_RM_wide$X2025 <- ifelse(resight_RM_wide$X2025 > 1, 1, resight_RM_wide$X2025)


#NOTE column x2023 is after x2024!! So need to make sure the paste is the correct order
#combine into one column #NB labeled 'ch' because this is the column name needed for RMark
resight_RM_wide_final <- resight_RM_wide %>% mutate(ch=paste(X2021 ,X2022 ,X2023, X2024, X2025, sep = "")) 


#write out
#write.csv(resight_RM_wide_final, here("data/2025 analysis/Curlew_BTO_DemOn_HS_RMark_col.csv"), row.names = F)


#GPS dataset
GPS <- read.csv(here("data/2025 analysis/gps_annual_encounter_matrix.csv"), header=T)


#combine all year columns into one column #NB labeled 'ch' because this is the column name needed for RMark
GPS_RM <- GPS %>% mutate(ch=paste(X2021 ,X2022, X2023,X2024 , sep = ""))

head(GPS_RM)
summary(GPS_RM)


#write out
#write.csv(resight_RM_wide_final, here("data/2025 analysis/gps_annual_encounter_matrix_RMark_col.csv"), row.names = F)




##RMark analysis for curlew headstarting ####

# Setup model structures for each parameter
#reason for using -1 for the time parameters: 
  # ∼time creates a design matrix with an intercept and a Beta for each time beyond the first time, so it is additive which is not allowed. 
    #However, we can specify a design matrix without an intercept using ∼-1 + time (found at this link: http://www.phidot.org/software/mark/docs/book/pdf/app_3.pdf)

Phi.dot = list(formula = ~ 1)

# force use of an identity matrix by putting '-1' in formula
Phi.time = list(formula = ~ -1 + time) 

p.dot = list(formula = ~ 1)

# force use of an identity matrix by putting '-1' in formula 
p.time = list(formula = ~ -1 + time) 




###Resighting ####

#make hs_year a factor
resight_RM_wide_final$hs_year <- as.factor(resight_RM_wide_final$hs_year)

#process the data and can use 'group' to specify a specific group if needed (in the dipper data it has sex as a column... here we have year headstarted)
#resight.pr <- process.data(resight_RM_wide_final, model = "CJS", group = "hs_year")

#taking group out for now
resight.pr <- process.data(resight_RM_wide_final, model = "CJS")



#This seems to be useful if you have groups you're interested in differentiating / comparing - but it is also a needed part of analysis according to the help page for mark
resight.ddl <- make.design.data(resight.pr)


#write out the individual models
curlew_resight_phi.dot.p.dot <- mark(resight.pr, resight.ddl,
                                     model.parameters = list(Phi = Phi.dot, p = p.dot))

curlew_resight_phi.time.p.dot <- mark(resight.pr, resight.ddl,
                                     model.parameters = list(Phi = Phi.time, p = p.dot))

curlew_resight_phi.dot.p.time <- mark(resight.pr, resight.ddl,
                                     model.parameters = list(Phi = Phi.dot, p = p.time))

curlew_resight_phi.time.p.time <- mark(resight.pr, resight.ddl,
                                     model.parameters = list(Phi = Phi.time, p = p.time))




#create a model table
curlew_resight.results <- model.table(model.list = c ("curlew_resight_phi.dot.p.dot",
                                                      "curlew_resight_phi.time.p.dot",
                                                      "curlew_resight_phi.dot.p.time",
                                                      "curlew_resight_phi.time.p.time"),
                                      model.name = F)

#select the top result from the table
curlew_resight.results

#For resighted birds the best model (lowest AIC = 339.22) was where the survival varied by time but the probability of detection remained constant across time



#beta - the estimates are in logit form so you'd need to us exp function or plogis BUT the output also has a 'real' output already calculated
#curlew_resight_phi.time.p.dot$results$beta

#real
curlew_resight_phi.time.p.dot$results$real

#So the results for the re-sighted tagged birds. The probability of animals survival from time 1 was 0.31, from time 2 was 0.57 from time 3 was 0.80 and time 4 was 0.11 and the probability of detection was 0.50! 
#estimate        se       lcl       ucl fixed note
#Phi g1 c1 a0 t1 0.3051436 0.0713047 0.1851166 0.4591445           
#Phi g1 c1 a1 t2 0.5747348 0.1006158 0.3762119 0.7517653           
#Phi g1 c1 a2 t3 0.8001420 0.1489878 0.3920053 0.9613300           
#Phi g1 c1 a3 t4 0.1073813 0.0441095 0.0465417 0.2286764           
#p g1 c1 a1 t2   0.5010953 0.0832719 0.3433337 0.6586392   


resight_outputs <- data.frame(curlew_resight_phi.time.p.dot$results$real)
resight_outputs$year <- c(2021, 2022, 2023, 2024, "P")


ggplot(data = resight_outputs[c(1:4),], aes(x = year, y=estimate))+
  geom_bar(stat="identity", fill="lightgrey")+
  geom_errorbar(aes(ymin=(estimate-se), ymax=(estimate+se)))+
  xlab("Year of observation")+
  ylab("Probability of survival")+
  geom_hline(aes(yintercept=0.50,  linetype = "Probability of detection"), show.legend = T) +
  scale_linetype_manual(name = "", values = c("dashed"))+
  ggtitle("a")+
  ylim(c(0,1))+
  theme_classic()


setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/output/Figures 2025/extra_plots/") #HH NB laptop
ggsave(file=paste0("NE103_2024_Headtsart CURLE_survival_resightedbirds.png"), width=15, height=15, units="cm", dpi=300)  ## UPDATE FILENAME





####GPS####

#make hs_year a factor
GPS_RM$year <- as.factor(GPS_RM$year)

#process the data and can use 'group' to specify a specific group if needed (in the dipper data it has sex as a column... here we have year headstarted)
#gps.pr <- process.data(GPS_RM, model = "CJS", group = "year")

#taking group out for now
gps.pr <- process.data(GPS_RM, model = "CJS")


#This seems to be useful if you have groups you're interested in differentiating / comparing - but it is also a needed part of analysis according to the help page for mark
gps.pr.ddl <- make.design.data(gps.pr)


#write out the individual models
curlew_gps_phi.dot.p.dot <- mark(gps.pr, gps.pr.ddl,
                                     model.parameters = list(Phi = Phi.dot, p = p.dot))

curlew_gps_phi.time.p.dot <- mark(gps.pr, gps.pr.ddl,
                                      model.parameters = list(Phi = Phi.time, p = p.dot))

#warning error that only 3 parameters counted of 4 specified parameters
#can update the code to adult the number of parameters but this doesn't do anything for the AIC
#curlew_gps_phi.time.p.dot <- adjust.parameter.count(curlew_gps_phi.time.p.dot, 3)

curlew_gps_phi.dot.p.time <- mark(gps.pr, gps.pr.ddl,
                                      model.parameters = list(Phi = Phi.dot, p = p.time))

curlew_gps_phi.time.p.time <- mark(gps.pr, gps.pr.ddl,
                                       model.parameters = list(Phi = Phi.time, p = p.time))




#create a model table
curlew_gps.results <- model.table(model.list = c ("curlew_gps_phi.dot.p.dot",
                                                      "curlew_gps_phi.time.p.dot",
                                                      "curlew_gps_phi.dot.p.time",
                                                      "curlew_gps_phi.time.p.time"),
                                      model.name = F)

#select the top result from the table
curlew_gps.results

#For GPS birds the best model (lowest AIC = 68.95) was where the survival varied by time but the probability of detection remained constant across time


#beta - the estimates are in logit form so you'd need to us exp function or plogis BUT the output also has a 'real' output already calculated
#curlew_gps_phi.time.p.dot$results$beta


#real
curlew_gps_phi.time.p.dot$results$real

#So the results for the GPS tagged birds. The probability of animals survival from time 1 was 0.25, from time 2 was 0.76 from time 3 was 0.47 and the probability of detection was 1! 


#curlew_gps_phi.time.p.dot$results$real
#estimate        se       lcl       ucl fixed note
#Phi g1 c1 a0 t1 0.2500000 0.2165063 0.0335101 0.7621677           
#Phi g1 c1 a1 t2 0.7692308 0.1168545 0.4784491 0.9237344           
#Phi g1 c1 a2 t3 0.4666667 0.0910840 0.2992613 0.6419310           
#p g1 c1 a1 t2   1.0000000 0.0000000 1.0000000 1.0000000      



gps_outputs <- data.frame(curlew_gps_phi.time.p.dot$results$real)
gps_outputs$year <- c(2021, 2022, 2023, "P")



ggplot(data = gps_outputs[c(1:3),], aes(x = year, y=estimate))+
  geom_bar(stat="identity", fill="lightgrey")+
  geom_errorbar(aes(ymin=(estimate-se), ymax=(estimate+se)))+
  xlab("Year of observation")+
  ylab("Probability of survival")+
  geom_hline(aes(yintercept=1,  linetype = "Probability of detection"), show.legend = T) +
  scale_linetype_manual(name = "", values = c("dashed"))+
  ggtitle("b")+
  ylim(c(0,1))+
  theme_classic()


setwd("~/Projects/2024_curlewheadstarting/curlew_headstarting/output/Figures 2025/extra_plots/") #HH NB laptop
ggsave(file=paste0("NE103_2024_Headtsart CURLE_survival_GPSbirds.png"), width=15, height=15, units="cm", dpi=300)  ## UPDATE FILENAME





