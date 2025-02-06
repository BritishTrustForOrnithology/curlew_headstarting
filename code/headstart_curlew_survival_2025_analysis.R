#### NE103 -- Curlew headstarting -- survival analysis


## NEW SCRIPT - Hannah Hereward, Katharine Bowgen, Sam Franks (2024-)

##Using this website as a tester for how to analysis survival data: https://www.montana.edu/rotella/documents/502/lab03RMark.html 

#NOTES
  #Phi = survival
  #p = chance of re-observing (sighting or capture etc...) 
  #time = the time interval 

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




#NE103 -- Curlew headstarting -- survival analysis ####
##import libraries ####
library(knitr)
library(RMark)

##read in data####

#resightings
resight <- read.csv(here("data/2025 analysis/Curlew_BTO_DemOn_HS.csv"), header=T)

resight$year_obs <- str_sub(resight$visit_date, 7,10)

#GPS 
GPS <- read.csv(here("data/2025 analysis/gps_annual_encounter_matrix.csv"), header=T)


##create data frames ready for RMark ####

#Resightings
resight_RM <- resight %>%
  group_by(ring_no, year_obs) %>%
  count()

resight_RM <- data.frame(resight_RM)

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
resight_RM_wide$X2025 <- ifelse(resight_RM_wide$X2025 < 1, 1, resight_RM_wide$X2025)


#combine into one column
resight_RM_wide_final <- resight_RM_wide %>% mutate(survival=paste(X2021 ,X2022 ,X2023, X2024, X2025, sep = "")) 



#GPS 
GPS_RM <- GPS %>% mutate(survival=paste(X2021 ,X2022, X2023,X2024 , sep = ""))

head(GPS_RM)



