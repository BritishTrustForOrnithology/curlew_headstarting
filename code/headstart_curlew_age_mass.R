##############################
#
#    NE103: Headstarted Curlew age vs biometrics
#
##############################

# ======================   Variables to pass to setup code source  ===========

# Header code to install and load packages, set directory paths, set seed, capture session info

# project_details <- list(project_name, output_version_name, workspace_version_name)
# package_details <- c("package name 1", "package name 2")

project_details <- list(project_name="curlew", output_version_date="2024_headstarting", workspace_version_date="2024_headstarting")
package_details <- c("sf","tidyverse","patchwork","move","moveVis","RColorBrewer","viridisLite","rcartocolor","lubridate", "nlme", "lme4", "ggeffects", "broom.mixed", "patchwork")
seed_number <- 1



# =======================    Read header source code   =================

# header code source:
# 1. sets working directories
# 2. loads required packages
# 3. prints session info to file
# 4. sets seed for reproducibility

# should run on either PC or Mac if using .Rproj
source(file.path("code/source_setup_code_rproj.R"))

# project directories created:
# parentwd = Git
# projectwd = project name
# codewd = directory containing code, functions, source, etc
# datawd = directory containing data
# outputwd = directory containing outputs and results (within the appropriate version date)
# workspacewd = directory containing workspace files (.rds, .rda, .RData; within the appropriate version date)
# topoutputwd = top level output directory
# topworkspacewd= top level workspace directory


# =======================    Control values   =================

current_year <- 2024

# TRUE = fresh download of google drive data
update_gdrive_data <- TRUE


# =======================    Load data   =================

today_date <- format(Sys.Date(), "%d-%b-%Y")

# Load data
# Toggle logic value above if fresh download of google drive data is needed
# will need to provide authentication for R to access Google Drive
if (update_gdrive_data) source(file.path("code", "source", "download_gdrive_data.R"))
source(file.path("code", "source", "load_gdrive_data.R"))

# Merge biometric with metadata
# In previous seasons, removed rows with no weights, but in 2023 birds were handled / measured multiple times to check feather growth, but weights not always taken (only wing measurements)
# dt_all <- merge(dt_meta, dt_biometric %>% filter(!is.na(weight)), by = c("ring", "year", "flag_id"))
dt_all <- merge(dt_meta, dt_biometric, by = c("ring", "year", "flag_id"))


dt <- dt_all %>% 
  mutate(cohort_num = as.factor(cohort_num)) %>% 
  filter(!is.na(cohort_num))

dt <- dt %>% 
  mutate(date = as.POSIXct(strptime(paste(day, month, year, sep="/"), format = "%d/%m/%Y", tz="UTC")))%>%
  mutate(release_date = as.POSIXct(strptime(release_date, format = "%d/%m/%Y", tz="UTC"))) %>% 
  mutate(tagged_date = as.POSIXct(strptime(tagged_date, format = "%d/%m/%Y", tz="UTC"))) %>% 
  mutate(age_released = as.integer(days_age + (release_date - date)))

# =======================    Figures - 2021 Pensthorpe birds   =================

year_list <- list(2021, 2022, 2023, 2024)
age_mass <- list()
age_wing <- list()
weight_wing <- list()

for (y in year_list) {
  
  # 2021 data from Pensthorpe
  age_mass[[y]] <- ggplot(data = dt %>% filter(year == y)) +
    geom_point(aes(y = weight, x = days_age, colour = cohort_num)) +
    geom_smooth(aes(y = weight, x = days_age, fill = cohort_num, colour = cohort_num)) +
    # scale_color_carto_d(type = "qualitative", palette = "Bold") +
    # scale_fill_carto_d(type = "qualitative", palette = "Bold") +
    scale_color_brewer(type = "qual", palette = "Dark2") +
    scale_fill_brewer(type = "qual", palette = "Dark2") +
    labs(x = "Age (days)", y = "Weight (g)", title = paste("Weight vs age:", y))
  
  
  
  # Wing vs age
  age_wing[[y]] <- ggplot(data = dt %>% filter(year == y)) +
    geom_point(aes(y = wing, x = days_age, colour = cohort_num)) +
    geom_smooth(aes(y = wing, x = days_age, fill = cohort_num, colour = cohort_num)) +
    scale_color_brewer(type = "qual", palette = "Dark2") +
    scale_fill_brewer(type = "qual", palette = "Dark2") +
    labs(x = "Age (days)", y = "Wing length (mm)", title = paste("Wing length vs age:", y))
  
  
  
  # Weight vs wing
  weight_wing[[y]] <- ggplot(data = dt %>% filter(year == y)) +
    geom_point(aes(y = weight, x = wing, colour = cohort_num)) +
    geom_smooth(aes(y = weight, x = wing, fill = cohort_num, colour = cohort_num)) +
    scale_color_brewer(type = "qual", palette = "Dark2") +
    scale_fill_brewer(type = "qual", palette = "Dark2") +
    labs(x = "Wing length (mm)", y = "Weight (g)", title = paste("Weight vs wing length:", y))
  
  
}

# This gives a plot comparing 2021 and 2022
biom_all <- (age_mass[[2021]] + age_wing[[2021]] + weight_wing[[2021]]) / (age_mass[[2022]] + age_wing[[2022]] + weight_wing[[2022]]) + plot_layout(guides= "collect")

ggsave(biom_all, 
       filename = paste0("all_biometrics_per_year_21-22_", today_date, ".png"), 
       device = "png", 
       path = outputwd, 
       width = 40, 
       height = 20,
       units = "cm")

# Plot 2021, 2022 and 2024 together
biom_all_3yr <- (age_mass[[2021]] + age_wing[[2021]] + weight_wing[[2021]]) / (age_mass[[2022]] + age_wing[[2022]] + weight_wing[[2022]]) / (age_mass[[2024]] + age_wing[[2024]] + weight_wing[[2024]]) + plot_layout(guides= "collect")

ggsave(biom_all_3yr, 
       filename = paste0("all_biometrics_per_year_21-22-24_", today_date, ".png"), 
       device = "png", 
       path = outputwd, 
       width = 40, 
       height = 20,
       units = "cm")

# Showing how bad 2023 was in comparison to the rest
biom_all_4yr <- (age_mass[[2021]] + age_wing[[2021]] + weight_wing[[2021]]) / (age_mass[[2022]] + age_wing[[2022]] + weight_wing[[2022]]) / (age_mass[[2023]] + age_wing[[2023]] + weight_wing[[2023]]) / (age_mass[[2024]] + age_wing[[2024]] + weight_wing[[2024]]) + plot_layout(guides= "collect")

ggsave(biom_all_4yr, 
       filename = paste0("all_biometrics_per_year_21-22-23-24_", today_date, ".png"), 
       device = "png", 
       path = outputwd, 
       width = 40, 
       height = 20,
       units = "cm")

# ggsave(paste0("age_weight_per_cohort_", today_date, ".png"), device = "png", path = outputwd, width = 30, height = 20, units = "cm")
# 
# ggsave(paste0("age_wing_per_cohort_", today_date, ".png"), device = "png", path = outputwd, width = 30, height = 20, units = "cm")
# 
# ggsave(paste0("weight_wing_per_cohort_", today_date, ".png"), device = "png", path = outputwd, width = 30, height = 20, units = "cm")


# =======================    Growth trajectories - 2021  =================

# # 2021 data from Pensthorpe
# 
# # ------  Exploratory growth trajectory plots & models  ----------
# 
# # Linear weight vs age, with cohorts, simple lm
# linear_age_mass <- ggplot(data = dt) +
#   geom_point(aes(y = weight, x = days_age, colour = cohort_num)) +
#   geom_smooth(aes(y = weight, x = days_age, fill = cohort_num, colour = cohort_num), method = "lm") +
#   scale_color_brewer(type = "qual", palette = "Dark2") +
#   scale_fill_brewer(type = "qual", palette = "Dark2") +
#   labs(x = "Age (days)", y = "Weight (g)", title = "Linear regression of weight vs age per cohort group")
# 
# ggsave(paste0("lm_age_weight_per_cohort_", today_date, ".png"), device = "png", path = outputwd, width = 30, height = 20, units = "cm")
# 
# # # Weight vs age, no cohort grouping
# # linear_age_mass <- ggplot(data = dt) +
# #   geom_point(aes(y = weight, x = days_age)) +
# #   geom_smooth(aes(y = weight, x = days_age), method = "lm") +
# #   labs(x = "Age (days)", y = "Weight (g)", title = "Linear regression of weight vs age overall")
# # ggsave(paste0("lm_age_weight_", today_date, ".png"), device = "png", path = outputwd, width = 30, height = 20, units = "cm")
# 
# # Model with cohort interaction
# mod_wt_age_coh <- lm(weight ~ days_age*cohort_num, data = dt)
# summary(mod_wt_age_coh)
# 
# # Model without cohort interaction
# mod_wt_age <- lm(weight ~ days_age, data = dt)
# summary(mod_wt_age)
# # Model forumula
# # weight = 222.797 + 6.357*days_age


# ------  Formal growth trajectory model for final report  ----------

# includes individual (ring number) as a random effect

year_list <- list(2021, 2022, 2024) # KMB: excluding 2023 due to feather growth issue

for (y in year_list) {
  
  current_year <- y
  not_zeroed <- c("8T","8U","8V")
  
  dt_sub <- dt %>% 
    filter(year == current_year) %>% 
    mutate(new_weight = ifelse((current_year == 2022 & cohort_num == 3 & site_code == "Pensthorpe" & flag_id %in% not_zeroed), weight - 56, weight))
  
  
  # -----  Weight vs age*cohort_num  -----
  
  # Model with cohort interaction and ring as a random effect. KMB 2024 update: had to include na.action = na.omit to make this work
  mod_wt_age_coh_re <- nlme::lme(new_weight ~ days_age*cohort_num,
                                 # random = ~ 1 + days_age|ring,
                                 random = ~ 1|ring,
                                 na.action = na.omit,
                                 data = dt_sub)
  
  # mod_wt_age_coh_re <- lme4::lmer(weight ~ days_age*cohort_num + (1|ring), data = dt_sub)
  summary(mod_wt_age_coh_re)
  mod_coef_table <- summary(mod_wt_age_coh_re)$tTable
  write.csv(mod_coef_table, file.path(outputwd, paste0("wt_age_coh_mod_coef_", current_year,  ".csv")), row.names = TRUE)
  
  broom.mixed::tidy(mod_wt_age_coh_re) %>% filter(effect == "fixed") %>% dplyr::select(term:p.value)
  broom.mixed::glance(mod_wt_age_coh_re)
  
  # Use ggeffects package to get predicted fits to plot nicely
  mod_wt_re_pred <- ggeffects::ggpredict(mod_wt_age_coh_re,
                                         terms = c("days_age","cohort_num"))
  plot_wt_mod_re_fits <- plot(mod_wt_re_pred, colors = "set1", show_data = TRUE) +
    labs(
      x = "Age (days)", 
      y = "Weight (g)",
      title = current_year,
      colour = "Cohort number"
    ) # update to code 2024 to show_data instead of add.data following warning of deprecation.
  
  png(file.path(outputwd, paste0("lme_age_weight_per_cohort_", current_year, "_", today_date, ".png")),
      width = 30, height = 20, units = "cm", res = 150)
  print(plot_wt_mod_re_fits)
  dev.off()
  
  
  # -----  Wing vs age*cohort_num  -----
  
  # Model with cohort interaction and ring as a random effect
  mod_wing_age_coh_re <- nlme::lme(wing ~ days_age*cohort_num,
                                   # random = ~ 1 + days_age|ring,
                                   random = ~ 1|ring,
                                   data = dt_sub, # need [c(1:99,101:103,106:107),] for 2024 cohort??
                                   na.action = na.omit)
  summary(mod_wing_age_coh_re)
  
  mod_coef_table <- summary(mod_wing_age_coh_re)$tTable
  write.csv(mod_coef_table, file.path(outputwd, paste0("wing_age_coh_mod_coef_", current_year, ".csv")), row.names = TRUE)
  
  
  # Use ggeffects package to get predicted fits to plot nicely
  mod_wing_re_pred <- ggeffects::ggpredict(mod_wing_age_coh_re,
                                           terms = c("days_age","cohort_num"))
  plot_wing_mod_re_fits <- plot(mod_wing_re_pred, colors = "set1", show_data = TRUE) +
    labs(
      x = "Age (days)", 
      y = "Wing length (mm)",
      title = current_year,
      colour = "Cohort number"
    ) # update to code 2024 to show_data instead of add.data following warning of deprecation.
  
  png(file.path(outputwd, paste0("lme_age_wing_per_cohort_", current_year, "_", today_date, ".png")),
      width = 30, height = 20, units = "cm", res = 150)
  print(plot_wing_mod_re_fits)
  dev.off()
  
  
  all_biometrics_plot <- plot_wt_mod_re_fits + plot_wing_mod_re_fits + 
    plot_layout(guides = "collect") +
    plot_annotation(
      title = "Predicted (a) weight and (b) wing length relative to age for different release cohorts",
      tag_levels = "a")
  
  png(file.path(outputwd, paste0("lme_age_biometrics_per_cohort_", current_year, "_", today_date, ".png")),
      width = 30, height = 12, units = "cm", res = 150)
  print(all_biometrics_plot)
  dev.off()
  
  
  
  
  # -----  Weight vs age  -----
  
  # Model with cohort interaction and ring as a random effect
  mod_wt_age_re <- nlme::lme(weight ~ days_age,
                             # random = ~ 1 + days_age|ring,
                             random = ~ 1|ring,
                             na.action = na.omit,
                             data = dt_sub)
  summary(mod_wt_age_re)
  mod_coef_table <- summary(mod_wt_age_re)$tTable
  write.csv(mod_coef_table, file.path(outputwd, paste0("wt_age_mod_coef_", current_year, ".csv")), row.names = TRUE)
  
  broom.mixed::tidy(mod_wt_age_re) %>% filter(effect == "fixed") %>% dplyr::select(term:p.value)
  broom.mixed::glance(mod_wt_age_coh_re)
  
  # Use ggeffects package to get predicted fits to plot nicely
  mod_wt_re_pred <- ggeffects::ggpredict(mod_wt_age_re,
                                         terms = c("days_age"))
  plot_wt_mod_re_fits <- plot(mod_wt_re_pred,  show_data = TRUE) +
    labs(
      x = "Age (days)", 
      y = "Weight (g)",
      title = current_year
    ) # update to code 2024 to show_data instead of add.data following warning of deprecation.
  
  png(file.path(outputwd, paste0("lme_age_weight_mean_", current_year, "_",  today_date, ".png")),
      width = 30, height = 20, units = "cm", res = 150)
  print(plot_wt_mod_re_fits)
  dev.off()
  
}

# =======================    Growth trajectories - weight at release predictions - 2021   =================

# KMB - Not possible to run in 2024 as missing some models from above

predict_weight <- dt %>% 
  filter(tag_gps_radio_none == "gps") %>% 
  dplyr::select(flag_id, age_released, cohort_num) %>% 
  unique %>% 
  as.data.frame

names(predict_weight) <- c("flag_id", "days_age", "cohort_num")

last_tag_weights <- dt %>% 
  filter(tag_gps_radio_none == "gps") %>% 
  dplyr::select(flag_id, date, days_age, age_released, weight, cohort_num) %>% 
  group_by(flag_id) %>%
  slice(which.max(date)) %>% 
  as.data.frame

# coefficients and se for both models
mod_coefs_coh <- summary(mod_wt_age_coh)$coefficients

all_mod_coefs <- data.frame(cohort_num = c(1,2,3,4,5), 
                            coefs_coh = mod_coefs_coh[grepl("days_age", rownames(mod_coefs_coh)), "Estimate"],
                            se_coh = mod_coefs_coh[grepl("days_age", rownames(mod_coefs_coh)), "Std. Error"]
                            # coefs = mod_wt_age_coh$coefficients[grepl("days_age", names(mod_wt_age_coh$coefficients))]
) %>% 
  mutate(slopes_coh = ifelse(cohort_num == 1, coefs_coh, coefs_coh[1] + coefs_coh))

all_mod_coefs <- all_mod_coefs %>% 
  mutate(slopes = summary(mod_wt_age)$coefficients["days_age","Estimate"]) %>% 
  mutate(se = summary(mod_wt_age)$coefficients["days_age","Std. Error"])

# merge with weights at tagging
release_weights <- merge(last_tag_weights, all_mod_coefs, by = "cohort_num", all.x = TRUE)

release_weights <- release_weights %>% 
  mutate(weight_released = weight + (slopes*(age_released-days_age))) %>% 
  mutate(weight_released_coh = weight + (slopes_coh*(age_released-days_age))) %>% 
  mutate(weight_released_uci = weight_released + 1.96*se) %>% 
  mutate(weight_released_lci = weight_released - 1.96*se) %>% 
  mutate(weight_released_coh_uci = weight_released_coh + 1.96*se_coh) %>% 
  mutate(weight_released_coh_lci = weight_released_coh - 1.96*se_coh)

# Weight vs age, no cohort grouping, with release weights
release_weights_plot <- ggplot(data = dt) +
  geom_point(aes(y = weight, x = days_age), col = "grey50") +
  geom_smooth(aes(y = weight, x = days_age), method = "lm", col = "black") +
  geom_point(aes(y = weight, x = days_age), col = "red", size = 3, data = release_weights) +
  geom_text(aes(y = weight, x = days_age, label = flag_id),
            data = weight_gain,
            col = "red",
            size = 3,
            nudge_x = 0.5,
            nudge_y = -10) +
  geom_point(aes(y = weight, x = days_age), col = "blue",
             size = 3, 
             data = release_weights %>% dplyr::select(-days_age, -weight) %>% rename(days_age = age_released, weight = weight_released)) +
  # geom_errorbar(aes(x = age_released, y = weight_released, ymin = weight_released_lci, ymax = weight_released_uci),
  #               data = weight_gain, 
  #               col = "magenta"
  #               ) +
  geom_text(aes(y = weight, x = days_age, label = flag_id),
            data = release_weights %>% dplyr::select(-days_age, -weight) %>% rename(days_age = age_released, weight = weight_released),
            col = "blue",
            size = 3,
            nudge_x = 0.5,
            nudge_y = -10) +
  geom_hline(yintercept = 17.7/0.03, linetype = "dotted") +
  geom_hline(yintercept = 17.5/0.03, linetype = "dotted") +
  labs(x = "Age (days)", 
       y = "Weight (g)", 
       title = "Linear regression of weight vs age showing tagged birds", 
       subtitle = paste("Red = tagging weight; blue = estimated release weight", "Dotted lines show 3% weight threshold for 17.5g and 17.7g tag deployments", sep="\n"))

ggsave(paste0("lm_age_weight_released_", today_date, ".png"), device = "png", path = outputwd, width = 30, height = 20, units = "cm")

# # =======================    Figures - 2019 WWT birds  =================
# 
# 
# # Plot age vs weight
# curve_age_mass <- ggplot(data = dt) +
#   geom_point(aes(y = last_mass, x = age_last_mass)) +
#   geom_smooth(aes(y = last_mass, x = age_last_mass)) +
#   labs(x = "Age at last mass", y = "Lass mass (g)", title = "Curve of last mass vs age at last mass")
# 
# age_violin <- ggplot() +
#   geom_violin(data = dt,
#               aes(y = age_released,
#                   x = "Released")) +
#   geom_jitter(data = dt,
#               aes(y = age_released,
#                   x = "Released"),
#               height = 0, width = 0.1) +
#   geom_violin(data = dt,
#               aes(y = age_last_mass,
#                   x = "Last mass")) +
#   geom_jitter(data = dt,
#               aes(y = age_last_mass,
#                   x = "Last mass"),
#               height = 0, width = 0.1) +
#   labs(x = "", y = "Age", title = "Age of all birds when mass last measured, and on release")
# 
# mass_violin <- ggplot(data = dt %>% filter(age_last_mass >= 43),
#                       aes(y = last_mass,
#                           x = "")) +
#   geom_violin() +
#   geom_jitter(height = 0, width = 0.1) +
#   theme(axis.ticks = element_blank()) +
#   labs(x = "", y = "Last mass (g)", title = "Mass of all birds measured when >= 43 days old\n(minimum age when birds released)")
# 
# # Put plots together and save
# ((curve_age_mass / age_violin) | mass_violin) +
#   plot_annotation(tag_levels = 'A')
# 
# ggsave("age_mass_plots.png", device = "png", path = outputwd, width = 30, height = 20, units = "cm")
# 
# # Calculate descriptive stats
# dt %>% filter(age_last_mass >= 43) %>% summarise(mean(last_mass))
# dt %>% filter(age_last_mass >= 43) %>% summarise(range(last_mass))
# dt %>% summarise(range(age_last_mass))



