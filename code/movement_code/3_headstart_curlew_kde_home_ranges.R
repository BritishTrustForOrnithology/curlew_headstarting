# Home range KDE maps different seasons


bird_flag_list <- unique(all_tags_filtered$year_flag)
bird_flag_list

dt_study_month <- dt_study_month %>% 
  rename(gps_loc_year = year)

all_tags_filtered <- all_tags_filtered %>% 
  mutate(month_name = month(timestamp, label=TRUE, abbr=TRUE),
         month_num = month(timestamp),
         gps_loc_year = year(timestamp)) %>% 
  left_join(dt_study_month, by = c("gps_loc_year", "month_num"))

temp <- all_tags_filtered %>% 
  filter(year_flag %in% c("22_9J"))

temp %>% group_by(study_month) %>% tally() %>% print(n=nrow(.))

x_names <- temp %>% as.data.frame %>% dplyr::select(gps_loc_year, study_month, month_name, month_num) %>% unique


temp_list <- temp %>% 
  group_split(study_month)


maps_study_month <- lapply(temp_list, function(x){
    
  draw_movement_map_to_object(x, map_colour="magenta", basemap_alpha = 1,  map_buffer_km = 10)
    }
)


x <- paste(x_names$gps_loc_year, x_names$month_name)
names(maps_study_month) <- x

saveRDS(maps_study_month, file.path(workspacewd, "9J_maps_by_study_month.rds"))

library(patchwork)
patch_maps <- wrap_plots(maps_study_month, ncol = 6) +
  plot_annotation(tag_levels = list(names(maps_study_month)),
                  theme = theme(plot.title = element_text(size = 12))
  )


ggsave(file.path(outputwd, "maps_9J_by_month.jpg"),
       width = 16, height = 12, dpi = 300)






# transform data from epsg 4326 (move2 object default) to epsg 3857 (units in metres)
# units in metres easier for adding buffer to bounding box
# 3857 is projected CRS for rendering tile mapping
bird_df_sf_3857 <- st_transform(temp, crs = 3857)

bird_df_sf_3857 <- bird_df_sf_3857 %>% 
  mutate(lon_3857 = st_coordinates(bird_df_sf_3857)[,1],
         lat_3857 = st_coordinates(bird_df_sf_3857)[,2])

bird_df_sf_3857 <- group_by(bird_df_sf_3857, month_num)

skde1 <- eks::st_kde(bird_df_sf_3857)


map_buffer_km <- 30
basemap_alpha <- 0.8

# create basemaps for main & inset maps
basemap_main <- make_basemap(sf_data = bird_df_sf_3857,
                             buff_dist = map_buffer_km*1000,
                             map_type = "main",
                             map_provider = "Esri.WorldImagery",
                             alpha_level = basemap_alpha)


basemap_main$map + geom_sf(data=st_get_contour(skde1), aes(fill=contperc)) + facet_wrap(~month) +
  coord_sf(xlim=xlims, ylim=ylims) + scale_fill_viridis_d()


# ----- Map aesthetics  -------

xlims <- c(st_bbox(basemap_main$geom)[1], st_bbox(basemap_main$geom)[3])
ylims <- c(st_bbox(basemap_main$geom)[2], st_bbox(basemap_main$geom)[4])



# Main map

# Path + terminus point coloured with white outline
gg_main_map <- basemap_main$map +
  geom_path(data = bird_df_sf_3857, aes(x = lon_3857, y = lat_3857), col = map_colour, linewidth = 0.5, alpha = path_alpha) +
  geom_sf(data = bird_df_sf_3857 %>% slice(which.max(timestamp)), shape=21, size = 3, stroke = 0.8, col="white", fill=map_colour) +
  coord_sf(xlim = xlims, ylim = ylims) +
  theme_void()
# gg_main_map






# faceted
gs + geom_sf(data=st_get_contour(skde1g), aes(fill=contperc)) + 
  facet_wrap(~name) + coord_sf(xlim=xlim, ylim=ylim) 


#Set up KDE 
udbis_testbird <- kernelUD((as(testbird_sf_utm30, 'Spatial'))[ , 52], h="href", grid=500) # convert to spdf
image(udbis_testbird) #simple image to check it has worked. Should be a yellow fuzzy point over red.

ver_testbird <- getverticeshr(udbis_testbird, standardize = FALSE, unin = 'm', unout='m2') # initial set up
ver50_testbird <- getverticeshr(udbis_testbird, percent=50, unin='m', unout='m2')  # at 50% level
ver95_testbird <- getverticeshr(udbis_testbird, percent=95, unin='m', unout='m2')  # at 95% level

colours <- c("KDE95" = "grey87", "KDE50" = "grey47")
ggplot() + 
  geom_polygon(data = ver95_testbird, aes(long, lat, group = group, fill="KDE95"), col="black") +
  geom_polygon(data = ver50_testbird, aes(long, lat, group = group, fill="KDE50"), col="black") + 
  geom_sf(data=(testbird_sf_utm30), pch=".") + theme_classic() +
  scale_fill_manual(name="KDE percs", values = colours) +
  labs(title="KDE for Yorkshire Curlew - all breeding status locations", fill="legend") +
  theme(legend.position = "bottom")