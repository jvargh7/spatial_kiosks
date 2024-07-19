gc();rm(list=  ls()); source(".Rprofile");
library(maps)

highbp = readRDS(paste0(path_spatial_kiosks_folder,"/working/cleaned/skbr01_high blood pressure and obesity from cdc places.RDS")) %>% 
  dplyr::filter(measureid == "BPHIGH",datavaluetypeid == "CrdPrv") %>% 
  dplyr::select(year,locationid,data_value) %>% 
  mutate(data_value = as.numeric(data_value)) %>% 
  mutate(data_value_groups = case_when(data_value < 20 ~ 1,
                                       data_value < 40 ~ 2,
                                       data_value < 60 ~ 3,
                                       data_value < 80 ~ 4,
                                       data_value <=100 ~ 5,
                                       TRUE ~ NA_real_)) %>% 
  mutate(data_value_groups=factor(data_value_groups,levels=c(1:5),
                                  labels=c("0 to <20","20 to <40","40 to <60","60 to <80","80-100")))


# Read counties shapefile
# county_boundaries <- st_read(dsn = paste0(path_cms_mdpp_folder,"/working/tl_2022_us_county"))
county_boundaries <- tigris::counties(class="sf",cb=TRUE) %>%
  left_join(highbp %>% 
              dplyr::select(locationid,data_value,data_value_groups),
            by=c("GEOID"="locationid")) %>% 
  tigris::shift_geometry()  %>% 
  dplyr::filter(STATEFP < 60) 

state_boundaries <- tigris::states(class = "sf", cb = TRUE) %>% 
  tigris::shift_geometry() %>% 
  dplyr::filter(GEOID < 60) 

figA_highbp <- ggplot() +
  geom_sf(data=county_boundaries,aes(fill = data_value_groups),col=NA)  +
  geom_sf(data=state_boundaries,col="black",fill=NA)  +
  # https://stackoverflow.com/questions/66031935/ggplot2-and-sf-geom-sf-text-within-limits-set-by-coord-sf
  # coord_sf(xlim = c(-140,-60), ylim = c(22,50)) +
  coord_sf(crs = 5070, datum = NA) +
  theme_bw() +
  # ggtitle("BRFSS High Blood Pressure (%), 2021 (Crude)") +
  xlab("") +
  ylab("") +
  # scale_fill_gradient(name="",low = "lightblue",high="darkblue",na.value="grey90",limits=c(0,100)) +
  scale_fill_manual(name="",values=c("0 to <20" = "#027324","20 to <40" = "#449050","40 to <60" ="#56B4E9","60 to <80"="#E69F00","80-100" = "#FF6961"),na.value="grey90") +
  theme(legend.position = "bottom",
        title = element_text(size = 14),
        legend.text = element_text(size = 14))

figA_highbp %>% 
  ggsave(.,filename=paste0(path_spatial_kiosks_folder,"/figures/brfss high blood pressure.jpg"),width=6,height=4)
