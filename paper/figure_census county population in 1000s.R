gc();rm(list=  ls()); source(".Rprofile");
library(maps)


census_pop = read_csv(paste0(path_spatial_kiosks_folder,"/working/raw/DECENNIALDP2020.DP1_2024-07-17T144718/DECENNIALDP2020.DP1-Data.csv")) %>% 
  dplyr::select(GEO_ID, NAME, DP1_0021C) %>% 
  dplyr::filter(GEO_ID != "Geography") %>% 
  mutate(fips = str_extract(GEO_ID,"[0-9]{5}$")) %>% 
  mutate(state = str_extract(NAME,"[A-Za-z\\s]+$") %>% str_trim()) %>% 
  mutate(county = str_replace(NAME,paste0(", ",state),"")) %>%
  mutate(pop_1000s = as.numeric(DP1_0021C)/1000) %>% 
  mutate(data_value_groups = case_when(pop_1000s < 50 ~ 1,
                                       pop_1000s < 1000 ~ 2,
                                       pop_1000s < 8000 ~ 3,
                                       TRUE ~ NA_real_)) %>% 
  mutate(data_value_groups=factor(data_value_groups,levels=c(1:3),
                                  labels=c("0-49.9", "50-999.9","1000-8000")))

# Read counties shapefile
# county_boundaries <- st_read(dsn = paste0(path_cms_mdpp_folder,"/working/tl_2022_us_county"))
county_boundaries <- tigris::counties(class="sf",cb=TRUE) %>%
  left_join(census_pop %>% 
              dplyr::select(fips,pop_1000s,data_value_groups),
            by=c("GEOID"="fips")) %>% 
  tigris::shift_geometry()  %>% 
  dplyr::filter(STATEFP < 60) 

state_boundaries <- tigris::states(class = "sf", cb = TRUE) %>% 
  tigris::shift_geometry() %>% 
  dplyr::filter(GEOID < 60) 

figA_pop_1000s <- ggplot() +
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
  scale_fill_manual(name="",values=c("0-49.9" = "#027324","50-999.9" ="#56B4E9","1000-8000" = "#FF6961"),na.value="grey90") +
  theme(legend.position = "bottom",
        title = element_text(size = 14),
        legend.text = element_text(size = 14))

figA_pop_1000s %>% 
  ggsave(.,filename=paste0(path_spatial_kiosks_folder,"/figures/census adult population in 1000s.jpg"),width=6,height=4)
