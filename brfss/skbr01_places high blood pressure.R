rm(list=ls());gc();source(".Rprofile")

# https://data.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-County-Data-20/swc5-untb/about_data

# https://stackoverflow.com/questions/2617600/importing-data-from-a-json-file-into-r
# library(rjson)
# json_data = fromJSON("https://data.cdc.gov/resource/swc5-untb.json")

# https://dev.socrata.com/foundry/data.cdc.gov/swc5-untb#:~:text=The%20City%20of%20Chicago%20and%20community%20maintains%20a%20great%20RSocrata%20package%20on%20Github.
# library(RSocrata)
# df <- read.socrata(
#   "https://data.cdc.gov/resource/swc5-untb.json")



# https://dev.socrata.com/docs/datatypes/text.html#,
# https://support.socrata.com/hc/en-us/articles/202949268-How-to-query-more-than-1000-rows-of-a-dataset
json_data_bphigh = fromJSON(file = "https://data.cdc.gov/resource/swc5-untb.json?$limit=50000&measureid=BPHIGH") 
json_data_obesity = fromJSON(file = "https://data.cdc.gov/resource/swc5-untb.json?$limit=50000&measureid=OBESITY") 

bphigh = map_dfr(json_data_bphigh,
                    function(l){
                      type = pluck(l$geolocation$type)
                      lon = pluck(l$geolocation$coordinates[1])
                      lat = pluck(l$geolocation$coordinates[2])
                      
                      l$geolocation = NULL;
                      
                      df = data.frame(
                        l) %>% 
                        mutate(
                          type = type,
                          lon = lon,
                          lat = lat
                        )
                      
                      return(df)
                      
                      
                    })

obesity = map_dfr(json_data_obesity,
                 function(l){
                   type = pluck(l$geolocation$type)
                   lon = pluck(l$geolocation$coordinates[1])
                   lat = pluck(l$geolocation$coordinates[2])
                   
                   l$geolocation = NULL;
                   
                   df = data.frame(
                     l) %>% 
                     mutate(
                       type = type,
                       lon = lon,
                       lat = lat
                     )
                   
                   return(df)
                   
                   
                 })


bind_rows(bphigh,
          obesity) %>% 
  saveRDS(.,paste0(path_spatial_kiosks_folder,"/working/cleaned/skbr01_high blood pressure and obesity from cdc places.RDS"))
         