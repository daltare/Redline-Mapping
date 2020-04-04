library(dplyr)
library(readr)
library(jsonlite)
library(tidyr)
library(janitor)
library(readxl)
library(writexl)

# Links
    url_fresno <- 'https://dsl.richmond.edu/panorama/redlining/static/downloads/geojson/CAFresno1936.geojson'
    url_losangeles <- 'https://dsl.richmond.edu/panorama/redlining/static/downloads/geojson/CALosAngeles1939.geojson'
    url_oakland <- 'https://dsl.richmond.edu/panorama/redlining/static/downloads/geojson/CAOakland1937.geojson'
    url_sacramento <- 'https://dsl.richmond.edu/panorama/redlining/static/downloads/geojson/CASacramento1937.geojson'
    url_sandiego <- 'https://dsl.richmond.edu/panorama/redlining/static/downloads/geojson/CASanDiego1938.geojson'
    url_sanfrancisco <- 'https://dsl.richmond.edu/panorama/redlining/static/downloads/geojson/CASanFrancisco1937.geojson'
    url_sanjose <- 'https://dsl.richmond.edu/panorama/redlining/static/downloads/geojson/CASanJose1937.geojson'
    url_stockton <- 'https://dsl.richmond.edu/panorama/redlining/static/downloads/geojson/CAStockton1938.geojson'

# Convert geojson to dataframe
    geojson_raw <- readr::read_lines(url_losangeles)
    geojson_list <- jsonlite::fromJSON(geojson_raw)
    geojson_result <- geojson_list$features
    
    df_type <- geojson_result$type
    df_geometry <- geojson_result$geometry
    df_properties <- geojson_result$properties %>% select(-area_description_data)
    df_properties_2 <- geojson_result$properties$area_description_data
    df_properties_3 <- bind_cols(df_properties, df_properties_2)
        # dput(sort(names(df_properties_3)))
    df_properties_3 <- df_properties_3 %>% 
        select(c("0", "holc_id", "name", "holc_grade", 
                 "1a", "1b", "1c", "1d", "1e", "2", "2a", "2b", "2c", 
                 "2d", "2e", "2f", "2g", "2h", "2i", "2j", "2k", "2l", "2m", "2n", 
                 "2o", "2p", "3", "4a", "4b", 
                 "5", "5a", "5b", "6", "71", "72", 
                 "8", "9", "10", "V1"))
    
    # names(df_properties_3)
    
    # sort rows
    df_properties_3 <- df_properties_3 %>% 
        separate(col = holc_id, 
                 into = c('id_grade', 'id_numb'), 
                 sep = "(?<=[A-Z])(?=[0-9])", 
                 remove = FALSE) %>% 
        mutate(id_numb = as.numeric(id_numb)) %>% 
        arrange(id_grade, id_numb) %>% 
        select(-c('id_grade', 'id_numb')) %>% 
        select(-c('name', 'V1')) %>% 
        rename('region' = '0') %>% 
        clean_names()
    names(df_properties_3) <- gsub(pattern = 'x', replacement = 'q_', x = names(df_properties_3))
        
    
    write_csv(x = df_properties_3, path = 'area-descriptions/redline_los-angeles_area-descriptions.csv')
    write_xlsx(x = df_properties_3, path = 'area-descriptions/redline_los-angeles_area-descriptions.xlsx')
    
    # https://dsl.richmond.edu/panorama/redlining/#loc=9/34.005/-118.823&city=los-angeles-ca&area=A2&adview=full&adimage=3/76.619/-139.219
    # https://dsl.richmond.edu/panorama/redlining/#loc=9/34.005/-118.823&city=los-angeles-ca&area=A3&adview=full&adimage=3/76.619/-139.219
    

