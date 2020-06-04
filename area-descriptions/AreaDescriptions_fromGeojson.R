library(dplyr)
library(readr)
library(jsonlite)
library(tidyr)
library(janitor)
library(readxl)
library(writexl)
library(geojsonsf)

# Links ----
    # url_fresno <- 'https://dsl.richmond.edu/panorama/redlining/static/downloads/geojson/CAFresno1936.geojson'
    url_losangeles <- 'https://dsl.richmond.edu/panorama/redlining/static/downloads/geojson/CALosAngeles1939.geojson'
    url_oakland <- 'https://dsl.richmond.edu/panorama/redlining/static/downloads/geojson/CAOakland1937.geojson'
    url_sacramento <- 'https://dsl.richmond.edu/panorama/redlining/static/downloads/geojson/CASacramento1937.geojson'
    # url_sandiego <- 'https://dsl.richmond.edu/panorama/redlining/static/downloads/geojson/CASanDiego1938.geojson'
    # url_sanfrancisco <- 'https://dsl.richmond.edu/panorama/redlining/static/downloads/geojson/CASanFrancisco1937.geojson'
    url_sanjose <- 'https://dsl.richmond.edu/panorama/redlining/static/downloads/geojson/CASanJose1937.geojson'
    url_stockton <- 'https://dsl.richmond.edu/panorama/redlining/static/downloads/geojson/CAStockton1938.geojson'

# LOS ANGELES -- Convert geojson to dataframe ----
    # geojson_raw_la <- readr::read_lines(url_losangeles)
    geojson_sf_la <- geojson_raw_la %>% geojson_sf()
    # geojson_list_la <- jsonlite::fromJSON(geojson_raw_la)
    # geojson_result_la <- geojson_list_la$features
    geojson_la <- (readr::read_lines(url_losangeles) %>% jsonlite::fromJSON())$features
    
    # df_type_la <- geojson_result_la$type
    # df_geometry_la <- geojson_result_la$geometry
    # df_properties_la <- geojson_result_la$properties %>% select(-area_description_data)
    # df_properties_la_2 <- geojson_result_la$properties$area_description_data
    # df_description_la <- bind_cols(df_properties_la, df_properties_la_2)
    df_description_la <- bind_cols(geojson_la$properties %>% select(-area_description_data), 
                                   geojson_la$properties$area_description_data)
    
    
        # dput(sort(names(df_description_la)))
    df_description_la <- df_description_la %>% 
        mutate(city = 'Los Angeles') %>% 
        select(c('city',
                 "0", "holc_id", "name", "holc_grade", 
                 "1a", "1b", "1c", "1d", "1e", "2", "2a", "2b", "2c", 
                 "2d", "2e", "2f", "2g", "2h", "2i", "2j", "2k", "2l", "2m", "2n", 
                 "2o", "2p", "3", "4a", "4b", 
                 "5", "5a", "5b", "6", "71", "72", 
                 "8", "9", "10", "V1"))
    # sort rows
    df_description_la <- df_description_la %>% 
        separate(col = holc_id, 
                 into = c('id_grade', 'id_numb'), 
                 sep = "(?<=[A-Z])(?=[0-9])", 
                 remove = FALSE) %>% 
        mutate(id_numb = as.numeric(id_numb)) %>% 
        arrange(id_grade, id_numb) %>% 
        select(-c('id_grade', 'id_numb')) %>% 
        select(-c('name', 'V1')) %>% 
        rename('holc_region' = '0',
               'inhabitants_pop_trend' = '1a',
               'inhabitants_occupation' = '1b',
               # 'inhabitants_income' = '',
               'inhabitants_foreign_pct_predominating' = '1c',
               'inhabitants_negro' = '1d',
               'inhabitants_infiltration' = '1e',
               'buildings_predominating_pct' = '2',
               'buildings_type' = '2a',
               'buildings_construction' = '2b',
               'buildings_avg_age' = '2c',
               'buildings_repair' = '2d',
               'buildings_occupancy_pct' = '2e',
               'buildings_ownership_pct' = '2f',
               'buildings_1935_price_range_pct' = '2g',
               'buildings_1937_price_range_pct' = '2h',
               'buildings_1939_price_range_pct' = '2i',
               'buildings_sales_demand' = '2j',
               'buildings_price_trend' = '2k',
               'buildings_1935_rent_range_pct' = '2l',
               'buildings_1937_rent_range_pct' = '2m',
               'buildings_1939_rent_range_pct' = '2n',
               'buildings_rental_demand' = '2o',
               'buildings_rental_trend' = '2p',
               'buildings_constructed_past_yr' = '3',
               'buildings_overhang_holc' = '4a',
               'buildings_overhang_institutions' = '4b',
               'buildings_sales_yrs' = '5',
               'buildings_sales_holc' = '5a',
               'buildings_sales_institutions' = '5b',
               'mortgage_avial' = '6',
               'tax_rate_yr' = '71',
               'tax_rate_per_1000' = '72',
               'area_description' = '8',
               # 'area_terrain' = '1a',
               # 'area_favorable_influences' = '1b',
               # 'area_detrimental_influences' = '1c',
               # 'area_pct_improved' = '1d',
               # 'area_desirability_trend' = '1e',               
               'name_location_grade_number' = '9',
               'note' = '10') %>% 
        clean_names()
    # names(df_description_la) <- gsub(pattern = 'x', replacement = 'q_', x = names(df_description_la))
        
    z <- bind_cols(df_description_la, df_geometry_la)
    View(z)
    
    write_csv(x = df_description_la, path = 'area-descriptions/redline_los-angeles_area-descriptions.csv')
    write_xlsx(x = df_description_la, path = 'area-descriptions/redline_los-angeles_area-descriptions.xlsx')
    
    # https://dsl.richmond.edu/panorama/redlining/#loc=9/34.005/-118.823&city=los-angeles-ca&area=A2&adview=full&adimage=3/76.619/-139.219
    # https://dsl.richmond.edu/panorama/redlining/#loc=9/34.005/-118.823&city=los-angeles-ca&area=A3&adview=full&adimage=3/76.619/-139.219

    'https://dsl.richmond.edu/panorama/redlining/#loc=//&city=los-angeles-ca&area=A3&adview=full&adimage=3/1/-1'
    
# SACRAMENTO -- Convert geojson to dataframe ----
    geojson_sac <- (readr::read_lines(url_sacramento) %>% jsonlite::fromJSON())$features
    df_description_sac <- bind_cols(geojson_sac$properties %>% select(-area_description_data), 
                                   geojson_sac$properties$area_description_data)
        # dput(sort(names(df_description_sac)))
    df_description_sac <- df_description_sac %>% 
        mutate(city = 'Sacramento') %>% 
        select(c('city', 
                 "0", "holc_id", "name", "holc_grade", 
                 "1a", "1b", "1c", "1d", "1e", 
                 "2a", "2b", "2c", "2d", "2e", "2f", "2g", 
                 "31", "32", "33", 
                 "3a", "3b", "3c", "3d", "3e", "3f", "3g", 
                 "3h", "3i", "3j", "3k", "3l", "3m", "3n", 
                 "3o", "3p", "3q", 
                 "4a", "4b", "5", "6"))
    # sort rows
    df_description_sac <- df_description_sac %>% 
        separate(col = holc_id, 
                 into = c('id_grade', 'id_numb'), 
                 sep = "(?<=[A-Z])(?=[0-9])", 
                 remove = FALSE) %>% 
        mutate(id_numb = as.numeric(id_numb)) %>% 
        arrange(id_grade, id_numb) %>% 
        select(-c('id_grade', 'id_numb')) %>% 
        rename('holc_region' = '0',
               'area_terrain' = '1a',
               'area_favorable_influences' = '1b',
               'area_detrimental_influences' = '1c',
               'area_pct_improved' = '1d',
               'area_desirability_trend' = '1e',
               'inhabitants_occupation' = '2a',
               'inhabitants_income' = '2b',
               'inhabitants_foreign_pct_predominating' = '2c',
               'inhabitants_negro' = '2d',
               'inhabitants_infiltration' = '2e',
               'inhabitants_relief' = '2f',
               'inhabitants_pop_trend' = '2g',
               'buildings_predominating_pct' = '31',
               'buildings_other1_pct' = '32',
               'buildings_other2_pct' = '33',
               'buildings_type' = '3a',
               'buildings_construction' = '3b',
               'buildings_avg_age' = '3c',
               'buildings_repair' = '3d',
               'buildings_occupancy_pct' = '3e',
               'buildings_ownership_pct' = '3f',
               'buildings_constructed_past_yr' = '3g',
               'buildings_1929_price_range_pct' = '3h',
               'buildings_1935_price_range_pct' = '3i',
               'buildings_1938_price_range_pct' = '3j',
               'buildings_sales_demand' = '3k',
               'buildings_sales_activity' = '3l',
               'buildings_1929_rent_range_pct' = '3m',
               'buildings_1935_rent_range_pct' = '3n',
               'buildings_1938_rent_range_pct' = '3o',
               'buildings_rental_demand' = '3p',
               'buildings_rental_activity' = '3q',
               'mortgage_avial_home_purchase' = '4a',
               'mortgage_avial_home_building' = '4b',
               'clarifying_remarks' = '5',
               'name_location_grade_number' = '6') %>% 
        clean_names()
    write_csv(x = df_description_sac, path = 'area-descriptions/redline_sacramento_area-descriptions.csv')
    write_xlsx(x = df_description_sac, path = 'area-descriptions/redline_sacramento_area-descriptions.xlsx')
    

# STOCKTON -- Convert geojson to dataframe ----
    geojson_stk <- (readr::read_lines(url_stockton) %>% jsonlite::fromJSON())$features
    df_description_stk <- bind_cols(geojson_stk$properties %>% select(-area_description_data), 
                                    geojson_stk$properties$area_description_data)
        # dput(sort(names(df_properties_3)))
    df_description_stk <- df_description_stk %>% 
        mutate(city = 'Stockton') %>% 
        select(c('city', #"0", 
                 "holc_id", "name", "holc_grade", 
                 "1a", "1b", "1c", "1d", "1e", 
                 '2 ',
                 "2a", "2b", "2c", "2d", "2e", "2f", "2g", 
                 "31", "32", "33", 
                 "3a", "3b", "3c", "3d", "3e", "3f", "3g", 
                 "3h", "3i", "3j", "3k", "3l", "3m", "3n", 
                 "3o", "3p", "3q", 
                 "4a", "4b", "5", "6")) %>% 
        mutate('2b' = case_when(!is.na(`2 `) ~ `2 `,
                                TRUE ~ `2b`)) %>% 
        select(-c(`2 `))
    # sort rows
    df_description_stk <- df_description_stk %>% 
        separate(col = holc_id, 
                 into = c('id_grade', 'id_numb'), 
                 sep = "(?<=[A-Z])(?=[0-9])", 
                 remove = FALSE) %>% 
        mutate(id_numb = as.numeric(id_numb)) %>% 
        arrange(id_grade, id_numb) %>% 
        select(-c('id_grade', 'id_numb')) %>% 
        rename(# 'holc_region' = '0',
               'area_terrain' = '1a',
               'area_favorable_influences' = '1b',
               'area_detrimental_influences' = '1c',
               'area_pct_improved' = '1d',
               'area_desirability_trend' = '1e',
               'inhabitants_occupation' = '2a',
               'inhabitants_income' = '2b',
               'inhabitants_foreign_pct_predominating' = '2c',
               'inhabitants_negro' = '2d',
               'inhabitants_infiltration' = '2e',
               'inhabitants_relief' = '2f',
               'inhabitants_pop_trend' = '2g',
               'buildings_predominating_pct' = '31',
               'buildings_other1_pct' = '32',
               'buildings_other2_pct' = '33',
               'buildings_type' = '3a',
               'buildings_construction' = '3b',
               'buildings_avg_age' = '3c',
               'buildings_repair' = '3d',
               'buildings_occupancy_pct' = '3e',
               'buildings_ownership_pct' = '3f',
               'buildings_constructed_past_yr' = '3g',
               'buildings_1929_price_range_pct' = '3h',
               'buildings_1936_price_range_pct' = '3i',
               'buildings_1938_price_range_pct' = '3j',
               'buildings_sales_demand' = '3k',
               'buildings_sales_activity' = '3l',
               'buildings_1929_rent_range_pct' = '3m',
               'buildings_1936_rent_range_pct' = '3n',
               'buildings_1938_rent_range_pct' = '3o',
               'buildings_rental_demand' = '3p',
               'buildings_rental_activity' = '3q',
               'mortgage_avial_home_purchase' = '4a',
               'mortgage_avial_home_building' = '4b',
               'clarifying_remarks' = '5',
               'name_location_grade_number' = '6') %>% 
        clean_names()
    write_csv(x = df_description_stk, path = 'area-descriptions/redline_stockton_area-descriptions.csv')
    write_xlsx(x = df_description_stk, path = 'area-descriptions/redline_stockton_area-descriptions.xlsx')    

    
# EAST BAY (OAKLAND) -- Convert geojson to dataframe ----
    geojson_oak <- (readr::read_lines(url_oakland) %>% jsonlite::fromJSON())$features
    df_description_oak <- bind_cols(geojson_oak$properties %>% select(-area_description_data), 
                                    geojson_oak$properties$area_description_data)
    df_description_oak <- df_description_oak[-1, ]
        # dput(sort(names(df_description_oak)))
    df_description_oak <- df_description_oak %>% 
        mutate(city = 'East Bay') %>% 
        select(c('city', #"0", 
                 "holc_id", "name", "holc_grade", 
                 '1', '2', '3', '4',
                 "5a", "5b", "5c", "5d", "5e", "5f", "5g", 
                 "6a", "6b", "6c", "6d",
                 '7',
                 "8a", "8b", "8c",
                 "9a", "9b", "9c",
                 "10a", "10b", "10c", 
                 "11a", "11b", 
                 "12a", "12b", 
                 "13", "14", "15"))
    # sort rows
    df_description_oak <- df_description_oak %>% 
        separate(col = holc_id, 
                 into = c('id_grade', 'id_numb'), 
                 sep = "(?<=[A-Z])(?=[0-9])", 
                 remove = FALSE) %>% 
        mutate(id_numb = as.numeric(id_numb)) %>% 
        arrange(id_grade, id_numb) %>% 
        select(-c('id_grade', 'id_numb')) %>% 
        rename(# 'holc_region' = '0',
            'name_location_grade_number' = '1',
            'area_terrain' = '2',
            'area_favorable_influences' = '3',
            'area_detrimental_influences' = '4',
            'inhabitants_occupation' = '5a',
            'inhabitants_income' = '5b',
            'inhabitants_foreign_pct_predominating' = '5c',
            'inhabitants_negro' = '5d',
            'inhabitants_infiltration' = '5e',
            'inhabitants_relief' = '5f',
            'inhabitants_pop_trend' = '5g',
            'buildings_type' = '6a',
            'buildings_construction' = '6b',
            'buildings_avg_age' = '6c',
            'buildings_repair' = '6d',
            'buildings_sale_rental_history' = '7',
            'area_pct_improved' = '8a',           
            'buildings_occupancy_pct' = '8b',
            'buildings_ownership_pct' = '8c',
            'buildings_sales_demand' = '9a',
            'buildings_sales_price_range' = '9b',
            'buildings_sales_activity' = '9c',
            'buildings_rental_demand' = '10a',
            'buildings_rental_price_range' = '10b',
            'buildings_rental_activity' = '10c',
            'buildings_constructed_past_yr_type_price' = '11a',
            'buildings_constructed_past_yr' = '11b',
            'mortgage_avial_home_purchase' = '12a',
            'mortgage_avial_home_building' = '12b',
            'area_desirability_trend' = '13',            
            'clarifying_remarks' = '14',
            'information_source' = '15'
            ) %>% 
        clean_names()
    write_csv(x = df_description_oak, path = 'area-descriptions/redline_east-bay_area-descriptions.csv')
    write_xlsx(x = df_description_oak, path = 'area-descriptions/redline_east-bay_area-descriptions.xlsx')    
    

# SAN JOSE -- Convert geojson to dataframe ----
    geojson_sj <- (readr::read_lines(url_sanjose) %>% jsonlite::fromJSON())$features
    df_description_sj <- bind_cols(geojson_sj$properties %>% select(-area_description_data), 
                                    geojson_sj$properties$area_description_data)
        # dput(sort(names(df_description_sj)))
    df_description_sj <- df_description_sj %>% 
        mutate(city = 'San Jose') %>% 
        select(c('city', #"0", 
                 "holc_id", "name", "holc_grade", 
                 '1', '2', '3', '4',
                 "5a", "5b", "5c", "5d", "5e", "5f", "5g", 
                 "6a", "6b", "6c", "6d",
                 '7',
                 "8a", "8b", "8c",
                 "9a", "9b", "9c",
                 "10a", "10b", "10c", 
                 "11a", "11b", 
                 "12a", "12b", 
                 "13", "14", "15"))
    # sort rows
    df_description_sj <- df_description_sj %>% 
        separate(col = holc_id, 
                 into = c('id_grade', 'id_numb'), 
                 sep = "(?<=[A-Z])(?=[0-9])", 
                 remove = FALSE) %>% 
        mutate(id_numb = as.numeric(id_numb)) %>% 
        arrange(id_grade, id_numb) %>% 
        select(-c('id_grade', 'id_numb')) %>% 
        rename(# 'holc_region' = '0',
            'name_location_grade_number' = '1',
            'area_terrain' = '2',
            'area_favorable_influences' = '3',
            'area_detrimental_influences' = '4',
            'inhabitants_occupation' = '5a',
            'inhabitants_income' = '5b',
            'inhabitants_foreign_pct_predominating' = '5c',
            'inhabitants_negro' = '5d',
            'inhabitants_infiltration' = '5e',
            'inhabitants_relief' = '5f',
            'inhabitants_pop_trend' = '5g',
            'buildings_type' = '6a',
            'buildings_construction' = '6b',
            'buildings_avg_age' = '6c',
            'buildings_repair' = '6d',
            'buildings_sale_rental_history' = '7',
            'area_pct_improved' = '8a',           
            'buildings_occupancy_pct' = '8b',
            'buildings_ownership_pct' = '8c',
            'buildings_sales_demand' = '9a',
            'buildings_sales_price_range' = '9b',
            'buildings_sales_activity' = '9c',
            'buildings_rental_demand' = '10a',
            'buildings_rental_price_range' = '10b',
            'buildings_rental_activity' = '10c',
            'buildings_constructed_past_yr_type_price' = '11a',
            'buildings_constructed_past_yr' = '11b',
            'mortgage_avial_home_purchase' = '12a',
            'mortgage_avial_home_building' = '12b',
            'area_desirability_trend' = '13',            
            'clarifying_remarks' = '14',
            'information_source' = '15'
        ) %>% 
        clean_names()
    write_csv(x = df_description_sj, path = 'area-descriptions/redline_san-jose_area-descriptions.csv')
    write_xlsx(x = df_description_sj, path = 'area-descriptions/redline_san-jose_area-descriptions.xlsx')    
    

# BIND ----
    df_description_combined <- bind_rows(df_description_sac, 
                                  df_description_stk, 
                                  df_description_oak, 
                                  df_description_sj, 
                                  df_description_la)
    write_csv(x = df_descriptionc_combined, path = 'area-descriptions/_redline_combined_area-descriptions.csv')
    write_xlsx(x = df_description_combined, path = 'area-descriptions/_redline_combined_area-descriptions.xlsx')    
    