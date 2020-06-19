# Packages: ----
# General data analysis and transformation
    library(readr)
    library(readxl)
    library(dplyr)
    library(janitor)
    # library(gtools)
    library(stringr)
    library(units)
    library(tidyr)
    library(rlang)
# Mapping and GIS operations
    library(sf)
    # library(leaflet)
    # library(htmlwidgets)
    # library(geojsonsf)
    # library(rmapshaper)
    library(tmap)
    library(tmaptools)
    library(ceramic)
# workflow
    library(here)
# plotting
    library(ggplot2)
    library(forcats)

options(scipen = 999)
   
# Get list of CES 3 variable names and plain text names ----
    # ces_variables <- read_csv(here('data_processed', 
    #                                 'ces_names.csv'))
    # ces_variables <- ces_variables %>% 
    #     mutate(name_revised = make_clean_names(name)) 
    
# Define coordinate systems to use for transformations ----
    projected_crs <- 3310 # see: https://epsg.io/3310 

# load data ----
    # Redline Polygons
        redline_polygons <- st_read(here('data_processed', 
                                         'redline_polygons.gpkg'))
            # st_crs(redline_polygons) # to check the reference system
        # transform to projected CRS
            redline_polygons <- redline_polygons %>% st_transform(projected_crs)        
        # # fix self-intersecting polygons (if needed)
        #     if (sum(!st_is_valid(redline_polygons)) > 0) {
        #         redline_polygons <- sf::st_buffer(redline_polygons, dist = 0)
        #     }
        # drop un-needed fields
            redline_polygons <- redline_polygons %>% 
                select(-c(holc_name, holc_link, holc_year, area_description_excerpts))
        # add the area of each redline polygon
            redline_polygons <- redline_polygons %>% mutate(holc_poly_area = st_area(.))
        # convert fields to factors
            # redline_polygons <- redline_polygons %>% 
            #     mutate(holc_city = factor(holc_city),
            #            holc_grade = factor(holc_grade))   
            
    # NOTE: not all redline polygons have an HOLC ID listed - create a unique ID for each one with a missing ID (so that each unique HOLC polygon can be associated with a weighted average CES score)
            # sum(is.na(redline_polygons$holc_id)) # 11
        # create new column for the new holc id to be assigned
            redline_polygons <- redline_polygons %>% 
                mutate('holc_id_2' = holc_id) %>% 
                arrange(holc_city, holc_grade, holc_id)
        # initialiaze some variables for the loop below
            grade_city_vec <- c()
            j <- 1
        # loop through all HOLC polygons ...
            for (i_row in 1:nrow(redline_polygons)) {
                cur_id <- redline_polygons$holc_id[i_row]
                grade <- redline_polygons$holc_grade[i_row]
                city <- redline_polygons$holc_city[i_row] 
                # the current combination of grade and city
                    grade_city_cur <- paste0(city, '_', grade)
                # if the holc id is NA, check to see if there are already other missing holc id's for that city/grade combination - if so, increment a counter by 1, otherwise set counter (back) to 1
                    if (is.na(cur_id)) {
                        if(grade_city_cur %in%  grade_city_vec) {
                            grade_city_counter <- grade_city_counter + 1
                        } else {
                            grade_city_counter <- 1
                        }
                # enter a new unique ID for the row with the missing ID
                    redline_polygons$holc_id_2[i_row] <- paste0('unknown_', 
                                                                grade, '_', 
                                                                grade_city_counter)
                # add the grade/city combo to a running list to track number of missing holc id's for that city/grade combination
                    grade_city_vec[j] <- grade_city_cur
                    j <- j + 1
                }
            }
            # check: View(redline_polygons[is.na(redline_polygons$holc_id),])

    # CES 3 Polygons
        ces3_poly <- st_read(here('data_processed', 
                                  'ces3_poly.gpkg'))
            # st_crs(ces3_poly) # 3310
        # # fix self-intersecting polygons (if needed)
        #     if (sum(!st_is_valid(ces3_poly)) > 0) {
        #         ces3_poly <- sf::st_buffer(ces3_poly, dist = 0)
        #     }        
        # modify the 'City' column to better keep track when joining with other datasets
            ces3_poly <- ces3_poly %>% 
                rename('ces_city' = 'nearby_city')
        # # clean names
        #     ces3_poly <- ces3_poly %>% 
        #         clean_names()


# get the intersection of the CES polygons and redline polygons ----
    # this clips the ces3 polygons to the area contained within the redline polygons
    # gives the CES component polygons used to calculate the weighted average sorces for HOLC polyogons 
        ces_clipped_to_holc <- st_intersection(ces3_poly, redline_polygons)
    # inspect
        # glimpse(ces_clipped_to_holc)            
    # add the area of each clipped polygon to the data frame
        ces_clipped_to_holc <- ces_clipped_to_holc %>% 
          mutate(clipped_area = st_area(.))
            # glimpse(ces_redline_intersection)
    # write to output file
        st_write(obj = ces_clipped_to_holc,
                 here('data_processed-analysis', 'ces_clipped_to_holc_bounds.gpkg'),
                 append = FALSE)

    
# compute weighted average score for each HOLC ID ----
    # first compute the contribution of each component CES poly to the total for eac holc polygon
    ces_component_scores <- ces_clipped_to_holc %>% 
            st_drop_geometry() %>% 
            select(holc_id:clipped_area, census_tract, ces_3_score:other_percent) %>% 
            pivot_longer(cols = ces_3_score:other_percent, 
                         names_to = 'ces_measure',
                         values_to = 'ces_score') %>% 
            mutate(component_score = ces_score * clipped_area / holc_poly_area) %>% 
            drop_units() %>% 
            {.}
        # # check
        #     sum(ces_component_scores %>%
        #         filter(holc_city == 'Fresno',
        #                holc_id == 'A1',
        #                ces_measure == 'ces_3_score') %>%
        #         pull(component_score)) # 39.11558
        
    # then combine the component scores to compute the area weighted score for each holc polygon
    holc_area_weighted_scores <- ces_component_scores %>% 
        group_by(holc_city, holc_id, holc_id_2, holc_grade, ces_measure) %>% 
        summarize(area_weighted_score = sum(component_score)) %>% 
        ungroup() %>% 
        {.}
        # # check
        #     holc_area_weighted_scores %>%
        #         filter(holc_city == 'Fresno',
        #                holc_id == 'A1',
        #                ces_measure == 'ces_3_score') %>%
        #         pull(area_weighted_score) # 39.11558
        
# compute the city-wide average HOLC score for each CES measure
    holc_area_weighted_scores <- holc_area_weighted_scores %>% 
        group_by(holc_city, ces_measure) %>% 
        mutate(citywide_average_score = mean(area_weighted_score)) %>% 
        # summarize(citywide_average) %>% 
        ungroup() %>% 
        {.}
    # # check
    #     mean(holc_area_weighted_scores %>%
    #              filter(holc_city == 'Fresno',
    #                     ces_measure == 'ces_3_score') %>%
    #              pull(area_weighted_score)) # 62.00244
    #     holc_area_weighted_scores %>%
    #              filter(holc_city == 'Fresno',
    #                     ces_measure == 'ces_3_score') %>%
    #              pull(citywide_average_score) # 62.00244
    
    
# compute the departure for each HOLC polygon's CES scores from their respective citywide average score
    holc_area_weighted_scores <- holc_area_weighted_scores %>% 
        mutate(departure_score = area_weighted_score - citywide_average_score) %>% 
        {.}
    
# compute the z score of the departures for each HOLC polygon
    holc_area_weighted_scores <- holc_area_weighted_scores %>% 
        group_by(holc_city, ces_measure) %>% 
        mutate(std_dev_departure_city_measure = sd(departure_score)) %>% 
        mutate(z_score_departure_city_measure = departure_score / std_dev_departure_city_measure) %>% 
        # summarize(citywide_average) %>% 
        ungroup() %>% 
        {.}  
    # # check
    #     View(holc_area_weighted_scores %>% 
    #         filter(holc_city == 'Fresno', ces_measure == 'ces_3_score')
    #     )
    
    
# PIVOT THESE DATASETS TO WIDE FORMAT, JOIN WITH HOLC POLYGON GEOMETRY, WRITE TO GEOPACKAGE FILES
# pivot the raw area weighted average scores to a wide data frame
    df_wide_raw_scores <- holc_area_weighted_scores %>% 
        select(holc_city:holc_grade, ces_measure, area_weighted_score) %>% 
        pivot_wider(id_cols = holc_city:holc_grade, 
                    names_from = ces_measure, 
                    values_from = area_weighted_score) %>% 
        # reorder the column names to match the original CES dataset
        select(holc_city:holc_grade, names(ces3_poly %>% 
                                               st_drop_geometry() %>% 
                                               select(ces_3_score:other_percent))
        ) %>% 
        mutate(score_type = 'area weighted score') %>% 
        relocate(score_type, .after = holc_grade) %>%
        {.}
    df_wide_raw_scores <- left_join(x = redline_polygons %>% 
                                        select(holc_city, holc_grade, holc_id, 
                                               holc_id_2, geom),
                                    y = df_wide_raw_scores,
                                    by = c('holc_city', 'holc_grade', 'holc_id', 'holc_id_2'))
    # write to output file
        st_write(obj = df_wide_raw_scores,
                 here('data_processed-analysis', 
                      'area_weighted_scores.gpkg'),
                 append = FALSE)
    
# pivot the citywide average scores to a wide data frame (1 row per city)
    df_wide_citywide_scores <- holc_area_weighted_scores %>% 
        select(holc_city:holc_grade, ces_measure, citywide_average_score) %>% 
        pivot_wider(id_cols = holc_city:holc_grade, 
                    names_from = ces_measure, 
                    values_from = citywide_average_score) %>% 
        # reorder the column names to match the original CES dataset
        select(holc_city:holc_grade, names(ces3_poly %>% 
                                               st_drop_geometry() %>% 
                                               select(ces_3_score:other_percent))
        ) %>% 
        mutate(score_type = 'citywide average (area weighted score)') %>% 
        relocate(score_type, .after = holc_grade) %>%
        {.}
    df_wide_citywide_scores <- left_join(x = redline_polygons %>% 
                                             select(holc_city, holc_grade, holc_id, 
                                                    holc_id_2, geom),
                                         y = df_wide_citywide_scores,
                                         by = c('holc_city', 'holc_grade', 'holc_id', 'holc_id_2'))
    # write to output file
        st_write(obj = df_wide_citywide_scores,
                 here('data_processed-analysis', 
                      'citywide_avg-area_weighted_scores.gpkg'),
                 append = FALSE)
    
    # create df with 1 row per city    
    df_wide_citywide_scores_2 <- df_wide_citywide_scores %>%
        st_drop_geometry() %>% 
        select(-c(holc_id, holc_id_2, holc_grade)) %>% 
        group_by(holc_city) %>% 
        distinct() %>% 
        ungroup() %>% 
        {.}
    

# pivot the departure scores to a wide data frame
    df_wide_departure_scores <- holc_area_weighted_scores %>% 
        select(holc_city:holc_grade, ces_measure, departure_score) %>% 
        pivot_wider(id_cols = holc_city:holc_grade, 
                    names_from = ces_measure, 
                    values_from = departure_score) %>% 
        # reorder the column names to match the original CES dataset
        select(holc_city:holc_grade, names(ces3_poly %>% 
                                               st_drop_geometry() %>% 
                                               select(ces_3_score:other_percent))
        ) %>% 
        mutate(score_type = 'departure (area weighted score)') %>% 
        relocate(score_type, .after = holc_grade) %>%
        {.} 
    df_wide_departure_scores <- left_join(x = redline_polygons %>% 
                                              select(holc_city, holc_grade, holc_id, 
                                                     holc_id_2, geom),
                                          y = df_wide_departure_scores,
                                          by = c('holc_city', 'holc_grade', 'holc_id', 'holc_id_2'))
    # write to output file
        st_write(obj = df_wide_departure_scores,
                 here('data_processed-analysis', 
                      'departure-area_weighted_scores.gpkg'),
                 append = FALSE)

# pivot the z-scores to a wide data frame
    df_wide_z_scores <- holc_area_weighted_scores %>% 
        select(holc_city:holc_grade, ces_measure, z_score_departure_city_measure) %>% 
        pivot_wider(id_cols = holc_city:holc_grade, 
                    names_from = ces_measure, 
                    values_from = z_score_departure_city_measure) %>% 
        # reorder the column names to match the original CES dataset
        select(holc_city:holc_grade, names(ces3_poly %>% 
                                               st_drop_geometry() %>% 
                                               select(ces_3_score:other_percent))
        ) %>% 
        mutate(score_type = 'departure z-score (area weighted score)') %>% 
        relocate(score_type, .after = holc_grade) %>%
        {.}
    df_wide_z_scores <- left_join(x = redline_polygons %>% 
                                      select(holc_city, holc_grade, holc_id, 
                                             holc_id_2, geom),
                                  y = df_wide_z_scores,
                                  by = c('holc_city', 'holc_grade', 'holc_id', 'holc_id_2'))
    # write to output file
        st_write(obj = df_wide_z_scores,
                 here('data_processed-analysis', 
                      'z_scores-area_weighted_scores.gpkg'),
                 append = FALSE)

#######################################################################################-
# ALTERNATE METHOD (using wide datasets) ----------------------------------------------
        
# first compute the contribution of each component CES poly to the total for eac holc polygon
    ces_component_scores <- ces_clipped_to_holc %>% 
        st_drop_geometry() %>%
        # for each clipped CES poly, calculate: (CES score * clipped area) / total holc poly area
        mutate(across(c(ces_3_score:other_percent), ~ . * clipped_area / holc_poly_area))  
    # then combine the component scores to compute the area weighted score for each holc polygon
    holc_area_weighted_scores <- ces_component_scores %>% 
        group_by(holc_city, holc_id, holc_id_2, holc_grade) %>% 
        # mutate(across(c(ces_3_score:other_percent), sum)) %>% 
        summarize(across(c(ces_3_score:other_percent), sum)) %>% 
        drop_units() %>%
        ungroup() %>% 
        mutate(score_type = 'Area Weighted Average') %>% 
        relocate(score_type, .after = holc_grade) %>% 
        {.}
    # # check: Fresno A1
    #     # original scores
    #     ces_clipped_to_holc %>% 
    #         filter(holc_city == 'Fresno', holc_id == 'A1') %>% 
    #         select(holc_city, holc_id, holc_poly_area, clipped_area, ces_3_score, traffic)
    #     # Manual Calculations: 
    #         # CES3: (37.06 * 316342.86) / 386306.2 = 30.34812
    #         # CES3: (48.41 * 69963.32) / 386306.2 = 8.76746
    #         # CES3 sum: 30.34812 + 8.76746 = 39.11558
    #         # Traffic: (504.84 * 316342.86) / 386306.2 = 413.4092
    #         # Traffic: (681.10 * 69963.32) / 386306.2 = 123.353
    #         # Traffic sum: = 413.4092 + 123.353 = 536.7622
    #     ces_component_scores %>% 
    #         filter(holc_city == 'Fresno', holc_id == 'A1') %>% 
    #         select(holc_city, holc_id, holc_poly_area, clipped_area, ces_3_score, traffic)
    #         # CES3: (37.06 * 316342.86 + 48.41 * 69963.32) / 386306.2 = 39.11558
    #         # Traffic: (504.84 * 316342.86 + 681.10 * 69963.32) / 386306.2 = 536.7622
    #     holc_area_weighted_scores %>% 
    #         filter(holc_city == 'Fresno', holc_id == 'A1') %>% 
    #         select(holc_city, holc_id, ces_3_score, traffic)
    #         # CES3: 39.1
    #         # Traffic: 537

# compute the city-wide average HOLC score for each CES measure
    holc_citywide_average <- holc_area_weighted_scores %>% 
        group_by(holc_city) %>% 
        summarize(across(c(ces_3_score:other_percent), mean)) %>% 
        ungroup() %>% 
        mutate(score_type = 'Citywide Average') %>%
        {.}
    # check:
        mean(holc_area_weighted_scores %>% filter(holc_city == 'Fresno') %>% pull(ces_3_score)) # 62.00244
        holc_citywide_average %>% filter(holc_city == 'Fresno') %>% pull(ces_3_score) # 62.00244
        
# compute the departure for each HOLC polygon's CES scores from their respective citywide average score
    holc_area_weighted_departures <- holc_area_weighted_scores %>% 
        # !!!!!!!!!! NOT FINISHED !!!!!!!!!!!!!!
        {.}
    