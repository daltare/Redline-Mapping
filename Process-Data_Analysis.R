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
            redline_polygons <- redline_polygons %>% 
                st_transform(projected_crs)        
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
                      'holc_area_weighted_scores.gpkg'),
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

######################################################################################-
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
        mutate(score_type = 'area weighted average') %>% 
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
        mutate(score_type = 'citywide average') %>%
        {.}
    # check:
        mean(holc_area_weighted_scores %>% filter(holc_city == 'Fresno') %>% pull(ces_3_score)) # 62.00244
        holc_citywide_average %>% filter(holc_city == 'Fresno') %>% pull(ces_3_score) # 62.00244
        
# compute the departure for each HOLC polygon's CES scores from their respective citywide average score
    holc_area_weighted_departures <- holc_area_weighted_scores %>% 
        # !!!!!!!!!! NOT FINISHED !!!!!!!!!!!!!!
        {.}
    
    
    
    
#######################################################################################-
# CENTROID METHOD ----------------------------------------------------------------------
    # compute centroids of redline polygons
        redline_polygons_centroid <- redline_polygons %>% 
            st_centroid(.)
        # write to output file
            st_write(obj = redline_polygons_centroid,
                     here('data_processed-analysis', 
                          'redline_polygons_centroid.gpkg'),
                     append = FALSE)
        
    # compute centroids of CES polygons
        ces3_poly_centroid <- ces3_poly %>% 
            st_centroid(.)
        # write to output file
            # for the output geopackage file, just get centroids of CES tracts in relevant counties
                # get selected california counties
                    ca_counties <- st_read('data_processed\\CA_Counties.gpkg') %>% 
                        clean_names() %>% 
                        filter(cnty_name %in% c('Fresno', 'Los Angeles', 'Alameda', 'Sacramento', 
                                                'Yolo', 'San Diego', 'San Francisco', 
                                                'Santa Clara','San Joaquin'))
                    
            st_write(obj = ces3_poly_centroid[ca_counties,], # %>% select(census_tract, geom),
                     here('data_processed-analysis', 
                          'ces_centroids.gpkg'),
                     append = FALSE)

    # connect the centroid of each redline polygon to the nearest centroid of a CES polygon
        redline_nearest_feature <- st_nearest_feature(redline_polygons_centroid, ces3_poly_centroid)
    
    # OUTPUT - get the CES polygon that matches each HOLC polygon, based on the centroid matching
        holc_centroid_scores <- redline_polygons %>% 
            bind_cols(ces3_poly %>% 
                          slice(redline_nearest_feature) %>% 
                          st_drop_geometry()
                      ) %>% 
            # rearrange and select/remove columns
            select(holc_city, holc_grade, holc_id, holc_id_2, ces_3_score:other_percent, geom) %>% 
            mutate(score_type = 'centroid matched score') %>% 
            relocate(score_type, .after = holc_id_2)
        # write to output file
        st_write(obj = holc_centroid_scores,
                 here('data_processed-analysis', 
                      'holc_centroid_scores.gpkg'),
                 append = FALSE)
    # get the lines connecting the holc polygons to their matched ces polygons
        # draw lines
            connecting_lines <- st_nearest_points(redline_polygons_centroid, 
                                                  ces3_poly_centroid %>% slice(redline_nearest_feature), 
                                                  pairwise = TRUE)
        # write to output file
        st_write(obj = connecting_lines,
                 here('data_processed-analysis', 
                      'holc_centroid_ces_connecting_lines.gpkg'),
                 append = FALSE)
    # # check - plot (first 10)
    #     plot(st_geometry(redline_polygons)[1:10], col = 'red')
    #     plot(st_geometry(redline_polygons_centroid)[1:10], add = T, col = 'green')
    #     plot(st_geometry(ces3_poly)[redline_nearest_feature[1:10]], add = T) #, col = 'blue')
    #     plot(st_geometry(ces3_poly_centroid)[redline_nearest_feature[1:10]], add = T, col = 'black')
    #     # plot(st_geometry(redline_nearest_ces_centroid)[1:10], col = 'red', add = T)
    #     plot(connecting_lines[1:10], add = TRUE)

    # compute departure scores
        # pivot from wide to long
            holc_centroid_scores_long <- holc_centroid_scores %>% 
                st_drop_geometry() %>% 
                select(holc_city:holc_id_2, ces_3_score:other_percent) %>% 
                pivot_longer(cols = ces_3_score:other_percent, 
                             names_to = 'ces_measure',
                             values_to = 'ces_score') %>% 
                {.}
    # compute the city-wide average HOLC score for each CES measure
        holc_centroid_scores_long <- holc_centroid_scores_long %>% 
            group_by(holc_city, ces_measure) %>% 
            mutate(citywide_average_score = mean(ces_score)) %>% 
            ungroup() %>% 
            {.}
    # compute the departure for each HOLC polygon's CES scores from their respective citywide average score
        holc_centroid_scores_long <- holc_centroid_scores_long %>% 
            mutate(departure_score = ces_score - citywide_average_score) %>% 
            {.}
    
    # compute the z score of the departures for each HOLC polygon
        holc_centroid_scores_long <- holc_centroid_scores_long %>% 
            group_by(holc_city, ces_measure) %>% 
            mutate(std_dev_departure_city_measure = sd(departure_score)) %>% 
            mutate(z_score_departure_city_measure = departure_score / std_dev_departure_city_measure) %>% 
            # summarize(citywide_average) %>% 
            ungroup() %>% 
            {.}  
    # PIVOT THESE DATASETS TO WIDE FORMAT, JOIN WITH HOLC POLYGON GEOMETRY, WRITE TO GEOPACKAGE FILES
        # pivot the departure scores to a wide data frame
            df_wide_centroid_departure_scores <- holc_centroid_scores_long %>% 
                select(holc_city:holc_id_2, ces_measure, departure_score) %>% 
                pivot_wider(id_cols = holc_city:holc_id_2, 
                            names_from = ces_measure, 
                            values_from = departure_score) %>% 
                mutate(score_type = 'departure (centroid score)') %>% 
                relocate(score_type, .after = holc_id_2) %>%
                {.} 
            df_wide_centroid_departure_scores <- left_join(x = redline_polygons %>% 
                                                      select(holc_city, holc_grade, holc_id, 
                                                             holc_id_2, geom),
                                                  y = df_wide_centroid_departure_scores,
                                                  by = c('holc_city', 'holc_grade', 'holc_id', 'holc_id_2'))
            # write to output file
                st_write(obj = df_wide_centroid_departure_scores,
                         here('data_processed-analysis', 
                              'departure-centroid_scores.gpkg'),
                         append = FALSE)
        
        # pivot the z-scores to a wide data frame
        df_wide_centroid_z_scores <- holc_centroid_scores_long %>% 
                select(holc_city:holc_id_2, ces_measure, z_score_departure_city_measure) %>% 
                pivot_wider(id_cols = holc_city:holc_id_2, 
                            names_from = ces_measure, 
                            values_from = z_score_departure_city_measure) %>% 
                mutate(score_type = 'departure z-score (centroid score)') %>% 
                relocate(score_type, .after = holc_id_2) %>%
                {.} 
            df_wide_centroid_z_scores <- left_join(x = redline_polygons %>% 
                                                      select(holc_city, holc_grade, holc_id, 
                                                             holc_id_2, geom),
                                                  y = df_wide_centroid_z_scores,
                                                  by = c('holc_city', 'holc_grade', 'holc_id', 'holc_id_2'))
            # write to output file
                st_write(obj = df_wide_centroid_z_scores,
                         here('data_processed-analysis', 
                              'z_scores-centroid_scores.gpkg'),
                         append = FALSE)

# look at correlations between the scores calculated with the centroid method versus scores calculated with
# the area weighted average method
    area_weighted_scores <- st_read(here('data_processed-analysis', 
                                         'holc_area_weighted_scores.gpkg'))
    centroid_scores <- st_read(here('data_processed-analysis', 
                                         'holc_centroid_scores.gpkg'))
        
    check_df <- area_weighted_scores %>% 
        select(holc_city:holc_id_2, ces_3_score) %>% 
        st_drop_geometry() %>% 
        rename(ces_3_score_area_wt = ces_3_score)
    check_df <- check_df %>% left_join(centroid_scores %>% 
                                           select(holc_city:holc_id_2, ces_3_score) %>% 
                                           st_drop_geometry() %>% 
                                           rename(ces_3_score_centroid = ces_3_score))
    check_scatter <- ggplot(data = check_df, 
                            mapping = aes(x = ces_3_score_area_wt,
                                          y = ces_3_score_centroid)) +
        geom_point() + 
        geom_abline(slope = 1) +
        xlim(c(0,100)) + 
        ylim(c(0,100))
    check_scatter
    
    correlation_spearman_test <- cor.test(x = check_df$ces_3_score_area_wt, 
                                          y = check_df$ces_3_score_centroid, 
                                          method = 'spearman')
        correlation_spearman_test
            # p-value < 0.00000000000000022
            # rho: 0.9663347

    # compare departure scores
    area_weighted_scores_departure <- st_read(here('data_processed-analysis', 
                                                   'departure-area_weighted_scores.gpkg'))
    centroid_scores_departure <- st_read(here('data_processed-analysis', 
                                              'departure-centroid_scores.gpkg'))
    check_df_departure <- area_weighted_scores_departure %>% 
        select(holc_city:holc_id_2, ces_3_score) %>% 
        st_drop_geometry() %>% 
        rename(ces_3_score_area_wt_dep = ces_3_score)
    check_df_departure <- check_df_departure %>% left_join(centroid_scores_departure %>% 
                                           select(holc_city:holc_id_2, ces_3_score) %>% 
                                           st_drop_geometry() %>% 
                                           rename(ces_3_score_centroid_dep = ces_3_score))
    check_scatter_departure <- ggplot(data = check_df_departure, 
                            mapping = aes(x = ces_3_score_area_wt_dep,
                                          y = ces_3_score_centroid_dep)) +
        geom_point() + 
        geom_abline(slope = 1) +
        xlim(c(-50,50)) + 
        ylim(c(-50,50))
    check_scatter_departure
    correlation_spearman_test_dep <- cor.test(x = check_df_departure$ces_3_score_area_wt_dep, 
                                          y = check_df_departure$ces_3_score_centroid_dep, 
                                          method = 'spearman')
    correlation_spearman_test_dep 
        # p-value < 0.00000000000000022
        # rho: 0.9389074 
        
######################################################################################-
# ROAD LENGTH METHOD ------------------------------------------------------------------
        get_roads <- function(county) {
            rd_county <- tigris::roads(state = 'CA', county = county)
            rd_county <- st_as_sf(rd_county)
            rd_county <- rd_county %>% st_transform(3310)
            return(rd_county)
        }
        
        rd_sac <- get_roads('Sacramento')
        rd_yolo <- get_roads('Yolo')
        
        
            st_crs(rd_sac)
            st_crs(redline_polygons)
        # PLOT
        plot(st_geometry(rd_yolo)) # %>% slice(1:10000)))
        plot(st_geometry(rd_sac), add = TRUE, col = 'yellow')
        plot(st_geometry(redline_polygons %>% filter(holc_city == 'Sacramento')), add = TRUE, col = 'red')
        
        rd_sac <- bind_rows(rd_sac, rd_yolo)
        rd_overlap <- rd_sac[redline_polygons %>% filter(holc_city == 'Sacramento'), ]
        plot(st_geometry(rd_overlap))
        plot(st_geometry(redline_polygons %>% filter(holc_city == 'Sacramento')), add = TRUE, col = 'red')
        
        rd_intersection <- st_intersection(redline_polygons %>% filter(holc_city == 'Sacramento'), 
                                           rd_sac)
        plot(st_geometry(rd_intersection))
        
        
        # USE THIS !!!!!!!!!!!!!!!!!!!!
            # SAC
            rd_ces_intersection <- st_intersection(ces_clipped_to_holc %>% filter(holc_city == 'Sacramento'),
                                                   rd_sac)
            rd_ces_intersection <- rd_ces_intersection %>% 
                mutate(length = st_length(geom))
            
            plot(st_geometry(ces_clipped_to_holc %>% filter(holc_city == 'Sacramento')))
            plot(st_geometry(rd_ces_intersection), add = T)
            
            # FRESNO
            rd_fr <- get_roads('Fresno')
            rd_fr_intersection <- st_intersection(ces_clipped_to_holc %>% 
                                                      filter(holc_city == 'Fresno'),
                                                   rd_fr)
            rd_fr_intersection <- rd_fr_intersection %>% 
                mutate(length = st_length(geom))
            
            plot(st_geometry(ces_clipped_to_holc %>% filter(holc_city == 'Fresno')))
            plot(st_geometry(rd_fr_intersection), add = T)
            
            # calculate length of roads within each overlapping portion of a CES polygon
                rd_fr_summary <- rd_fr_intersection %>% 
                    group_by(census_tract, holc_id_2) %>% 
                    mutate(tract_rd_length = sum(length)) %>% 
                    ungroup()
            # calculate length of roads within each HOLC polygon   
                rd_fr_summary <- rd_fr_summary %>% 
                    group_by(holc_id_2) %>% 
                    mutate(holc_rd_length = sum(length)) %>% 
                    ungroup()
            # 
            rd_fr_summary <- rd_fr_summary %>% 
                group_by(census_tract, holc_id_2) %>%
                mutate(score_portion = tract_rd_length/ holc_rd_length)
            
            
            
            rd_fr_holc_summary <-  rd_fr_intersection %>% 
                group_by(holc_id_2) %>% 
                summarize(holc_rd_length = sum(length))
            
            rd_fr_holc_summary_2 <-  rd_fr_intersection %>% 
                
        
        
# create a list of counties corresponding to the HOLC city maps
    cities_counties <- list('Fresno' = 'Fresno',
                            'Los Angeles' = 'Los Angeles',
                            'Oakland' = 'Alameda',
                            'Sacramento' = c('Sacramento', 'Yolo'),
                            'San Diego' = 'San Diego',
                            'San Francisco' = 'San Francisco',
                            'San Jose' = 'Santa Clara',
                            'Stockton' = 'San Joaquin')       
    
