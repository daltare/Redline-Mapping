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
    # library(tmap)
    # library(tmaptools)
    # library(ceramic)
# workflow
    library(here)
# plotting
    library(ggplot2)
    library(forcats)
    library(patchwork)

options(scipen = 999)


# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   
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

            
            
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
        # st_write(obj = ces_clipped_to_holc,
        #          here('data_processed-analysis', 'ces_clipped_to_holc_bounds.gpkg'),
        #          append = FALSE)

    
# compute weighted average score for each HOLC ID ----
    # first compute the contribution of each component CES poly to the total for eac holc polygon
    ces_component_scores <- ces_clipped_to_holc %>% 
            st_drop_geometry() %>% 
            select(holc_id:clipped_area, census_tract, ces_3_score:other_percent, population_2010) %>% 
            pivot_longer(cols = ces_3_score:population_2010, 
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
    
    demographics <- holc_area_weighted_scores %>% 
        filter(ces_measure == 'ces_3_score') %>% 
        rename(ces_3_score = area_weighted_score) %>% 
        select(-ces_measure) %>% 
        left_join(holc_area_weighted_scores %>% 
                     filter(ces_measure == 'population_2010') %>% 
                     rename(population_2010 = area_weighted_score) %>% 
                     select(-ces_measure)) %>% 
        left_join(holc_area_weighted_scores %>% 
                      filter(ces_measure == 'hispanic_percent') %>% 
                      rename(hispanic_percent = area_weighted_score) %>% 
                      select(-ces_measure)) %>% 
        left_join(holc_area_weighted_scores %>% 
                      filter(ces_measure == 'white_percent') %>% 
                      rename(white_percent = area_weighted_score) %>% 
                      select(-ces_measure)) %>% 
        left_join(holc_area_weighted_scores %>% 
                      filter(ces_measure == 'african_american_percent') %>% 
                      rename(african_american_percent = area_weighted_score) %>% 
                      select(-ces_measure)) %>% 
        left_join(holc_area_weighted_scores %>% 
                      filter(ces_measure == 'native_american_percent') %>% 
                      rename(native_american_percent = area_weighted_score) %>% 
                      select(-ces_measure)) %>% 
        left_join(holc_area_weighted_scores %>% 
                      filter(ces_measure == 'asian_american_percent') %>% 
                      rename(asian_american_percent = area_weighted_score) %>% 
                      select(-ces_measure)) %>% 
        left_join(holc_area_weighted_scores %>% 
                      filter(ces_measure == 'other_percent') %>% 
                      rename(other_percent = area_weighted_score) %>% 
                      select(-ces_measure)) %>% 
        mutate(total_percent = hispanic_percent + white_percent + 
                   african_american_percent + native_american_percent + 
                   asian_american_percent + other_percent)

    demographics <- demographics %>% 
        mutate(hispanic_pop = hispanic_percent / 100 * population_2010 * (100 / total_percent)) %>% 
        mutate(white_pop = white_percent / 100 * population_2010 * (100 / total_percent)) %>% 
        mutate(african_american_pop = african_american_percent / 100 * population_2010 * (100 / total_percent)) %>% 
        mutate(native_american_pop = native_american_percent / 100 * population_2010 * (100 / total_percent)) %>% 
        mutate(asian_american_pop = asian_american_percent / 100 * population_2010 * (100 / total_percent)) %>% 
        mutate(other_pop = other_percent / 100 * population_2010 * (100 / total_percent)) %>% 
        mutate(total_pop = hispanic_pop + white_pop + 
                   african_american_pop + native_american_pop + 
                   asian_american_pop + other_pop)
    
    dem_summary <- demographics %>% 
        group_by(holc_grade) %>% 
        summarize(across(.cols = hispanic_pop:other_pop, .fns = sum)) %>% 
        ungroup() %>% 
        bind_rows(demographics %>% 
                      summarize(across(.cols = hispanic_pop:other_pop, .fns = sum)) %>% 
                      ungroup()) %>% 
        mutate(total_pop = hispanic_pop + white_pop + 
                   african_american_pop + native_american_pop + 
                   asian_american_pop + other_pop) %>% 
        mutate(hispanic_pct = hispanic_pop / total_pop) %>% 
        mutate(white_pct = white_pop / total_pop) %>%
        mutate(african_american_pct = african_american_pop / total_pop) %>%
        mutate(native_american_pct = native_american_pop / total_pop) %>%
        mutate(asian_american_pct = asian_american_pop / total_pop) %>%
        mutate(other_pct = other_pop / total_pop) %>% 
        mutate(total_pct = hispanic_pct + white_pct + 
                   african_american_pct + native_american_pct + 
                   asian_american_pct + other_pct)
    dem_summary$holc_grade[5] <- 'Total'
    
    dem_summary_long <- dem_summary %>% 
        select(-(hispanic_pop:total_pop), -total_pct) %>% 
        pivot_longer(cols = hispanic_pct:other_pct, 
                     names_to = 'category', 
                     values_to = 'percent') %>% 
        mutate(holc_grade = str_replace(string = holc_grade, 
                                        pattern = 'Total', 
                                        replacement =  'All Neighborhoods')) %>% 
        mutate(holc_grade = factor(holc_grade, 
                               levels = rev(c('A', 'B', 'C', 'D', 'All Neighborhoods')))) %>% 
        mutate(category = str_remove(category, pattern = '_pct')) %>% 
        mutate(category = str_replace(category, pattern = '_', replacement = ' ')) %>%
        mutate(category = str_to_title(category)) %>% 
        mutate(category = factor(category, 
                                 levels = rev(c('White', 'Hispanic', 'Asian American', 'African American', 'Native American', 'Other'))))
    
    
    
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# PLOT ----
    color_palette_1 <- RColorBrewer::brewer.pal(name = 'Dark2', n = 6)
    race_plot <- ggplot(dem_summary_long %>% filter(holc_grade != 'All Neighborhoods')) +
        geom_bar(mapping = aes(x = holc_grade, 
                               y = percent, 
                               fill = category), stat = 'identity') +
        scale_fill_manual(values = color_palette_1) +
        guides(fill = guide_legend(reverse = TRUE, title = 'Race/Ethnicity')) +
        scale_y_continuous(labels = scales::percent) + 
        labs(x = 'HOLC Rating',
             y = 'Percent of Population',
             title = 'Present-Day Racial/Ethnic Composition of Neighborhoods Assessed by the HOLC in the 1930s in California', 
             caption = 'Note: Present-day demographic information is from 2010 U.S. Census data') + # ,
             # subtitle = 'Demographic Data from 2010 Census') +
        coord_flip()
    
    ggsave(filename = here('_storymap', 'storymap_images', 'race_plot_bar.png'), 
       plot = race_plot, width = 10, height = 4.5, dpi = 125)
    
    # race_plot_2 <- ggplot(dem_summary_long) + # %>% filter(holc_grade != 'Total')) +
    #     geom_bar(mapping = aes(x = holc_grade,
    #                            y = percent,
    #                            fill = category), stat = 'identity', position = 'dodge') +
    #     guides(fill = guide_legend(reverse = TRUE)) +
    #     scale_y_continuous(labels = scales::percent) +
    #     xlab('HOLC Grade') +
    #     ylab('Percent') +
    #     coord_flip()
    
    # demographics_all_long <- demographics %>%
    #     select(-(population_2010:total_percent), -total_pop) %>% 
    #     pivot_longer(cols = hispanic_pop:other_pop)
    
    pop_distributions_by_race <- dem_summary %>% 
        select(holc_grade:total_pop) %>% 
        filter(holc_grade != 'Total') %>% 
        mutate(pct_total = total_pop / sum(total_pop)) %>% 
        mutate(pct_hispanic = hispanic_pop / sum(hispanic_pop)) %>% 
        mutate(pct_white = white_pop / sum(white_pop)) %>% 
        mutate(pct_african_american = african_american_pop / sum(african_american_pop)) %>% 
        mutate(pct_native_american = native_american_pop / sum(native_american_pop)) %>%
        mutate(pct_asian_american = asian_american_pop / sum(asian_american_pop)) %>%
        mutate(pct_other = other_pop / sum(other_pop)) %>% 
        select(holc_grade, pct_total:pct_other) %>% 
        pivot_longer(cols = pct_total:pct_other) %>% 
        mutate(holc_grade = factor(holc_grade, 
                               levels = rev(c('A', 'B', 'C', 'D')))) %>% 
        mutate(name = str_remove(name, pattern = 'pct_')) %>% 
        mutate(name = str_replace(name, pattern = '_', replacement = ' ')) %>%
        mutate(name = str_replace(name, pattern = 'total', replacement = 'total population')) %>%
        mutate(name = str_to_title(name)) %>% 
        mutate(name = factor(name, 
                             levels = rev(c('White', 'Hispanic', 'Asian American', 'African American', 'Native American', 'Other', 'Total Population'))))
        
        
    color_palette <- RColorBrewer::brewer.pal(name = 'Dark2', n = 6) # Dark2
    color_palette <- c('#000000', color_palette)
    # color_palette <- RColorBrewer::brewer.pal(name = 'Dark2', n = 7)
    # color_palette[1] <- c('#000000', color_palette)
    # color_palette <- viridis::viridis(n = 7)
    # color_palette[1] <- c('#000000', color_palette)
    
    
    # pop_distributions_by_race_plot <- ggplot(data = pop_distributions_by_race, 
    #                                          mapping = aes(x = holc_grade, y = value)) +
    #     geom_bar(mapping = aes(fill = name), stat = 'identity', position = 'dodge') +
    #     # scale_fill_brewer(
    #     #     type = "seq",
    #     #     palette = 1,
    #     #     direction = 1,
    #     #     aesthetics = "fill"
    #     # ) +
    #     # scale_fill_viridis_d() + 
    #     # scale_fill_viridis_d(option = 'plasma') +
    #     scale_fill_manual(values = color_palette) +
    #     # scale_fill_brewer(palette = 'Dark2') +
    #     scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    #     # geom_text( size = 3, position = position_stack(vjust = 0.5))
    #     xlab('HOLC Grade') + 
    #     ylab('Percent of Population of Given Race/Ethnicity Currently Residing in HOLC Grade') +
    #     guides(fill = guide_legend(reverse = TRUE, title = 'Race/Ethnicity')) + 
    #     coord_flip()
        
# to get the tabular values
    pop_distributions_by_race %>% filter(name == 'Total Population')
    pop_distributions_by_race %>% filter(name == 'White')
    pop_distributions_by_race %>% filter(name == 'Hispanic')
    pop_distributions_by_race %>% filter(name == 'African American')


    
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# PLOT ----
# facet plot
pop_distributions_by_race_2 <- pop_distributions_by_race %>% 
    mutate(holc_grade = case_when(holc_grade == 'A' ~ 'A (Best)', 
                                  holc_grade == 'B' ~ 'B (Desirable)', 
                                  holc_grade == 'C' ~ 'C (Declining)', 
                                  holc_grade == 'D' ~ 'D (Hazardous)')) %>% 
    mutate(holc_grade = factor(holc_grade, 
                               levels = c('A (Best)', 'B (Desirable)', 'C (Declining)', 'D (Hazardous)')))
color_palette_2 <- RColorBrewer::brewer.pal(name = 'Dark2', n = 6) # Dark2
pop_distributions_by_race_plot_2 <- ggplot(data = pop_distributions_by_race_2 %>% 
                                               filter(name != 'Total Population'), 
                                           mapping = aes(x = '', y = value)) +
    geom_bar(mapping = aes(fill = name), stat = 'identity', position = 'dodge') +
    geom_hline(data = pop_distributions_by_race_2 %>% filter(name == 'Total Population'), 
               mapping = aes(yintercept = value), 
               linetype = "dashed") +
    facet_wrap(~holc_grade, scales = 'fixed') +
    # scale_fill_brewer(
    #     type = "seq",
    #     palette = 1,
    #     direction = 1,
    #     aesthetics = "fill"
    # ) +
    # scale_fill_viridis_d() + 
    # scale_fill_viridis_d(option = 'plasma') +
    scale_fill_manual(values = color_palette_2) +
    # scale_fill_brewer(palette = 'Dark2') +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    # geom_text( size = 3, position = position_stack(vjust = 0.5))
    # xlab('HOLC Grade') + 
    labs(y = 'Percent of 2010 Population of Given Race/Ethnicity Residing in Neighborhoods with Given HOLC Rating',
         title = 'Present-Day Distribution of Racial or Ethnic Group Populations by Historical HOLC Neighborhood Rating',
         subtitle = 'Dashed lines represent distribution of overall present-day population by HOLC neighborhood rating') +
    guides(fill = guide_legend(reverse = TRUE, title = 'Race/Ethnicity')) + 
    theme(axis.ticks.y = element_blank(), axis.title.y = element_blank()) +
    coord_flip()
ggsave(filename = here('_storymap', 'storymap_images', 'race_plot_facet.png'), 
       plot = pop_distributions_by_race_plot_2, width = 10, height = 4.5, dpi = 125)    


# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ordering_a <- pop_distributions_by_race_2 %>% 
    filter(holc_grade == 'A (Best)' & name != 'Total Population') %>% 
    arrange(desc(value)) %>% 
    pull(name) %>% 
    as.character()

ordering_d <- pop_distributions_by_race_2 %>% 
    filter(holc_grade == 'D (Hazardous)' & name != 'Total Population') %>% 
    arrange(desc(value)) %>% 
    pull(name) %>% 
    as.character()
    
# race plot - bar plot by ethnic group
    race_plot_by_race <- ggplot(pop_distributions_by_race_2 %>% 
                                    filter(name != 'Total Population') %>% 
                                    mutate(name = fct_relevel(name, rev(ordering_a)))) +
        geom_bar(mapping = aes(x = name, 
                               y = value, 
                               fill = fct_rev(holc_grade)), stat = 'identity') +
        # scale_fill_manual(values = color_palette_1) +
        scale_fill_manual(values = rev(alpha(c('green', 'blue', 'orange', 'red'), 1.0)),
                           name = 'HOLC Grade') +
        # guides(fill = guide_legend(reverse = TRUE, title = 'HOLC Grade')) +
        guides(fill = guide_legend(reverse = TRUE, title = 'HOLC Grade')) +
        scale_y_continuous(labels = scales::percent) + 
        labs(x = 'Racial / Ethnic Group',
             y = 'Percent of Population',
             title = 'Present-Day Distribution of Racial / Ethnic Group Populations Within Neighborhoods \nAssessed by the HOLC in the 1930s in California', #) + # ,
             # subtitle = 'Present-Day Demographic Data from 2010 Census', 
            caption = 'Note: Present-day demographic information is from 2010 U.S. Census data'
             ) +
        coord_flip()
    
    ggsave(filename = here('_storymap', 'storymap_images', 'race_plot_bar_by_race.png'), 
       plot = race_plot_by_race, width = 10, height = 4.5, dpi = 125)

