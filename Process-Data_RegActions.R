library(dplyr)
library(readr)
library(vroom)
library(data.table)
library(here)
library(dplyr)
library(lubridate)
library(data.table)
library(janitor)
library(tidyr)
library(sf)
library(tictoc)
library(tidylog)


    # CalEPA regulated sites
        calepa_reg_sites <- fread('data_regulatory_actions/Site.csv') %>%
        # calepa_reg_sites <- read_csv('data_regulatory_actions/Site.csv') %>%
            tibble() %>% 
            clean_names() %>% 
            select(-dplyr::ends_with(as.character(0:9))) %>% 
            rename(zip_code = zip) %>% 
            mutate(state = 'ca', 
                   data_source = 'CalEPA Regulated Site Portal') %>% 
            # mutate(coordinates = Map(c, longitude, latitude)) %>%  # see: https://stackoverflow.com/a/46396386
            arrange(site_id) %>% 
            # mutate(site_id = factor(site_id)) %>%
            {.}
            # check (NOTE: no missing coordinate data in the flat file)
                # range(calepa_reg_sites$latitude)
                # range(calepa_reg_sites$longitude)
        
    # join the CES and HOLC data to the sites data to get the scores/ratings for all sites
        # convert sites to sf
            calepa_reg_sites <- st_as_sf(calepa_reg_sites, 
                                         coords = c('longitude', 'latitude'),
                                         crs = 4269,# 4326 # this is just a guess
                                         # agr = 'constant',
                                         remove = FALSE) %>% 
                st_transform(3310) %>%
                {.}
                st_crs(calepa_reg_sites) # 3310
            
        # get CES polygons
            ces3_poly <- st_read(here('data_processed',
                                      'ces3_poly.gpkg')) %>% 
                select(-c(latitude, longitude))
                # st_crs(ces3_poly) # 4326
        # join CES data to the sites data
            # calepa_reg_sites <- st_join(calepa_reg_sites %>% st_transform(3310), 
            #                             ces3_poly %>% st_transform(3310), 
            #                             join = st_nearest_feature) # need to use nearest_feature because some sites plot between CES polygons or in overlapping areas between 2 polygons
            
            # use the 2010 tiger census file to get the census tract (the CES3 file has more gaps/overlaps)
            # the tiger data comes from: https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2010&layergroup=Census+Tracts
                tiger_2010_tracts <- st_read(here('data_raw', '2010_Census_Tracts', 'tl_2010_06_tract10.shp'))
                    # st_crs(tiger_2010_tracts) # 4269
                tiger_2010_tracts <- tiger_2010_tracts %>% 
                    select(GEOID10, geometry) %>% 
                    mutate(GEOID10 = as.numeric(GEOID10)) %>% 
                    st_transform(3310) %>% 
                    {.}
                tiger_2010_tracts <- tiger_2010_tracts %>% 
                    filter(GEOID10 %in% ces3_poly$census_tract) %>% 
                    {.}
                # join
                    calepa_reg_sites_joined <- st_join(calepa_reg_sites, 
                                                       tiger_2010_tracts, 
                                                       join = st_intersects)
                   # check
                    # sum(duplicated(calepa_reg_sites_joined$site_id)) 7 duplicated sites
                    # sum(is.na(calepa_reg_sites_joined$GEOID10)) # 196 sites not joined 
                # # check what the join looks like using the CES3 polygons
                #     calepa_reg_sites_joined_2 <- st_join(calepa_reg_sites, 
                #                                          ces3_poly, 
                #                                          join = st_intersects)
                #     # nrow(calepa_reg_sites_joined_2) - nrow(calepa_reg_sites) # 584 -- there are a number of duplicates
                #     # sum(duplicated(calepa_reg_sites_joined_2$site_id)) # 584 duplicates
                #     sum(is.na(calepa_reg_sites_joined_2$Census_Tract)) # 1263 sites not joined
            # remove the 7 duplicates
                calepa_reg_sites_joined <- calepa_reg_sites_joined[!duplicated(calepa_reg_sites_joined$site_id),]
            # make sure the site_id field matches
                # tf <- calepa_reg_sites_joined$site_id == calepa_reg_sites$site_id
                # sum(tf)
            # join the CES3 data to the sites data using the census tract identified using the tiger file
                calepa_reg_sites <- left_join(calepa_reg_sites_joined,
                                              ces3_poly %>% st_drop_geometry(), 
                                              by = c('GEOID10' = 'census_tract'),
                                              keep = TRUE)
                calepa_reg_sites <- calepa_reg_sites %>% select(-GEOID10)
            # save the 2010 tiger tracts
                # add the CES data
                    tiger_2010_tracts <- left_join(tiger_2010_tracts, 
                                                   ces3_poly %>% st_drop_geometry(),
                                                   by = c('GEOID10' = 'census_tract'),
                                                   keep = TRUE) %>% 
                        select(-GEOID10)
                st_write(tiger_2010_tracts, 
                         here('data_processed', 'tiger_2010_tracts.gpkg'), 
                         append = FALSE)
            # remove the joined dataset
                rm(list = c('calepa_reg_sites_joined', 'tiger_2010_tracts'))
                gc()
            
                
            # alternate method: get the sites within a certain distance of the CES polygons - not used (and not complete)
                # ces3_poly_buffer <- ces3_poly %>% 
                #     st_transform(3310) %>% # transform to projected coordinate system
                #     st_buffer(dist = 100)
                # sites_in_buffer <- calepa_reg_sites %>% st_transform(crs = st_crs(ces3_poly_buffer))
                # sites_in_buffer <- sites_in_buffer[ces3_poly_buffer,]
                # sites_not_in_buffer <- calepa_reg_sites %>% filter(!(site_id %in% sites_in_buffer$site_id))
                # sites_not_in_buffer %>% 
                #     st_transform(crs = st_crs(ces3_poly_buffer)) %>% 
                #     st_geometry() %>% 
                #     plot()
                # ca_boundary <- st_read(here('data_raw', 'CA_Boundary.gpkg'))
                # ca_boundary %>% 
                #     st_transform(crs = st_crs(ces3_poly_buffer)) %>% 
                #     st_geometry() %>% 
                #     plot(add = TRUE)
            
        
            
        # get HOLC (Redline) polygons
            redline_polygons <- st_read(here('data_processed',
                                             'redline_polygons.gpkg')) %>% 
                st_transform(3310)
        # join redline data to the sites data
            calepa_reg_sites <- st_join(calepa_reg_sites, 
                                        redline_polygons %>% select(-c(area_description_excerpts, holc_link, holc_year)), 
                                        join = st_intersects) # only join sites that fall within HOLC polygons
        # drop the geometry (make into data frame)
            calepa_reg_sites_df <- calepa_reg_sites %>% st_drop_geometry()
    # save the revised sites dataset to csv
        write_csv(x = calepa_reg_sites_df,
                  path = here('data_regulatory_actions',
                              'regulated_sites_processed.csv'))

    # create a filtered dataset with just the sites in the holc mapped areas
        calepa_reg_sites_filter <- calepa_reg_sites[redline_polygons, ]
        calepa_reg_sites_filter_df <- calepa_reg_sites_filter %>% st_drop_geometry()
        # save the revised sites dataset to csv
            write_csv(x = calepa_reg_sites_filter_df,
                      path = here('data_regulatory_actions_filter',
                                  'regulated_sites_processed_filter.csv'))
            
            
            
    # CalEPA regulatory data
        # inspections
            inspections_all_download <- fread('data_regulatory_actions/Evaluations.csv') %>%
                # inspections_all_download <- read_csv('data_regulatory_actions/Evaluations.zip') %>% 
                # inspections_all_download <- read_csv('data_regulatory_actions/Evaluations.csv') %>%
                # inspections_all_download <- vroom('data_regulatory_actions/Evaluations.csv') %>% 
                tibble() %>% 
                clean_names() %>%
                select(-dplyr::ends_with(as.character(0:9))) %>%
                mutate(eval_date = mdy(eval_date)) %>% 
                arrange(site_id, eval_date) %>% 
                # mutate(site_id = factor(site_id, levels = calepa_reg_sites$site_id)) %>%
                {.}
            inspections_summary <- inspections_all_download %>% 
                select(site_id, eval_date, eval_general_type, 
                       eval_division, eval_program, eval_source) %>% 
                mutate(action = 'inspection') %>% 
                rename(action_date = eval_date,
                       type = eval_general_type,
                       division = eval_division,
                       program = eval_program,
                       source = eval_source) %>% 
                relocate(action, .after = action_date) %>% 
                {.}
        # violations
            violations_all_download <- fread('data_regulatory_actions/Violations.csv') %>% #, guess_max = 1000, trim_ws = FALSE) %>%
                # violations_all_download <- read_csv('data_regulatory_actions/Violations.zip') %>% #, guess_max = 1000, trim_ws = FALSE) %>% 
                # violations_all_download <- read_csv('data_regulatory_actions/Violations.csv') %>% #, guess_max = 1000, trim_ws = FALSE) %>%
                # violations_all_download <- vroom('data_regulatory_actions/Violations.csv') %>% #, guess_max = 1000, trim_ws = FALSE) %>% 
                tibble() %>%
                clean_names() %>% 
                select(-dplyr::ends_with(as.character(0:9))) %>%
                mutate(violation_date = mdy(violation_date)) %>% 
                arrange(site_id, violation_date) %>% 
                # mutate(site_id = factor(site_id, levels = calepa_reg_sites$site_id)) %>%
                {.}  
            violations_summary <- violations_all_download %>% 
                select(site_id, violation_date, citation, 
                       violation_division, violation_program, violation_source) %>% 
                mutate(action = 'violation') %>% 
                mutate(site_id = as.integer(site_id)) %>% 
                rename(action_date = violation_date,
                       type = citation,
                       division = violation_division,
                       program = violation_program,
                       source = violation_source) %>% 
                relocate(action, .after = action_date) %>% 
                {.}
        # enforcement actions
            enforcement_all_download <- fread('data_regulatory_actions/EA.csv') %>%
                # enforcement_all_download <- read_csv('data_regulatory_actions/Enforcements.zip') %>% 
                # enforcement_all_download <- read_csv('data_regulatory_actions/EA.csv') %>%
                # enforcement_all_download <- vroom('data_regulatory_actions/EA.csv') %>% 
                tibble() %>%
                clean_names() %>% 
                select(-dplyr::ends_with(as.character(0:9))) %>%
                mutate(enf_action_date = mdy(enf_action_date)) %>% 
                arrange(site_id, enf_action_date) %>% 
                # mutate(site_id = factor(site_id, levels = calepa_reg_sites$site_id)) %>%
                {.}
            enforcement_summary <- enforcement_all_download %>% 
                select(site_id, enf_action_date, enf_action_type, 
                       enf_action_division, enf_action_program, enf_action_source) %>%
                mutate(action = 'enf_action') %>% 
                rename(action_date = enf_action_date,
                       type = enf_action_type,
                       division = enf_action_division,
                       program = enf_action_program,
                       source = enf_action_source) %>% 
                relocate(action, .after = action_date) %>% 
                {.}
            
    # write filtered datasets to csv
        # Inspections
        write_csv(x = inspections_all_download %>% 
                      filter(site_id %in% calepa_reg_sites_filter_df$site_id),
                  path = here('data_regulatory_actions_filter',
                              'Evaluations_filter.csv'))
        # Violations
        write_csv(x = violations_all_download %>% 
                      filter(site_id %in% calepa_reg_sites_filter_df$site_id),
                  path = here('data_regulatory_actions_filter',
                              'Violations_filter.csv'))
        # Enforcement Actions
        write_csv(x = enforcement_all_download %>% 
              filter(site_id %in% calepa_reg_sites_filter_df$site_id),
          path = here('data_regulatory_actions_filter',
                      'EA_filter.csv'))
        # Program Types (Site EI)
        program_types <- fread('data_regulatory_actions/SiteEI.csv') %>% 
            tibble() %>% clean_names() %>% select(-dplyr::ends_with(as.character(0:9)))
        write_csv(x = program_types %>% 
              filter(site_id %in% calepa_reg_sites_filter_df$site_id),
          path = here('data_regulatory_actions_filter',
                      'SiteEI_filter.csv'))

        rm(list = c('inspections_all_download', 'violations_all_download', 
                    'enforcement_all_download', 'program_types'))            
        gc()

# for option 2 below
    reg_actions_all_2 <- bind_rows(inspections_summary, 
                                   violations_summary, 
                                   enforcement_summary)
    
    
    # add fiscal year info to the dataset
        reg_actions_all_2 <- reg_actions_all_2 %>% 
            mutate(eval_fiscal_year = case_when(month(ymd(action_date)) <= 6 ~ 
                                                  paste0(year(ymd(action_date))-1, ' / ', year(ymd(action_date))),
                                              TRUE ~ paste0(year(ymd(action_date)), ' / ', year(ymd(action_date)) + 1)
            )) %>% 
            separate(col = eval_fiscal_year, 
                     into = c('fy_start', 'fy_end'), 
                     sep = ' / ', 
                     remove = FALSE, 
                     convert = TRUE)

            
    # save data to csv
        write_csv(x = reg_actions_all_2,
                  path = here('data_regulatory_actions',
                              'regulatory_actions_processed.csv'))
        
    # summarize by fiscal year
        reg_actions_all_2_fy_summary <- reg_actions_all_2 %>% 
            group_by(site_id, action, eval_fiscal_year, fy_start, fy_end) %>% 
            summarise(count = n())
    
    # save summarized data to csv
        write_csv(x = reg_actions_all_2_fy_summary,
                  path = here('data_regulatory_actions',
                              'regulatory_actions_processed-fy_summary.csv'))
            
    # create filtered datasets with just reg action data for sites in holc mapped areas
        reg_actions_all_2_filter <- reg_actions_all_2 %>% 
            filter(site_id %in% calepa_reg_sites_filter_df$site_id)
        write_csv(x = reg_actions_all_2_filter,
                  path = here('data_regulatory_actions_filter',
                              'regulatory_actions_processed_filter.csv'))
        reg_actions_all_2_fy_summary_filter <- reg_actions_all_2_fy_summary %>% 
            filter(site_id %in% calepa_reg_sites_filter_df$site_id)
        write_csv(x = reg_actions_all_2_fy_summary_filter,
                  path = here('data_regulatory_actions_filter',
                              'regulatory_actions_processed-fy_summary_filter.csv'))
        
        
# # tests (to be done in app)
##############################################################-
## OPTION 1 - all joins prior to loading to app 
#     reg_actions_all <- bind_rows(inspections_summary, violations_summary, enforcement_summary)    
#         # rm(list = c('inspections_summary', 'violations_summary', 'enforcement_summary'))            
#         # gc()
#         
#     reg_actions_all <- reg_actions_all %>% left_join(calepa_reg_sites)
#         #rm('calepa_reg_sites')
#         #gc()
# 
#     # save file to csv
#         write_csv(x = reg_actions_all, 
#                   path = here('data_regulatory_actions',
#                               'regulatory_actions.csv'))
#
#   #### This is the part to be done in the app: ####-
#     {
#         tic()
#         keep_fields <- names(reg_actions_all)[-c(2, 4:7)]
#         reg_actions_summary <- reg_actions_all %>% 
#             # filter(action_date >= as.Date('2018-07-01'), action_date <= as.Date('2019-06-30')) %>% 
#             group_by_at(vars(all_of(keep_fields))) %>% 
#                 #  site_id, action,
#                 #      site_name,
#                 #      address, city, zip_code,
#                 #      county, latitude, longitude,
#                 #      state, data_source) %>%
#             # count() %>% 
#             summarize(count = n()) %>%
#             ungroup() %>% 
#             pivot_wider(names_from = action, values_from = count) %>% # count) %>% 
#             rename(inspections_count = inspection,
#                    violations_count = violation,
#                    enforcement_actions_count = enf_action) %>% 
#             relocate(inspections_count, 
#                      violations_count, 
#                      enforcement_actions_count, 
#                      .after = site_id) %>%
#             # ungroup() %>%
#             {.}
#         # toc()
#         
#         # replace NAs with zeros
#         # tic()
#             reg_actions_summary <- reg_actions_summary %>% 
#                 mutate_if(is.integer, ~replace(., is.na(.), 0))
#         # toc()
#     
#         # find sites in the sites dataset that don't have any regulatory actions and add a zero record to make sure they are included
#             tf <- calepa_reg_sites$site_id %in% reg_actions_summary$site_id
#             sum(tf)
#             sum(!tf)
#             
#             missing_sites <- calepa_reg_sites %>% 
#                 st_drop_geometry() %>% 
#                 filter(!(site_id %in% reg_actions_summary$site_id)) %>% 
#                 mutate(inspections_count = 0,
#                        violations_count = 0,
#                        enforcement_actions_count = 0)
#             
#         # bind the sites with no reg actions to the dataset (this is the dataset to use)
#             reg_actions_revised <- bind_rows(reg_actions_summary, missing_sites) %>% 
#                 arrange(site_id)
#                 # View(reg_actions_revised)
#         toc()
#     }
#     
#     # summaries
#     dim(reg_actions_revised) # 303348     90
#     dim(calepa_reg_sites)    # 303348     87
#     # time: 65.28 sec elapsed
    
    
#############################################################-
# Option 2 - summarize inspections before loading to app, then join to sites (with CES and Redline info already added) in app

    # !!!!!!!!! JUST FOR TESTING !!!!!!!!!!!!!!!!!!
    # This is the part to be done in the app:
    {
        tic()
        reg_actions_summary_2 <- reg_actions_all_2 %>% 
            filter(action_date >= as.Date('2018-07-01'), action_date <= as.Date('2019-06-30')) %>%
            group_by(site_id, action) %>% 
            # count() %>% 
            summarize(count = n()) %>%
            ungroup() %>% 
            pivot_wider(names_from = action, 
                        values_from = count) %>% # count) %>% 
            rename(inspections_count = inspection,
                   violations_count = violation,
                   enforcement_actions_count = enf_action) %>% 
            relocate(inspections_count, 
                     violations_count, 
                     enforcement_actions_count, 
                     .after = site_id) %>%
            # ungroup() %>%
            {.}
        
        # replace NAs with zeros
        reg_actions_summary_2 <- reg_actions_summary_2 %>% 
            mutate_if(is.integer, ~replace(., is.na(.), 0))
        
        # join the site info to the summarized regulatory actions
        reg_actions_summary_2 <- left_join(reg_actions_summary_2, 
                                           calepa_reg_sites)
            
        # find sites in the sites dataset that don't have any regulatory actions and add a zero record to make sure they are included
        tf_2 <- calepa_reg_sites$site_id %in% reg_actions_summary_2$site_id
        sum(tf_2)
        sum(!tf_2)
        
        missing_sites_2 <- calepa_reg_sites %>% 
            filter(!(site_id %in% reg_actions_summary_2$site_id)) %>% 
            mutate(inspections_count = 0,
                   violations_count = 0,
                   enforcement_actions_count = 0)
        
        # bind the sites with no reg actions to the dataset (this is the dataset to use)
        reg_actions_revised_2 <- bind_rows(reg_actions_summary_2, 
                                           missing_sites_2) %>% 
            arrange(site_id)
            # View(reg_actions_revised)
        
        toc()
    }
        dim(reg_actions_revised_2) # 303348     90
        dim(calepa_reg_sites)    # 303348     87
        # time: 35.69 sec elapsed
