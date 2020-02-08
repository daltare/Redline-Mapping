# Load Packages 
    # General data analysis and transformation
        library(readr)
        library(readxl)
        library(dplyr)
        library(janitor)
        library(pryr)
    # mapping and geospatial data analysis/transformation
        library(sf)
        library(rmapshaper)


# Redlined cities -----------------------------------------------------------------------------------------------------------
    # create a list of the redlined cities (including source URLs)
        redline_cities <- list('Fresno' = list(url = 'https://dsl.richmond.edu/panorama/redlining/static/downloads/shapefiles/CAFresno1936.zip',
                                               zipfile = 'CAFresno1936'),
                               'LosAngeles' = list(url = 'https://dsl.richmond.edu/panorama/redlining/static/downloads/shapefiles/CALosAngeles1939.zip',
                                         zipfile = 'CALosAngeles1939'),
                               'Oakland' = list(url = 'https://dsl.richmond.edu/panorama/redlining/static/downloads/shapefiles/CAOakland1937.zip',
                                         zipfile = 'CAOakland1937.zip'),
                               'Sacramento' = list(url = 'https://dsl.richmond.edu/panorama/redlining/static/downloads/shapefiles/CASacramento1937.zip',
                                         zipfile = 'CASacramento1937'),
                               'SanDiego' = list(url = 'https://dsl.richmond.edu/panorama/redlining/static/downloads/shapefiles/CASanDiego1938.zip',
                                         zipfile = 'CASanDiego1938'),
                               'SanFrancisco' = list(url = 'https://dsl.richmond.edu/panorama/redlining/static/downloads/shapefiles/CASanFrancisco1937.zip',
                                         zipfile = 'CASanFrancisco1937'),
                               'SanJose' = list(url = 'https://dsl.richmond.edu/panorama/redlining/static/downloads/shapefiles/CASanJose1937.zip',
                                         zipfile = 'CASanJose1937'),
                               'Stockton' = list(url = 'https://dsl.richmond.edu/panorama/redlining/static/downloads/shapefiles/CAStockton1938.zip',
                                         zipfile = 'CAStockton1938')
                               )
    # download the shapefiles and read into R
        redline_poly_list <- list()
        for (i in seq(length(redline_cities))) {
            # download the files
                temp <- tempfile()
                download.file(url = redline_cities[[i]]$url,
                              destfile = temp)
                unzip(zipfile = temp, exdir = paste0('./data_raw/RedlineMaps/', names(redline_cities[i])))
            # read data into R
                redline_poly_list[[i]] <- st_read(paste0('./data_raw/RedlineMaps/',
                                                         names(redline_cities[i]),
                                                         '/',
                                                         'cartodb-query',
                                                         '.shp')
                                                  )
                redline_poly_list[[i]] <- redline_poly_list[[i]] %>% mutate('city' = names(redline_cities[i]))
        }
    # bind the redline polygons together into one data frame
        redline_polygons <- ''
        for (i in seq(length(redline_poly_list))) {
            if (i == 1) {
                redline_polygons <- as.data.frame(redline_poly_list[[1]])
            } else {
                redline_polygons <- bind_rows(redline_polygons, as.data.frame(redline_poly_list[[i]]))
            }
        }
        redline_polygons <- redline_polygons %>% mutate(city = case_when(city == 'LosAngeles' ~ 'Los Angeles',
                                                                         city == 'SanDiego' ~ 'San Diego',
                                                                         city == 'SanFrancisco' ~ 'San Francisco',
                                                                         city == 'SanJose' ~ 'San Jose',
                                                                         TRUE ~ city))
        redline_polygons <- st_as_sf(redline_polygons)
        st_crs(redline_polygons) <- st_crs(redline_poly_list[[1]])
        
    # save to RDS file
        saveRDS(object = redline_polygons, file = 'data_prepared/redline_polygons.RDS')

        
        

# Regional Board Office Boundaries (from saved internal waterboard dataset)--------------------------------------------------
        # NOTE: NO LONGER USED -- INSTEAD USING WATERBOARDS GIS WEB SERVICES (SEE APPLICATION SCRIPT): http://gispublic.waterboards.ca.gov/arcgis/rest/services/Administrative/RB_OfficeAreas/MapServer/0
        # # read data
        #     rb_boundary <- st_read('data_raw/Regional_Board_Office_Boundaries_ModifiedR6_6A_6B/Regional_Board_Offices.shp') %>% 
        #         st_transform(4326) 
        # # save to RDS file
        #     saveRDS(object = rb_boundary, file = 'data_prepared/Regional_Board_Offices.RDS')




# CalEnviroScreen -----------------------------------------------------------------------------------------------------------
    # Source: https://oehha.ca.gov/calenviroscreen/report/calenviroscreen-30
    # shapefile at: https://oehha.ca.gov/media/downloads/calenviroscreen/document/ces3shp.zip
    # download the file 
        temp <- tempfile()
        download.file(url = 'https://oehha.ca.gov/media/downloads/calenviroscreen/document/ces3shp.zip',
                      destfile = temp)
        unzip(zipfile = temp, exdir = './data_raw/CalEnviroScreen3')
    # read data into R
        ces_3_poly <- st_read('data_raw/CalEnviroScreen3/CESJune2018Update_SHP/CES3June2018Update.shp') %>% 
            st_transform(4326)
    # fix some of the column names to make more readable
        ces_3_names <- read_csv('data_raw/ces_names.csv') %>% # manually prepared this file to make more descriptive names for the fields
            mutate(ces_variable = make_clean_names(name, 'parsed'))
        ces_3_names_cleaned <- c(ces_3_names %>% pull(ces_variable), 'geometry')
        names(ces_3_poly) <- ces_3_names_cleaned
    # save to RDS file
        saveRDS(object = ces_3_poly, file = 'data_prepared/ces_3_poly.RDS')
        saveRDS(object = ces_3_names, file = 'data_prepared/ces_3_names.RDS')
    # # SIMPLIFY
    #     ces_3_poly_simplify <- rmapshaper::ms_simplify(ces_3_poly)
    #     # save to RDS file
    #         saveRDS(object = ces_3_poly_simplify, file = 'data_prepared/ces_3_poly_simplify.RDS')

        


        
# Water Supplier Service Areas ----------------------------------------------------------------------------------------------
    # NOTE: NO LONGER USED -- INSTEAD USING WATERBOARDS GIS WEB SERVICES (SEE APPLICATION SCRIPT): https://gispublic.waterboards.ca.gov/portalserver/rest/services/Hosted/California_Drinking_Water_Service_Areas/FeatureServer/0/
    # # File Source: https://gispublic.waterboards.ca.gov/portal/home/item.html?id=bb21fcee16ea4af2a8d57aa39447aa9c#overview
    #     # NOTE: Followed link to 'Open in ArcGIS Desktop' and saved shapefile from there
    # # read data
    # service_areas <- st_read("data_raw/Service_Area_Boundaries/Service_Area_Boundaries_2019-12-13.shp") %>% 
    #     st_transform(4326)
    # # save to RDS file
    #     # saveRDS(object = service_areas, file = 'data_prepared/Service_Area_Boundaries_2019-12-13.RDS')
    # # SIMPLIFY
    #     service_areas_simplify <- rmapshaper::ms_simplify(service_areas)
    #     # save to RDS file
    #         saveRDS(object = service_areas_simplify, file = 'data_prepared/service_areas_simplify.RDS')




# California Counties (from saved internal waterboard dataset) --------------------------------------------------------------
    # # read data into R
    #     counties_poly <- st_read('data_raw/CA_Counties/WBGIS_Counties.shp') %>% 
    #         st_transform(4326)
    # # save to RDS file
    #     saveRDS(object = counties_poly, file = 'data_prepared/CA_Counties.RDS')
    # # SIMPLIFY
    #     # counties_poly_simplify <- rmapshaper::ms_simplify(counties_poly)
    #     # save to RDS file
    #         # saveRDS(object = counties_poly_simplify, file = 'data_prepared/CA_Counties_simplify.RDS')




# 303d Listed Waterbodies ---------------------------------------------------------------------------------------------------
    # 303d Polygons ---
        # download the files 
            temp <- tempfile()
            download.file(url = 'https://gispublic.waterboards.ca.gov/webmap/303d_2014_2016/files/IR_1416_Impaired_Polys.zip',
                          destfile = temp, method = 'curl')
            unzip(zipfile = temp, exdir = './data_raw/2014_2016_303d_Polygons_Final', junkpaths = TRUE)
        # Read the shapefile into R
            impaired_303d_poly <- st_read('data_raw/2014_2016_303d_Polygons_Final/IR_1416_Impaired_Polys.shp') %>% 
                st_transform(4326)
        # clean names
            impaired_303d_poly <- clean_names(impaired_303d_poly)
        # SIMPLIFY
            #     # impaired_303d_poly_simplify <- rmapshaper::ms_simplify(impaired_303d_poly)
            #     # save to RDS file
            #         # saveRDS(object = impaired_303d_poly_simplify, file = 'data/impaired_303d_poly_simplify.RDS')

    # 303d Lines ---
        # download the files
            temp <- tempfile()
            download.file(url = 'https://gispublic.waterboards.ca.gov/webmap/303d_2014_2016/files/IR_1416_Impaired_Lines.zip',
                          destfile = temp, method = 'curl')
            unzip(zipfile = temp, exdir = './data_raw/2014_2016_303d_Lines_Final', junkpaths = TRUE)
        # Read the shapefile into R
            impaired_303d_lines <- st_read('data_raw/2014_2016_303d_Lines_Final/IR_1416_Impaired_Lines.shp') %>% 
                st_transform(4326)
        # clean names
            impaired_303d_lines <- clean_names(impaired_303d_lines)
        # SIMPLIFY AND REMOVE REGION 1 LINES (TOO MUCH DATA USED FOR R1 LINES)
            impaired_303d_lines_simplify <- rmapshaper::ms_simplify(impaired_303d_lines)
            impaired_303d_lines_simplify_R1removed <- impaired_303d_lines_simplify %>% filter(region_num != 1)
            # check file sizes
                # pryr::object_size(impaired_303d_lines) # 91.3 MB
                # pryr::object_size(impaired_303d_lines_simplify) # 40.6 MB
                # pryr::object_size(impaired_303d_lines_simplify_R1removed) # 2.15 MB

    # 303d Waterbodies Pollutant Information (table) ---
        # access and transform the 303d tabular data
                # download the files
                    download.file(url = 'https://gispublic.waterboards.ca.gov/webmap/303d_2014_2016/files/2014_2016_303d_SWRCB_Approved_List_no_sources_final.xls',
                                  destfile = 'data_raw/2014_2016_303d_SWRCB_Approved_List_no_sources_final.xls',
                                  method = 'curl')
                impaired_pollutants_1 <- read_excel('data_raw/2014_2016_303d_SWRCB_Approved_List_no_sources_final.xls',
                                                  sheet = '303(d) List no srcs')
                impaired_pollutants_1 <- clean_names(impaired_pollutants_1)
                # Create a column for comments that also includes the pollutant (only for rows where there is a comment)
                    impaired_pollutants_1 <- impaired_pollutants_1 %>%
                        mutate(pollutant_comment = if_else(!is.na(comments_included_on_303_d_list),
                                                           paste0(pollutant, ': ',comments_included_on_303_d_list ),
                                                           'NA'))
                    impaired_pollutants_1$pollutant_comment[impaired_pollutants_1$pollutant_comment == 'NA'] <- NA # replace text NAs from formual above with actual NAs
                # Create a list of the unique IDs
                    impaired_ids_1 <- impaired_pollutants_1 %>% distinct(wbid)
                # for each ID in the list, append the list of pollutants associated with that ID, and the comments associated with those pollutants (if any)
                    # initialize the new columns (this just helps to prevent a warning message in the next step)
                    impaired_ids_1 <- impaired_ids_1 %>% mutate(pollutant = '',
                                                            comments = '')
                    for (i in seq(nrow(impaired_ids_1))){
                        temp <- impaired_pollutants_1 %>% filter(wbid == impaired_ids_1$wbid[i])
                        impaired_ids_1$pollutant[i] <- paste0(temp$pollutant, collapse = ' | ')
                        temp2 <- impaired_pollutants_1 %>% filter(wbid == impaired_ids_1$wbid[i] & !is.na(pollutant_comment))
                        impaired_ids_1$comments[i] <- paste0(temp2$pollutant_comment, collapse = ' | ')
                    }

    # 303d Pollutant Potential Sources (table 2) ---
            # access and transform the 303d tabular data
                # download the files
                    download.file(url = 'https://gispublic.waterboards.ca.gov/webmap/303d_2014_2016/files/2014_2016_303d_SWRCB_Approved_List_with_sources_final.xls',
                                  destfile = 'data_raw/2014_2016_303d_SWRCB_Approved_List_with_sources_final.xls',
                                  method = 'curl')
                # read the data
                    impaired_pollutants_2 <- read_excel('data_raw/2014_2016_303d_SWRCB_Approved_List_with_sources_final.xls',
                                                      sheet = '303(d) List with srcs')
                    impaired_pollutants_2 <- clean_names(impaired_pollutants_2)
                # Create a column for source that also includes the pollutant (only for rows where there is a source given)
                    impaired_pollutants_2 <- impaired_pollutants_2 %>%
                        mutate(source = if_else(potential_sources != 'Source Unknown',
                                                       paste0(Hmisc::capitalize(tolower(source_category)), ' (', potential_sources, ')'),
                                                       'NA'))
                    impaired_pollutants_2$source[impaired_pollutants_2$source == 'NA'] <- NA # replace text NAs from formual above with actual NAs
                # Create a list of the unique IDs
                    impaired_ids_source_2 <- impaired_pollutants_2 %>%
                        distinct(wbid)
                    impaired_ids_pollutant_2 <- impaired_pollutants_2 %>%
                        select(wbid, pollutant) %>%
                        distinct()
                # for each ID in the list of ID & pollutant combinations, append the list of sources associated with that combination of ID and pollutant (if any)
                    # initialize the new columns (this just helps to prevent a warning message in the next step)
                        impaired_ids_pollutant_2 <- impaired_ids_pollutant_2 %>% mutate(source = '')
                    for (i in seq(nrow(impaired_ids_pollutant_2))){
                        temp <- impaired_pollutants_2 %>%
                            filter(wbid == impaired_ids_pollutant_2$wbid[i] & pollutant == impaired_ids_pollutant_2$pollutant[i] & !is.na(source))
                        impaired_ids_pollutant_2$source[i] <- paste0(temp$source, collapse = '; ')
                    }
                    impaired_ids_pollutant_2 <- impaired_ids_pollutant_2 %>%
                        mutate(pollutant_source = if_else(source != '',
                                                          paste0(pollutant, ': ', source),
                                                          'NA'))
                    impaired_ids_pollutant_2$pollutant_source[impaired_ids_pollutant_2$pollutant_source == 'NA'] <- NA
                # for each ID in the list of WDIDs, append the list of sources for all pollutants associated with that ID (if any)
                    # initialize the new columns (this just helps to prevent a warning message in the next step)
                        impaired_ids_source_2 <- impaired_ids_source_2 %>% mutate(pollutant = '', sources= '')
                    for (i in seq(nrow(impaired_ids_source_2))){
                        temp2 <- impaired_ids_pollutant_2 %>%
                            filter(wbid == impaired_ids_source_2$wbid[i])
                        impaired_ids_source_2$pollutant[i] <- paste0(temp2$pollutant, collapse = ' | ')
                        temp3 <- impaired_ids_pollutant_2 %>%
                            filter(wbid == impaired_ids_source_2$wbid[i] & !is.na(pollutant_source))
                        impaired_ids_source_2$sources[i] <- paste0(temp3$pollutant_source, collapse = ' | ')
                    }

    # join the pollution source info to the list of pollutants and comments
            impaired_303d_list <- impaired_ids_1 %>% left_join(impaired_ids_source_2, by = c('wbid', 'pollutant'))
            # make blanks into NAs
                impaired_303d_list$comments[impaired_303d_list$comments == ''] <- NA
                impaired_303d_list$sources[impaired_303d_list$sources == ''] <- NA

    # join the 303d information to the polygons and lines shapefile datasets, by WBID
        impaired_303d_poly <- st_as_sf(impaired_303d_poly %>% left_join(impaired_303d_list, by = 'wbid'))
        impaired_303d_lines_simplify_R1removed <- st_as_sf(impaired_303d_lines_simplify_R1removed %>% left_join(impaired_303d_list, by = 'wbid'))

    # write 303d data to an RDS file
        saveRDS(object = impaired_303d_poly, file = 'data_prepared/impaired_303d_poly.RDS')
        saveRDS(object = impaired_303d_lines_simplify_R1removed, file = 'data_prepared/impaired_303d_lines_simplify_R1removed.RDS')




# Regulated facilities info (source: https://siteportal.calepa.ca.gov/nsite/map/export) ---------------------------------
    # Regulated Site Programs
        cal_epa_sites_programs <- read_csv('data_raw/CalEPA_RegulatedSites/SiteEI.csv') %>%
            select(-X10) %>%
            select(SiteID, EI_Description) %>% 
            distinct()
        # write to RDS file
            saveRDS(object = cal_epa_sites_programs, 
                    file = 'data_prepared/cal_epa_sites_programs.RDS')
    # Violations
        cal_epa_sites_violations_count <- read_csv('data_raw/CalEPA_RegulatedSites/Violations.csv') %>% 
            select(-X10) %>% 
            group_by(SiteID) %>% 
            summarize(violations_records = n())
        # write to RDS file
            saveRDS(object = cal_epa_sites_violations_count, 
                    file = 'data_prepared/cal_epa_sites_violations_count.RDS')
    # Enforcement Actions
        cal_epa_sites_enforcement_count <- read_csv('data_raw/CalEPA_RegulatedSites/EA.csv') %>% 
            select(-X13) %>% 
            group_by(SiteID) %>% 
            summarize(enforcement_records = n())
        # write to RDS file
            saveRDS(object = cal_epa_sites_enforcement_count, 
                    file = 'data_prepared/cal_epa_sites_enforcement_count.RDS')
