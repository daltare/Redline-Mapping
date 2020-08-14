# Load Packages 
    # General data analysis and transformation
        library(readr)
        library(data.table)
        library(readxl)
        library(dplyr)
        library(janitor)
        library(pryr)
        # library(lobstr)
        library(skimr)
        library(tidylog)
        library(lubridate)
        library(tidyr)
    # mapping and geospatial data analysis/transformation
        library(sf)
        library(rmapshaper)
        library(geojsonsf)
    # workflow
        library(here)


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
                unzip(zipfile = temp,
                      exdir = here('data_raw',
                                   'Redline_Maps',
                                   names(redline_cities[i])))
            # read data into R
                redline_poly_list[[i]] <- st_read(here('data_raw', 
                                                       'Redline_Maps',
                                                       names(redline_cities[i]),
                                                       'cartodb-query.shp'))
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
        st_crs(redline_polygons) <- st_crs(redline_poly_list[[1]]) # make sure the CRS is defined (4326)
    # add extra information to the redline polygons
        # some polygons are missing holc_id's and can't be joined - separate these out
            redline_polygons_missing_id <- redline_polygons %>% filter(is.na(holc_id))
            redline_polygons <- redline_polygons %>% filter(!is.na(holc_id))
        # links to holc forms for each holc polygon
            holc_form_links <- read_csv(here('data_raw', 'redline-polygons-list.csv')) %>% 
                select(holc_id, city, link, year) %>% 
                filter(!is.na(holc_id))
            redline_polygons <- redline_polygons %>% 
                left_join(holc_form_links, 
                          by = c('city', 'holc_id'))
        # area description for each holc polygon
            area_descriptions <- read_csv(here('area-descriptions', 
                                               '_redline_combined_area-descriptions.csv'))
            redline_polygons <- redline_polygons %>% 
                left_join(area_descriptions %>% select(city, holc_id, area_description_excerpts), 
                          by = c('city', 'holc_id'))
        # add back in the polygons with missing id's
            redline_polygons <- bind_rows(redline_polygons, redline_polygons_missing_id)
            
    # rename the columns to add 'holc_' prefix
        redline_polygons <- redline_polygons %>% 
            rename(holc_name = name,
                   holc_city = city,
                   holc_link = link,
                   holc_year = year)
    # fix self-intersecting polygons (if needed)
        crs_redline <- st_crs(redline_polygons)
        redline_polygons <- redline_polygons %>% st_transform(3310)
        if (sum(!st_is_valid(redline_polygons)) > 0) {
            redline_polygons <- sf::st_buffer(redline_polygons, dist = 0)
        }
        redline_polygons <- redline_polygons %>% st_transform(crs_redline)
    # save to geopackage file
        st_write(obj = redline_polygons, 
                 here('data_processed', 'redline_polygons.gpkg'),
                 append = FALSE) 
    # # simplify and save simplified version
    #     # simplify
    #         redline_polygons_simplify <- redline_polygons %>% 
    #             ms_simplify(keep = 0.05, keep_shapes = TRUE, snap = TRUE)
    #     # save to geopackage file
    #         st_write(obj = redline_polygons_simplify, 
    #                  here('data_processed', 
    #                       'redline_polygons_simplified.gpkg'),
    #                  append = FALSE)
        
        

# Regional Board Office Boundaries (from saved internal waterboard dataset)--------------------------------------------------
        # NOTE: may also use waterboards GIS web services (see application code file): http://gispublic.waterboards.ca.gov/arcgis/rest/services/Administrative/RB_OfficeAreas/MapServer/0
        # read data
            url_rb_office_areas <- paste0("http://gispublic.waterboards.ca.gov/arcgis/rest/services/Administrative/RB_OfficeAreas/MapServer/0/query?where=1=1",
                                              "&outFields=*&outSR=4326&f=geojson") 
            rb_boundary <- read_lines(url_rb_office_areas) %>% 
                geojson_sf()
            # st_crs(rb_boundary) # 4326
        # write the raw data to geopackage file
            st_write(obj = rb_boundary, 
                     here('data_raw', 'RB_Office_Boundaries',
                          'rb_office_boundaries.gpkg'),
                     append = FALSE)
            object_size(rb_boundary)
        # simplify
            rb_boundary_simplify <- rb_boundary %>% 
                # ms_simplify(keep = 0.05, keep_shapes = TRUE, snap = TRUE) %>% 
                ms_simplify(keep = 0.2, keep_shapes = TRUE, snap = TRUE) %>% 
                {.}
            object_size(rb_boundary_simplify)
        # save to geopackage file
            st_write(obj = rb_boundary_simplify, 
                     here('data_processed', 
                          'rb_boundary_simplified.gpkg'), 
                     append = FALSE)




# # CalEnviroScreen -----------------------------------------------------------------------------------------------------------
#     # Source: https://oehha.ca.gov/calenviroscreen/report/calenviroscreen-30
#     # shapefile at: https://oehha.ca.gov/media/downloads/calenviroscreen/document/ces3shp.zip
    # download the file
        temp <- tempfile()
        download.file(url = 'https://oehha.ca.gov/media/downloads/calenviroscreen/document/ces3shp.zip',
                      destfile = temp)
        unzip(zipfile = temp,
              exdir = here('data_raw',
                           'CalEnviroScreen3'))
    # read data into R
        ces3_poly <- st_read(here('data_raw',
                                   'CalEnviroScreen3',
                                   'CESJune2018Update_SHP',
                                   'CES3June2018Update.shp')) #%>%
            st_crs(ces3_poly) # 3310
        # Fix self-intersecting polygons
        if (sum(!st_is_valid(ces3_poly)) > 0) {
            ces3_poly <- sf::st_buffer(ces3_poly, dist = 0)
        }
    # revise column names
        ces3_names <- read_csv('data_processed/ces_names.csv') %>% # manually processed this file to make more descriptive names for the fields
            mutate(ces_variable = make_clean_names(name)) #, 'parsed'))
        ces3_names_cleaned <- c(ces3_names %>% pull(ces_variable), 'geometry')
        names(ces3_poly) <- ces3_names_cleaned
    # save to geopackage file
        st_write(obj = ces3_poly, 
                 here('data_processed', 'ces3_poly.gpkg'),
                 append = FALSE)
    # simplify
        ces3_poly_simplify <- ces3_poly %>% 
                # ms_simplify(keep = 0.05, keep_shapes = TRUE, snap = TRUE)
                ms_simplify(keep = 0.3)#, keep_shapes = TRUE, snap = TRUE)
        # Fix self-intersecting polygons
        if (sum(!st_is_valid(ces3_poly_simplify)) > 0) {
            ces3_poly_simplify <- sf::st_buffer(ces3_poly_simplify, dist = 0)
        }
        # check
            # object_size(ces3_poly_simplify)
            # object_size(ces3_poly)
            # mapview::mapview(ces3_poly_simplify %>% filter(Nearby_City == 'Sacramento'))
    # save simplified version to geopackage file
        st_write(obj = ces3_poly_simplify, 
                 here('data_processed', 'ces3_poly_simplified.gpkg'),
                 append = FALSE)
        
    # create filtered dataset with just CES3 polygons in or near holc mapped areas
        redlined_cities_list <- c("Fresno", "Los Angeles", "Oakland", 
                                  "Sacramento", "San Diego", "San Francisco", 
                                  "San Jose", "Stockton")
        ces3_poly_filtered <- ces3_poly %>% slice(0)
        redline_polygons <- redline_polygons %>% st_transform(3310)
        for (city in redlined_cities_list) {
            hull <- st_convex_hull(st_union(redline_polygons %>% filter(holc_city == city)))
            ifelse(city == 'Fresno', 
                   hulls <- st_sf(hull) %>% mutate(city = city), 
                   hulls <- bind_rows(hulls, st_sf(hull) %>% mutate(city = city)))
            ces3_poly_filtered <- bind_rows(ces3_poly_filtered, ces3_poly[hull, ])
        }
        st_write(obj = ces3_poly_filtered, 
                 here('data_processed', 'ces3_poly_filtered.gpkg'),
                 append = FALSE)
        st_write(obj = hulls, 
                 here('data_processed', 'holc_map_hulls.gpkg'),
                 append = FALSE)
        

        
# Water Supplier Service Areas ----------------------------------------------------------------------------------------------
    # NOTE: may also use waterboards GIS web services (see application code file): 
        # Available at: https://gispublic.waterboards.ca.gov/portalserver/rest/services/Drinking_Water/California_Drinking_Water_System_Area_Boundaries/FeatureServer/0/query?outFields=*&where=1%3D1
        # See also: https://gispublic.waterboards.ca.gov/portal/home/item.html?id=fbba842bf134497c9d611ad506ec48cc#overview
            # NOTE: at site above, can follow link to 'Open in ArcGIS Desktop' and save shapefile from there
    # read data
        service_areas_base_url <- 'https://gispublic.waterboards.ca.gov/portalserver/rest/services/Drinking_Water/California_Drinking_Water_System_Area_Boundaries/FeatureServer/0/'
        service_areas_url <- paste0(service_areas_base_url,
                                    'query?where=1%3D1', 
                                    '&outFields=*',
                                    '&outSR=4326',
                                    '&f=geojson')
        service_areas <- read_lines(service_areas_url) %>% geojson_sf()
            # st_crs(service_areas) # 4326
    # write the raw data to geopackage file
        st_write(obj = service_areas, 
                 here('data_raw', 'Drinking_Water_Service_Areas',
                      'drinking_water_service_areas.gpkg'),
                 append = FALSE)
        object_size(service_areas)
    # simplify
        service_areas_simplify <- service_areas %>% 
            # ms_simplify(keep = 0.05, keep_shapes = TRUE, snap = TRUE) %>%
            ms_simplify(keep = 0.2, keep_shapes = TRUE, snap = TRUE) %>%
            {.}
        object_size(service_areas_simplify)
    # save to geopackage file
        st_write(obj = service_areas_simplify, 
                 here('data_processed', 
                      'drinking_water_service_areas_simplified.gpkg'),
                 append = FALSE)



# California Counties (from saved internal waterboard dataset) --------------------------------------------------------------
    # read data into R
        counties_poly <- st_read(here('data_raw', 'CA_Counties', 'WBGIS_Counties.shp')) 
            st_crs(counties_poly) # 3310
    # save to RDS file
            st_write(counties_poly, here('data_processed', 'CA_Counties.gpkg'))
    # SIMPLIFY
        # counties_poly_simplify <- rmapshaper::ms_simplify(counties_poly)




# 303d Listed Waterbodies ---------------------------------------------------------------------------------------------------
    # 303d Polygons ---
        # download the files 
            temp <- tempfile()
            download.file(url = 'https://gispublic.waterboards.ca.gov/webmap/303d_2014_2016/files/IR_1416_Impaired_Polys.zip',
                          destfile = temp, 
                          method = 'curl')
            unzip(zipfile = temp, 
                  exdir = here('data_raw', 
                               '2014_2016_303d_Polygons_Final'), 
                  junkpaths = TRUE)
        # Read the shapefile into R
            impaired_303d_poly <- st_read(here('data_raw', 
                                               '2014_2016_303d_Polygons_Final', 
                                               'IR_1416_Impaired_Polys.shp')) 
                st_crs(impaired_303d_poly) # 3310
                object_size(impaired_303d_poly)
        # clean names
            impaired_303d_poly <- clean_names(impaired_303d_poly)
        # simplify
            impaired_303d_poly_simplified <- impaired_303d_poly %>% 
                # ms_simplify(keep = 0.05, keep_shapes = TRUE, snap = TRUE)
                ms_simplify(keep = 0.2, keep_shapes = TRUE, snap = TRUE)
            object_size(impaired_303d_poly_simplified)

    # 303d Lines ---
        # download the files
            temp <- tempfile()
            download.file(url = 'https://gispublic.waterboards.ca.gov/webmap/303d_2014_2016/files/IR_1416_Impaired_Lines.zip',
                          destfile = temp, method = 'curl')
            unzip(zipfile = temp, exdir = here('data_raw', 
                                               '2014_2016_303d_Lines_Final'), 
                  junkpaths = TRUE)
        # Read the shapefile into R
            impaired_303d_lines <- st_read(here('data_raw', 
                                                '2014_2016_303d_Lines_Final', 
                                                'IR_1416_Impaired_Lines.shp'))
            st_crs(impaired_303d_lines) # 3310
        # clean names
            impaired_303d_lines <- clean_names(impaired_303d_lines)
        # SIMPLIFY AND REMOVE REGION 1 LINES (TOO MUCH DATA USED FOR R1 LINES)
            impaired_303d_lines_R1removed <- impaired_303d_lines %>% 
                filter(region_num != 1)
            object_size(impaired_303d_lines_R1removed)
            impaired_303d_lines_R1removed_simplify <- impaired_303d_lines_R1removed %>% 
                # ms_simplify(keep = 0.05, keep_shapes = TRUE, snap = TRUE)
                ms_simplify(keep = 0.2, keep_shapes = TRUE, snap = TRUE)
            object_size(impaired_303d_lines_R1removed_simplify)
            # impaired_303d_lines_simplify_R1removed <- impaired_303d_lines_simplify %>% filter(region_num != 1)
            # check file sizes
                # object_size(impaired_303d_lines) # 91.3 MB
                # object_size(impaired_303d_lines_R1removed) # 6.67 MB
                # object_size(impaired_303d_lines_R1removed_simplify) # 1.63 MB

    # 303d Waterbodies Pollutant Information (table) ---
        # access and transform the 303d tabular data
                # download the files
                    download.file(url = 'https://gispublic.waterboards.ca.gov/webmap/303d_2014_2016/files/2014_2016_303d_SWRCB_Approved_List_no_sources_final.xls',
                                  destfile = here('data_raw', 
                                                  '2014_2016_303d_SWRCB_Approved_List_no_sources_final.xls'),
                                  method = 'curl')
                impaired_pollutants_1 <- read_excel(here('data_raw', 
                                                         '2014_2016_303d_SWRCB_Approved_List_no_sources_final.xls'),
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
                                  destfile = here('data_raw', 
                                                  '2014_2016_303d_SWRCB_Approved_List_with_sources_final.xls'),
                                  method = 'curl')
                # read the data
                    impaired_pollutants_2 <- read_excel(here('data_raw', 
                                                             '2014_2016_303d_SWRCB_Approved_List_with_sources_final.xls'),
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
                        impaired_ids_source_2 <- impaired_ids_source_2 %>% 
                            mutate(pollutant = '', sources= '')
                    for (i in seq(nrow(impaired_ids_source_2))){
                        temp2 <- impaired_ids_pollutant_2 %>%
                            filter(wbid == impaired_ids_source_2$wbid[i])
                        impaired_ids_source_2$pollutant[i] <- paste0(temp2$pollutant, collapse = ' | ')
                        temp3 <- impaired_ids_pollutant_2 %>%
                            filter(wbid == impaired_ids_source_2$wbid[i] & !is.na(pollutant_source))
                        impaired_ids_source_2$sources[i] <- paste0(temp3$pollutant_source, collapse = ' | ')
                    }

    # join the pollution source info to the list of pollutants and comments
            impaired_303d_list <- impaired_ids_1 %>% 
                left_join(impaired_ids_source_2, by = c('wbid', 'pollutant'))
            # make blanks into NAs
                impaired_303d_list$comments[impaired_303d_list$comments == ''] <- NA
                impaired_303d_list$sources[impaired_303d_list$sources == ''] <- NA

    # join the 303d information to the polygons and lines shapefile datasets, by WBID
        impaired_303d_poly_simplified <- st_as_sf(impaired_303d_poly_simplified %>% 
                                           left_join(impaired_303d_list, by = 'wbid'))
        impaired_303d_lines_R1removed_simplify <- st_as_sf(impaired_303d_lines_R1removed_simplify %>% 
                                                               left_join(impaired_303d_list, by = 'wbid'))

    # write to geopackage
        st_write(obj = impaired_303d_poly_simplified, 
                 here('data_processed', '303d_polygons_simplified.gpkg'),
                 append = FALSE) 
        st_write(obj = impaired_303d_lines_R1removed_simplify, 
                 here('data_processed', '303d_lines_R1removed_simplified.gpkg'),
                 append = FALSE)




# # Regulated facilities info (source: https://siteportal.calepa.ca.gov/nsite/map/export) ------------
#     # Regulated Site Programs ----------------------------------------------------------------------
#         cal_epa_sites_programs <- read_csv(here('data_raw',
#                                                 'CalEPA_RegulatedSites',
#                                                 'Site Regulated Programs.csv'), # SiteEI.csv
#                                            guess_max = 100000) %>%
#             select(-X10) %>%
#             select(SiteID, EI_Description) %>%
#             distinct() %>%
#             {.}
#         # write to RDS file
#             saveRDS(object = cal_epa_sites_programs,
#                     file = here('data_processed' ,
#                                 'cal_epa_sites_programs.RDS'))
#     # Sites (NOT USED - SITES COME FROM THE CalEPA Geoserver) --------------------------------------
        
    # # Violations (count by either Year, Fiscal Year, or Year/Month) --------------------------------
    #     cal_epa_sites_violations_count <- read_csv(here('data_raw', 
    #                                                     'CalEPA_RegulatedSites', 
    #                                                     'Violations.zip'), # 'Violations.csv'
    #                                                guess_max = 100000) %>% 
    #         select(-X10) %>% 
    #         # mutate(ViolationYear = year(mdy(ViolationDate))) %>%
    #         # group_by(SiteID, ViolationYear) %>%
    #         # mutate(ViolationFiscalYear = if (month(mdy(ViolationDate)) <= 6) {
    #         #     paste0(year(mdy(ViolationDate))-1, '-', year(mdy(ViolationDate)))} else {
    #         #        paste0(year(mdy(ViolationDate)), '-', year(mdy(ViolationDate)) + 1)
    #         #     }) %>%
    #         # group_by(SiteID, ViolationFiscalYear) %>%
    #         mutate(ViolationMonth = as.Date(paste(year(mdy(ViolationDate)), 
    #                                       month(mdy(ViolationDate)), 
    #                                       1, 
    #                                       sep = '-'))) %>% 
    #         group_by(SiteID, ViolationMonth) %>%
    #         summarize(violations_count = n()) %>%
    #         {.}
    #     # write to RDS file
    #         saveRDS(object = cal_epa_sites_violations_count, 
    #                 file = here('data_processed', 
    #                             'cal_epa_sites_violations_count.RDS'))
    # # Enforcement Actions (count by either Year, Fiscal Year, or Year/Month) -----------------------
    #     cal_epa_sites_enforcement_count <- read_csv(here('data_raw', 
    #                                                      'CalEPA_RegulatedSites', 
    #                                                      'Enforcements.zip'), # EA.csv
    #                                                 guess_max = 100000) %>% 
    #         select(-X13) %>% 
    #         # mutate(EnfActionYear = year(mdy(EnfActionDate))) %>%
    #         # group_by(SiteID, EnfActionYear) %>%
    #         # mutate(EnfActionFiscalYear = if (month(mdy(EnfActionDate)) <= 6) {
    #         #     paste0(year(mdy(EnfActionDate))-1, '-', year(mdy(EnfActionDate)))} else {
    #         #        paste0(year(mdy(EnfActionDate)), '-', year(mdy(EnfActionDate)) + 1)
    #         #     }) %>%
    #         # group_by(SiteID, EnfActionFiscalYear) %>%
    #         mutate(EnfActionMonth = as.Date(paste(year(mdy(EnfActionDate)), 
    #                                       month(mdy(EnfActionDate)), 
    #                                       1, 
    #                                       sep = '-'))) %>% 
    #         group_by(SiteID, EnfActionMonth) %>%
    #         summarize(enforcements_count = n()) %>% 
    #         {.}
    #     # write to RDS file
    #         saveRDS(object = cal_epa_sites_enforcement_count, 
    #                 file = here('data_processed', 
    #                             'cal_epa_sites_enforcement_count.RDS'))
    # # Inspections (count by either Year, Fiscal Year, or Year/Month) -------------------------------
    #     cal_epa_sites_inspections_count <- read_csv(here('data_raw', 
    #                                                      'CalEPA_RegulatedSites', 
    #                                                      'Evaluations.zip'), # EA.csv
    #                                                 guess_max = 100000) %>% 
    #         select(-X11) %>% 
    #         # mutate(EvalYear = year(mdy(EvalDate))) %>%
    #         # group_by(SiteID, EvalYear) %>%
    #         # mutate(EvalFiscalYear = if (month(mdy(EvalDate)) <= 6) {
    #         #     paste0(year(mdy(EvalDate))-1, '-', year(mdy(EvalDate)))} else {
    #         #        paste0(year(mdy(EvalDate)), '-', year(mdy(EvalDate)) + 1)
    #         #     }) %>%
    #         # group_by(SiteID, EvalFiscalYear) %>%
    #         mutate(EvalMonth = as.Date(paste(year(mdy(EvalDate)),
    #                                       month(mdy(EvalDate)),
    #                                       1,
    #                                       sep = '-'))) %>%
    #         group_by(SiteID, EvalMonth) %>%
    #         summarize(inspections_count = n()) %>%
    #         {.}
    #     # write to RDS file
    #         saveRDS(object = cal_epa_sites_inspections_count, 
    #                 file = here('data_processed', 
    #                             'cal_epa_sites_inspections_count.RDS'))
    #     # NOTE: Not all of the inspections are done by CalEPA agencies (as indicated in the 'EvalDivision' field)
    #     # many done by city and county agencies - his helps to narrow down the list
    #     # It looks like these are the only CalEPA relevant entries in the 'EvalDivision' field:
    #     # c('California Environmental Protection Agency', 'Department of Toxic Substances Control', 'Water Boards')
    #         # cal_epa_sites_inspections <- read_csv(here('data_raw',
    #         #                                            'CalEPA_RegulatedSites',
    #         #                                            'Evaluations.zip'),
    #         #                                       guess_max = 100000) %>%
    #         #     select(-X11)
    #         # z <- tabyl(cal_epa_sites_inspections$EvalDivision)
    #         # zz <- cal_epa_sites_inspections %>% distinct(EvalDivision)
    #         # tf <- !grepl(pattern = paste('City', 'County', 'Fire', 'Del Norte', # to remove entires with 'City' 'County' 'Fire' or other specific city/county place names or non-CalEPA agencies in EvalDivision field
    #         #                              'Healdsburg', 'Imperial', 'Livermore', 
    #         #                              'Long Beach', 'Sunnyvale', 'Vernon',
    #         #                              'OSHA',
    #         #                              sep = '|'), 
    #         #                x = cal_epa_sites_inspections %>% 
    #         #                  distinct(EvalDivision) %>% 
    #         #                  pull(EvalDivision))
    #         # View(cal_epa_sites_inspections %>% # shows recoreds where 'EvalDivision' field doesn't contain any of the above words
    #         #          distinct(EvalDivision) %>% 
    #         #          filter(tf))
