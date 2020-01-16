# Load Packages 
    # Shiny platform
        library(shiny) # the Shiny web application framework (https://shiny.rstudio.com) (https://cran.r-project.org/package=shiny)
    # General data analysis and transformation
        library(readr)
        library(readxl)
        library(dplyr)
        library(janitor)
        # library(gtools)
        library(stringr)
    # API-related
        library(jsonlite)
        library(urltools)
    # Mapping and GIS operations
        library(sf)
        library(leaflet)
        library(htmlwidgets)
        library(geojsonsf)
        library(rmapshaper)
        
        
# Read data into R ------------------------------------------------------------------------------------------------------------------
    # Redline Polygons
        redline_polygons <- read_rds('data_prepared/redline_polygons.RDS')
            
    # List of the CES parameter choices to plot
        ces_choices <- read_rds('data_prepared/ces_3_names.RDS') %>% 
            filter(grepl(pattern = 'Percentile$', x = .$name))

# ----------------------------------------------------------------------------------------------------------------------------------------------- #       
# ----------------------------------------------------------------------------------------------------------------------------------------------- #
# Define UI --------------------------------------------------------------------
ui <- navbarPage(title = "California's Redlined Communities", # theme = shinythemes::shinytheme('flatly'),
    # First Tab ----
        tabPanel('Maps',
            # Sidebar layout with input and output definitions ----
            sidebarLayout(
                # Sidebar
                sidebarPanel(
                    # Inputs:  ----
                    selectInput(inputId = 'city_selected_1', 
                                label = 'Select City:', 
                                choices = c(unique(redline_polygons$city)), 
                                selected = 'Sacramento'), # 'All'
                    selectInput(inputId = 'ces_parameter', 
                                label = 'Select CalEnviroScreen (CES) Parameter:', 
                                choices = ces_choices$name, 
                                selected = ces_choices$name[1]),
                    checkboxGroupInput(inputId = 'rating_selected_1',
                                       label = 'Select HOLC Rating:',
                                       choices = list('A (Best)' = 'A',
                                                      'B (Still Desirable)' = 'B',
                                                      'C (Definitely Declining)' = 'C',
                                                      'D (Hazardous)' = 'D'),
                                       selected = c('A','B','C','D')),
                    checkboxGroupInput(inputId = 'site_type_1', 
                                       label = 'Select CalEPA Regulated Site Types:', 
                                       choices = c('CIWQS', 'SMARTS', 'GeoTracker'),
                                       selected = NULL),
                    checkboxGroupInput(inputId = 'show_violations_enforcement',
                                       label = 'Filter For CalEPA Regulated Sites With:',
                                       choices = c('Violations', 'Enforcement Actions'),
                                       selected = NULL),
                    hr(style="border: 1px solid darkgrey"),
                    p(tags$b('NOTE:'), 'Use the left side map to pan/zoom, and use the button in the upper left corner of each map to toggle layers on or off.')
                ),
                # Main panel for displaying outputs ----
                mainPanel(
                  fluidRow(
                      column(6, 
                             tags$h4('Environmental / Public Health Indicators & Regulated Facilities:'), 
                             leafletOutput(outputId = 'map1', height = 800)),
                      column(6, 
                             tags$h4('Redline Map:'),
                             leafletOutput(outputId = 'map2', height = 800))
                      )
                )
            )
        ),
    # Background Info Tab ----
        tabPanel('Background Info',
                 p('This draft tool displays California\'s Redlined communites, and helps to assess potential correlations between those policies and indicators of enviornmental and public health (e.g., 303d impaired water bodies, CalEnviroScreen scores), as well as facilities regulated by the CalEPA. More layers will be added in the future.'),
                 # Redlining History
                     h3('Redlining History'),
                     p('For more information on the history around Redline mapping see:'),
                     tags$li(tags$a(href = 'https://dsl.richmond.edu/panorama/redlining/#loc=4/36.71/-96.93&text=intro',
                                    'Mapping Inequality: Redlining in New Deal America'), 
                             '(University of Richmond)',
                     ),
                     tags$li(tags$a(href = 'https://www.nytimes.com/2017/08/24/upshot/how-redlinings-racist-effects-lasted-for-decades.html',
                                    'How Redlining’s Racist Effects Lasted for Decades'), 
                             '(New York Times)',
                     ),
                     tags$li(tags$a(href = 'https://www.chicagofed.org/publications/working-papers/2017/wp2017-12',
                                    'The Effects of the 1930s HOLC “Redlining” Maps'), 
                             '(Federal Reserve Bank of Chicago)',
                     ),
                 # Data Sources
                     h3('Data Sources'),
                     p('Data used in this application comes from the following sources:'),
                     tags$li(tags$a(href = 'http://dsl.richmond.edu/panorama/redlining/#text=downloads',
                                    'Redline Maps'), 
                             '(University of Richmond)'
                     ),
                     tags$li(tags$a(href = 'https://oehha.ca.gov/calenviroscreen/report/calenviroscreen-30', 'CalEnviroScreen 3.0'), 
                             '(California Office of Environmental Health Hazard Assessment) | Web-service available ',
                             tags$a(href = 'https://services.calepa.ca.gov/geoserver/', 'here'), 
                             ' (click on the "Layer Preview" link, layer name: "calepa:CES3June2018Update")'
                     ),
                     tags$li(tags$a(href = 'https://www.waterboards.ca.gov/water_issues/programs/tmdl/integrated2014_2016.shtml', 
                                    '2014/2016 Clean Water Act Section 303(d) Listed Waterbodies'), 
                             '(California State Water Resources Control Board)'
                     ),   
                     tags$li(tags$a(href = 'https://services.calepa.ca.gov/geoserver/', 'CalEPA Regulated Sites '), 
                             '(CalEPA Geoserver) | Web-service (click on the "Layer Preview" link, layer names: "calepa:mv_fac_from_ciwqs", "calepa:mv_fac_from_smarts", "calepa:mv_fac_from_geotracker")', 
                     ),
                     tags$li(tags$a(href = 'https://siteportal.calepa.ca.gov/nsite/map/export',
                                    'CalEPA Regulated Sites Violations and Enforcement Actions'), 
                             '(CalEPA Regulated Site Portal)'
                     ),
                     tags$li(tags$a(href = 'https://gispublic.waterboards.ca.gov/portal/home/item.html?id=bb21fcee16ea4af2a8d57aa39447aa9c',
                                    'California Drinking Water Service Areas'), 
                             '(California State Water Resources Control Board\'s Map Services) | For web-service, use the "Service URL" link'
                     ),
                     tags$li(tags$a(href = 'http://gispublic.waterboards.ca.gov/arcgis/rest/services/Administrative/RB_OfficeAreas/MapServer/0',
                                    'California State Water Board Regional Board Office Boundaries'), 
                             '(California State Water Resources Control Board\'s Map Services)'
                     ),
                 # Application Information
                     h3('Application Information'),
                     p('This application is built with the ', 
                       tags$a(href = 'https://www.r-project.org/',
                              'R programming language'),
                       ' and the ', 
                       tags$a(href = 'https://shiny.rstudio.com/',
                              'Shiny'),
                       ' R package. Source code is available here: '),
                     tags$li(tags$a(href = 'https://github.com/daltare/Redline-Mapping',
                                    'Github Site')
                     )
        )
)

# Define server logic ----
server <- function(input, output) {

    # create list of which region each city is in (for filtering related datasets)
        cities_regions <- list('Fresno' = '5F',
                               'Los Angeles' = '4',
                               'Oakland' = '2',
                               'Sacramento' = '5S',
                               'San Diego' = '9',
                               'San Francisco' = '2',
                               'San Jose' = '2',
                               'Stockton' = '5S')
    # create list of simplification levels that work best for each region (for use in APIs as a geographic filter)
        simplification_regions <- list('1' = '0.001',
                                       '2' = '0.0035',
                                       '3' = '0.001',
                                       '4' = '0.0015',
                                       '5R' = '0.003',
                                       '5S' = '0.0025',
                                       '5F' = '0.0025',
                                       '6A' = '0.004',
                                       '6B' = '0.0025',
                                       '7' = '0.004',
                                       '8' = '0.007',
                                       '9' = '0.006')

        
        # get reactive values (to isolate from each other and prevent map from completely rebuilding when an input is changed)
            # regional board boundary containing the selected city
                rb_boundary <- reactive({
                    # Get Regional Board Office Areas from WB GIS Services (GEOJSON) - note that this filters for just the regional board containing the selected city
                    url_rb_office_areas <- paste0("http://gispublic.waterboards.ca.gov/arcgis/rest/services/Administrative/RB_OfficeAreas/MapServer/0/query?where=UPPER(rb_off)%20like%20'%25",
                                                  cities_regions[[input$city_selected_1]],
                                                  "%25'&outFields=*&outSR=4326&f=geojson") 
                    rb_boundary <- read_lines(url_rb_office_areas) %>% 
                        geojson_sf()
                    return(rb_boundary)
                    
                    # # get the regional board boundary containing the selected city from a saved file (old method - not used)
                    #     rb_boundary <- read_rds('data_prepared/Regional_Board_Offices.RDS') %>% 
                    #         filter(RB_OFF == cities_regions[[input$city_selected_1]])
                })
                
            # create a simplified version of the regional board boundary for use in api filtering
                rb_boundary_simplify <- reactive({
                    # simplify and convert rb boundary to geojson
                        rb_boundary() %>% 
                            ms_simplify(keep = as.numeric(
                                simplification_regions[[cities_regions[[input$city_selected_1]]]])) 
                })
                
                
            # CalEnvironScreen Polygons (just for the region containing the selected city)
                ces_3_poly <- reactive({
                    # # get data
                    #     ces_3_poly <- read_rds('data_prepared/ces_3_poly.RDS')
                    # get data from API
                        # transform to rb boundary to coordinate system used in web service
                            rb_boundary_simplify_transform <- st_transform(rb_boundary_simplify(), 3310)
                        # convert to geojson
                            rb_boundary_simplify_transform_geojson <- rb_boundary_simplify_transform %>% sf_geojson()
                        # get coordinates
                            rb_boundary_coordinates <- rb_boundary_simplify_transform_geojson %>%
                                str_sub(start = str_locate(string = ., pattern = 'coordinates\":') %>% as.data.frame() %>% pull(end) + 4,
                                        end = nchar(rb_boundary_simplify_transform_geojson)-7)
                        # format the coordinates for the api
                            rb_boundary_coordinates <- str_replace_all(string = rb_boundary_coordinates, pattern = '\\],\\[', replacement = '|')
                            rb_boundary_coordinates <- str_replace_all(string = rb_boundary_coordinates, pattern = ',', replacement = '%20')
                            rb_boundary_coordinates <- str_replace_all(string = rb_boundary_coordinates, pattern = '\\|', replacement = ',%20')
                        # construct api call - NOTE -- for more info see: https://docs.geoserver.org/stable/en/user/tutorials/cql/cql_tutorial.html
                            url_ces3_api <- paste0('https://services.calepa.ca.gov/geoserver/calepa/',
                                                   'ows?service=WFS&version=1.0.0',
                                                   '&request=GetFeature',
                                                   '&typeName=calepa:CES3June2018Update',
                                                   '&outputFormat=application%2Fjson',
                                                   '&CQL_FILTER=INTERSECTS(the_geom,POLYGON((',
                                                   rb_boundary_coordinates,
                                                   ')))')
                        # make api call
                            ces_3_poly <- read_lines(url_ces3_api) %>% geojson_sf()
                            st_crs(ces_3_poly) <- 3310 # the crs is incorrectly defined in the data returned by the api - have to reset it
                            ces_3_poly <- st_transform(ces_3_poly, crs = 4326) # transform back to lat/lon coordinates
                        # check
                            # plot(ces_3_poly$geometry, border = 'blue', col = 'grey')
                            # plot(rb_boundary()$geometry, lwd = 3, add = TRUE)
                            # plot(rb_boundary_simplify() %>% select(geometry), border = 'red', add = TRUE)
                        # revise column names
                            ces_3_poly <- ces_3_poly %>% select(-CES2018_Rn)
                            col_names_original <- names(ces_3_poly)
                            col_names_new <- read_csv('data_prepared/ces_names.csv')
                            col_names_original_df <- as.data.frame(x = col_names_original)
                            col_names_original_df <- col_names_original_df %>% left_join(col_names_new, by = c('col_names_original' = 'id'))
                            col_names_original_df$name[nrow(col_names_original_df)] <- 'geometry'
                            names(ces_3_poly) <- col_names_original_df$name
                            names(ces_3_poly) <- make_clean_names(names(ces_3_poly), 'parsed')
                            return(ces_3_poly)
                })
                
            # 303d polygons
                impaired_303d_poly <- reactive({
                    # get data
                        impaired_303d_poly <- read_rds('data_prepared/impaired_303d_poly.RDS')
                    # filter for 303d polygons in the region containing the selected city
                        impaired_poly_filter <- st_intersects(x = impaired_303d_poly,
                                                              y = rb_boundary(),
                                                              sparse = FALSE)
                        impaired_303d_poly <- impaired_303d_poly[impaired_poly_filter,]
                        return(impaired_303d_poly)
                })
                
            # 303d lines
                impaired_303d_lines <- reactive({
                    # get data
                        impaired_303d_lines <- read_rds('data_prepared/impaired_303d_lines_simplify_R1removed.RDS')
                    # filter out records with empty geometries
                        impaired_303d_lines <- impaired_303d_lines %>% filter(!is.na(st_dimension(.)))
                    # filter for 303d lines in the region containing the selected city
                        impaired_lines_filter <- st_intersects(x = impaired_303d_lines,
                                                               y = rb_boundary(),
                                                               sparse = FALSE)
                        impaired_303d_lines <- impaired_303d_lines[impaired_lines_filter,]
                        return(impaired_303d_lines)
                })
                
            # CalEPA regulated sites
                cal_epa_sites <- reactive({
                    if (length(input$site_type_1) > 0) {    
                        # get CalEPA sites data from the geoserver api
                            # get the coordinates of the simplified rb boundary containing the selected city and convert to geojson
                                rb_boundary_simplify_geojson <- rb_boundary_simplify() %>% sf_geojson()
                            # extract coordinates
                                rb_boundary_coordinates <- rb_boundary_simplify_geojson %>%
                                    str_sub(start = str_locate(string = ., pattern = 'coordinates\":') %>% as.data.frame() %>% pull(end) + 4,
                                            end = nchar(rb_boundary_simplify_geojson)-7)
                            # format the coordinates for the api
                                rb_boundary_coordinates <- str_replace_all(string = rb_boundary_coordinates, pattern = '\\],\\[', replacement = '|')
                                rb_boundary_coordinates <- str_replace_all(string = rb_boundary_coordinates, pattern = ',', replacement = '%20')
                                rb_boundary_coordinates <- str_replace_all(string = rb_boundary_coordinates, pattern = '\\|', replacement = ',%20')
                            # construct and make the api call
                                calepa_sites_typenames <- list('CIWQS' = 'calepa:mv_fac_from_ciwqs',
                                                          'GeoTracker' = 'calepa:mv_fac_from_geotracker',
                                                          'SMARTS' = 'calepa:mv_fac_from_smarts'
                                )
                                counter_sites <- 0
                                for (site_type in input$site_type_1) {
                                    counter_sites <- counter_sites + 1
                                    # url <- calepa_sites_typenames[[site_type]]
                                    url_sites_api <- paste0('https://services.calepa.ca.gov/geoserver/calepa/',
                                                               'ows?service=WFS&version=1.0.0',
                                                               '&request=GetFeature',
                                                               '&typeName=', calepa_sites_typenames[[site_type]], # 'calepa:mv_fac_from_ciwqs', # calepa:mv_fac_from_geotracker # calepa:mv_fac_from_smarts
                                                               '&outputFormat=application%2Fjson',
                                                               '&CQL_FILTER=INTERSECTS(fac_point,POLYGON((',
                                                               rb_boundary_coordinates,
                                                               ')))')
                                    api_output <- readr::read_lines(url_sites_api)
                                    json_list <- jsonlite::fromJSON(api_output)
                                    df_api_result <- json_list$features
                                    df_api_result <- df_api_result %>% mutate('data_source' = site_type)
                                    # reformat the data frame (some of it is nested)
                                    df_api_result <- bind_cols(df_api_result %>% select(id, data_source),
                                                               df_api_result$geometry,
                                                               df_api_result$properties)
                                    if (counter_sites == 1) {
                                        cal_epa_sites <- df_api_result
                                    } else {
                                        cal_epa_sites <- bind_rows(cal_epa_sites, df_api_result)
                                    }
                                }
                            # get the violation and enforcement record counts and join to the sites data frame
                                violations_count <- read_rds("data_prepared/cal_epa_sites_violations_count.RDS")
                                enforcement_count <- read_rds("data_prepared/cal_epa_sites_enforcement_count.RDS")
                                # join the violation and enforcement record counts to the sites data frame
                                    cal_epa_sites <- cal_epa_sites %>% left_join(violations_count, by = c('site_id' = 'SiteID'))
                                    cal_epa_sites <- cal_epa_sites %>% left_join(enforcement_count, by = c('site_id' = 'SiteID'))
                                    # replace NAs with zeros for the counts
                                        cal_epa_sites <- cal_epa_sites %>% mutate(violations_records = case_when(is.na(violations_records) ~ 0L,
                                                                                                               TRUE ~ violations_records),
                                                                                  enforcement_records = case_when(is.na(enforcement_records) ~ 0L,
                                                                                                               TRUE ~ enforcement_records))
                            # Create an sf object from the sites data
                                cal_epa_sites <- st_as_sf(cal_epa_sites %>% filter(!is.na(latitude) & !is.na(longitude)),
                                                          coords = c('longitude', 'latitude'),
                                                          crs = 4326,
                                                          agr = 'constant')
                            # # filter for sites in the region containing the selected city -- now done in the API call
                            #     sites_filter <- st_intersects(x = cal_epa_sites,
                            #                                   y = rb_boundary(),
                            #                                   sparse = FALSE)
                            #     cal_epa_sites <- cal_epa_sites[sites_filter, ]
                            # return the object
                                return(cal_epa_sites)
                        }
                })
                
    # MAP 1 -----------------------------------------------------------------------------------------------------------------#
    output$map1 <- renderLeaflet({
        # get the bounds of the redline polygons for the selected city
            bounds_1 <- attributes(st_geometry(redline_polygons %>% filter(city == input$city_selected_1)))$bbox

        # create the new (empty) map
            l_map1 <- leaflet()
            
            l_map1 <- l_map1 %>% addMapPane("ces_polygons", zIndex = 410) %>% 
                addMapPane("redline_polygons", zIndex = 420) %>% 
                addMapPane("303d_lines", zIndex = 430) %>% 
                addMapPane("303d_polygons", zIndex = 440) %>% 
                addMapPane("region_polygon", zIndex = 450) %>% 
                addMapPane('serviceareas_polygon', zIndex = 460) %>% 
                addMapPane('calepa_points', zIndex = 470)
        
        # Basemap Options
            basemap_options <- c('Esri.WorldTopoMap', 'CartoDB.Positron', 'Esri.WorldGrayCanvas','Esri.WorldImagery','Esri.WorldStreetMap') 
            for (provider in basemap_options) {
                l_map1 <- l_map1 %>% addProviderTiles(provider, group = provider)
            }
        
        # add the min-map window
            l_map1 <- l_map1 %>% addMiniMap(tiles = basemap_options[[1]], toggleDisplay = TRUE, position = "bottomleft")
        
        # code to make the basemap/min-map selector work (copied from: https://rstudio.github.io/leaflet/morefeatures.html)
            l_map1 <- l_map1 %>% onRender(
                "function(el, x) {
                    var myMap = this;
                    myMap.on('baselayerchange',
                    function (e) {
                    myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
                    })
                    }"
            )
        
        # create a button to re-center the map
            l_map1 <- l_map1 %>% addEasyButton(easyButton(
                icon="fa-globe", title="Center Map on Selected City",
                onClick=JS(paste0('function(btn, map){ map.fitBounds([[',
                                           round(bounds_1[[2]],4)-0.01, ', ',
                                           round(bounds_1[[1]],4)-0.01, '],[',
                                           round(bounds_1[[4]],4)+0.01, ', ',
                                           round(bounds_1[[3]],4)+0.01, ']]); }'))))

        # Add the 303d polygons
        #     # get data
        #         impaired_303d_poly <- read_rds('data_prepared/impaired_303d_poly.RDS')
        #     # filter for 303d polygons in the region containing the selected city
        #         impaired_poly_filter <- st_intersects(x = impaired_303d_poly,
        #                                               y = rb_boundary(),
        #                                               sparse = FALSE)
        #         impaired_303d_poly <- impaired_303d_poly[impaired_poly_filter,]
            # add polygons
                l_map1 <- l_map1 %>% addPolygons(data = impaired_303d_poly(),
                                                 options = pathOptions(pane = "303d_polygons"),
                                                 color = 'darkblue',
                                                 weight = 0.5,
                                                 opacity = 0.8,
                                                 fillColor = 'blue',
                                                 fillOpacity = 0.5,
                                                 smoothFactor = 1.0,
                                                 highlightOptions = highlightOptions(color = "white", weight = 2),
                                                 popup = ~paste0('<b>', '<u>', '303d Listed Waterbody (2014/2016)', '</u>', '</b>', '<br/>',
                                                                 '<b>', 'Water Body Name: ', '</b>', wbname, '<br/>',
                                                                 '<b>', 'Type: ', '</b>', wbtype, '<br/>',
                                                                 '<b>', 'Region: ', '</b>', region_num, ' (', region_nam,')', '<br/>',
                                                                 '<b>', 'ID: ', '</b>', wbid, '<br/>',
                                                                 '<b>', 'Listed Pollutants: ', '</b>', pollutant, '<br/>',
                                                                 '<b>', 'Listing Comments: ', '</b>', comments, '<br/>',
                                                                 '<b>', 'Potential Sources: ', '</b>', sources),
                                                 group = '303d Listed Waters')

        # Add the 303d lines
            # # get data
            #     impaired_303d_lines <- read_rds('data_prepared/impaired_303d_lines_simplify_R1removed.RDS')
            # # filter out records with empty geometries
            #     impaired_303d_lines <- impaired_303d_lines %>% filter(!is.na(st_dimension(.)))
            # # filter for 303d lines in the region containing the selected city
            #     impaired_lines_filter <- st_intersects(x = impaired_303d_lines,
            #                                            y = rb_boundary() %>% filter(RB_OFF == cities_regions[[input$city_selected_1]]),
            #                                            sparse = FALSE)
            #     impaired_303d_lines <- impaired_303d_lines[impaired_lines_filter,]
            # add polylines
                l_map1 <- l_map1 %>% addPolylines(data = impaired_303d_lines(), 
                                                  options = pathOptions(pane = "303d_lines"),
                                                  color = 'blue',
                                                  weight = 2.0, 
                                                  opacity = 1.0, 
                                                  # fillOpacity = 0.5,
                                                  smoothFactor = 1.0,
                                                  highlightOptions = highlightOptions(color = "white", weight = 2),
                                                  popup = ~paste0('<b>', '<u>','303d Listed Waterbody (2014/2016)','</u>', '</b>','<br/>',
                                                                  '<b>', 'Water Body Name: ', '</b>', wbname,'<br/>',
                                                                  '<b>', 'Type: ', '</b>', wbtype,'<br/>',
                                                                  '<b>', 'Region: ', '</b>', region_num, ' (', region_nam,')','<br/>',
                                                                  '<b>', 'ID: ', '</b>', wbid, '<br/>',
                                                                  '<b>', 'Listed Pollutants: ', '</b>', pollutant, '<br/>',
                                                                  '<b>', 'Listing Comments: ', '</b>', comments,  '<br/>',
                                                                  '<b>', 'Potential Sources: ', '</b>', sources),
                                                  group = '303d Listed Waters')

        
        # add the region boundary
            l_map1 <- l_map1 %>% addPolygons(data = rb_boundary(),
                                             options = pathOptions(pane = "region_polygon"),
                                             color = 'black', # "#444444",
                                             weight = 1.0,
                                             smoothFactor = 1.0,
                                             opacity = 1.0,
                                             fill = FALSE,
                                             # fillOpacity = 0.5,
                                             # fillColor = 'lightblue',
                                             # fillColor = ~redline_leaflet_pal(holc_grade),
                                             highlightOptions = highlightOptions(color = "white", weight = 2),#,bringToFront = TRUE
                                             popup = ~paste0('<b>', '<u>', 'Regional Board Boundary', '</u>', '</b>','<br/>',
                                                             '<b>', 'Region Name: ', '</b>', RB_NAME, '<br/>',
                                                             '<b>', 'Region Number: ', '</b>', RB_OFF),
                                             group = 'Regional Board Boundary'
            )
        
        # add the drinking water provider service areas
            # # get data from saved file
            #     service_areas <- read_rds("data_prepared/service_areas_simplify.RDS")
            # # filter for polygons in the region containing the selected city
            #     service_areas_filter <- st_intersects(x = service_areas,
            #                                           y = rb_boundary(),
            #                                           sparse = FALSE)
            #     service_areas <- service_areas[service_areas_filter,]
                
            # get data from the waterboard web services
                # extract the coordinates of the simplified rb boundary and encode as url
                    rb_boundary_simplify_geojson <- rb_boundary_simplify() %>% sf_geojson()
                    rb_boundary_coordinates <- rb_boundary_simplify_geojson %>% 
                        str_sub(start = str_locate(string = ., pattern = 'coordinates\":') %>% as.data.frame() %>% pull(end) + 4, 
                                end = nchar(rb_boundary_simplify_geojson)-7) %>% 
                        url_encode()
                # construct the api call and get the data
                    service_url <- 'https://gispublic.waterboards.ca.gov/portalserver/rest/services/Hosted/California_Drinking_Water_Service_Areas/FeatureServer/0/'
                    url_polygon_filter_serviceboundaries <- paste0(service_url,
                                                                   'query?where=1%3D1', 
                                                                   '&outFields=*',
                                                                   '&geometry=%7B%22rings','%22','%3A','%5B','%5B','%5B',
                                                                   rb_boundary_coordinates,
                                                                   '%5D','%5D','%5D','%7D',
                                                                   '&geometryType=esriGeometryPolygon&inSR=4326&spatialRel=esriSpatialRel',
                                                                   'Intersects', # 'Contains', 'Intersects'
                                                                   '&f=geojson')
                    service_areas <- read_lines(url_polygon_filter_serviceboundaries) %>% geojson_sf() 
                    # check
                        # plot(rb_boundary() %>% select(geometry), lwd = 3, reset = FALSE) 
                        # plot(service_areas$geometry, border = 'blue', add = TRUE)
                        # plot(rb_boundary_simplify() %>% select(geometry), border = 'red', add = TRUE)
                
            # add polygons
                l_map1 <- l_map1 %>% 
                    addPolygons(data = service_areas,
                                options = pathOptions(pane = "serviceareas_polygon"),
                                color = 'black', # "#444444", 
                                weight = 0.5, 
                                smoothFactor = 1.0,
                                opacity = 0.8, 
                                fillOpacity = 0.5,
                                fillColor = 'lightblue',
                                highlightOptions = highlightOptions(color = "white", weight = 2),
                                popup = ~paste0('<b>', '<u>','Drinking Water Provider Service Area Boundary', '</u>','</b>','<br/>',
                                                '<b>', 'PWSID: ', '</b>', pwsid,'<br/>',
                                                '<b>', 'Name: ', '</b>',  name,'<br/>',
                                                '<b>', 'County: ', '</b>', d_prin_cnt,'<br/>',
                                                '<b>', 'Population: ', '</b>', d_populati,'<br/>'),
                                                #'<b>', 'Verified: ', '</b>', verified_s), # verified_status
                                group = 'Drinking Water Provider Service Areas'
                    )
        
        # Set the bounds of the map dynamically - initial view is based on the full extent of the selected city, after that the map is based on the most recent bounds when a new option is selected
            isolate(if (is.null(input$map1_bounds)) {
                l_map1 <- l_map1 %>% fitBounds(lng1 = bounds_1[[1]], 
                                               lat1 = bounds_1[[2]], 
                                               lng2 = bounds_1[[3]], 
                                               lat2 = bounds_1[[4]])
            } else { # maintain the current view
                l_map1 <- l_map1 %>% setView(lng = mean(c(input$map1_bounds$west, input$map1_bounds$east)), 
                                             lat = mean(c(input$map1_bounds$north, input$map1_bounds$south)), 
                                             zoom = input$map1_zoom)                                
            })

        # Add controls to select the basemap and layers
            l_map1 <- l_map1 %>% addLayersControl(baseGroups = basemap_options,
                                                  overlayGroups = c('CalEnviroScreen', 
                                                                    'Redlined Areas', 
                                                                    '303d Listed Waters', 
                                                                    'CalEPA Regulated Sites',
                                                                    'Drinking Water Provider Service Areas',
                                                                    'Regional Board Boundary',
                                                                    'Legend'),
                                                  options = layersControlOptions(collapsed = TRUE,
                                                                                 autoZIndex = TRUE))
        # Hide some groups by default (can be turned on with the layers control box on the map)
            l_map1 <- l_map1 %>% hideGroup(c('Drinking Water Provider Service Areas', 'Redlined Areas')) 
                
        # output the map object
            l_map1
    })
    
    # Use a separate observer to recreate some parts of the map as they are updated, without re-drawing the entire map ----
        # redlined areas
            observe({
                leafletProxy('map1') %>%
                    clearGroup('Redlined Areas') %>% 
                    addPolygons(data = redline_polygons %>% filter(holc_grade %in% input$rating_selected_1),
                                options = pathOptions(pane = "redline_polygons"),
                                color = 'black', # "#444444",
                                weight = 2.0,
                                smoothFactor = 1.0,
                                opacity = 1.0,
                                # fill = FALSE,
                                fillOpacity = 0, # input$redline_fill_1,
                                fillColor = 'lightgrey',
                                # fillColor = ~redline_leaflet_pal(holc_grade),
                                highlightOptions = highlightOptions(color = "white", weight = 2),#,bringToFront = TRUE
                                popup = ~paste0('<b>', '<u>', 'Redline Polygon', '</u>', '</b>','<br/>',
                                                '<b>', 'City: ', '</b>', city, '<br/>',
                                                '<b>', 'Name: ', '</b>', name, '<br/>',
                                                '<b>', 'Grade (A-D): ', '</b>', holc_grade),
                                group = 'Redlined Areas'
                                )
            })
        
        # CalEPA sites
            observe({
                if (length(input$site_type_1) > 0) {
                    cal_epa_sites <- cal_epa_sites()
                    # if the option is selected, filter for sites with violations and/or enforcement actions
                        if ('Violations' %in% input$show_violations_enforcement) {
                            cal_epa_sites <- cal_epa_sites %>% filter(violations_records > 0)
                        }
                        if ('Enforcement Actions' %in% input$show_violations_enforcement) {
                            cal_epa_sites <- cal_epa_sites %>% filter(enforcement_records > 0)
                        }
                    leafletProxy('map1') %>%
                        clearGroup('CalEPA Regulated Sites') %>% 
                        addCircleMarkers(data = cal_epa_sites,
                                         options = pathOptions(pane = "calepa_points"),
                                         radius = 4,
                                         stroke = TRUE, weight = 0.5, color = 'black', opacity = 1,
                                         fill = TRUE, fillOpacity = 1, fillColor = 'black', # 'grey', # ~wqi.leaflet.pal(WQI),
                                         # clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier = 2),# freezeAtZoom = 13, maxClusterRadius = 10),#,#singleMarkerMode = TRUE),
                                         popup = ~paste0('<b>', '<u>', 'CalEPA Regulated Site', '</u>','</b>','<br/>',
                                                         # '<b>', '<u>', 'Site Information:', '</u>', '</b>','<br/>',
                                                         '<b>', 'Name: ', '</b>', facility_name,'<br/>',
                                                         '<b>', 'ID: ', '</b>', site_id,'<br/>',
                                                         '<b>', 'Address: ', '</b>', address, '<br/>',
                                                         '<b>', 'City: ', '</b>', city, '<br/>',
                                                         #'<b>', 'County: ', '</b>', County,'<br/>',
                                                         '<b>', 'Zip Code: ', '</b>', zip_code, '<br/>',
                                                         '<b>', 'Source: ', '</b>', data_source, '<br/>',
                                                         '<b>', 'Violations / Enforcement Actions:', '</b>','<br/>',
                                                         '<b>', HTML('&nbsp;'), HTML('&nbsp;'), ' - Number of Violation Records: ', '</b>', violations_records, '<br/>', # chr(149),
                                                         '<b>', HTML('&nbsp;'), HTML('&nbsp;'), ' - Number of Enforcement Records: ', '</b>', enforcement_records # chr(149),
                                         ),
                                         group = 'CalEPA Regulated Sites') 
                }
            })
            
        # CalEnvironScreen Polygons (just for the region containing the selected city)
            observe({
                # create the color palette for the CES polygons
                    parameter <- ces_choices %>% filter(name == input$ces_parameter) %>% pull(ces_variable)
                    ces_pal_domain <- as.data.frame(ces_3_poly()) %>% select(parameter) %>% pull(parameter)
                    # create the palette
                        ces_pal_color <- 'RdYlGn' # 'YlOrBr' #'Blues' #'Greys'
                        ces_leaflet_pal <- colorNumeric(
                            palette = ces_pal_color,
                            domain = ces_pal_domain,
                            reverse = TRUE
                        )
                # get the dataset
                    ces_3_poly <- ces_3_poly() %>% mutate(fill_variable = ces_pal_domain)
                # add polygons
                    leafletProxy('map1') %>%
                        clearGroup('CalEnviroScreen') %>% 
                        addPolygons(data = ces_3_poly, # ces_3_poly %>% filter(California_County == cities_counties[[input$city_selected_1]]), 
                                    options = pathOptions(pane = "ces_polygons"),
                                    color = 'grey', # "#444444", 
                                    weight = 0.5, 
                                    smoothFactor = 1.0,
                                    opacity = 0.8, 
                                    fillOpacity = 0.8,
                                    # fillColor = ~colorNumeric('YlOrBr', Poll_pctl)(Poll_pctl), # view RColorBrewer palettes with: RColorBrewer::display.brewer.all()
                                    fillColor = ~ces_leaflet_pal(fill_variable),
                                    # fillColor = 'green',
                                    highlightOptions = highlightOptions(color = "white", weight = 2), # fill = TRUE, fillColor = "white"),#,bringToFront = TRUE
                                    popup = ~paste0('<b>', '<u>','CalEnviroScreen 3.0 Tract', '</u>','</b>','<br/>',
                                                    '<b>', 'Census Tract: ', '</b>', Census_Tract,'<br/>',
                                                    '<b>', 'Location: ', '</b>', Nearby_City,', ', California_County, ' County, ', ZIP,'<br/>',
                                                    '<b>', 'Population (2010): ', '</b>', Population_2010,'<br/>',
                                                    
                                                    '<b>', '<u>', 'Overall Score: ','</u>', '</b>','<br/>',
                                                    '<b>', 'CES Score: ', '</b>', CES_3_Score,'&ensp;','|', '&ensp;',
                                                    '<b>', 'CES Percentile: ', '</b>', CES_3_Percentile,'<br/>',
                                                    
                                                    '<b>', '<u>','Water-Related Environmental/Health Scores (Percentiles): ', '</u>', '</b>','<br/>',
                                                    '<b>', 'Drinking Water: ', '</b>', Drinking_Water_Percentile,'&ensp;','|', '&ensp;',
                                                    '<b>', 'Impaired Waterbodies: ', '</b>', Imp_Water_Bodies_Percentile,'<br/>',
                                                    '<b>', 'Pollution Burden: ', '</b>', Pollution_Burden_Percentile,'&ensp;','|', '&ensp;',
                                                    '<b>', 'Pesticides: ', '</b>', Pesticides_Percentile,'<br/>',
                                                    '<b>', 'Toxic Releases: ', '</b>', Tox_Releases_Percentile,'&ensp;','|', '&ensp;',
                                                    '<b>', 'Cleanup Sites: ', '</b>', Cleanups_Percentile,'<br/>',
                                                    '<b>', 'Groundwater Threats: ', '</b>', Ground_Water_Threats_Percentile,'&ensp;','|', '&ensp;',
                                                    '<b>', 'Solid Waste: ', '</b>', Solid_Waste_Percentile,'<br/>',
                                                    '<b>', 'Hazardous Waste Generators: ', '</b>', Haz_Waste_Percentile,'<br/>',
                                                    
                                                    '<b>', '<u>','Other Environmental Scores (Percentiles): ', '</u>', '</b>','<br/>',
                                                    '<b>', 'Ozone: ', '</b>', Ozone_Percentile,'&ensp;','|', '&ensp;',
                                                    '<b>', 'PM 2.5: ', PM_2_5_Percentile,'<br/>',
                                                    '<b>', 'Diesel PM: ', Diesel_PM_Percentile,'&ensp;','|', '&ensp;',
                                                    '<b>', 'Traffic Percentile: ', Traffic_Percentile,'<br/>',
                                                    
                                                    '<b>', '<u>','Public Health Scores (Percentiles): ', '</u>','</b>','<br/>',
                                                    '<b>', 'Asthma: ', '</b>', Asthma_Percentile,'&ensp;','|', '&ensp;',
                                                    '<b>', 'Low Birth Weight: ', '</b>', Low_Birth_Weight_Percentile,'<br/>',
                                                    '<b>', 'Cardiovascular Disease: ', '</b>', Cardiovascular_Disease_Percentile,'<br/>',
                                                    
                                                    '<b>', '<u>','Socioeconomic Scores (Percentiles): ', '</u>','</b>','<br/>',
                                                    '<b>', 'Education: ', '</b>', Education_Percentile,'&ensp;','|', '&ensp;',
                                                    '<b>', 'Linguistic Isolation: ', '</b>', Linguistic_Isolation_Percentile,'<br/>',
                                                    '<b>', 'Poverty: ', '</b>', Poverty_Percentile,'&ensp;','|', '&ensp;',
                                                    '<b>', 'Unemployment: ', '</b>', Unemployment_Percentile,'<br/>',
                                                    '<b>', 'Housing Burden: ', '</b>', Housing_Burden_Percentile,'&ensp;','|', '&ensp;',
                                                    '<b>', 'Population Characteristics: ', '</b>', Population_Characteristics_Percentile,'<br/>',
                                                    
                                                    '<b>', '<u>','Demographics (% of Population): ', '</u>','</b>','<br/>',
                                                    '<b>', 'Children (Age <=10) : ', '</b>', Children_10_percent,'&ensp;','|', '&ensp;',
                                                    '<b>', 'Age 11-64: ', '</b>', Pop_11_64_years_percent,'<br/>',
                                                    '<b>', 'Elderly (Age >65): ', '</b>', Elderly_65_percent,'<br/>',
                                                    '<b>', 'Hispanic: ', '</b>', Hispanic_percent,'&ensp;','|', '&ensp;',
                                                    '<b>', 'White: ', '</b>', White_percent,'<br/>',
                                                    '<b>', 'African American: ', '</b>', African_American_percent,'&ensp;','|', '&ensp;',
                                                    '<b>', 'Native American: ', '</b>', Native_American_percent,'<br/>',
                                                    '<b>', 'Asian American: ', '</b>', Asian_American_percent,'&ensp;','|', '&ensp;',
                                                    '<b>', 'Other: ', '</b>', Other_percent
                                    ),
                                    # # popupOptions = popupOptions(textsize = '15px'),
                                    group = 'CalEnviroScreen') %>% 
                        clearControls() %>%
                        addLegend(position = 'bottomright',
                                  pal = ces_leaflet_pal,
                                  values = ces_3_poly$fill_variable,
                                  opacity = 1,
                                  layerId = 'ces_legend',
                                  bins = 4,
                                  group = 'Legend',
                                  title = paste0('CalEnviroScreen'))
            })

        
    
    
    # MAP 2 -----------------------------------------------------------------------------------------------------------------#    
    output$map2 <- renderLeaflet({
        # get the bounds of the redline polygons for the selected city
            bounds_2 <- attributes(st_geometry(redline_polygons %>% filter(city == input$city_selected_1)))$bbox
        
        # create the new (empty) map
            l_map2 <- leaflet(options = leafletOptions(zoomControl = FALSE, 
                                                       dragging = FALSE))
        
        # Basemap Options
            basemap_options <- c('Esri.WorldTopoMap', 'CartoDB.Positron', 'Esri.WorldGrayCanvas','Esri.WorldImagery','Esri.WorldStreetMap') 
            for (provider in basemap_options) {
                l_map2 <- l_map2 %>% addProviderTiles(provider, group = provider)
            }
        
        # add the min-map window
            # l_map2 <- l_map2 %>% addMiniMap(tiles = basemap_options[[1]], toggleDisplay = TRUE, position = "bottomleft")
        
        # # code to make the basemap/min-map selector work (copied from: https://rstudio.github.io/leaflet/morefeatures.html)
        #     l_map2 <- l_map2 %>% onRender(
        #         "function(el, x) {
        #             var myMap = this;
        #             myMap.on('baselayerchange',
        #             function (e) {
        #             myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        #             })
        #             }"
        #     )
        
        # Create the color palette for the Redline scores
            redline_leaflet_pal <- colorFactor(palette = c('green', 'blue', 'yellow', 'red'), # 'YlOrBr'   # c("#FACD7B","#D1A149","#916714","#6B4703")
                                               domain = redline_polygons$holc_grade, 
                                               levels = c('A', 'B', 'C', 'D'))
        
        # add the legend
            l_map2 <- l_map2 %>% 
                addLegend(position = 'bottomright', 
                          pal = redline_leaflet_pal, 
                          values = c('A', 'B', 'C', 'D'), 
                          opacity = 1, 
                          layerId = 'redline_legend', 
                          group = 'Legend', 
                          title = paste0('Redlined Areas'))
        
        # Add controls to select the basemap and layers
            l_map2 <- l_map2 %>% addLayersControl(baseGroups = basemap_options,
                                                 overlayGroups = c('Redlined Areas', 'Legend'),
                                                 options = layersControlOptions(collapsed = TRUE, autoZIndex = TRUE)) 
        
        # Set the bounds of the map dynamically - initial view is based on the full extent of the selected city, after that the map is based on the most recent bounds when a new option is selected
            isolate(if (is.null(input$map2_bounds)) {
                l_map2 <- l_map2 %>% fitBounds(lng1 = bounds_2[[1]], 
                                               lat1 = bounds_2[[2]], 
                                               lng2 = bounds_2[[3]], 
                                               lat2 = bounds_2[[4]])
            } else { # maintain the current view
                l_map2 <- l_map2 %>% setView(lng = mean(c(input$map2_bounds$west, input$map2_bounds$east)),
                                             lat = mean(c(input$map2_bounds$north, input$map2_bounds$south)),
                                             zoom = input$map2_zoom)
            })
        
        # output the map object
            l_map2
    })
    
    
    # Use a separate observer to recreate some parts of Map 2 as they are updated, without re-drawing the entire map
        # redline polygons
            observe({
                input$city_selected_1 # to re-draw polygons when city is changed
                # Create the color palette for the Redline scores
                redline_leaflet_pal <- colorFactor(palette = c('green', 'blue', 'yellow', 'red'), # 'YlOrBr'   # c("#FACD7B","#D1A149","#916714","#6B4703")
                                                   domain = redline_polygons$holc_grade, 
                                                   levels = c('A', 'B', 'C', 'D'))
                leafletProxy("map2") %>% 
                    clearShapes() %>% 
                    addPolygons(data = redline_polygons %>% filter(holc_grade %in% input$rating_selected_1),
                                color = 'black', # "#444444",
                                weight = 1.0,
                                smoothFactor = 1.0,
                                opacity = 1.0,
                                # fill = FALSE,
                                fillOpacity = 1.0,
                                # fillColor = 'lightblue',
                                fillColor = ~redline_leaflet_pal(holc_grade),
                                highlightOptions = highlightOptions(color = "white", weight = 2),#,bringToFront = TRUE
                                popup = ~paste0('<b>', '<u>', 'Redline Polygon', '</u>', '</b>','<br/>',
                                                '<b>', 'City: ', '</b>', city, '<br/>',
                                                '<b>', 'Name: ', '</b>', name, '<br/>',
                                                '<b>', 'Grade (A-D): ', '</b>', holc_grade),
                                group = 'Redlined Areas'
                    )
            })


# Helper functions ---------------------------------------------------------------------------------------------------------#
    # Center map1 and map2 on change in city selection
        observeEvent(input$city_selected_1, {
            if (input$city_selected_1 == 'All') {
                redline_selected_1 <- redline_polygons
            } else {
                redline_selected_1 <- redline_polygons %>% filter(city == input$city_selected_1)
            }
            bounds_1 <- attributes(st_geometry(redline_selected_1))$bbox
            # update map 1 to appropriate bounds
            leafletProxy(mapId = 'map1') %>% 
                # fitBounds(lng1 = min(filtered.data$Longitude, na.rm = TRUE), lat1 = min(filtered.data$Latitude, na.rm = TRUE), lng2 = max(filtered.data$Longitude, na.rm = TRUE), lat2 = max(filtered.data$Latitude, na.rm = TRUE))
                fitBounds(lng1 = bounds_1[[1]]-0.01, 
                          lat1 = bounds_1[[2]]-0.01, 
                          lng2 = bounds_1[[3]]+0.01, 
                          lat2 = bounds_1[[4]]+0.01)
            # update map 2 to appropriate bounds
            leafletProxy(mapId = 'map2') %>% 
                # fitBounds(lng1 = min(filtered.data$Longitude, na.rm = TRUE), lat1 = min(filtered.data$Latitude, na.rm = TRUE), lng2 = max(filtered.data$Longitude, na.rm = TRUE), lat2 = max(filtered.data$Latitude, na.rm = TRUE))
                fitBounds(lng1 = bounds_1[[1]]-0.01, 
                          lat1 = bounds_1[[2]]-0.01, 
                          lng2 = bounds_1[[3]]+0.01, 
                          lat2 = bounds_1[[4]]+0.01)
        })
    
    # Observer to respond to zoom / pan of map1 and apply to map2
    # from: https://github.com/rstudio/leaflet/issues/347
        observe({
            coords <- input$map1_bounds
            if (!is.null(coords)) {
                tproxy <- leafletProxy('map2') %>% 
                    fitBounds(coords$west,
                              coords$south,
                              coords$east,
                              coords$north)
            }
        })

    # # Observer to respond to zoom / pan of map2 and apply to map1
    # # from: https://github.com/rstudio/leaflet/issues/347
    #     observe({
    #         coords_2 <- input$map2_bounds
    #         if (!is.null(coords_2)) {
    #             tproxy <- leafletProxy('map1') %>% 
    #                 fitBounds(coords_2$west,
    #                           coords_2$south,
    #                           coords_2$east,
    #                           coords_2$north)
    #         }
    #     })
    
}

shinyApp(ui, server)
        