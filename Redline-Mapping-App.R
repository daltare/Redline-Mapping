# NOTE: to publish this app, include the following folders (along with this script file):
#   - data_processed
#   - data_processed-analysis
#   - data_regulatory_actions_filter

# Define some initial settings ----
    # DEFINE DATA SOURCES - local or remote (remote = API)
        data_source_ces3 <- 'local' # CalEnviroScreen - getting from local source allows them to be simplified first
        data_source_rb_bounds <- 'local' # regional board boundaries
        data_source_service_areas <- 'local' # drinking water service areas
        # NOTE: CalEPA regulated sites can be either local or remote, select in app
        # other data sources that are always local
            # 303d_lines # these are from local souce because there is pre-processing done to get pollutant info
            # 303d_polygons - these are from local souce because there is pre-processing done to get pollutant info
            # redline_polygons - these could be pulled on the fly from the U of R site, but it's faster to download and join them ahead of time
    
        # Define coordinate systems to use for transformations
            projected_crs <- 3310 # see: https://epsg.io/3310 
                # other options: 26910 see: https://epsg.io/26910
                # resources: 
                    # https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=109326&inline
                    # 
            geographic_crs <- 4269 # see: https://epsg.io/4269
                # see: https://epsg.io/4326
                
                    
     
    # initial zoom level / location
        initial_zoom_level <- 'City' # options: 'Redline Bounds (All)', 'State','City'
    
# Load Packages ----
    # Shiny platform
        library(shiny) # the Shiny web application framework (https://shiny.rstudio.com) (https://cran.r-project.org/package=shiny)
    # General data analysis and transformation
        library(Hmisc)
        library(readr)
        library(readxl)
        library(janitor)
        # library(gtools)
        library(stringr)
        library(lubridate)
        library(vroom)
        library(janitor)
        library(data.table)
        library(tidyselect)
        library(purrr)
        library(tidyr)
        library(ggplot2)
        library(glue)
        library(units)
         library(dplyr)
    # API-related
        library(jsonlite)
        library(urltools)
    # Mapping and GIS operations
        library(sf)
        library(leaflet)
        library(htmlwidgets)
        library(geojsonsf)
        library(rmapshaper)
        library(htmltools)
        library(tmap)
    # workflow
        library(here)
    # shiny stuff
        library(shinycssloaders)
        library(DT)
        library(shinyWidgets)
        library(shinyjs)

# Read some static data into R ------------------------------------------------------------------------------------------------------------------
    # HOLC (Redline) Polygons
        # redline_polygons <- read_rds('data_processed/redline_polygons.RDS')
        # st_crs(redline_polygons) <- 4326
        redline_polygons <- st_read('data_processed/redline_polygons.gpkg')
        # transform to projected crs (for mapping and analysis it's best to use a projected CRS -- see: https://s3.amazonaws.com/files.zevross.com/workshops/spatial/slides/html/4-crs.html#31)
            redline_polygons <- redline_polygons %>% 
                st_transform(crs = projected_crs)
            redline_polygons <- redline_polygons %>% 
                mutate(area_calc_meters_sq = st_area(.)) %>% 
                drop_units()
            # st_crs(redline_polygons)
            
    # List of the CES parameter choices to plot
        ces_choices <- read_csv('data_processed/ces_names.csv') %>% # manually processed this file to make more descriptive names for the fields
            mutate(ces_variable = make_clean_names(name)) %>% #, 'parsed')) %>% 
            slice(8:64) %>%  # rows 8 to 64
            filter(type != 'other?')
        # reorder
        ces_choices <- ces_choices %>% 
            filter(group == 'overall', subgroup == 'overall') %>% 
            bind_rows(ces_choices %>% filter(group == 'pollution burden', subgroup == 'overall')) %>% 
            bind_rows(ces_choices %>% filter(group == 'pollution burden', subgroup == 'exposures')) %>% 
            bind_rows(ces_choices %>% filter(group == 'pollution burden', subgroup == 'environmental effects')) %>%
            bind_rows(ces_choices %>% filter(group == 'population characteristics', subgroup == 'overall')) %>% 
            bind_rows(ces_choices %>% filter(group == 'population characteristics', subgroup == 'sensitive populations')) %>% 
            bind_rows(ces_choices %>% filter(group == 'population characteristics', subgroup == 'socioeconomic factors')) %>%
            bind_rows(ces_choices %>% filter(group == 'demographic', subgroup == 'demographic')) %>%
            {.}
    # all CES field names
        ces_field_names <- read_csv('data_processed/ces_names.csv') %>% 
            mutate(ces_variable = make_clean_names(name)) #, 'parsed'))


    # CES 3 polygons using the 2010 tiger census tract geometry
        ces3_tiger <- st_read('data_processed/tiger_2010_tracts.gpkg') %>% 
            st_transform(projected_crs)
        
    # CalEPA regulated sites
        # calepa_reg_sites <- fread('data_regulatory_actions/Site.csv') %>%
        # # calepa_reg_sites <- read_csv('data_regulatory_actions/Site.csv') %>%
        #     tibble() %>% 
        #     clean_names() %>% 
        #     select(-dplyr::ends_with(as.character(0:9))) %>% 
        #     rename(zip_code = zip) %>% 
        #     mutate(state = 'ca', 
        #            data_source = 'CalEPA Regulated Site Portal') %>% 
        #     # mutate(coordinates = Map(c, longitude, latitude)) %>%  # see: https://stackoverflow.com/a/46396386
        #     arrange(site_id) %>% 
        #     # mutate(site_id = factor(site_id)) %>%
        #     {.}
        #     # check (NOTE: no missing coordinate data in the flat file)
        #         # range(calepa_reg_sites$latitude)
        #         # range(calepa_reg_sites$longitude)
    # regulated sites - processed version (with CES and HOLC data for each site included)
        calepa_sites_processed <- fread('data_regulatory_actions_filter/regulated_sites_processed_filter.csv') %>% 
            tibble() %>% 
            {.}
        
    # define choices for sources of CalEPA regulated site data
        site_source_choices <- c('None',
                                 'Select By Type (Source: CalEPA Geoserver)',
                                 'All Sites (Source: CalEPA Regulated Site Portal)'
                                 )
        
    # List of program types to select
        program_types <- fread('data_regulatory_actions_filter/SiteEI_filter.csv') %>%
        # program_types <- read_csv('data_regulatory_actions/SiteEI.csv') %>%
        # program_types <- read_csv('data_regulatory_actions/Site Regulated Programs.zip', guess_max = 100000) %>%
            tibble() %>% 
            clean_names() %>% 
            select(-dplyr::ends_with(as.character(0:9))) %>%
            select(site_id, ei_description) %>%
            distinct() %>%
            {.}
        program_types_distinct <- program_types %>% 
            select(ei_description) %>% 
            distinct() %>% 
            arrange(ei_description) %>% 
            pull(ei_description)

    # CalEPA regulatory data
        # all regulatory data (processed)
            regulatory_data_processed <- fread('data_regulatory_actions_filter/regulatory_actions_processed_filter.csv') %>% 
                tibble()
        # all regulatory data (processed and summarized by FY)
            regulatory_data_processed_fy_summary <- fread('data_regulatory_actions_filter/regulatory_actions_processed-fy_summary_filter.csv') %>% 
                tibble()
        # inspections
            inspections_all_download <- fread('data_regulatory_actions_filter/Evaluations_filter.csv') %>%
                # # inspections_all_download <- read_csv('data_regulatory_actions/Evaluations.zip') %>% 
                # # inspections_all_download <- read_csv('data_regulatory_actions/Evaluations.csv') %>%
                # # inspections_all_download <- vroom('data_regulatory_actions/Evaluations.csv') %>% 
                # tibble() %>% 
                # clean_names() %>%
                # select(-dplyr::ends_with(as.character(0:9))) %>%
                # mutate(eval_date = mdy(eval_date)) %>% 
                # mutate(site_id = as.integer(site_id)) %>% 
                # arrange(site_id, eval_date) %>% 
                # # mutate(site_id = factor(site_id, levels = calepa_reg_sites$site_id)) %>%
                {.}
        # violations
            violations_all_download <- fread('data_regulatory_actions_filter/Violations_filter.csv') %>% #, guess_max = 1000, trim_ws = FALSE) %>%
                # # violations_all_download <- read_csv('data_regulatory_actions/Violations.zip') %>% #, guess_max = 1000, trim_ws = FALSE) %>% 
                # # violations_all_download <- read_csv('data_regulatory_actions/Violations.csv') %>% #, guess_max = 1000, trim_ws = FALSE) %>%
                # # violations_all_download <- vroom('data_regulatory_actions/Violations.csv') %>% #, guess_max = 1000, trim_ws = FALSE) %>% 
                # tibble() %>%
                # clean_names() %>% 
                # select(-dplyr::ends_with(as.character(0:9))) %>%
                # mutate(violation_date = mdy(violation_date)) %>% 
                # mutate(site_id = as.integer(site_id)) %>% 
                # arrange(site_id, violation_date) %>% 
                # # mutate(site_id = factor(site_id, levels = calepa_reg_sites$site_id)) %>%
                {.}      
        # enforcement actions
            enforcement_all_download <- fread('data_regulatory_actions_filter/EA_filter.csv') %>%
                # # enforcement_all_download <- read_csv('data_regulatory_actions/Enforcements.zip') %>% 
                # # enforcement_all_download <- read_csv('data_regulatory_actions/EA.csv') %>%
                # # enforcement_all_download <- vroom('data_regulatory_actions/EA.csv') %>% 
                # tibble() %>%
                # clean_names() %>% 
                # select(-dplyr::ends_with(as.character(0:9))) %>%
                # mutate(enf_action_date = mdy(enf_action_date)) %>% 
                # mutate(site_id = as.integer(site_id)) %>% 
                # arrange(site_id, enf_action_date) %>% 
                # # mutate(site_id = factor(site_id, levels = calepa_reg_sites$site_id)) %>%
                {.}
        # get the date of the most recent regulatory records
            most_recent_reg_records <- max(
                max(inspections_all_download %>% 
                        filter(eval_date <= Sys.Date()) %>% 
                        pull(eval_date)),
                max(violations_all_download %>% 
                        filter(violation_date <= Sys.Date()) %>% 
                        pull(violation_date)),
                max(enforcement_all_download %>%
                        filter(enf_action_date <= Sys.Date()) %>% 
                        pull(enf_action_date))
            )
            
    # read data for the Redline-CES analysis
        analysis_clipped_ces <- st_read('data_processed-analysis/ces_clipped_to_holc_bounds.gpkg')
        analysis_raw_scores <- st_read('data_processed-analysis/holc_area_weighted_scores.gpkg')
            analysis_raw_scores <- analysis_raw_scores %>% mutate(geom_centroid = st_centroid(.))
        analysis_departure_scores <- st_read('data_processed-analysis/departure-area_weighted_scores.gpkg')
        analysis_citywide_scores <- st_read('data_processed-analysis/citywide_avg-area_weighted_scores.gpkg')
        analysis_z_scores <- st_read('data_processed-analysis/z_scores-area_weighted_scores.gpkg')
        
        # centroid scores / info
            analysis_centroid_scores <- st_read('data_processed-analysis/holc_centroid_scores.gpkg')
                analysis_centroid_scores <- analysis_centroid_scores %>% mutate(geom_centroid = st_centroid(.))
            analysis_centroid_departure_scores <- st_read('data_processed-analysis/departure-centroid_scores.gpkg')
            analysis_ces_centroids <- st_read('data_processed-analysis/ces_centroids.gpkg')
            analysis_holc_centroids <- st_read('data_processed-analysis/redline_polygons_centroid.gpkg')
            analysis_centroid_connecting_lines <- st_read('data_processed-analysis/holc_centroid_ces_connecting_lines.gpkg')

    # California Counties
        ca_counties <- st_read('data_processed/CA_Counties.gpkg') %>% 
            clean_names()

# create widget for data range input (selecting month/year only) ----
# see: https://stackoverflow.com/a/54922170
    dateRangeInput2 <- function(inputId, label, minview = "days", maxview = "decades", ...) {
      d <- shiny::dateRangeInput(inputId, label, ...)
      d$children[[2L]]$children[[1]]$attribs[["data-date-min-view-mode"]] <- minview
      d$children[[2L]]$children[[3]]$attribs[["data-date-min-view-mode"]] <- minview
      d$children[[2L]]$children[[1]]$attribs[["data-date-max-view-mode"]] <- maxview
      d$children[[2L]]$children[[3]]$attribs[["data-date-max-view-mode"]] <- maxview
      d
    }
        
        
# select a city to plot when the app loads
    initial_selected_city <- sample(unique(redline_polygons$holc_city), 1) # pick a random city # un-comment this line if the 'City' option is selected in the line above

# get hulls for holc map areas (used to filter regulated site data from the geoserver)
    holc_map_hulls <- st_read('data_processed/holc_map_hulls.gpkg')

# ----------------------------------------------------------------------------------------------------------------------------------------------- #       
# ----------------------------------------------------------------------------------------------------------------------------------------------- #
# Define UI --------------------------------------------------------------------
ui <- navbarPage(title = "California's Redlined Communities", # theme = shinythemes::shinytheme('flatly'),
    # Home tab ----
    id = "tab_being_displayed", # will set input$tab_being_displayed
        tabPanel('Home',
                 h3('Welcome to the CalEPA Redlining and Environmental Justice Tool'),
                 p('This tool was developed by the CalEPA to allow staff and the public
                   to access and assess CalEPA data as it relates to environmental justice and
                   racial equity isses.'),
                 p('The ',
                   tags$b('Maps & CalEPA Data'),
                   ' tab allows users to import CalEPA regulatory data (inspections, violations,
                     and enforcement actions) in areas that were assessed by the federal
                     government\'s Home Owners Loan Corporation (HOLC) in the 1930s (part of a process
                     now commonly known as \'redlining\'), and view that data relative to the HOLC
                     \'redlining\' maps as well as present-day indicators of pollution burden and
                     vulnerability from the CalEPA\'s CalEnviroScreen 3.0 (CES) tool. Use the options
                   in the pane on the left side of the screen to select a city, select data types to
                   import, and apply additional filters to the data. The dataset is also available in
                   the table below the map, and can be downloaded for use outside of this tool.'
                   ),
                   p('The ',
                     tags$b('Redline-CES Analysis'),
                     ' tab takes users through a study that investigates potential correlations
                       between the historical HOLC redlining maps and the current day indicators
                       from CES 3.0, to determine if the trends detected and promoted by the HOLC
                       appraisal and mapping practices are still present today. Users can select any
                     CES indicator to assess, and can view maps showing any of the eight California
                     cities that were assessed by the HOLC in the 1930s.'
                   ),
                 p('The ',
                   tags$b('Background Information'),
                   ' tab has additional information about the history of HOLC redlining, information
                   about the datasets used in this tool, and links to the source code and contact
                   information.'
                   )
                 ),
        # Maps / Tabular Data Tab ----
        tabPanel('Maps & CalEPA Data', icon = icon('map'),
                 useShinyjs(),
                 tags$head(
                     # Code to resize main panel to 100% width after show/hide sidebar button clicked
                     # see: https://stackoverflow.com/a/33424463
                     tags$script(
                         HTML("
                                $(document).ready(function(){
                                  // Mark columns we want to toggle
                                  $('body').find('div [class=col-sm-4]').addClass('sidebarPanel');
                                  $('body').find('div [class=col-sm-8]').addClass('mainPanel');
                                })
                    
                    
                                Shiny.addCustomMessageHandler ('resize',function (message) {
                                  $('.sidebarPanel').toggle();
                                  $('.mainPanel').toggleClass('col-sm-8 col-sm-12');
                                  $(window).trigger('resize')
                                });
                    
                               ")
                     ),
                     # define button style (background color and font color)
                     tags$style(".buttonstyle{background-color:#f2f2f2;} .buttonstyle{color: black;}")
                 ), 
                 # Sidebar layout with input and output definitions
                 sidebarLayout(
                     # Sidebar
                     # div(id ="Sidebar", sidebarPanel(
                     sidebarPanel(
                         # Inputs:
                         selectInput(inputId = 'city_selected_1', 
                                     label = 'Zoom To:', 
                                     choices = c(unique(redline_polygons$holc_city)), # 'Statewide', 
                                     selected = initial_selected_city), # ifelse (initial_zoom_level == 'State', 'Statewide', initial_selected_city)),
                         hr(style = "border: 1px solid darkgrey"),
                         h4('CalEPA Regulated Sites:'),
                         # checkboxGroupInput(inputId = 'site_type_1', 
                         #                    label = 'Select CalEPA Regulated Site Types:', 
                         #                    choices = c('CIWQS', 'SMARTS', 'GeoTracker'),
                         #                    selected = NULL),
                         selectInput(inputId = 'sites_source', 
                                     label = 'Site Selection Options:', 
                                     choices = site_source_choices, 
                                     selected = site_source_choices[1]),
                         selectInput(inputId = 'site_type_1', 
                                     label = 'Select Site Types:', 
                                     choices = c('California Integrated Water Quality System (CIWQS)', 
                                                 'GeoTracker', 
                                                 'Storm Water Multiple Application and Report Tracking System (SMARTS)',
                                                 'Chemical Hazards - Fire',
                                                 'Chemical Hazards - Reactivity',
                                                 'Chemical Hazards - Sudden Release of Pressure',
                                                 'California Environmental Reporting System (CERS)',
                                                 'EnviroStor Cleanup (ENVSTORCLN)',
                                                 'EnviroStor Hazardous Waste (ENVSTORHAZ)',
                                                 'National Emissions Inventory System (EIS)',
                                                 'Toxic Release Inventory (TRI)'
                                     ),
                                     selected = NULL, 
                                     multiple = TRUE),
                         # checkboxGroupInput(inputId = 'holc_rating_sites_filter',
                         #                    label = 'Select HOLC Rating:',
                         #                    choices = list('A (Best)' = 'A',
                         #                                   'B (Still Desirable)' = 'B',
                         #                                   'C (Definitely Declining)' = 'C',
                         #                                   'D (Hazardous)' = 'D'),
                         #                    selected = c('A','B','C','D')),
                         hr(style = "border: 1px solid darkgrey"),
                         h4('Redlining Data:'),
                         tags$b('Filter For Sites By HOLC Grade or Location:'),
                         switchInput(inputId = 'holc_rating_sites_filter_on_off', 
                                     value = FALSE, 
                                     size = 'small'),
                         selectInput(inputId = 'holc_rating_sites_filter', 
                                     label = 'Select Sites By HOLC Polygon Rating:',
                                     choices = list('A (Best)' = 'A',
                                                    'B (Still Desirable)' = 'B',
                                                    'C (Definitely Declining)' = 'C',
                                                    'D (Hazardous)' = 'D'),
                                     selected = c('A','B','C','D'),
                                     multiple = TRUE),
                         selectInput(inputId = 'holc_city_sites_filter', 
                                     label = 'Select Sites By City:',
                                     choices = c('Fresno', 'Los Angeles', 'Oakland', 
                                                 'Sacramento', 'San Diego', 'San Francisco',
                                                 'San Jose', 'Stockton'),
                                     selected = c('Fresno', 'Los Angeles', 'Oakland', 
                                                  'Sacramento', 'San Diego', 'San Francisco',
                                                  'San Jose', 'Stockton'),
                                     multiple = TRUE),
                         hr(style = "border: 1px solid darkgrey"),
                         h4('Environmental, Public Health, & Socieconomic Data:'),
                         selectInput(inputId = 'ces_parameter', 
                                     label = 'Select CalEnviroScreen (CES) Parameter:', 
                                     choices = ces_choices$name, 
                                     selected = ces_choices$name[1]),
                         # sliderInput(inputId = 'ces_score_range', 
                         #             label = 'Filter Sites By Score of Selected CES Parameter:', 
                         #             min = 0, max = 100, value = c(0,100)),
                         uiOutput('ces_range_filter'),
                         hr(style = "border: 1px solid darkgrey"),
                         h4('CalEPA Regulatory Actions:'),
                         # dateRangeInput2(inputId = "sites_date_range", 
                         #                 label = "Select Date Range For Inspections, Violations, & Enforcement Actions:*", 
                         #                 startview = "year", 
                         #                 minview = "years", # "months"
                         #                 maxview = "decades", 
                         #                 format = 'yyyy/yyyy+1', # 'yyyy-mm',
                         #                 start = NULL, # as.Date("2010-01-01"),
                         #                 end = NULL, #as.Date(paste(year(Sys.Date()), month(Sys.Date()), 1, sep = '-')),
                         #                 min = as.Date("1900-01-01"),
                         #                 max = as.Date(paste(year(most_recent_reg_records), month(most_recent_reg_records), 1, sep = '-'))),
                         sliderTextInput(
                             inputId = "date_range_fy", 
                             label = "Select Date Range For Inspections, Violations, & Enforcement Actions by Fiscal Year (Fiscal Year is July 1 through June 30):*", 
                             grid = FALSE, 
                             force_edges = TRUE,
                             # choices = if (lubridate::month(Sys.Date()) <= 6) {
                             #     c(paste0(str_pad(c(90:99,0:(lubridate::year(Sys.Date())-1-2000)), width = 2, pad = 0), 
                             #              '/', 
                             #              str_pad(c(91:99, 0:(lubridate::year(Sys.Date())-2000)), width = 2, pad = 0)))
                             # } else {
                             #     c(paste0(str_pad(c(90:99,0:(lubridate::year(Sys.Date())-2000)), width = 2, pad = 0), 
                             #              '/', 
                             #              str_pad(c(91:99, 0:(lubridate::year(Sys.Date())-2000+1)), width = 2, pad = 0)))
                             # },
                             # selected = c('90/91', 
                             #              if (lubridate::month(Sys.Date()) <= 6) {
                             #                  paste0(lubridate::year(Sys.Date())-1-2000, '/',lubridate::year(Sys.Date())-2000)
                             #                  } else {
                             #                      paste0(lubridate::year(Sys.Date())-2000, '/',lubridate::year(Sys.Date())-2000+1)})
                             if (lubridate::month(Sys.Date()) <= 6) {
                                 c(paste0(1990:(lubridate::year(Sys.Date())-1), 
                                          ' / ', 
                                          1991:(lubridate::year(Sys.Date()))))
                             } else {
                                 c(paste0(1990:(lubridate::year(Sys.Date())), 
                                          ' / ', 
                                          1991:(lubridate::year(Sys.Date())+1)))
                             },
                             selected = c('1990 / 1991', 
                                          if (lubridate::month(Sys.Date()) <= 6) {
                                              paste0(lubridate::year(Sys.Date())-1, ' / ',lubridate::year(Sys.Date()))
                                              } else {
                                                  paste0(lubridate::year(Sys.Date()), ' / ',lubridate::year(Sys.Date())+1)})
                         ),
                         tags$em(textOutput('date_range_selected')),
                         tags$em(textOutput('data_availability_msg')),
                         br(),
                         # checkboxGroupInput(inputId = 'show_violations_enforcement',
                         #                    label = 'Filter For CalEPA Regulated Sites With:',
                         #                    choices = c('Violations', 'Enforcement Actions'),
                         #                    selected = NULL),
                         selectInput(inputId = 'show_violations_enforcement',
                                     label = 'Filter For Sites With Inspections, Violations, or Enforcement Actions In Selected Time Period:',
                                     choices = c('Inspections', 'Violations', 'Enforcement Actions'),
                                     selected = NULL, 
                                     multiple = TRUE),
                         # select program types
                         # NOTE: can be either static (showing all options) or dynamic (showing only the options available as the result of the filtered sites))
                         # the dynamic option might be problematic becasue it automatically resets every time a new filter is chosen
                         uiOutput('program_type_1'),
                         tags$em(p('**this filter resets whenever the other filters above are changed')),
                         # selectInput(inputId = 'program_type_1',
                         #             label = 'Filter Sites By Program Type:',
                         #             multiple = TRUE,
                         #             choices = program_types_distinct)
                         hr(style = "border: 1px solid darkgrey"),
                         h4('Additional Map Layers:'),
                         selectInput(inputId = 'additional_map_layers',
                                     label = 'Select Layers:',
                                     choices = c('303d Listed Waterbodies', 
                                                 'Drinking Water Provider Service Areas',
                                                 'State Water Board Region Boundaries'),
                                     selected = NULL, 
                                     multiple = TRUE),
                         hr(),
                         br(),
                         br()
                         # )
                     ),
                # Main panel for displaying outputs 
                mainPanel(
                    fluidRow(
                        div(style = "display:inline-block;vertical-align:top;",
                            # actionButton("toggleSidebar", "Toggle sidebar"),
                            actionButton("showpanel", "Show/Hide Sidebar", 
                                         class = "buttonstyle", icon = icon('bars'), 
                                         style = 'padding:4px; font-size:80%'), # 'display:inline;'), 
                            p(tags$b('NOTE:'),
                              paste('Use the left side map to pan/zoom, and use the button in the upper right corner of each map to toggle layers on or off (some map layers must be activated in the sidebar on the left first).',
                                    'Tabular data for the selected sites is available to view/download below the maps.'),
                              style = 'padding:4px; font-size:80%'#,
                              # style = 'display:inline'
                              # hr()
                            )
                        )
                    ),
                    fluidRow(
                        column(width = 6, 
                               #tags$h5('CalEPA Data:', style = 'padding:4px;'),
                               p(tags$b('CalEPA Data:'), style = 'padding:1px;'),
                               leafletOutput(outputId = 'map1', height = 700) %>% withSpinner(color="#0dc5c1")),
                        column(width = 6, 
                               # tags$h5('HOLC (Redline) Data:', style = 'padding:4px;'),
                               p(tags$b('HOLC (Redline) Data:'), style = 'padding:1px;'),
                               leafletOutput(outputId = 'map2', height = 700) %>% withSpinner(color="#0dc5c1"))
                    ),
                    fluidRow(
                        hr(style = "border: 3px solid darkgrey"),
                        h4('Tabular Data For Selected Sites & Regulatory Actions:')
                    ),
                    fluidRow(
                        div(style = "display:inline-block;vertical-align:top;",
                            p('Download all supporting regulatory data: ', style = 'display:inline'), HTML('&emsp;'),
                            downloadButton('downloadInspections', 'Inspection Data', class = "buttonstyle", style = 'display:inline'), HTML('&emsp;'),
                            downloadButton('downloadViolations', 'Violation Data', class = "buttonstyle", style = 'display:inline'), HTML('&emsp;'),
                            downloadButton('downloadEnforcements', 'Enforcement Data', class = "buttonstyle", style = 'display:inline'), HTML('&emsp;')
                        )
                    ),
                    fluidRow(
                        hr(),
                        div(style = "display:inline-block;vertical-align:top;",
                            p('Download data in the table below: ', style = 'display:inline'), HTML('&emsp;'),  
                            downloadButton('download_summary', 'Download Data', class = "buttonstyle"), HTML('&emsp;')
                        ),
                        DT::dataTableOutput('summary_table')
                    )
                )
            ),
        ),
    # Analysis Tab ----
    tabPanel('Redline-CES Analysis', 
             icon = icon('chart-bar'),
             fluidRow(
                 column(width = 12, 
                        style = 'padding-left:9px; padding-right:9px; padding-top:5px; padding-bottom:5px', 
                        offset = 0,
                        # p('Working on it...'),
                        h3('Redline-CalEnviroScreen Analysis'),
                        #  --------------------- INTRODUCTORY TEXT ---------------------
                        p('This section investigates potential correlations between: (1) Residential 
                          Security (i.e., redline) maps created by the federal government\'s 
                          Home Owners’ Loan Corporation (HOLC) in major California cities in 
                          the 1930s, and (2) current measures of public health, environmental 
                          conditions, and socioeconomic characteristics in areas assessed by the 
                          HOLC maps.'),
                        # p('This section investigates potential correlations between: '),
             #            )
             #     ),
             # fluidRow(
             #     column(width = 12, 
             #            style = 'padding-left:20px; padding-right:9px; padding-top:0px; padding-bottom:5px',
             #            offset = 0,
             #            tags$li('Residential Security (i.e., Redline) maps created by the federal government\'s Home Owners’ 
             #            Loan Corporation (HOLC) in major California cities in the 1930s, and'),
             #            tags$li('current measures of public health, environmental conditions, and socioeconomic 
             #            characteristics in areas assessed by the HOLC maps.'),
             #            br()
             #     )
             # ),
             # fluidRow(
             #     column(width = 12, 
             #            style = 'padding-left:9px; padding-right:9px; padding-top:5px; padding-bottom:5px',
             #            offset = 0,
                        p('The HOLC maps evaluated mortgage lending risk in different neighborhoods within 
                         each city on a scale of A through D (A = "Best", B = "Still Desirable", C = "Definitely 
                         Declining", D = "Hazardous"), and relied in part on explicit assessments of the racial and 
                         ethnic makeup of each neighborhood. 
                           This process - now more commonly known as "Redlining" - likely had 
                           significant impacts on the trajectories of neighborhoods and their residents by 
                           creating systematic differences in access to public and private capital, with 
                           resulting disparities in pathways to home ownership, community development, 
                           and other economic and social opportunities. 
                           For more information about the history of Redlining and other studies of its effects, 
                           see the \"Background Information\" tab.'),
                        # systematically reduced or denied access to public and private 
                        # capital to neighborhoods and residents in areas classified in higher risk categories. 
                        # The resulitng inequalities in pathways to home ownership, community development, 
                        # and other economic opportunities between areas with different classifications likely 
                        # had significant impacts on the socioeconomic trajectories of those neighborhoods 
                        # and their residents. 
                        p('Indicators of current conditions come from ', 
                          tags$a(href = 'https://oehha.ca.gov/calenviroscreen/report/calenviroscreen-30', 
                                 'CalEnviroScreen 3.0'), 
                          ' (CES), which assigns a score to each census tract in California for 12 indicators 
                           of pollution burden and 8 indicators of population characteristics associated with 
                           increased vulnerability to pollution\'s health effects. 
                           Scores increase with increasing pollution burden or vulnerability for 
                           the selected indicator (i.e., lower scores indicate census tracts with relatively low 
                           pollution burden or vulnerabilty, while higher scores indicate tracts with relatively 
                           high pollution burden or vulnerability). For more information about CalEnviroScreen 
                           3.0, see the ', 
                          tags$a(href = 'https://oehha.ca.gov/media/downloads/calenviroscreen/fact-sheet/ces30factsheetfinal.pdf',
                                 'Fact Sheet'),
                          ' and the ',
                          tags$a(href = 'https://oehha.ca.gov/calenviroscreen/indicators',
                                 'Indicators Overview'), 
                          'webpage.'),
                        p('Because the geographic unit of analysis of the HOLC maps and the CES indicators is not 
                          consistent, this analysis starts by calculating an aggregated CES indicator score 
                          for each polygon in the HOLC maps (for a given CES indicator). This aggregation process
                          is described in more detail in the section below and shown in the accompanying maps. The 
                          aggregated scores at the HOLC polygon level are then used in the plots in the following 
                          section to analyze potential patterns and relationships between current conditions 
                          and historical HOLC ratings.'),
                        # hr(),
                        h4('Inputs'),
                        p('For the analysis below, select a CES indicator to analyze, and a method for aggregating CES scores at the 
                          HOLC polygon level:')
                        )
                 ), 
                fluidRow(
                    #  --------------------- INPUTS ---------------------
                    column(width = 11,
                        style = 'padding-left:9px; padding-right:9px; padding-top:5px; padding-bottom:5px',
                        offset = 1,
                        div(style = "display:inline-block;vertical-align:top;",
                            selectInput(inputId = 'analysis_indicator_selection',
                                        label = 'Select a CES Indicator:',
                                        choices = ces_choices %>% filter(type == 'score') %>% pull(name),
                                        selected = ces_choices$name[1], 
                                        multiple = FALSE)),
                        div(style = "display:inline-block;vertical-align:top;",
                            HTML('&emsp;')),
                        div(style = "display:inline-block;vertical-align:top;", 
                            selectInput(inputId = 'analysis_aggregation_method', 
                                        label = 'Select a CES Score Aggregation Method:',
                                        choices = c('Area Weighted Average', 'Nearest Centroid'),
                                        selected = c('Area Weighted Average'),
                                        multiple = FALSE))
                    )
                ),
             fluidRow(
                    column(width = 12,
                        style = 'padding-left:9px; padding-right:9px; padding-top:5px; padding-bottom:5px',
                        offset = 0,
                        #  --------------------- ANALYSIS MAPS ---------------------
                        hr(style = "border: 3px solid darkgrey"),
                        h4('Aggregation of CES Scores by HOLC Map Polygons:'),
                        p('The areas delineated in the HOLC maps and the census tracts used to assign the CES 
                           scores have different coverages, as shown in the maps below. There are multiple 
                           methods that could be used to approximate a CES score for each polygon in the HOLC 
                           maps, such as area weighted average or nearest centroid'),
                           # ', or weighted average by road length.'),
                        p('An area weighted average method identifies the portions of the CES polygons 
                           that overlap each HOLC polygon, then computes the area weighted average of those 
                           overlapping portions of CES polygons. The maps below demonstrate this method: (1) the 
                           left pane shows the polygons from the HOLC map for the selected city, (2) the 
                           second pane (from left) shows the CES indicator scores (at the census tract level) 
                           with the HOLC map superimposed on top (border colors represent HOLC rating), 
                           (3) the third pane (from left) shows the portions of the CES polygons that 
                           overlap with the HOLC polygons, and (4) the right pane shows the aggregated CES
                           scores for each HOLC polygon.'),
                        p('The nearest centroid method matches each HOLC polygon with a single CES polygon by
                          matching that HOLC polygon\'s centroid with the nearest centroid of a CES polygon.
                          When this method is selected, the map in the third pane from left (3) shows the centroids of
                          all HOLC polgons (shown as points colored by HOLC grade), centroids of CES polygons 
                          (shows as grey colored points), and the matched HOLC-CES centroid pairs (showns as 
                          grey lines).'),
                        # p('Select a city to display in the maps below:'),
                        # div(style = "display:inline-block;vertical-align:top;",
                        #     HTML('&emsp;')),
                        div(style = "display:inline-block;vertical-align:top;",
                            selectInput(inputId = 'analysis_city_selection',
                                        label = 'Select a City to Display in the Maps Below:',
                                        choices = unique(redline_polygons$holc_city),
                                        selected = sample(unique(redline_polygons$holc_city), 1), # pick a random city
                                        multiple = FALSE)#,
                            ),
                        # div(style = "display:inline-block;vertical-align:top;",
                        #     HTML('&emsp;')),
                        div(style = "display:inline-block;vertical-align:top;",
                            p(tags$b('(NOTE: use the left side pane to pan/zoom all panes | hover or click on map elements for more information)')))
                        # (for example, if an HOLC polygon with an area of 200 units 
                        #   overlaps portions of CES polygons A and B, where A has a score of 30 and an 
                        #   overlapping area of 120 units, and B has a score of 50 and an overlapping area of 80 
                        #   units, the area weighted average score for the HOLC polygon is: 
                        #   [30 x 120 + 50 x 80] / 200 = 38')  
                 )
             ),
             # INSERT MAPS
             fluidRow(
                 column(width =  3, 
                        style = 'padding-left:2px; padding-right:2px; padding-top:5px; padding-bottom:5px; text-align:center;',
                        p(tags$b('(1) HOLC Polygons:')),
                        leafletOutput('analysis_map_holc') %>% withSpinner(color="#0dc5c1")
                 ),
                 column(width =  3, 
                        style = 'padding-left:2px; padding-right:2px; padding-top:5px; padding-bottom:5px; text-align:center',
                        p(tags$b('(2) HOLC & CES Polygons:')),
                        leafletOutput('analysis_map_overlap') %>% withSpinner(color="#0dc5c1")
                 ),
                 column(width =  3, 
                        style = 'padding-left:2px; padding-right:2px; padding-top:5px; padding-bottom:5px; text-align:center',
                        uiOutput('map_3_text'),
                        leafletOutput('analysis_map_intersection') %>% withSpinner(color="#0dc5c1")
                 ),
                 column(width =  3, 
                        style = 'padding-left:2px; padding-right:2px; padding-top:5px; padding-bottom:5px; text-align:center',
                        p(tags$b('(4) Aggregated CES Scores by HOLC Polygon:')),
                        leafletOutput('analysis_map_aggregated_scores') %>% withSpinner(color="#0dc5c1")
                 )
             ),
             fluidRow(
                 column(width = 12, 
                        style = 'padding-left:9px; padding-right:9px; padding-top:5px; padding-bottom:5px',
                     hr(style = "border: 1px solid darkgrey"),
                     p('The maps below show the aggregated CES score for each HOLC polygon (from the 
                       right-side plot above), separated by HOLC grade.')
                 )
             ), 
             fluidRow(
                 # title
                 column(width = 10, 
                        style = 'padding-left:2px; padding-right:2px; padding-top:2px; padding-bottom:2px', 
                        offset = 2,
                        div(style = "display:inline-block;vertical-align:top;",
                            p(tags$b('Aggregated CES Scores by HOLC Polygon (use the upper-left 
                       pane to pan/zoom all panes):')),
                            # h5('Aggregated CES Scores by HOLC Polygon:')
                            # h4('Aggregated CES Scores by HOLC Polygon:')
                        )# ,
                        # div(style = "display:inline-block;vertical-align:top;",
                        #     HTML('&emsp;')
                        # ),
                        # div(style = "display:inline-block;vertical-align:top;",
                        #     checkboxInput(inputId = 'facet_map_labels', 
                        #                   label = 'Show score labels', 
                        #                   value = FALSE)
                        # )
                 )
             ),
             fluidRow(
                 # A
                 column(width = 4, 
                        style = 'padding-left:2px; padding-right:2px; padding-top:0px; padding-bottom:0px; text-align:center;', 
                        offset = 2,
                        p(tags$b('HOLC Grade A (Best)')),
                        leafletOutput('analysis_map_facet_A', height = 300) %>% withSpinner(color="#0dc5c1")
                 ),
                 # B
                 column(width = 4, 
                        style = 'padding-left:2px; padding-right:2px; padding-top:0px; padding-bottom:0px; text-align:center;', 
                        offset = 0,
                        p(tags$b('HOLC Grade B (Still Desirable)')),
                        leafletOutput('analysis_map_facet_B', height = 300) %>% withSpinner(color="#0dc5c1")
                 )
             ),
             fluidRow(
                 # C
                 column(width = 4, 
                        style = 'padding-left:2px; padding-right:2px; padding-top:10px; padding-bottom:0px; text-align:center;', 
                        offset = 2,
                        p(tags$b('HOLC Grade C (Definitely Declining)')),
                        leafletOutput('analysis_map_facet_C', height = 300) %>% withSpinner(color="#0dc5c1")
                 ),
                 # D
                 column(width = 4, 
                        style = 'padding-left:2px; padding-right:2px; padding-top:10px; padding-bottom:0px; text-align:center;', 
                        offset = 0,
                        p(tags$b('HOLC Grade D (Hazardous)')),
                        leafletOutput('analysis_map_facet_D', height = 300) %>% withSpinner(color="#0dc5c1")
                 )
             ),
             fluidRow(
                 column(width = 10, 
                        style = 'padding-left:2px; padding-right:2px; padding-top:2px; padding-bottom:2px', 
                        offset = 2,
                        div(style = "display:inline-block;vertical-align:top;",
                            checkboxInput(inputId = 'facet_map_labels',
                                          label = 'Show score labels',
                                          value = FALSE)
                        )
                 )
             ),
             #  --------------------- ANALYSIS PLOTS ---------------------
             fluidRow(
                 column(width = 12, 
                        style = 'padding-left:9px; padding-right:9px; padding-top:5px; padding-bottom:5px',
                        hr(style = "border: 3px solid darkgrey"),
                        h4('Analysis of CES scores by HOLC grade:'),
                        # DOT PLOTS BY CITY (RAW AND DEPARTURE SCORES - NOT FACETED)
                        p('In the plots below, each point represents an individual polygon in the HOLC maps. 
                           The score for each point represents the aggregated CES score for that HOLC polygon for the selected indicator, 
                           and the color represents the grade assigned to that HOLC polygon (A = green, B = 
                           blue, C = yellow, D = red).'),
                        p('The left-side plot below shows the score for each HOLC polygon, grouped by by city. 
                             The black circles represent the average score for each city, and the black lines 
                             represent one standard deviation above and below the mean.'),
                        p('The right-side plot shows the difference between each polygon\'s score and its
                             respective city-wide average score (the city-wide average score is subtracted from 
                             each polygon\'s score in that city). This 
                            \"departure\" score makes it possible to compare the development of different HOLC 
                            rated areas within each city and helps take into account some of the differences in
                            trajectories of development between cities (e.g., regional patterns of economic 
                            development, gentrification, etc.).'),
                        p('The lower plot below shows those \"departure\" scores (from the right-side plot) separated
                            by HOLC grade.'),
                 )
             ),
             fluidRow(
                 column(width = 6, 
                        style = 'padding-left:9px; padding-right:9px; padding-top:5px; padding-bottom:5px',
                        plotOutput('plot_point_raw')),
                 column(width = 6, 
                        style = 'padding-left:9px; padding-right:9px; padding-top:5px; padding-bottom:5px', 
                        plotOutput('plot_point_departures'))
             ),
             # fluidRow(
             #     # plotOutput('plot_raw_scores_cities'),
             # ),
             fluidRow(
                 column(width = 12, 
                        style = 'padding-left:9px; padding-right:9px; padding-top:5px; padding-bottom:5px',
                        # hr(style = "border: 1px solid darkgrey"),
                        # FACETED DOT PLOT (BY CITY AND HOLC GRADE) - DEPARTURES
                        plotOutput('plot_departures_cities'),
                 )
             ),
             fluidRow(
                 column(width = 12, 
                        style = 'padding-left:9px; padding-right:9px; padding-top:5px; padding-bottom:5px',
                        hr(style = "border: 1px solid darkgrey"),
                        # BOX PLOT
                        p('The box plot below displays the distribution of the \"departure\" scores (represented in the plot
               above) across all cities, grouped by HOLC grade. Each of the dots represents an 
               individual polygon from the HOLC maps (and corresponds to a dot in the plot above).'
               # The notch in the center of each box represents the approximate 95% confidence interval for the true value of the 
               # median for that group (if the notches of two boxes do not overlap, this suggests that the 
               #     medians are significantly different).'
                   ),
                        plotOutput('plot_box_departures')
                        )
                 ),
             fluidRow(
                 column(width = 12, 
                        style = 'padding-left:100px; padding-right:9px; padding-top:5px; padding-bottom:5px',
                        div(style = "display:inline-block;vertical-align:center;",
                            p(tags$b('Adjust axis range: '))),
                        div(style = "display:inline-block;vertical-align:center;",
                            HTML('&emsp;')),
                        div(style = "display:inline-block;vertical-align:center;",
                            numericInput(inputId = 'boxplot_axis_min', 
                                         label = 'Axis Minimum Value: ',
                                         value = NULL)),
                        div(style = "display:inline-block;vertical-align:center;",
                            HTML('&emsp;')),
                        div(style = "display:inline-block;vertical-align:center;",
                            numericInput(inputId = 'boxplot_axis_max', 
                                         label = 'Axis Maximum Value: ', 
                                         value = NULL))
                 )
             ),
             fluidRow(
                  column(width = 12, 
                         style = 'padding-left:9px; padding-right:9px; padding-top:5px; padding-bottom:5px',
                        hr(style = "border: 1px solid darkgrey"),
                        # DOT PLOT OF AVERAGES
                        p('The plot below shows the average departure score for each HOLC class within
               each city.'), # the size of the dots represent the number of HOLC polygons in that class/city?
                        plotOutput('plot_average_departures')
                 )
             )
    ),
    # Background Info Tab ---- 
        tabPanel('Background Information', icon = icon('info-circle'), # icon = icon('book-reader')
                 # p('This draft tool displays California\'s Redlined communites, and helps to assess potential correlations between those policies and indicators of enviornmental and public health (e.g., 303d impaired water bodies, CalEnviroScreen scores), as well as facilities regulated by the CalEPA. More layers will be added in the future.'),
                 # Redlining History
                     h3('Redlining History'),
                     p('For more information about the history of the federal government\'s Home 
                       Owners’ Loan Corporation (HOLC) Residential Security (\"Redline\") mapping
                       and some previous studies of its effects, see:'),
                     tags$li(tags$a(href = 'https://dsl.richmond.edu/panorama/redlining/#loc=4/36.71/-96.93&text=intro',
                                    'Mapping Inequality: Redlining in New Deal America'), 
                             '(University of Richmond)',
                     ),
                     tags$li(tags$a(href = 'https://www.npr.org/sections/thetwo-way/2016/10/19/498536077/interactive-redlining-map-zooms-in-on-americas-history-of-discrimination',
                                    "Interactive Redlining Map Zooms In On America's History Of Discrimination"), 
                             '(NPR)',
                     ),
                     tags$li(tags$a(href = 'https://www.nytimes.com/2017/08/24/upshot/how-redlinings-racist-effects-lasted-for-decades.html',
                                    'How Redlining\'s Racist Effects Lasted for Decades'), 
                             '(New York Times)',
                     ),
                     tags$li(tags$a(href = 'https://www.washingtonpost.com/news/wonk/wp/2018/03/28/redlining-was-banned-50-years-ago-its-still-hurting-minorities-today/',
                                    'Redlining was banned 50 years ago. It\'s still hurting minorities today.'), 
                             '(Washington Post)',
                     ),
                     tags$li(tags$a(href = 'https://www.chicagofed.org/publications/working-papers/2017/wp2017-12',
                                    'The Effects of the 1930s HOLC \'Redlining\' Maps'), 
                             '(Federal Reserve Bank of Chicago)',
                     ),
                    tags$li(tags$a(href = 'https://www.npr.org/2020/01/14/795961381/racist-housing-practices-from-the-1930s-linked-to-hotter-neighborhoods-today',
                                    'Racist Housing Practices From The 1930s Linked To Hotter Neighborhoods Today'), 
                             '(NPR)',
                    ),
                 # Data Sources
                     h3('Data Sources'),
                     p('Data used in this application comes from the following sources (NOTE: some geospatial data displayed in this tool has been processed to show a simplied version of the original source data):'),
                     tags$li(tags$a(href = 'http://dsl.richmond.edu/panorama/redlining/#text=downloads',
                                    'HOLC Residential Security (Redline) Maps'), 
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
                                    'CalEPA Regulatory Actions (Regulated Sites, Site Program Types, Inspections, Violations, and Enforcement Actions)'), 
                             '(CalEPA Regulated Site Portal)'
                     ),
                     tags$li(tags$a(href = 'https://gispublic.waterboards.ca.gov/portal/home/item.html?id=fbba842bf134497c9d611ad506ec48cc#overview',
                                    'California Drinking Water Service Areas'), 
                             '(California State Water Resources Control Board\'s Map Services) | For web-service, use the "Service URL" link',
                             ' | Also available from the ' , 
                             tags$a(href = 'https://gis.data.ca.gov/datasets/waterboards::california-drinking-water-system-area-boundaries',
                                    'California State Geoportal')
                     ),
                     tags$li(tags$a(href = 'http://gispublic.waterboards.ca.gov/arcgis/rest/services/Administrative/RB_OfficeAreas/MapServer/0',
                                    'California State Water Board Regional Board Office Boundaries'), 
                             '(California State Water Resources Control Board\'s Map Services)', 
                             ' | Also available from the ' , 
                             tags$a(href = 'https://gis.data.ca.gov/datasets/waterboards::regional-board-boundaries',
                                    'California State Geoportal')
                     ),
                 # Application Information
                     h3('Application Information'),
                     p('This application is built with the ', 
                       tags$a(href = 'https://www.r-project.org/',
                              'R programming language'),
                       ' and the ', 
                       tags$a(href = 'https://shiny.rstudio.com/',
                              'Shiny'),
                       ' R package. Source code is available on the GitHub site here: ',
                     tags$a(href = 'https://github.com/daltare/Redline-Mapping',
                                    'https://github.com/daltare/Redline-Mapping')),
                 #br(),
                 # Contact Information
                 h3('Contact Information'),
                 p('If you have any questions or comments about the application, please contact David Altare at: ', 
                   tags$a(href = 'mailto:david.altare@waterboards.ca.gov',
                                    'david.altare@waterboards.ca.gov')),
                 p('You can also report any bugs in the application by submitting an issue on the GitHub site (please also 
                   send an email message to be sure we see the issue).')
        )
)

# Define server logic ----
server <- function(input, output, session) {
    selected_site_types <- reactive(input$site_type_1)
    selected_site_types_d <- selected_site_types %>% debounce(3000)
    
    selected_ces_range_filter <- reactive(input$ces_range_filter)
    selected_ces_range_filter_d <- selected_ces_range_filter %>% debounce(3000)
    
    output$date_range_selected <-  renderText(
        paste0('*Selected date range: ', 
               as.Date(paste0(str_split(string = input$date_range_fy[1], 
                                        pattern = ' / ', 
                                        simplify = FALSE)[[1]][1],
                              '-',7,'-',1)),
               ' through ',
               as.Date(paste0(str_split(string = input$date_range_fy[2], 
                                        pattern = ' / ', 
                                        simplify = FALSE)[[1]][2],
                              '-',6,'-',30))
               )
    )
    # # create list of which region each city is in (for filtering related datasets) ----
    #     cities_regions <- list('Fresno' = '5F',
    #                            'Los Angeles' = '4',
    #                            'Oakland' = '2',
    #                            'Sacramento' = '5S',
    #                            'San Diego' = '9',
    #                            'San Francisco' = '2',
    #                            'San Jose' = '2',
    #                            'Stockton' = '5S')
    # # create list of simplification levels that work best for each region (for use in APIs as a geographic filter) ----
    #     simplification_regions <- list('1' = '0.001',
    #                                    '2' = '0.0035',
    #                                    '3' = '0.001',
    #                                    '4' = '0.0015',
    #                                    '5R' = '0.003',
    #                                    '5S' = '0.0025',
    #                                    '5F' = '0.0025',
    #                                    '6A' = '0.004',
    #                                    '6B' = '0.0025',
    #                                    '7' = '0.004',
    #                                    '8' = '0.007',
    #                                    '9' = '0.006')
    
    # toggle sidebar panel
    observeEvent(input$showpanel,{
        session$sendCustomMessage(type = 'resize', message = 1)
    })
    
    # # toggle sidebar panel
    # observeEvent(input$toggleSidebar, {
    #     shinyjs::toggle(id = "Sidebar")
    # })
    
    # toggle the HOLC (redline) polygon selection input based on the on/off switch ----
     observe({
        toggle(id = 'holc_rating_sites_filter', 
               condition = input$holc_rating_sites_filter_on_off == TRUE)
      })
    
    observe({
        toggle(id = 'holc_city_sites_filter', 
               condition = input$holc_rating_sites_filter_on_off == TRUE)
      })
    
    observe({
        toggle(id = 'site_type_1',
               condition = input$sites_source == 'Select By Type (Source: CalEPA Geoserver)')
      })
    
    # create text output for map panel 3
        output$map_3_text <- renderUI({
            if (input$analysis_aggregation_method == 'Area Weighted Average') {
                p(tags$b('(3) HOLC / CES Polygon Overlap:'))
            } else if (input$analysis_aggregation_method == 'Nearest Centroid') {
                p(tags$b('(3) HOLC / CES Centroid Matching:'))
            }
        })

    # get reactive values (to isolate from each other and prevent map from completely rebuilding when an input is changed) 
        # regional board boundary containing the selected city ----
            rb_boundary <- reactive({
                withProgress(message = 'Downloading Data...', style = 'notification', value = 1, {
                    if (data_source_rb_bounds == 'local') {
                        rb_boundary_download <- st_read('data_processed/rb_boundary_simplified.gpkg')
                    } else if (data_source_rb_bounds == 'remote') {
                        # # Get Regional Board Office Areas from WB GIS Services (GEOJSON) - note that this filters for just the regional board containing the selected city
                        # url_rb_office_areas <- paste0("http://gispublic.waterboards.ca.gov/arcgis/rest/services/Administrative/RB_OfficeAreas/MapServer/0/query?where=UPPER(rb_off)%20like%20'%25",
                        #                               cities_regions[[input$city_selected_1]],
                        #                               "%25'&outFields=*&outSR=4326&f=geojson") 
                        # rb_boundary_download <- read_lines(url_rb_office_areas) %>% 
                        #     geojson_sf()
                        # Get ALL Regional Board Office Areas from WB GIS Services (GEOJSON) - no filter for the regional board containing the selected city
                            url_rb_office_areas <- paste0("http://gispublic.waterboards.ca.gov/arcgis/rest/services/Administrative/RB_OfficeAreas/MapServer/0/query?where=1=1",
                                                          "&outFields=*&outSR=4326&f=geojson") 
                            rb_boundary_download <- read_lines(url_rb_office_areas) %>% 
                                geojson_sf()
                    }
                # transform to projected crs (for mapping and analysis it's best to use a projected CRS -- see: https://s3.amazonaws.com/files.zevross.com/workshops/spatial/slides/html/4-crs.html#31)
                    rb_boundary_download <- rb_boundary_download %>% 
                        st_transform(crs = projected_crs)
                    # st_crs(rb_boundary_download)
                return(rb_boundary_download)
                # # get the regional board boundary containing the selected city from a saved file (old method - not used)
                #     rb_boundary_download <- read_rds('data_processed/Regional_Board_Offices.RDS') %>% 
                #         filter(RB_OFF == cities_regions[[input$city_selected_1]])
                })
            })
            
        # # create a simplified version of the regional board boundary for use in api filtering
        #     rb_boundary_simplify <- reactive({
        #         # simplify and convert rb boundary to geojson
        #             rb_boundary() %>% 
        #                 ms_simplify(keep = as.numeric(
        #                     simplification_regions[[cities_regions[[input$city_selected_1]]]])) 
        #     })
            
            
        # CalEnvironScreen Polygons (entire state - commented parts are just for the region containing the selected city)
            ces3_poly <- reactive({
                withProgress(message = 'Downloading Data...', style = 'notification', value = 1, {
                if (data_source_ces3 == 'local') {
                    # ces3_poly_download <- st_read('data_processed/ces3_poly.gpkg') # st_read('data_processed/ces3_poly_simplified.gpkg')
                    ces3_poly_download <- st_read('data_processed/ces3_poly_filtered.gpkg') # st_read('data_processed/ces3_poly_simplified.gpkg')
                } else if (data_source_ces3 == 'remote') {
                # # get data
                #     ces3_poly_download <- read_rds('data_processed/ces3_poly_download.RDS')
                # get data from API
                    # # transform to rb boundary to coordinate system used in web service
                    #     rb_boundary_simplify_transform <- st_transform(rb_boundary_simplify(), 3310)
                    # # convert to geojson
                    #     rb_boundary_simplify_transform_geojson <- rb_boundary_simplify_transform %>% sf_geojson()
                    # # get coordinates
                    #     rb_boundary_coordinates <- rb_boundary_simplify_transform_geojson %>%
                    #         str_sub(start = str_locate(string = ., pattern = 'coordinates\":') %>% as.data.frame() %>% pull(end) + 4,
                    #                 end = nchar(rb_boundary_simplify_transform_geojson)-7)
                    # # format the coordinates for the api
                    #     rb_boundary_coordinates <- str_replace_all(string = rb_boundary_coordinates, pattern = '\\],\\[', replacement = '|')
                    #     rb_boundary_coordinates <- str_replace_all(string = rb_boundary_coordinates, pattern = ',', replacement = '%20')
                    #     rb_boundary_coordinates <- str_replace_all(string = rb_boundary_coordinates, pattern = '\\|', replacement = ',%20')
                    # construct api call - NOTE -- for more info see: https://docs.geoserver.org/stable/en/user/tutorials/cql/cql_tutorial.html
                        # url_ces3_api <- paste0('https://services.calepa.ca.gov/geoserver/calepa/',
                        #                        'ows?service=WFS&version=1.0.0',
                        #                        '&request=GetFeature',
                        #                        '&typeName=calepa:CES3June2018Update',
                        #                        '&outputFormat=application%2Fjson',
                        #                        '&CQL_FILTER=INTERSECTS(the_geom,POLYGON((',
                        #                        rb_boundary_coordinates,
                        #                        ')))')
                        url_ces3_api <- paste0('https://services.calepa.ca.gov/geoserver/calepa/',
                                               'ows?service=WFS&version=1.0.0',
                                               '&request=GetFeature',
                                               '&typeName=calepa:CES3June2018Update',
                                               '&outputFormat=application%2Fjson'#,
                                               # '&CQL_FILTER=INTERSECTS(the_geom,POLYGON((',
                                               # rb_boundary_coordinates,
                                               # ')))'
                                               )
                    # make api call
                        ces3_poly_download <- read_lines(url_ces3_api) %>% geojson_sf()
                        st_crs(ces3_poly_download) <- 3310 # think the crs is incorrectly defined in the data returned by the api - have to reset it
                        ces3_poly_download <- ces3_poly_download # %>% 
                            # st_transform(crs = geographic_crs) # transform back to lat/lon coordinates
                    # check
                        # plot(ces3_poly_download$geometry, border = 'blue', col = 'grey')
                        # plot(rb_boundary$geometry, lwd = 3, add = TRUE)
                        # plot(redline_polygons$geometry, lwd = 3, add = TRUE, col = 'red', border = 'red')
                        # # plot(rb_boundary_simplify() %>% select(geometry), border = 'red', add = TRUE)
                    # revise column names
                        # ces3_poly_download <- ces3_poly_download %>% select(-CES2018_Rn)
                        col_names_original <- names(ces3_poly_download)
                        col_names_new <- read_csv('data_processed/ces_names.csv')
                            col_names_new$id[nrow(col_names_new)] <- 'CES2018_Rn'
                        col_names_original_df <- as.data.frame(x = col_names_original)
                        col_names_original_df <- col_names_original_df %>% left_join(col_names_new, by = c('col_names_original' = 'id'))
                        col_names_original_df$name[nrow(col_names_original_df)] <- 'geom'
                        names(ces3_poly_download) <- col_names_original_df$name
                        names(ces3_poly_download) <- make_clean_names(names(ces3_poly_download), 'parsed')
                }
                    # transform to projected crs (for mapping and analysis it's best to use a projected CRS -- see: https://s3.amazonaws.com/files.zevross.com/workshops/spatial/slides/html/4-crs.html#31)
                        ces3_poly_download <- ces3_poly_download %>% 
                            st_transform(crs = projected_crs)
                        # st_crs(ces3_poly_download)
                        return(ces3_poly_download)
                })
            })
            
    # create a dynamic input for CES score range selection
        output$ces_range_filter <- renderUI({
            parameter <- ces_choices %>% filter(name == input$ces_parameter) %>% pull(ces_variable)
            min_score <- as.data.frame(ces3_poly()) %>% pull(all_of(parameter)) %>% min()
            max_score <- as.data.frame(ces3_poly()) %>% pull(all_of(parameter)) %>% max()
            sliderInput(inputId = 'ces_range_filter', 
                        label = 'Filter Sites By Score of Selected CES Parameter:', 
                        min = min_score, 
                        max = max_score, 
                        value = c(min_score, max_score))
        })
            
        # 303d polygons
            impaired_303d_poly <- reactive({
                withProgress(message = 'Downloading Data...', style = 'notification', value = 1, {
                # get data
                    # impaired_303d_poly_download <- read_rds('data_processed/impaired_303d_poly_download.RDS')
                    impaired_303d_poly_download <- st_read('data_processed/303d_polygons_simplified.gpkg')
                # filter for 303d polygons in the region containing the selected city
                    # impaired_poly_filter <- st_intersects(x = impaired_303d_poly_download,
                    #                                       y = rb_boundary(),
                    #                                       sparse = FALSE)
                    # impaired_303d_poly_download <- impaired_303d_poly_download[impaired_poly_filter,]
                # transform to projected crs (for mapping and analysis it's best to use a projected CRS -- see: https://s3.amazonaws.com/files.zevross.com/workshops/spatial/slides/html/4-crs.html#31)
                    impaired_303d_poly_download <- impaired_303d_poly_download %>% 
                        st_transform(crs = projected_crs)
                    # st_crs(impaired_303d_poly_download)
                    return(impaired_303d_poly_download)
                })
            })
            
        # 303d lines
            impaired_303d_lines <- reactive({
                withProgress(message = 'Downloading Data...', style = 'notification', value = 1, {
                # get data
                    # impaired_303d_lines_download <- read_rds('data_processed/impaired_303d_lines_download_simplify_R1removed.RDS')
                    impaired_303d_lines_download <- st_read('data_processed/303d_lines_R1removed_simplified.gpkg')
                # filter out records with empty geometries
                    impaired_303d_lines_download <- impaired_303d_lines_download %>% filter(!is.na(st_dimension(.)))
                # # filter for 303d lines in the region containing the selected city
                #     impaired_lines_filter <- st_intersects(x = impaired_303d_lines_download,
                #                                            y = rb_boundary(),
                #                                            sparse = FALSE)
                #     impaired_303d_lines_download <- impaired_303d_lines_download[impaired_lines_filter,]
                # transform to projected crs (for mapping and analysis it's best to use a projected CRS -- see: https://s3.amazonaws.com/files.zevross.com/workshops/spatial/slides/html/4-crs.html#31)
                    impaired_303d_lines_download <- impaired_303d_lines_download %>% 
                        st_transform(crs = projected_crs)
                    # st_crs(impaired_303d_lines_download)
                    return(impaired_303d_lines_download)
                })
            })
            
        # Water Service Provider Boundaries
            # get data from the waterboard web services
            service_areas <- reactive({
                withProgress(message = 'Downloading Data...', style = 'notification', value = 1, {
                    if (data_source_service_areas == 'local') {
                        service_areas_download <- st_read('data_processed/drinking_water_service_areas_simplified.gpkg')
                        # # filter for polygons in the region containing the selected city
                        #     service_areas_download_filter <- st_intersects(x = service_areas_download,
                        #                                           y = rb_boundary(),
                        #                                           sparse = FALSE)
                        #     service_areas_download <- service_areas_download[service_areas_download_filter,]
                    } else if (data_source_service_areas == 'remote') {
                        # # extract the coordinates of the simplified rb boundary and encode as url
                        #     rb_boundary_simplify_geojson <- rb_boundary_simplify() %>% sf_geojson()
                        #     rb_boundary_coordinates <- rb_boundary_simplify_geojson %>% 
                        #         str_sub(start = str_locate(string = ., pattern = 'coordinates\":') %>% as.data.frame() %>% pull(end) + 4, 
                        #                 end = nchar(rb_boundary_simplify_geojson)-7) %>% 
                        #         url_encode()
                        # construct the api call and get the data
                        # OLD: service_url <- 'https://gispublic.waterboards.ca.gov/portalserver/rest/services/Hosted/California_Drinking_Water_Service_Areas/FeatureServer/0/'
                        service_url <- 'https://gispublic.waterboards.ca.gov/portalserver/rest/services/Drinking_Water/California_Drinking_Water_System_Area_Boundaries/FeatureServer/0/'
                        url_polygon_filter_serviceboundaries <- paste0(service_url,
                                                                       'query?where=1%3D1', 
                                                                       '&outFields=*',
                                                                       # '&geometry=%7B%22rings','%22','%3A','%5B','%5B','%5B',
                                                                       # rb_boundary_coordinates,
                                                                       # '%5D','%5D','%5D','%7D',
                                                                       # '&geometryType=esriGeometryPolygon&inSR=4326&spatialRel=esriSpatialRel',
                                                                       # 'Intersects', # 'Contains', 'Intersects'
                                                                       '&outSR=4326',
                                                                       '&f=geojson')
                        service_areas_download <- read_lines(url_polygon_filter_serviceboundaries) %>% geojson_sf()
                    }
                    # transform to projected crs (for mapping and analysis it's best to use a projected CRS -- see: https://s3.amazonaws.com/files.zevross.com/workshops/spatial/slides/html/4-crs.html#31)
                        service_areas_download <- service_areas_download %>% st_transform(crs = projected_crs)
                        # st_crs(service_areas_download)
                    # clean names
                        service_areas_download <- clean_names(service_areas_download)
                    # check
                        # plot(rb_boundary %>% select(geometry), lwd = 3) 
                        # plot(service_areas_download$geometry, border = 'blue', add = TRUE)
                        # # plot(rb_boundary_simplify() %>% select(geometry), border = 'red', add = TRUE)
                    return(service_areas_download)
                })
            })
        

        # Get the CalEPA regulated sites
            cal_epa_sites_raw <- reactive({
                if (input$sites_source == 'All Sites (Source: CalEPA Regulated Site Portal)') {
                        cal_epa_sites_raw_download <- calepa_sites_processed %>% 
                            filter(!is.na(latitude) & !is.na(longitude)) %>% 
                            {.}
                    # # Create an sf object using the sites data from the saved flat file
                    #     cal_epa_sites_raw_download <- st_as_sf(calepa_sites_processed %>% filter(!is.na(latitude) & !is.na(longitude)),
                    #                                            coords = c('longitude', 'latitude'),
                    #                                            crs = 4269,
                    #                                            # agr = 'constant',
                    #                                            remove = FALSE)
                    # # transform to projected crs (for mapping and analysis it's best to use a projected CRS -- see: https://s3.amazonaws.com/files.zevross.com/workshops/spatial/slides/html/4-crs.html#31)
                    #     cal_epa_sites_raw_download <- cal_epa_sites_raw_download %>% st_transform(crs = projected_crs)
                    #     # st_crs(cal_epa_sites_raw_download)
                    # return the object
                        return(cal_epa_sites_raw_download)
                } else if (input$sites_source == 'Select By Type (Source: CalEPA Geoserver)' & 
                           length(selected_site_types_d()) > 0) {    
                    withProgress(message = 'Getting Site Data', style = 'notification', value = 1, { # style = 'notification' 'old'
                        # get CalEPA sites data from the geoserver api
                        for (city_number in seq(nrow(holc_map_hulls))) {
                            # get the coordinates of the hulls around each holc mapped city and convert to geojson
                            holc_map_hulls_geojson <- holc_map_hulls[city_number,] %>% st_transform(4326) %>% sf_geojson()
                            # extract coordinates
                            hulls_coordinates <- holc_map_hulls_geojson %>% 
                                str_sub(start = str_locate(string = ., pattern = 'coordinates\":') %>% as.data.frame() %>% pull(end) + 4,
                                        end = nchar(holc_map_hulls_geojson)-7)
                            # format the coordinates for the api
                            hulls_coordinates <- str_replace_all(string = hulls_coordinates, pattern = '\\],\\[', replacement = '|')
                            hulls_coordinates <- str_replace_all(string = hulls_coordinates, pattern = ',', replacement = '%20')
                            hulls_coordinates <- str_replace_all(string = hulls_coordinates, pattern = '\\|', replacement = ',%20')
                            # construct and make the api call
                            calepa_sites_typenames <- list('California Integrated Water Quality System (CIWQS)' = 'calepa:mv_fac_from_ciwqs',
                                                           'GeoTracker' = 'calepa:mv_fac_from_geotracker',
                                                           'Storm Water Multiple Application and Report Tracking System (SMARTS)' = 'calepa:mv_fac_from_smarts',
                                                           'Chemical Hazards - Fire' = 'calepa:mv_fac_chem_summary_fire',
                                                           'Chemical Hazards - Reactivity' = 'calepa:mv_fac_chem_summary_reactive',
                                                           'Chemical Hazards - Sudden Release of Pressure' = 'calepa:mv_fac_chem_summary_sudden_rel',
                                                           'California Environmental Reporting System (CERS)' = 'calepa:mv_fac_from_cers',
                                                           'EnviroStor Cleanup (ENVSTORCLN)' = 'calepa:mv_fac_from_envstorcln',
                                                           'EnviroStor Hazardous Waste (ENVSTORHAZ)' = 'calepa:mv_fac_from_envstorhaz',
                                                           'National Emissions Inventory System (EIS)' = 'calepa:mv_fac_from_frs',
                                                           'Toxic Release Inventory (TRI)' = 'calepa:mv_fac_from_tri'
                            )
                            counter_sites <- 0
                            for (site_type in selected_site_types_d()) {
                                counter_sites <- counter_sites + 1
                                # url <- calepa_sites_typenames[[site_type]]
                                url_sites_api <- paste0('https://services.calepa.ca.gov/geoserver/calepa/',
                                                        'ows?service=WFS&version=1.0.0',
                                                        '&request=GetFeature',
                                                        '&typeName=', calepa_sites_typenames[[site_type]], # 'calepa:mv_fac_from_ciwqs', # calepa:mv_fac_from_geotracker # calepa:mv_fac_from_smarts
                                                        '&outputFormat=application%2Fjson',
                                                        '&CQL_FILTER=INTERSECTS(fac_point,POLYGON((',
                                                        hulls_coordinates,
                                                        ')))'
                                )
                                api_output <- readr::read_lines(url_sites_api)
                                json_list <- jsonlite::fromJSON(api_output)
                                df_api_result <- json_list$features
                                df_api_result <- df_api_result %>% mutate('data_source' = site_type)
                                # reformat the data frame (some of it is nested)
                                df_api_result <- bind_cols(df_api_result %>% select(id, data_source),
                                                           df_api_result$geometry,
                                                           df_api_result$properties)
                                ifelse(counter_sites == 1,
                                       cal_epa_sites_raw_download_city <- df_api_result,
                                       cal_epa_sites_raw_download_city <- bind_rows(cal_epa_sites_raw_download_city, 
                                                                                    df_api_result)
                                )
                            }
                            ifelse (city_number == 1,
                                    cal_epa_sites_raw_download <- cal_epa_sites_raw_download_city,
                                    cal_epa_sites_raw_download <- bind_rows(cal_epa_sites_raw_download, 
                                                                            cal_epa_sites_raw_download_city)
                            )
                        }
                    # if (input$sites_source == 'All Sites (Source: CalEPA Regulated Site Portal)' | 
                    #     (input$sites_source == 'Select By Type (Source: CalEPA Geoserver)' & 
                    #        length(selected_site_types_d()) > 0)) {
                        
                    
                    # Create an sf object from the sites data
                    cal_epa_sites_raw_download <- st_as_sf(cal_epa_sites_raw_download %>% filter(!is.na(latitude) & !is.na(longitude)),
                                                         coords = c('longitude', 'latitude'),
                                                         crs = 4269,
                                                         # agr = 'constant',
                                                         remove = FALSE)
                    # rename some columns to match the data from the regulated site portal
                        cal_epa_sites_raw_download <- cal_epa_sites_raw_download %>%
                            rename(site_name = facility_name)
                    # transform to projected crs (for mapping and analysis it's best to use a projected CRS -- see: https://s3.amazonaws.com/files.zevross.com/workshops/spatial/slides/html/4-crs.html#31)
                        cal_epa_sites_raw_download <- cal_epa_sites_raw_download %>% 
                            st_transform(crs = projected_crs)
                        # st_crs(cal_epa_sites_raw_download)
                        
                    # filter for sites within the holc map polygons
                        cal_epa_sites_raw_download <- cal_epa_sites_raw_download[redline_polygons, ]
                    # join to get the CES3 data (using the geometry from the tiger census tracts)
                        cal_epa_sites_raw_download <- st_join(cal_epa_sites_raw_download, 
                                                              ces3_tiger, 
                                                              join = st_intersects)
                        # remove duplicates
                            cal_epa_sites_raw_download <- cal_epa_sites_raw_download[!duplicated(cal_epa_sites_raw_download$site_id),]

                    # join to get the redline data
                        cal_epa_sites_raw_download <- st_join(cal_epa_sites_raw_download,
                                                              redline_polygons,
                                                              join = st_intersects)
                        
                    # # convert site_id column to factor
                    #     cal_epa_sites_raw_download <- cal_epa_sites_raw_download %>% 
                    #         mutate(site_id = factor(site_id, levels = calepa_reg_sites$site_id))
                    # ALTERNATE METHOD
                    # # filter to get these sites
                    #     cal_epa_sites_raw_download <- calepa_sites_processed %>% 
                    #         filter(site_id %in% api_result_all$site_id)
                    #         filter(!is.na(latitude) & !is.na(longitude)) %>% 
                    #         {.}
                    # drop geometry
                        cal_epa_sites_raw_download <- cal_epa_sites_raw_download %>% 
                            st_drop_geometry()
                    # drop un-needed fields to match the 'calepa_sites_processed' data frame
                        cal_epa_sites_raw_download <- cal_epa_sites_raw_download %>% 
                            select(-c(id, type, coordinates, originating_system_seq, url)) %>%
                            mutate(county = NA) %>%
                            select(names(calepa_sites_processed)) # reorder the columns
                    # return the object
                        return(cal_epa_sites_raw_download)
                    })
                    } else {
                        return(tibble())
                }
            })
            
            
    # REGULATORY ACTIONS
        # get the start and end date
            # FOR MONTH / YEAR SELECTION OPTIONS:
                # start_date <- reactive({
                #     start_date <- if (!is.na(input$sites_date_range[1])) {
                #         as.Date(paste(year(input$sites_date_range[1]), # set to the first day of the selected month
                #                       month(input$sites_date_range[1]),
                #                       1,
                #                       sep = '-'))
                #     } else {as.Date('0001-01-01')} # arbitrarily small date if no start date selected
                #     return(start_date)
                # })
                # end_date <- reactive({
                #     end_date <- if (!is.na(input$sites_date_range[2])) {
                #         as.Date(paste(year(input$sites_date_range[2]),
                #                       month(input$sites_date_range[2]),
                #                       1,
                #                       sep = '-'))
                #     } else {as.Date('9999-01-01')} # arbitrarily large date if no end date selected
                #     day(end_date) <- days_in_month(end_date) # set the end date to be the last day in the selected month, rather than the first
                #     return(end_date)
                # })
            
        # by fiscal year
            start_date_raw <- reactive({
                return(
                    as.Date(paste0(str_split(string = input$date_range_fy[1], # the [1] gives the start FY
                                             pattern = ' / ', 
                                             simplify = FALSE)[[1]][1], # the [1] gives the beginning calendar year of the FY
                                   '-',7,'-',1))
                )
            })
            start_date <- start_date_raw %>% debounce(5000)
            
            end_date_raw <- reactive({
                return(
                    as.Date(paste0(str_split(string = input$date_range_fy[2], # the [2] gives the end FY
                                             pattern = ' / ', 
                                             simplify = FALSE)[[1]][2], # the [2] gives the ending calendar year of the FY
                                   '-',6,'-',30))
                )
            })
            end_date <- end_date_raw %>% debounce(5000)
            
            # start_date_fy_filter_raw <- reactive({
            #     return(
            #         str_split(string = input$date_range_fy[1], # the [1] gives the start FY
            #                   pattern = ' / ', 
            #                   simplify = FALSE)[[1]][1] # the [1] gives the beginning calendar year of the FY
            #         )
            #     })
            # start_date_fy_filter <- start_date_fy_filter_raw %>% debounce(5000)
            
            # end_date_fy_filter_raw <- reactive({
            #     return(
            #         str_split(string = input$date_range_fy[2], # the [2] gives the end FY
            #                   pattern = ' / ', 
            #                   simplify = FALSE)[[1]][1] # the [1] gives the beginning calendar year of the FY
            #         )
            #     })
            # end_date_fy_filter <- end_date_fy_filter_raw %>% debounce(5000)
            
            date_range_raw <- reactive({
                return(
                    paste0(
                        str_replace_all(string = input$date_range_fy[1], 
                                        pattern = ' ', 
                                        replacement = ''),
                        ' ', HTML('&ndash;'), ' ',
                        str_replace_all(string = input$date_range_fy[2], 
                                        pattern = ' ', 
                                        replacement = '')
                    )
                )
            })
            
            date_range <- date_range_raw %>% debounce(5000)
           
    # summarize the inspections, violations, and enforcement actions for the selected period
        regulatory_data_summary <- reactive({ # use 'debounce' above to slow down reactions to the start/end date filter
            withProgress(message = 'Summarizing Data...', 
                         style = 'notification', 
                         value = 1, 
                         {
                             if (nrow(cal_epa_sites_raw()) > 0) {
                                 # summarize the regulatory data for the given time period, then add the site info (via a left join)
                                 # reg_data_summary <- regulatory_data_processed %>% 
                                 reg_data_summary <- regulatory_data_processed_fy_summary %>% 
                                     # filter(action_date >= start_date(), action_date <= end_date()) %>%
                                     # filter(fy_start >= start_date_fy_filter(), 
                                     #        fy_start <= end_date_fy_filter()) %>%
                                     filter(fy_start >= year(start_date()), 
                                            fy_end <= year(end_date())) %>%
                                     filter(site_id %in% (cal_epa_sites_raw() %>% pull(site_id))) %>%
                                     group_by(site_id, action) %>% 
                                     # summarize(count = sum(count)) %>%
                                     summarise(count = sum(count)) %>%
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
                                     mutate_if(is.integer, ~replace(., is.na(.), 0)) %>% # replace NAs with zeros
                                     # left_join(calepa_sites_processed) %>% 
                                     left_join(cal_epa_sites_raw(), by = c('site_id')) %>% 
                                     {.}
                                 # find sites that don't have any regulatory actions and add a zero record to make sure they are included
                                    missing_site_ids <- cal_epa_sites_raw() %>% 
                                        filter(!(site_id %in% reg_data_summary$site_id)) %>% 
                                        pull(site_id) %>% 
                                        {.}
                                    missing_sites <- cal_epa_sites_raw() %>% 
                                        filter(site_id %in% missing_site_ids) %>% 
                                        mutate(inspections_count = 0,
                                               violations_count = 0,
                                               enforcement_actions_count = 0) %>% 
                                        {.}
                                # bind the sites with no reg actions to the dataset (this is the dataset to use)
                                    reg_data_summary <- bind_rows(reg_data_summary, 
                                                                  missing_sites) %>% 
                                        arrange(site_id)
                                # return the dataset
                                    return(reg_data_summary)
                             } else {
                                 return(tibble())
                             }
                         })
        })
            
        # inspections_summary <- reactive({
        #     withProgress(message = 'Summarizing Data...', 
        #                  style = 'notification', 
        #                  value = 1, 
        #                  {
        #                      if (nrow(cal_epa_sites_raw()) > 0) {
        #                          return(
        #                              inspections_all_download %>% 
        #                                  filter(eval_date >= start_date(),
        #                                         eval_date <= end_date()) %>%
        #                                  filter(site_id %in% (cal_epa_sites_raw() %>% 
        #                                                           pull(site_id))) %>%
        #                                  group_by(site_id) %>%
        #                                  summarize(inspections_count = n()) %>%
        #                                  ungroup() %>% 
        #                                  {.}
        #                          )
        #                      } else {
        #                          return(tibble())
        #                      }
        #                  })
        # })
        # inspection_types <- reactive({
        #     return(
        #         inspections_all_download %>% 
        #             select(eval_type) %>% 
        #             distinct(eval_type) %>% 
        #             pull(eval_type)
        #     )
        # })
        # violations_summary <- reactive({
        #     withProgress(message = 'Summarizing Data...', 
        #                  style = 'notification', 
        #                  value = 1, 
        #                  {
        #                      if (nrow(cal_epa_sites_raw()) > 0) {
        #                          return(
        #                              violations_all_download %>% 
        #                                  filter(violation_date >= start_date(),
        #                                         violation_date <= end_date()) %>%
        #                                  filter(site_id %in% (cal_epa_sites_raw() %>% 
        #                                                           pull(site_id))) %>%
        #                                  group_by(site_id) %>%
        #                                  summarize(violations_count = n()) %>%
        #                                  ungroup() %>% 
        #                                  {.}
        #                          )
        #                      } else {
        #                          return(tibble())
        #                      }
        #                  })
        # })
        # enforcement_summary <- reactive({
        #     withProgress(message = 'Summarizing Data...', 
        #                  style = 'notification', 
        #                  value = 1, 
        #                  {
        #                      if (nrow(cal_epa_sites_raw()) > 0) {
        #                          return(
        #                              enforcement_all_download %>% 
        #                                  filter(enf_action_date >= start_date(),
        #                                         enf_action_date <= end_date()) %>%
        #                                  filter(site_id %in% (cal_epa_sites_raw() %>% 
        #                                                           pull(site_id))) %>%
        #                                  group_by(site_id) %>%
        #                                  summarize(enforcement_actions_count = n()) %>%
        #                                  ungroup() %>% 
        #                                  {.}
        #                          )
        #                      } else {
        #                          return(tibble())
        #                      }
        #                  })
        # })
        
    # create an output text statement to report the most recent regulatory records available
        output$data_availability_msg <- renderText({
            paste0('*Most recent regulatory records available: ', 
                   most_recent_reg_records,
                   ''
            )
        })
        
    ###########################################################################################
    # # summarize the inspections, violations, and/or enforcement records for the given time period, then join to the sites data
    #     cal_epa_sites_summarized <- reactive({
    #         withProgress(message = 'Summarizing Regulatory Action Data', 
    #                      style = 'notification', 
    #                      value = 1, { # style = 'notification' 'old'
    #                          
    #                          if (nrow(cal_epa_sites_raw()) > 0) {
    #                              # join the regulatory data to the sites data
    #                              cal_epa_sites_summarized_compute <- cal_epa_sites_raw()
    #                              cal_epa_sites_summarized_compute <- cal_epa_sites_summarized_compute %>% 
    #                                  # left_join(inspections_count, by = c('site_id'))
    #                                  left_join(inspections_summary(), by = c('site_id'))
    #                              cal_epa_sites_summarized_compute <- cal_epa_sites_summarized_compute %>% 
    #                                  # left_join(violations_count, by = c('site_id'))
    #                                  left_join(violations_summary(), by = c('site_id'))
    #                              cal_epa_sites_summarized_compute <- cal_epa_sites_summarized_compute %>% 
    #                                  # left_join(enforcement_count, by = c('site_id')) %>%
    #                                  left_join(enforcement_summary(), by = c('site_id'))
    #                              # replace NAs with zeros for the counts
    #                              cal_epa_sites_summarized_compute <- cal_epa_sites_summarized_compute %>% 
    #                                  # mutate(inspections_count = case_when(is.na(inspections_count) ~ 0L,
    #                                  #                                      TRUE ~ inspections_count),
    #                                  #        violations_count = case_when(is.na(violations_count) ~ 0L,
    #                                  #                                     TRUE ~ violations_count),
    #                                  #        enforcement_actions_count = case_when(is.na(enforcement_actions_count) ~ 0L,
    #                                  #                                       TRUE ~ enforcement_actions_count)) %>% 
    #                                  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>% 
    #                                  {.}
    #                              return(cal_epa_sites_summarized_compute)
    #                          } else {
    #                              return(tibble())
    #                          }
    #                      })
    #     }) 
            

    # # filter for sites... 
    #     # (1) with inspections, violations, and/or enforcement records > 0 if selected
    #     # (2) by CES polygon score
    #     # (3) by HOLC polygon rating
    #     cal_epa_sites_filtered_0 <- reactive({
    #         withProgress(message = 'Filtering Regulatory Action Data',
    #                      style = 'notification', value = 1, { # style = 'notification' 'old'
    #                          if (nrow(cal_epa_sites_raw()) > 0) {
    #                              cal_epa_sites_filtered_0_compute <- cal_epa_sites_summarized()
    #                              # 1 - filter for sites with inspections, violations, and/or enforcement records > 0
    #                              if ('Inspections' %in% input$show_violations_enforcement) {
    #                                  cal_epa_sites_filtered_0_compute <- cal_epa_sites_filtered_0_compute %>%
    #                                      filter(inspections_count > 0)
    #                              }
    #                              if ('Violations' %in% input$show_violations_enforcement) {
    #                                  cal_epa_sites_filtered_0_compute <- cal_epa_sites_filtered_0_compute %>%
    #                                      filter(violations_count > 0)
    #                              }
    #                              if ('Enforcement Actions' %in% input$show_violations_enforcement) {
    #                                  cal_epa_sites_filtered_0_compute <- cal_epa_sites_filtered_0_compute %>%
    #                                      filter(enforcement_actions_count > 0)
    #                              }
    #                              # 2 - filter for sites by CES polygon score
    #                              # find the CES polygons that meet the selected criteria (range of values)
    #                              parameter <- ces_choices %>% 
    #                                  filter(name == input$ces_parameter) %>% 
    #                                  pull(ces_variable)
    #                              ces3_poly_filtered <- ces3_poly() %>% 
    #                                  filter(!!as.name(parameter) >= selected_ces_range_filter_d[1]) %>%
    #                                  # filter(CES_3_Score >= selected_ces_range_filter_d[1]) %>% 
    #                                  filter(!!as.name(parameter) <= selected_ces_range_filter_d[2]) %>%
    #                                  # filter(CES_3_Score <= selected_ces_range_filter_d[2]) %>%
    #                                  st_as_sf() %>% 
    #                                  {.}
    #                              # filter for sites with CES polygons meeting the selected criteria
    #                              cal_epa_sites_filtered_0_compute <- cal_epa_sites_filtered_0_compute[ces3_poly_filtered, ] # filter for sites within the selected ces polygons
    #                              # 3 - filter for sites by HOLC rating
    #                              if (input$holc_rating_sites_filter_on_off == TRUE) {
    #                                  cal_epa_sites_filtered_0_compute <- cal_epa_sites_filtered_0_compute[redline_polygons %>% 
    #                                                                                                           filter(holc_grade %in% input$holc_rating_sites_filter) %>% 
    #                                                                                                           filter(city %in% input$holc_city_sites_filter), ]
    #                              }
    #                              
    #                              return(cal_epa_sites_filtered_0_compute)
    #                          } else {
    #                              return(tibble())
    #                          }
    #                      })
    #     })
        
    
    # filter for sites... 
        # (1) with inspections, violations, and/or enforcement records > 0 if selected
        # (2) by CES polygon score
        # (3) by HOLC polygon rating
        cal_epa_sites_filtered_0 <- reactive({
            withProgress(message = 'Filtering Regulatory Action Data',
                         style = 'notification', value = 1, { # style = 'notification' 'old'
                             if (nrow(cal_epa_sites_raw()) > 0) {
                                 cal_epa_sites_filtered_0_compute <- regulatory_data_summary()
                                 # 1 - filter for sites with inspections, violations, and/or enforcement records > 0
                                 if ('Inspections' %in% input$show_violations_enforcement) {
                                     cal_epa_sites_filtered_0_compute <- cal_epa_sites_filtered_0_compute %>%
                                         filter(inspections_count > 0)
                                 }
                                 if ('Violations' %in% input$show_violations_enforcement) {
                                     cal_epa_sites_filtered_0_compute <- cal_epa_sites_filtered_0_compute %>%
                                         filter(violations_count > 0)
                                 }
                                 if ('Enforcement Actions' %in% input$show_violations_enforcement) {
                                     cal_epa_sites_filtered_0_compute <- cal_epa_sites_filtered_0_compute %>%
                                         filter(enforcement_actions_count > 0)
                                 }
                                 # 2 - filter for sites by CES polygon score
                                    # get the name of the parameter
                                         parameter <- ces_choices %>% 
                                             filter(name == input$ces_parameter) %>% 
                                             pull(ces_variable)
                                    # filter for sites with CES polygons meeting the selected criteria
                                         cal_epa_sites_filtered_0_compute <- cal_epa_sites_filtered_0_compute %>% 
                                             filter(!!as.name(parameter) >= selected_ces_range_filter_d()[1]) %>% 
                                             filter(!!as.name(parameter) <= selected_ces_range_filter_d()[2]) %>%
                                             {.}
                                 # 3 - filter for sites by HOLC rating
                                     if (input$holc_rating_sites_filter_on_off == TRUE) {
                                         cal_epa_sites_filtered_0_compute <- cal_epa_sites_filtered_0_compute %>% 
                                             filter(holc_grade %in% input$holc_rating_sites_filter) %>% 
                                             filter(holc_city %in% input$holc_city_sites_filter)
                                     }
                                 return(cal_epa_sites_filtered_0_compute)
                             } else {
                                 return(tibble())
                             }
                         })
        })
    

    # filter for sites by program type if selected
        cal_epa_sites_filtered <- reactive({
            withProgress(message = 'Filtering Regulatory Action Data', 
                         style = 'notification', value = 1, { # style = 'notification' 'old'
                             if (nrow(cal_epa_sites_raw()) > 0) {
                                 cal_epa_sites_filtered_compute <- cal_epa_sites_filtered_0()
                                 # if program type filters are selected, filter for the selected program types
                                 if (!is.null(input$program_type_1)) {
                                     program_types_filter <- program_types %>% 
                                         filter(ei_description %in% input$program_type_1)
                                     cal_epa_sites_filtered_compute <- cal_epa_sites_filtered_compute %>% 
                                         filter(site_id %in% program_types_filter$site_id)
                                 }
                                 return(cal_epa_sites_filtered_compute)
                             } else {
                                 return(tibble())
                             }
                         })
        })

    # create a dynamic input for program type
        output$program_type_1 <- renderUI({
            selectInput(inputId = 'program_type_1',
                        label = '**Filter Sites By Program Type:**',
                        multiple = TRUE,
                        choices = if (nrow(cal_epa_sites_raw()) > 0) { #(length(selected_site_types_d()) > 0 | input$sites_source == 'Select By Type (Source: CalEPA Geoserver)') {
                            program_types %>%
                                filter(site_id %in% (cal_epa_sites_filtered_0()$site_id)) %>%
                                # st_drop_geometry() %>%
                                select(ei_description) %>%
                                pull(ei_description)
                        } else {
                            NULL
                        }
            )
        })
        
    # Create output table for the summary data displayed in the map
        # custom column names
            # col_name_custom <- reactive({
            #     return(paste0(
            #         if(!is.na(input$sites_date_range[1])) {
            #             paste0(year(start_date()), '-', str_pad(month(start_date()), 2, pad = 0),
            #                    '_')},
            #         if(!is.na(input$sites_date_range[2])) {
            #             paste0('to_',year(end_date()), '-', str_pad(month(end_date()), 2, pad = 0),
            #             )} else {
            #                 paste0('to_',year(Sys.Date()), '-', str_pad(month(Sys.Date()), 2, pad = 0))
            #             }
            #     )
            #     )
            # })
        # revise the data frame for use in the data table
            summary_table_df <- reactive({ # eventReactive(selected_site_types_d(), {
                # if(input$sites_source == site_source_choices[1]) { 
                #     shiny::showNotification("No data", type = "error")
                #     NULL
                # } else if (input$sites_source == 'Select By Type (Source: CalEPA Geoserver)' & length(selected_site_types_d()) == 0) {
                #     shiny::showNotification("No data", type = "error")
                #     NULL
                if (nrow(cal_epa_sites_raw()) == 0) {
                    shiny::showNotification("No data", type = "error")
                    return(NULL)
                } else {
                    # get the selected CES parameter
                        parameter <- ces_choices %>%
                            filter(name == input$ces_parameter) %>%
                            pull(ces_variable)
                    # ces3_poly_join <- ces3_poly() %>% # create a df with the ces3 polygons, with just the selected ces score
                    #     select(all_of(parameter)) %>% 
                    #     rename(ces3_polygon_score := !!parameter) # rename the field to use a consistent name (the ces measure/parameter will be reported in a separate field)
                #     parameter <- ces_choices %>% filter(name == input$ces_parameter) %>% pull(ces_variable)
                #     ces_pal_domain <- as.data.frame(ces3_poly() %>% st_drop_geometry()) %>% 
                #         select(all_of(parameter)) %>% 
                #         pull(all_of(parameter))
                # # get the dataset
                #     ces3_poly <- ces3_poly() %>% 
                #         st_transform(crs = geographic_crs) %>% # have to convert to geographic coordinate system for leaflet,
                #         mutate(fill_variable = ces_pal_domain)
                    return( 
                        cal_epa_sites_filtered() %>% 
                            # st_join(redline_polygons %>% select(-city)) %>% 
                            # st_join(ces3_poly_join) %>%
                            select(site_id, site_name, address,
                                   city, state, zip_code, 
                                   inspections_count, violations_count,
                                   enforcement_actions_count, data_source, 
                                   latitude, longitude,
                                   holc_grade, 
                                   all_of(parameter) # ces3_polygon_score
                                   ) %>% 
                            rename(ces3_polygon_score := !!parameter) %>% # rename the ces score field to use a consistent name (the ces measure/parameter will be reported in a separate field)
                            # mutate(site_id = as.character(site_id), # trying to get rid of the error: "DataTables warning: table id=DataTables_Table_1 - Ajax error. For more information about this error, please see http://datatables.net/tn/7"
                            #        zip_code = as.character(zip_code)) %>% 
                            # mutate(inspections_count = as.numeric(inspections_count), # trying to get rid of the error: "DataTables warning: table id=DataTables_Table_1 - Ajax error. For more information about this error, please see http://datatables.net/tn/7"
                            #        violations_count = as.numeric(violations_count), 
                            #        enforcement_actions_count = as.numeric(enforcement_actions_count), 
                            #        latitude = as.numeric(latitude), 
                            #        longitude = as.numeric(longitude), 
                            #        ces3_polygon_score = as.numeric(ces3_polygon_score)) %>% 
                            # rename(!!paste0('inspections_', col_name_custom()) := 'inspections_count') %>% 
                            # st_drop_geometry() %>%
                            mutate(ces3_measure = input$ces_parameter) %>% 
                            # mutate(reg_action_filter_start_date = if (is.na(input$sites_date_range[1]) | # this adds a new column showing the start date filter
                            #                                is.null(input$sites_date_range[1])) {
                            #     'NA'
                            #     } else {
                            #         input$sites_date_range[1]
                            #         },
                            #     reg_action_filter_end_date = if (is.na(input$sites_date_range[2]) | # this adds a new column showing the end date filter
                            #                           is.null(input$sites_date_range[2])) {
                            #         'NA'
                            #         } else {
                            #             min(
                            #                 as.Date(paste0(year(input$sites_date_range[2]), '-',
                            #                                month(input$sites_date_range[2]), '-',
                            #                                days_in_month(input$sites_date_range[2]))),
                            #                 most_recent_reg_records
                            #             )
                            #             }) %>%
                            mutate(reg_action_filter_start_date = start_date(),
                                reg_action_filter_end_date = end_date()) %>%
                            rename(holc_polygon_grade = holc_grade) %>% 
                            # double check for cases where there are multiple joins on the CES data (overlapping polygons)
                            # filter(ces3_polygon_score >= selected_ces_range_filter_d[1] & # first re-filter for sites within the selected range
                            #            ces3_polygon_score <= selected_ces_range_filter_d[2]) %>%
                            # filter(!duplicated(site_id)) %>%  # if duplicates still remain, just keep the first one
                            # in text columns, convert NAs to "NA"
                            mutate_if(is.character, ~replace(., is.na(.), 'NA')) %>% # replace NAs with "NA" in character columns
                            {.}
                    )
                }
            })
            
        # create table
            # file_name <- reactive({
            #     return(paste0('Filtered_Regulatory_Actions_Summary_',
            #                   if(start_date() > ymd('1800-01-01')) { # !is.na(input$sites_date_range[1])) {
            #                       paste0(year(start_date()), '-', str_pad(month(start_date()), 2, pad = 0),
            #                              '_')
            #                       } else {''},
            #                   if(end_date() < Sys.Date()) { # !is.na(input$sites_date_range[2])) {
            #                       paste0('to_',year(end_date()), '-', str_pad(month(end_date()), 2, pad = 0))
            #                       } else {
            #                           paste0('to_',year(Sys.Date()), '-', str_pad(month(Sys.Date()), 2, pad = 0))
            #                       }
            #                   # '_', Sys.Date(),
            #                   )
            #     )
            # })
            output$summary_table = DT::renderDataTable(
                summary_table_df(), # %>% mutate_if(is.numeric, as.numeric),
                extensions = c('Buttons', 'Scroller'),
                options = list(dom = 'Bfrtip',
                               buttons = list('colvis'#, 
                                              # list(
                                              #     extend = 'collection',
                                              #     buttons = list(list(extend='csv',
                                              #                         filename = paste0('Filtered_Regulatory_Actions_Summary_',
                                              #                                           str_replace_all(
                                              #                                               string = str_replace(string =  as.character(Sys.time()),
                                              #                                                                    pattern = ' ',
                                              #                                                                    replacement = '_'),
                                              #                                               pattern = ':',
                                              #                                               replacement = '-'))),
                                              #                    list(extend='excel',
                                              #                         title = NULL,
                                              #                         filename = paste0('Filtered_Regulatory_Actions_Summary_',
                                              #                                           str_replace_all(
                                              #                                               string = str_replace(string =  as.character(Sys.time()),
                                              #                                                                    pattern = ' ',
                                              #                                                                    replacement = '_'),
                                              #                                               pattern = ':',
                                              #                                               replacement = '-')))
                                              #     ),
                                              #     text = 'Download Data')
                                              ),
                               scrollX = TRUE,
                               scrollY = 250,
                               scroller = TRUE,
                               deferRender = TRUE),
                class = 'cell-border stripe',
                server = TRUE, ## NOTE: TRUE may not allow for download of the full file
                rownames = FALSE
            )
            
        # button to download summary data
            output$download_summary <- downloadHandler(
                filename = paste0('RegActionSummary_',
                                  Sys.Date(),
                                  '.csv'), 
                content = function(con) {
                    write_csv(summary_table_df(), 
                              con)
                },
                contentType = 'text/csv'
            )

            
        # buttons to download all supporting regulatory data
            #sites_list <- summary_table_df()$site_id
            output$downloadInspections <- downloadHandler(
                filename = paste0('RegActionSummary_InspectionData_', 
                                  Sys.Date(), 
                                  '.csv'), 
                content = function(con) {
                    write_csv(inspections_all_download %>% 
                                  filter(eval_date >= start_date(), 
                                         eval_date <= end_date()) %>% 
                                  filter(site_id %in% summary_table_df()$site_id), 
                              con)
                },
                contentType = 'text/csv'
            )
            output$downloadViolations <- downloadHandler(
                filename = paste0('RegActionSummary_ViolationData_', 
                                  Sys.Date(), 
                                  '.csv'), 
                content = function(con) {
                    write_csv(violations_all_download %>% 
                                  filter(violation_date >= start_date(), 
                                         violation_date <= end_date()) %>%
                                  filter(site_id %in% summary_table_df()$site_id), 
                              con)
                },
                contentType = 'text/csv'
            )
            output$downloadEnforcements <- downloadHandler(
                filename = paste0('RegActionSummary_EnforcementData_', 
                                  Sys.Date(), 
                                  '.csv'), 
                content = function(con) {
                    write_csv(enforcement_all_download %>% 
                                  filter(enf_action_date >= start_date(), 
                                         enf_action_date <= end_date()) %>% 
                                  filter(site_id %in% summary_table_df()$site_id), 
                              con)
                },
                contentType = 'text/csv'
            )


    # MAP 1 -----------------------------------------------------------------------------------------------------------------#
    output$map1 <- renderLeaflet({
        withProgress(message = 'Drawing Map', value = 1, style = 'notification', {
            
        # specify the initial zoom level to use in the map
            if (initial_zoom_level == 'Redline Bounds (All)') {
                bounds_selected <- attributes(st_geometry(redline_polygons %>% 
                                                              st_transform(crs = geographic_crs)))$bbox # have to convert to geographic coordinate system for leaflet
            } else if (initial_zoom_level == 'State') {
                bounds_selected <- attributes(st_geometry(rb_boundary() %>%
                                               st_transform(crs = geographic_crs)))$bbox # have to convert to geographic coordinate system for leaflet
            } else if (initial_zoom_level == 'City') {
                bounds_selected <- attributes(st_geometry(redline_polygons %>%
                                                              st_transform(crs = geographic_crs) %>% # have to convert to geographic coordinate system for leaflet
                                                              filter(holc_city == initial_selected_city)))$bbox
            }
            
        # create the new (empty) map
            l_map1 <- leaflet()
            
            l_map1 <- l_map1 %>% addMapPane("ces_polygons", zIndex = 410) %>% 
                addMapPane("redline_polygons_pane", zIndex = 420) %>% 
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
        
        # create a button to center the map on CA
            # get the bounds of the entire state
                bounds_ca <- attributes(st_geometry(rb_boundary() %>%
                               st_transform(crs = geographic_crs)))$bbox # have to convert to geographic coordinate system for leaflet
            # make button
                l_map1 <- l_map1 %>% 
                    addEasyButton(easyButton(
                        icon="fa-globe", title="Statewide View",
                        onClick=JS(paste0('function(btn, map){ map.fitBounds([[',
                                          round(bounds_ca[[2]],4)-0.01, ', ',
                                          round(bounds_ca[[1]],4)-0.01, '],[',
                                          round(bounds_ca[[4]],4)+0.01, ', ',
                                          round(bounds_ca[[3]],4)+0.01, ']]); }'))))
        # add a 'locate me' button
            l_map1 <- l_map1 %>% 
                addEasyButton(easyButton(
                    icon="fa-crosshairs", title="Locate Me",
                    onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
        # add measurement button
            l_map1 <- l_map1 %>% 
                addMeasure()
            
        # Add the Redline scores
            # Create the color palette for the Redline scores
                redline_leaflet_pal <- colorFactor(palette = c('green', 'blue', 'yellow', 'red'), # 'YlOrBr'   # c("#FACD7B","#D1A149","#916714","#6B4703")
                                                   domain = redline_polygons$holc_grade,
                                                   levels = c('A', 'B', 'C', 'D'))
            # Add HOLC polygons to map
                 l_map1 <- l_map1 %>% 
                    # addPolygons(data = redline_polygons %>%
                    addPolylines(data = redline_polygons %>%
                                    st_transform(crs = geographic_crs) %>% # have to convert to geographic coordinate system for leaflet,
                                    # filter(holc_grade %in% input$holc_rating_sites_filter) %>%
                                    {.},
                                options = pathOptions(pane = "redline_polygons_pane"),
                                color = ~redline_leaflet_pal(holc_grade), # 'black', # "#444444",
                                weight = 2.0,
                                smoothFactor = 1.0,
                                opacity = 1.0,
                                # fill = FALSE,
                                fillOpacity = 0, # input$redline_fill_1,
                                fillColor = 'lightgrey',
                                # fillColor = ~redline_leaflet_pal(holc_grade),
                                highlightOptions = highlightOptions(color = "white", weight = 2),#,bringToFront = TRUE
                                popup = ~paste0('<b>', '<u>', paste0('HOLC Assessed Area (', holc_year, ')'), '</u>', '</b>','<br/>',
                                                '<b>', 'City: ', '</b>', holc_city, '<br/>',
                                                '<b>', 'HOLC Name: ', '</b>', holc_name, '<br/>',
                                                '<b>', 'HOLC Grade (A-D): ', '</b>', holc_grade, '<br/>',
                                                '<b>', 'HOLC ID: ', '</b>', holc_id, '<br/>',
                                                # '<b>', paste0('HOLC Form Link (', year, '): '), '</b>', link, '</b>',
                                                '<b>', 'HOLC Form Link: ', '</b>',
                                                paste0('<a href = "', holc_link, '" ', 'target="_blank"> ', holc_link, ' </a>'), '<br/>',
                                                '<b>', 'Area Description Excerpts: ', '</b>', area_description_excerpts),
                                group = 'HOLC Polygons',
                                label = ~glue('HOLC Polygon (Grade {holc_grade})')
                                )

        # # Add the 303d polygons
        #     # add polygons
        #         l_map1 <- l_map1 %>% addPolygons(data = impaired_303d_poly() %>% st_transform(crs = geographic_crs), # have to convert to geographic coordinate system for leaflet
        #                                          options = pathOptions(pane = "303d_polygons"),
        #                                          color = 'darkblue',
        #                                          weight = 0.5,
        #                                          opacity = 0.8,
        #                                          fillColor = 'blue',
        #                                          fillOpacity = 0.5,
        #                                          smoothFactor = 1.0,
        #                                          highlightOptions = highlightOptions(color = "white", weight = 2),
        #                                          popup = ~paste0('<b>', '<u>', '303d Listed Waterbody (2014/2016)', '</u>', '</b>', '<br/>',
        #                                                          '<b>', 'Water Body Name: ', '</b>', wbname, '<br/>',
        #                                                          '<b>', 'Type: ', '</b>', wbtype, '<br/>',
        #                                                          '<b>', 'Region: ', '</b>', region_num, ' (', region_nam,')', '<br/>',
        #                                                          '<b>', 'ID: ', '</b>', wbid, '<br/>',
        #                                                          '<b>', 'Listed Pollutants: ', '</b>', pollutant, '<br/>',
        #                                                          '<b>', 'Listing Comments: ', '</b>', comments, '<br/>',
        #                                                          '<b>', 'Potential Sources: ', '</b>', sources),
        #                                          group = '303d Listed Waterbodies')
        # 
        # # Add the 303d lines
        #     # add polylines
        #         l_map1 <- l_map1 %>% addPolylines(data = impaired_303d_lines() %>% 
        #                                               st_transform(crs = geographic_crs), # have to convert to geographic coordinate system for leaflet 
        #                                           options = pathOptions(pane = "303d_lines"),
        #                                           color = 'blue',
        #                                           weight = 2.0, 
        #                                           opacity = 1.0, 
        #                                           # fillOpacity = 0.5,
        #                                           smoothFactor = 1.0,
        #                                           highlightOptions = highlightOptions(color = "white", weight = 2),
        #                                           popup = ~paste0('<b>', '<u>','303d Listed Waterbody (2014/2016)','</u>', '</b>','<br/>',
        #                                                           '<b>', 'Water Body Name: ', '</b>', wbname,'<br/>',
        #                                                           '<b>', 'Type: ', '</b>', wbtype,'<br/>',
        #                                                           '<b>', 'Region: ', '</b>', region_num, ' (', region_nam,')','<br/>',
        #                                                           '<b>', 'ID: ', '</b>', wbid, '<br/>',
        #                                                           '<b>', 'Listed Pollutants: ', '</b>', pollutant, '<br/>',
        #                                                           '<b>', 'Listing Comments: ', '</b>', comments,  '<br/>',
        #                                                           '<b>', 'Potential Sources: ', '</b>', sources),
        #                                           group = '303d Listed Waterbodies')

        
        # # add the region boundary
        #     l_map1 <- l_map1 %>% addPolygons(data = rb_boundary() %>% 
        #                                          st_transform(crs = geographic_crs), # have to convert to geographic coordinate system for leaflet
        #                                      options = pathOptions(pane = "region_polygon"),
        #                                      color = 'black', # "#444444",
        #                                      weight = 1.0,
        #                                      smoothFactor = 1.0,
        #                                      opacity = 1.0,
        #                                      fill = FALSE,
        #                                      # fillOpacity = 0.5,
        #                                      # fillColor = 'lightblue',
        #                                      # fillColor = ~redline_leaflet_pal(holc_grade),
        #                                      highlightOptions = highlightOptions(color = "white", weight = 2),#,bringToFront = TRUE
        #                                      popup = ~paste0('<b>', '<u>', 'State Water Board Region Boundary', '</u>', '</b>','<br/>',
        #                                                      '<b>', 'Region Name: ', '</b>', RB_NAME, '<br/>',
        #                                                      '<b>', 'Region Number: ', '</b>', RB_OFF),
        #                                      group = 'State Water Board Region Boundaries'
        #     )
        
        # # add the drinking water provider service areas
        #     # add polygons
        #         l_map1 <- l_map1 %>% 
        #             addPolygons(data = service_areas() %>% 
        #                             st_transform(crs = geographic_crs), # have to convert to geographic coordinate system for leaflet
        #                         options = pathOptions(pane = "serviceareas_polygon"),
        #                         color = 'black', # "#444444", 
        #                         weight = 0.5, 
        #                         smoothFactor = 1.0,
        #                         opacity = 0.8, 
        #                         fillOpacity = 0.5,
        #                         fillColor = 'lightblue',
        #                         highlightOptions = highlightOptions(color = "white", weight = 2),
        #                         popup = ~paste0('<b>', '<u>','Drinking Water Provider Service Area Boundary', '</u>','</b>','<br/>',
        #                                         '<b>', 'PWSID: ', '</b>', pwsid,'<br/>',
        #                                         '<b>', 'Name: ', '</b>',  name,'<br/>',
        #                                         '<b>', 'County: ', '</b>', d_prin_cnt,'<br/>',
        #                                         '<b>', 'Population: ', '</b>', d_populati,'<br/>'),
        #                                         #'<b>', 'Verified: ', '</b>', verified_s), # verified_status
        #                         group = 'Drinking Water Provider Service Areas'
        #             )

        
        # Set the bounds of the map dynamically - initial view is based on the full extent of the selected city or entire state, 
        # after that the map is based on the most recent bounds when a new option is selected
            isolate(if (is.null(input$map1_bounds)) { 
                l_map1 <- l_map1 %>% fitBounds(lng1 = bounds_selected[[1]],
                                               lat1 = bounds_selected[[2]],
                                               lng2 = bounds_selected[[3]],
                                               lat2 = bounds_selected[[4]])
            } else { # maintain the current view
                l_map1 <- l_map1 %>% setView(lng = mean(c(input$map1_bounds$west, input$map1_bounds$east)),
                                             lat = mean(c(input$map1_bounds$north, input$map1_bounds$south)),
                                             zoom = input$map1_zoom)
            })

        # Add controls to select the basemap and layers
            l_map1 <- l_map1 %>% addLayersControl(baseGroups = basemap_options,
                                                  overlayGroups = c('CalEnviroScreen', 
                                                                    'HOLC Polygons', 
                                                                    '303d Listed Waterbodies', 
                                                                    'CalEPA Regulated Sites',
                                                                    'Drinking Water Provider Service Areas',
                                                                    'State Water Board Region Boundaries',
                                                                    'Legend'),
                                                  options = layersControlOptions(collapsed = TRUE,
                                                                                 autoZIndex = TRUE))
            # Hide some groups by default (can be turned on with the layers control box on the map)
                # l_map1 <- l_map1 %>% hideGroup(c('Drinking Water Provider Service Areas')) #, 'HOLC Polygons')) 
                
        # output the map object
            l_map1
        })
    })
    
    # Use a separate observer to recreate some parts of the map as they are updated, without re-drawing the entire map ----
        # # create a button to re-center the map - NOT WORKING!!!!!!!!!
        #     observe({
        #         bounds_center <- attributes(st_geometry(redline_polygons %>% 
        #                                                st_transform(crs = geographic_crs) %>% # have to convert to geographic coordinate system for leaflet,
        #                                                filter(city == input$city_selected_1)))$bbox
        #         leafletProxy('map1') %>% 
        #             addEasyButton(easyButton(
        #                 icon="fa-globe", title="Center Map on Selected City",
        #                 onClick=JS(paste0('function(btn, map){ map.fitBounds([[',
        #                                   round(bounds_center[[2]],4)-0.01, ', ',
        #                                   round(bounds_center[[1]],4)-0.01, '],[',
        #                                   round(bounds_center[[4]],4)+0.01, ', ',
        #                                   round(bounds_center[[3]],4)+0.01, ']]); }'))))
        #     })
    
    
        # # HOLC rated (redlined) areas
        #     observe({
        #         withProgress(message = 'Drawing Map', value = 1, style = 'notification', {
        #         # Create the color palette for the Redline scores
        #         redline_leaflet_pal <- colorFactor(palette = c('green', 'blue', 'yellow', 'red'), # 'YlOrBr'   # c("#FACD7B","#D1A149","#916714","#6B4703")
        #                                            domain = redline_polygons$holc_grade,
        #                                            levels = c('A', 'B', 'C', 'D'))
        #         leafletProxy('map1') %>%
        #             clearGroup('HOLC Polygons') %>%
        #             addPolygons(data = redline_polygons %>%
        #                             st_transform(crs = geographic_crs) %>% # have to convert to geographic coordinate system for leaflet,
        #                             # filter(holc_grade %in% input$holc_rating_sites_filter) %>%
        #                             {.},
        #                         options = pathOptions(pane = "redline_polygons_pane"),
        #                         color = ~redline_leaflet_pal(holc_grade), # 'black', # "#444444",
        #                         weight = 2.0,
        #                         smoothFactor = 1.0,
        #                         opacity = 1.0,
        #                         # fill = FALSE,
        #                         fillOpacity = 0, # input$redline_fill_1,
        #                         fillColor = 'lightgrey',
        #                         # fillColor = ~redline_leaflet_pal(holc_grade),
        #                         highlightOptions = highlightOptions(color = "white", weight = 2),#,bringToFront = TRUE
        #                         popup = ~paste0('<b>', '<u>', 'HOLC Rated Polygon', '</u>', '</b>','<br/>',
        #                                         '<b>', 'City: ', '</b>', city, '<br/>',
        #                                         '<b>', 'Name: ', '</b>', name, '<br/>',
        #                                         '<b>', 'HOLC Grade (A-D): ', '</b>', holc_grade, '<br/>',
        #                                         '<b>', 'HOLC ID: ', '</b>', holc_id, '<br/>',
        #                                         # '<b>', paste0('HOLC Form Link (', year, '): '), '</b>', link, '</b>',
        #                                         '<b>', paste0('HOLC Form Link (', year, '): '), '</b>',
        #                                         paste0('<a href = "', link, '" ', 'target="_blank"> ', link, ' </a>')),
        #                         group = 'HOLC Polygons'
        #                         )
        #         })
        #     })
    
    
    
        
        # CalEPA sites
            observe({
                withProgress(message = 'Drawing Map', value = 1, style = 'notification', {
                if ((input$sites_source == 'Select By Type (Source: CalEPA Geoserver)' & length(selected_site_types_d()) > 0) | input$sites_source == 'All Sites (Source: CalEPA Regulated Site Portal)') {
                    cal_epa_sites <- cal_epa_sites_filtered() %>% 
                        filter(!is.na(latitude) & !is.na(longitude)) %>% 
                        st_as_sf(coords = c('longitude', 'latitude'),
                                 crs = 4326,
                                 # agr = 'constant',
                                 remove = FALSE) %>% 
                        st_transform(crs = geographic_crs) %>% # have to convert to geographic coordinate system for leaflet
                        {.}
                    # # if the option is selected, filter for sites with violations and/or enforcement actions
                    #     if ('Violations' %in% input$show_violations_enforcement) {
                    #         cal_epa_sites <- cal_epa_sites %>% filter(violations_count > 0)
                    #     }
                    #     if ('Enforcement Actions' %in% input$show_violations_enforcement) {
                    #         cal_epa_sites <- cal_epa_sites %>% filter(enforcement_actions_count > 0)
                    #     }
                    # # if program type filters are selected, filter for the selected program types
                    #     if (!is.null(input$program_type_1)) {
                    #         program_types_filter <- program_types %>% filter(EI_Description %in% input$program_type_1)
                    #         cal_epa_sites <- cal_epa_sites %>% filter(site_id %in% program_types_filter$SiteID)
                    #     }
                    leafletProxy('map1') %>%
                        clearGroup('CalEPA Regulated Sites') %>%
                        addCircleMarkers(data = cal_epa_sites,
                                         options = pathOptions(pane = "calepa_points"),
                                         radius = 4,
                                         stroke = TRUE, weight = 0.5, color = 'black', opacity = 1,
                                         fill = TRUE, fillOpacity = 1, fillColor = 'black', # 'grey', # ~wqi.leaflet.pal(WQI),
                                         # clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier = 2),# freezeAtZoom = 13, maxClusterRadius = 10),#,#singleMarkerMode = TRUE),
                                         clusterOptions = markerClusterOptions(disableClusteringAtZoom = 11, 
                                                                               maxClusterRadius = 60),
                                         popup = ~paste0('<b>', '<u>', 'CalEPA Regulated Site', '</u>','</b>','<br/>',
                                                         # '<b>', '<u>', 'Site Information:', '</u>', '</b>','<br/>',
                                                         '<b>', 'Name: ', '</b>', site_name,'<br/>',
                                                         '<b>', 'ID: ', '</b>', site_id,'<br/>',
                                                         '<b>', 'Address: ', '</b>', address, '<br/>',
                                                         '<b>', 'City: ', '</b>', city, '<br/>',
                                                         #<b>', 'County: ', '</b>', County,'<br/>',
                                                         '<b>', 'Zip Code: ', '</b>', zip_code, '<br/>',
                                                         '<b>', 'Source: ', '</b>', data_source, '<br/>',
                                                         # '<b>', 'Regulatory Actions (for selected time period):', '</b>','<br/>',
                                                         '<b>', 'Regulatory Actions (FY ', date_range(), '):', '</b>','<br/>',
                                                         '<b>', HTML('&nbsp;'), HTML('&nbsp;'), ' - Number of Inspection Records: ', '</b>', inspections_count, '<br/>', # chr(149),
                                                         '<b>', HTML('&nbsp;'), HTML('&nbsp;'), ' - Number of Violation Records: ', '</b>', violations_count, '<br/>', # chr(149),
                                                         '<b>', HTML('&nbsp;'), HTML('&nbsp;'), ' - Number of Enforcement Records: ', '</b>', enforcement_actions_count # chr(149),
                                         ),
                                         group = 'CalEPA Regulated Sites',
                                         label = ~glue('CalEPA Regulated Site (Site Name: {site_name})')
                                         )
                } else {
                    leafletProxy('map1') %>%
                        clearGroup('CalEPA Regulated Sites')
                }
                })
            })
            
        # CalEnvironScreen Polygons 
            observe({
                req(input$tab_being_displayed == "Maps & CalEPA Data") # Only display if tab is 'Map Tab' - see: https://github.com/rstudio/leaflet/issues/590
                withProgress(message = 'Drawing Map', value = 1, style = 'notification', {
                # create the color palette for the CES polygons
                    parameter <- ces_choices %>% filter(name == input$ces_parameter) %>% pull(ces_variable)
                    ces_pal_domain <- as.data.frame(ces3_poly() %>% st_drop_geometry()) %>% 
                        select(all_of(parameter)) %>% 
                        pull(all_of(parameter))
                    # create the palette
                        ces_pal_color <- 'RdYlGn' # 'YlOrBr' #'Blues' #'Greys'
                        ces_leaflet_pal <- colorNumeric(
                            palette = ces_pal_color,
                            domain = ces_pal_domain,
                            reverse = TRUE
                        )
                # get the dataset
                    ces3_poly <- ces3_poly() %>% 
                        st_transform(crs = geographic_crs) %>% # have to convert to geographic coordinate system for leaflet,
                        mutate(fill_variable = ces_pal_domain)
                # add polygons
                    leafletProxy('map1') %>%
                        clearGroup('CalEnviroScreen') %>% 
                        addPolygons(data = ces3_poly, # ces3_poly %>% filter(California_County == cities_counties[[input$city_selected_1]]), 
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
                                    popup = ~paste0('<b>', '<u>','CalEnviroScreen 3.0 (CES)', '</u>','</b>','<br/>',
                                                    '<b>', 'Census Tract: ', '</b>', 
                                                    census_tract, #eval(as.symbol(ces_field_names %>% filter(id == 'tract') %>% pull(ces_variable))),
                                                    '<br/>',
                                                    '<b>', 'Location: ', '</b>', 
                                                    nearby_city, # eval(as.symbol(ces_field_names %>% filter(id == 'City') %>% pull(ces_variable))),
                                                    ', ', 
                                                    california_county, # eval(as.symbol(ces_field_names %>% filter(id == 'California') %>% pull(ces_variable))),
                                                    ' County, ', 
                                                    zip, #eval(as.symbol(ces_field_names %>% filter(id == 'ZIP') %>% pull(ces_variable))),
                                                    '<br/>',
                                                    '<b>', 'Population (2010): ', '</b>', 
                                                    population_2010, # eval(as.symbol(ces_field_names %>% filter(id == 'pop2010') %>% pull(ces_variable))),
                                                    '<br/>',
                                                    '<b>', '<u>', 'CES Summary Scores (and Percentiles): ','</u>', '</b>',
                                                    '<br/>',
                                                    '<b>', 'Overall CES Score: ', '</b>', 
                                                    ces_3_score, # eval(as.symbol(ces_field_names %>% filter(id == 'CIscore') %>% pull(ces_variable))),
                                                    ' (',
                                                    # '&ensp;','|', '&ensp;',
                                                    #'<b>', 'CES Percentile: ', '</b>', 
                                                    ces_3_percentile, # eval(as.symbol(ces_field_names %>% filter(id == 'CIscoreP') %>% pull(ces_variable))),
                                                    ')',
                                                    '<br/>',
                                                    '<b>', 'Pollution Burden Group: ', '</b>', 
                                                    pollution_burden_group_score, # eval(as.symbol(ces_field_names %>% filter(id == 'PollutionS') %>% pull(ces_variable))),
                                                    ' (',
                                                    # '&ensp;','|', '&ensp;',
                                                    # '<b>', 'Pollution Burden Percentile: ', '</b>', 
                                                    pollution_burden_group_percentile, # eval(as.symbol(ces_field_names %>% filter(id == 'PollutionP') %>% pull(ces_variable))),
                                                    ')',
                                                    '<br/>',
                                                    '<b>', 'Population Characteristics Group: ', '</b>', 
                                                    population_characteristics_group_score, # eval(as.symbol(ces_field_names %>% filter(id == 'PopCharSco') %>% pull(ces_variable))),
                                                    ' (',
                                                    # '&ensp;','|', '&ensp;',
                                                    # '<b>', 'Population Characteristics Percentile: ', '</b>', 
                                                    population_characteristics_group_percentile, # eval(as.symbol(ces_field_names %>% filter(id == 'PopCharP') %>% pull(ces_variable))),
                                                    ')',
                                                    '<br/>',
                                                    
                                                    # POLLUTION BURDEN
                                                    '<b>', '<u>',
                                                    'Pollution Burden Scores (and Percentiles): ',
                                                    '</u>', '</b>',
                                                    '<br/>',
                                                    
                                                    '<b>', 'Ozone: ', '</b>',
                                                    ozone_score, # eval(as.symbol(ces_field_names %>% filter(id == 'ozone') %>% pull(ces_variable))),
                                                    ' (',
                                                    ozone_percentile, # eval(as.symbol(ces_field_names %>% filter(id == 'ozoneP') %>% pull(ces_variable))),
                                                    ')',
                                                    # '&ensp;',' | ', '&ensp;',
                                                    '&ensp;', '&ensp;',
                                                    '<b>', 'PM 2.5: ', '</b>',
                                                    pm_2_5_score, # eval(as.symbol(ces_field_names %>% filter(id == 'pm') %>% pull(ces_variable))),
                                                    ' (',
                                                    pm_2_5_percentile,
                                                    ')',
                                                    '<br/>',
                                                    
                                                    '<b>', 'Diesel PM: ', '</b>',
                                                    diesel_pm_score, # eval(as.symbol(ces_field_names %>% filter(id == 'diesel') %>% pull(ces_variable))),
                                                    ' (',
                                                    diesel_pm_percentile,
                                                    ')',
                                                    #'&ensp;',' | ', '&ensp;',
                                                    '&ensp;', '&ensp;',
                                                    '<b>', 'Pesticides: ', '</b>',
                                                    pesticides_score,
                                                    ' (',
                                                    pesticides_percentile, 
                                                    ')',
                                                    '<br/>',
                                                    
                                                    '<b>', 'Traffic: ', '</b>',
                                                    traffic_score, # eval(as.symbol(ces_field_names %>% filter(id == 'traffic') %>% pull(ces_variable))),
                                                    ' (',
                                                    traffic_percentile,
                                                    ')',
                                                    '&ensp;', '&ensp;',
                                                    '<b>', 'Drinking Water: ', '</b>',
                                                    drinking_water_score, # eval(as.symbol(ces_field_names %>% filter(id == 'drink') %>% pull(ces_variable))),
                                                    ' (',
                                                    drinking_water_percentile,
                                                    ')',
                                                    '<br/>',
                                                    
                                                    '<b>', 'Toxic Releases: ', '</b>',
                                                    toxic_releases_score, # eval(as.symbol(ces_field_names %>% filter(id == 'RSEIhaz') %>% pull(ces_variable))),
                                                    ' (',
                                                    toxic_releases_percentile,
                                                    ')',
                                                    '&ensp;', '&ensp;',
                                                    '<b>', 'Solid Waste: ', '</b>',
                                                    solid_waste_score, # eval(as.symbol(ces_field_names %>% filter(id == 'swis') %>% pull(ces_variable))),
                                                    ' (',
                                                    solid_waste_percentile,
                                                    ')',
                                                    '<br/>',
                                                    
                                                    '<b>', 'Cleanup Sites: ', '</b>',
                                                    cleanup_sites_score, # eval(as.symbol(ces_field_names %>% filter(id == 'cleanups') %>% pull(ces_variable))),
                                                    ' (',
                                                    cleanup_sites_percentile,
                                                    ')',
                                                    '&ensp;', '&ensp;',
                                                    '<b>', 'Groundwater Threats: ', '</b>',
                                                    groundwater_threats_score, # eval(as.symbol(ces_field_names %>% filter(id == 'gwthreats') %>% pull(ces_variable))),
                                                    ' (',
                                                    groundwater_threats_percentile,
                                                    ')',
                                                    '<br/>',

                                                    '<b>', 'Impaired Waterbodies: ', '</b>',
                                                    impaired_water_bodies_score, # eval(as.symbol(ces_field_names %>% filter(id == 'iwb') %>% pull(ces_variable))),
                                                    ' (',
                                                    impaired_water_bodies_percentile,
                                                    ')',
                                                    '&ensp;', '&ensp;',
                                                    '<b>', 'Hazardous Waste Generators: ', '</b>',
                                                    hazardous_waste_score, # eval(as.symbol(ces_field_names %>% filter(id == 'haz') %>% pull(ces_variable))),
                                                    ' (',
                                                    hazardous_waste_percentile,
                                                    ')',
                                                    '<br/>',
                                                    
                                                    # POPULATION CHARACTERISTICS
                                                    '<b>', '<u>',
                                                    'Population Characteristics Scores (and Percentiles): ',
                                                    '</u>', '</b>',
                                                    '<br/>',
                                                    
                                                    '<b>', 'Asthma: ', '</b>',
                                                    asthma_score, # eval(as.symbol(ces_field_names %>% filter(id == 'asthma') %>% pull(ces_variable))),
                                                    ' (',
                                                    asthma_percentile, 
                                                    ')',
                                                    '&ensp;', '&ensp;',
                                                    '<b>', 'Cardiovascular Disease: ', '</b>',
                                                    cardiovascular_disease_score, # eval(as.symbol(ces_field_names %>% filter(id == 'cvd') %>% pull(ces_variable))),
                                                    ' (',
                                                    cardiovascular_disease_percentile,
                                                    ')',
                                                    '<br/>',
                                                    
                                                    '<b>', 'Low Birth Weight: ', '</b>',
                                                    low_birth_weight_score, # eval(as.symbol(ces_field_names %>% filter(id == 'lbw') %>% pull(ces_variable))),
                                                    ' (',
                                                    low_birth_weight_percentile,
                                                    ')',
                                                    '&ensp;', '&ensp;',
                                                    '<b>', 'Poverty: ', '</b>',
                                                    poverty_score, # eval(as.symbol(ces_field_names %>% filter(id == 'pov') %>% pull(ces_variable))),
                                                    ' (',
                                                    poverty_percentile, 
                                                    ')',
                                                    '<br/>',
                                                    
                                                    '<b>', 'Unemployment: ', '</b>',
                                                    unemployment_score, # eval(as.symbol(ces_field_names %>% filter(id == 'unemp') %>% pull(ces_variable))),
                                                    ' (',
                                                    unemployment_percentile,
                                                    ')',
                                                    '&ensp;', '&ensp;',
                                                    '<b>', 'Education: ', '</b>',
                                                    education_score,# eval(as.symbol(ces_field_names %>% filter(id == 'edu') %>% pull(ces_variable))),
                                                    ' (',
                                                    education_percentile, 
                                                    ')',
                                                    '<br/>',
                                                    
                                                    '<b>', 'Linguistic Isolation: ', '</b>',
                                                    linguistic_isolation_score, # eval(as.symbol(ces_field_names %>% filter(id == 'ling') %>% pull(ces_variable))),
                                                    ' (',
                                                    linguistic_isolation_percentile, 
                                                    ')',
                                                    '&ensp;', '&ensp;',
                                                    '<b>', 'Housing Burden: ', '</b>',
                                                    housing_burden_score, # eval(as.symbol(ces_field_names %>% filter(id == 'housingB') %>% pull(ces_variable))),
                                                    ' (',
                                                    housing_burden_percentile, 
                                                    ')',
                                                    '<br/>',
                                                    
                                                    '<b>', '<u>',
                                                    'Demographics (% of Population): ',
                                                    '</u>', '</b>',
                                                    '<br/>',
                                                    
                                                    '<b>', 'Children (Age <=10) : ', '</b>',
                                                    children_10_percent, # eval(as.symbol(ces_field_names %>% filter(id == 'Children_u') %>% pull(ces_variable))),
                                                    '&ensp;', '&ensp;',
                                                    '<b>', 'Age 11-64: ', '</b>',
                                                    pop_11_64_years_percent, # eval(as.symbol(ces_field_names %>% filter(id == 'Pop_11_64_') %>% pull(ces_variable))),
                                                    '<br/>',
                                                    
                                                    '<b>', 'Elderly (Age >65): ', '</b>',
                                                    elderly_65_percent, # eval(as.symbol(ces_field_names %>% filter(id == 'Elderly_ov') %>% pull(ces_variable))),
                                                    '<br/>',
                                                    
                                                    '<b>', 'African American: ', '</b>',
                                                    african_american_percent, # eval(as.symbol(ces_field_names %>% filter(id == 'African_Am') %>% pull(ces_variable))),
                                                    '&ensp;', '&ensp;',
                                                    '<b>', 'Asian American: ', '</b>',
                                                    asian_american_percent, # eval(as.symbol(ces_field_names %>% filter(id == 'Asian_Amer') %>% pull(ces_variable))),
                                                    '<br/>',
                                                    
                                                    '<b>', 'Hispanic: ', '</b>',
                                                    hispanic_percent, # eval(as.symbol(ces_field_names %>% filter(id == 'Hispanic_p') %>% pull(ces_variable))),
                                                    '&ensp;', '&ensp;',
                                                    '<b>', 'Native American: ', '</b>',
                                                    native_american_percent, # eval(as.symbol(ces_field_names %>% filter(id == 'Native_Ame') %>% pull(ces_variable))),
                                                    '<br/>',
                                                    
                                                    '<b>', 'White: ', '</b>',
                                                    white_percent, # eval(as.symbol(ces_field_names %>% filter(id == 'White_pct') %>% pull(ces_variable))),
                                                    '&ensp;', '&ensp;',
                                                    '<b>', 'Other: ', '</b>',
                                                    other_percent # eval(as.symbol(ces_field_names %>% filter(id == 'Other_pct') %>% pull(ces_variable))),
                                    ),
                                    # # popupOptions = popupOptions(textsize = '15px'),
                                    group = 'CalEnviroScreen',
                                    label = ~paste0('CES Polygon (', input$ces_parameter, ': ', 
                                                    eval(as.symbol(ces_field_names %>% 
                                                                       filter(name == input$ces_parameter) %>%
                                                                       pull(ces_variable))), 
                                                    ')')
                                    ) %>% 
                        clearControls() %>%
                        addLegend(position = 'bottomright',
                                  pal = ces_leaflet_pal,
                                  values = ces3_poly$fill_variable,
                                  opacity = 1,
                                  layerId = 'ces_legend',
                                  bins = 4,
                                  group = 'Legend',
                                  title = paste0('CalEnviroScreen'))
                })
            })

        # 303d Listed Waterbodies
            observe({
                if ('303d Listed Waterbodies' %in% input$additional_map_layers) {
                    withProgress(message = 'Drawing Map', value = 1, style = 'notification', {
                        leafletProxy('map1') %>%
                            clearGroup('303d Listed Waterbodies') %>%
                            addPolygons(data = impaired_303d_poly() %>% st_transform(crs = geographic_crs), # have to convert to geographic coordinate system for leaflet
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
                                        group = '303d Listed Waterbodies') %>%
                            addPolylines(data = impaired_303d_lines() %>%
                                             st_transform(crs = geographic_crs), # have to convert to geographic coordinate system for leaflet
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
                                         group = '303d Listed Waterbodies',
                                         label = ~glue('303d Listed Waterbody (Name: {wbname})')
                            )
                    })
                } else {
                    leafletProxy('map1') %>%
                        clearGroup('303d Listed Waterbodies')
                }
            })
            
            
     # Drinking Water Provider Service Areas
             observe({
                 if ('Drinking Water Provider Service Areas' %in% input$additional_map_layers) {
                     withProgress(message = 'Drawing Map', value = 1, style = 'notification', {
                         leafletProxy('map1') %>%
                             clearGroup('Drinking Water Provider Service Areas') %>%
                             addPolygons(data = service_areas() %>%
                                             st_transform(crs = geographic_crs), # have to convert to geographic coordinate system for leaflet
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
                                         # <b>', 'Verified: ', '</b>', verified_s), # verified_status
                                         group = 'Drinking Water Provider Service Areas',
                                         label = ~glue('Drinking Water Provider Service Area Boundary (Name: {name})')
                                         )

                     })
                 } else {
                     leafletProxy('map1') %>%
                         clearGroup('Drinking Water Provider Service Areas')
                 }
             })


            # State Water Board Region Boundaries
            observe({
                if ('State Water Board Region Boundaries' %in% input$additional_map_layers) {
                    withProgress(message = 'Drawing Map', value = 1, style = 'notification', {
                        leafletProxy('map1') %>%
                            clearGroup('State Water Board Region Boundaries') %>%
                            addPolygons(data = rb_boundary() %>%
                                            st_transform(crs = geographic_crs), # have to convert to geographic coordinate system for leaflet
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
                                        popup = ~paste0('<b>', '<u>', 'State Water Board Region Boundary', '</u>', '</b>','<br/>',
                                                        '<b>', 'Region Name: ', '</b>', RB_NAME, '<br/>',
                                                        '<b>', 'Region Number: ', '</b>', RB_OFF),
                                        group = 'State Water Board Region Boundaries',
                                        label = ~glue('State Water Board Region Boundary ({RB_NAME})')
                                        )
                    })
                } else {
                    leafletProxy('map1') %>%
                        clearGroup('State Water Board Region Boundaries')
                }
            })
    
    
    # MAP 2 -----------------------------------------------------------------------------------------------------------------#    
    output$map2 <- renderLeaflet({
        # specify the initial zoom level to use in the map
            if (initial_zoom_level == 'Redline Bounds (All)') {
                bounds_selected <- attributes(st_geometry(redline_polygons %>% 
                                                              st_transform(crs = geographic_crs)))$bbox # have to convert to geographic coordinate system for leaflet
            } else if (initial_zoom_level == 'State') {
                bounds_selected <- attributes(st_geometry(rb_boundary() %>%
                                               st_transform(crs = geographic_crs)))$bbox # have to convert to geographic coordinate system for leaflet
            } else if (initial_zoom_level == 'City') {
                bounds_selected <- attributes(st_geometry(redline_polygons %>%
                                                              st_transform(crs = geographic_crs) %>% # have to convert to geographic coordinate system for leaflet
                                                              filter(holc_city == initial_selected_city)))$bbox
            }
        # create the new (empty) map
            l_map2 <- leaflet(options = leafletOptions(zoomControl = FALSE, 
                                                       dragging = FALSE))
        
        # Basemap Options
            basemap_options <- c('Esri.WorldTopoMap', 'CartoDB.Positron', 'Esri.WorldGrayCanvas','Esri.WorldImagery','Esri.WorldStreetMap') 
            for (provider in basemap_options) {
                l_map2 <- l_map2 %>% 
                    addProviderTiles(provider, group = provider)
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
                          title = paste0('HOLC Polygons'))
        
        # Add controls to select the basemap and layers
            l_map2 <- l_map2 %>% 
                addLayersControl(baseGroups = basemap_options,
                                 overlayGroups = c('HOLC Polygons', 'Legend'),
                                 options = layersControlOptions(collapsed = TRUE, autoZIndex = TRUE)) 
        
        # Set the bounds of the map dynamically - initial view is based on the full extent of the selected city, after that the map is based on the most recent bounds when a new option is selected
            isolate(if (is.null(input$map2_bounds)) {
                l_map2 <- l_map2 %>% 
                    fitBounds(lng1 = bounds_selected[[1]],
                              lat1 = bounds_selected[[2]],
                              lng2 = bounds_selected[[3]],
                              lat2 = bounds_selected[[4]])
            } else { # maintain the current view
                l_map2 <- l_map2 %>% 
                    setView(lng = mean(c(input$map2_bounds$west, input$map2_bounds$east)),
                            lat = mean(c(input$map2_bounds$north, input$map2_bounds$south)),
                            zoom = input$map2_zoom)
            })
            
        # Add the HOLC (redline) polygons
            l_map2 <- l_map2 %>% 
                addPolygons(data = redline_polygons %>% 
                                st_transform(crs = geographic_crs) %>% # have to convert to geographic coordinate system for leaflet
                                # filter(holc_grade %in% input$holc_rating_sites_filter) %>% 
                                {.},
                            color = 'black', # "#444444",
                            weight = 1.0,
                            smoothFactor = 1.0,
                            opacity = 1.0,
                            # fill = FALSE,
                            fillOpacity = 1.0,
                            # fillColor = 'lightblue',
                            fillColor = ~redline_leaflet_pal(holc_grade),
                            highlightOptions = highlightOptions(color = "white", weight = 2),#,bringToFront = TRUE
                            popup = ~paste0('<b>', '<u>', paste0('HOLC Assessed Area (', holc_year, ')'), '</u>', '</b>','<br/>',
                                            '<b>', 'City: ', '</b>', holc_city, '<br/>',
                                            '<b>', 'HOLC Name: ', '</b>', holc_name, '<br/>',
                                            '<b>', 'HOLC Grade (A-D): ', '</b>', holc_grade, '<br/>',
                                            '<b>', 'HOLC ID: ', '</b>', holc_id, '<br/>',
                                            # '<b>', paste0('HOLC Form Link (', year, '): '), '</b>', link, '</b>', 
                                            '<b>', 'HOLC Form Link: ', '</b>', 
                                            paste0('<a href = "', holc_link, '" ', 'target="_blank"> ', holc_link, ' </a>'), '<br/>',
                                            '<b>', 'Area Description Excerpts: ', '</b>', area_description_excerpts),
                            group = 'HOLC Polygons',
                            label = ~glue('HOLC Polygon (Grade {holc_grade})')
                )
        
        # output the map object
            l_map2
    })
    
    
    # # Use a separate observer to recreate some parts of Map 2 as they are updated, without re-drawing the entire map
    #     # HOLC (redline) polygons
    #         observe({
    #             #input$city_selected_1 # to re-draw polygons when city is changed
    #             # Create the color palette for the Redline scores
    #             redline_leaflet_pal <- colorFactor(palette = c('green', 'blue', 'yellow', 'red'), # 'YlOrBr'   # c("#FACD7B","#D1A149","#916714","#6B4703")
    #                                                domain = redline_polygons$holc_grade, 
    #                                                levels = c('A', 'B', 'C', 'D'))
    #             leafletProxy("map2") %>% 
    #                 clearShapes() %>% 
    #                 addPolygons(data = redline_polygons %>% 
    #                                 st_transform(crs = geographic_crs) %>% # have to convert to geographic coordinate system for leaflet
    #                                 # filter(holc_grade %in% input$holc_rating_sites_filter) %>% 
    #                                 {.},
    #                             color = 'black', # "#444444",
    #                             weight = 1.0,
    #                             smoothFactor = 1.0,
    #                             opacity = 1.0,
    #                             # fill = FALSE,
    #                             fillOpacity = 1.0,
    #                             # fillColor = 'lightblue',
    #                             fillColor = ~redline_leaflet_pal(holc_grade),
    #                             highlightOptions = highlightOptions(color = "white", weight = 2),#,bringToFront = TRUE
    #                             popup = ~paste0('<b>', '<u>', 'HOLC Rated Polygon', '</u>', '</b>','<br/>',
    #                                             '<b>', 'City: ', '</b>', city, '<br/>',
    #                                             '<b>', 'Name: ', '</b>', name, '<br/>',
    #                                             '<b>', 'HOLC Grade (A-D): ', '</b>', holc_grade, '<br/>',
    #                                             '<b>', 'HOLC ID: ', '</b>', holc_id, '<br/>',
    #                                             # '<b>', paste0('HOLC Form Link (', year, '): '), '</b>', link, '</b>', 
    #                                             '<b>', paste0('HOLC Form Link (', year, '): '), '</b>', 
    #                                             paste0('<a href = "', link, '" ', 'target="_blank"> ', link, ' </a>')),
    #                             group = 'HOLC Polygons'
    #                 )
    #         })


# Helper functions ---------------------------------------------------------------------------------------------------------#
    # Center map1 and map2 on change in city selection
        observeEvent(input$city_selected_1, {
            if (input$city_selected_1 == 'Statewide') {
                redline_selected_1 <- redline_polygons %>% 
                    st_transform(crs = geographic_crs) # have to convert to geographic coordinate system for leaflet,
            } else {
                redline_selected_1 <- redline_polygons %>% 
                    st_transform(crs = geographic_crs) %>% # have to convert to geographic coordinate system for leaflet
                    filter(holc_city == input$city_selected_1)
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
        
        
        
# PLOTS ----
    # # box plot - raw scores
    #     output$plot_box_raw <- renderPlot({
    #     # get variable values
    #         var_name <- ces_choices %>% 
    #             filter(name == input$analysis_indicator_selection) %>% 
    #             pull(ces_variable)
    #         measure_name <- input$analysis_indicator_selection
    #     # make plot
    #         raw_scores_boxplot <- ggplot(data = if (input$analysis_aggregation_method == 'Area Weighted Average') {
                                            #     analysis_raw_scores
                                            # } else if (input$analysis_aggregation_method == 'Nearest Centroid') {
                                            #     analysis_centroid_scores
                                            # }, 
    #                                      aes(x = holc_grade,
    #                                          y = !!as.name(var_name))) +
    #             geom_boxplot(aes(fill = holc_grade), notch = TRUE, outlier.shape = NA) +
    #             # scale_color_manual(values = alpha(c('green', 'blue', 'yellow', 'red'), 0.4)) +
    #             scale_fill_manual(values = alpha(c('green', 'blue', 'yellow', 'red'), 0.6)) +
    #             geom_jitter(color='black', size=0.6, alpha=0.5, width = 0.2) +
    #             theme(legend.position="none") +
    #             labs(x = 'HOLC Grade', y = glue('{measure_name} Raw Score')) +
    #             geom_blank()
    #     # return the plot
    #         raw_scores_boxplot
    # })

    # reset axis min/max values on change in indicator selection
        observeEvent(input$analysis_indicator_selection,{
            reset('boxplot_axis_min')
            reset('boxplot_axis_max')
            # input$boxplot_axis_min <- NULL
            # input$boxplot_axis_max <- NULL
        })
        
    # box plot - departures
    output$plot_box_departures <- renderPlot({
        # get variable values
            var_name <- ces_choices %>% 
                filter(name == input$analysis_indicator_selection) %>% 
                pull(ces_variable)
            measure_name <- input$analysis_indicator_selection
            if (!is.null(input$boxplot_axis_min)) {axis_min <- input$boxplot_axis_min} else {axis_min <- na_dbl}
            if (!is.null(input$boxplot_axis_max)) {axis_max <- input$boxplot_axis_max} else {axis_max <- na_dbl}
            
        # make plot
            departures_boxplot <- ggplot(data = if (input$analysis_aggregation_method == 'Area Weighted Average') {
                                                analysis_departure_scores
                                            } else if (input$analysis_aggregation_method == 'Nearest Centroid') {
                                                analysis_centroid_departure_scores
                                            }, 
                                         mapping = aes(x = holc_grade,
                                                       y = !!as.name(var_name))) +
                geom_boxplot(aes(fill = holc_grade), notch = FALSE, outlier.shape = NA) + # removed the notch part - makes plot hard to read for some indicators, and may not be that meaningful
                # scale_color_manual(values = alpha(c('green', 'blue', 'yellow', 'red'), 0.4)) +
                scale_fill_manual(values = alpha(c('green', 'blue', 'yellow', 'red'), 0.6)) +
                geom_jitter(color='black', size=0.6, alpha=0.5, width = 0.2) +
                scale_x_discrete(limits = rev(levels(factor(analysis_departure_scores$holc_grade)))) +
                coord_flip(ylim = c(axis_min, axis_max)) +
                # NOTE: if not flipping the coordinates, use coord_cartesian(ylim = c(axis_min, axis_max))
                theme(legend.position = "none") +
                labs(x = 'HOLC Grade', 
                     y = glue('Aggregated {measure_name} Departure'),
                     title = glue('HOLC Polygon Aggregated CES Score ({input$analysis_aggregation_method}) Departure from Citywide Average, by HOLC Grade'),
                     subtitle = 'Each point represents a polygon in the HOLC maps. Departure is aggregated score for that polygon versus average score of all polygons in respective city') +
                geom_blank()
            # get and set the axis limits
            # departures_boxplot <- departures_boxplot + 
            #     coord_cartesian(ylim = c(x_min, x_max))
                

        # return the plot
            departures_boxplot
    })

    
    # point plot - all cities - departures
    output$plot_point_departures <- renderPlot({
        # get variable values
            var_name <- ces_choices %>%
                filter(name == input$analysis_indicator_selection) %>%
                pull(ces_variable)
            measure_name <- input$analysis_indicator_selection
        # make plot
            departures_point <- ggplot(data = if (input$analysis_aggregation_method == 'Area Weighted Average') {
                                                analysis_departure_scores
                                            } else if (input$analysis_aggregation_method == 'Nearest Centroid') {
                                                analysis_centroid_departure_scores
                                            }, 
                                       mapping = aes(x = !!as.name(var_name),
                                                     y = holc_city)) +
                geom_jitter(aes(color = holc_grade), 
                            height = 0.25) +
                scale_color_manual(values = alpha(c('green', 'blue', 'orange', 'red'), 0.3), name = 'HOLC Grade') +
                labs(x = glue('Aggregated {measure_name} Departure'), 
                     y = 'City',
                     title = glue('Aggregated {measure_name} Departure from Citywide Average'),
                     subtitle = 'Departure is raw score versus average score of all polygons in respective city') +
                scale_y_discrete(limits = rev(levels(factor(analysis_departure_scores$holc_city)))) +
                stat_summary(fun.data = mean_sdl, 
                             fun.args = list(mult = 1), 
                             geom = 'pointrange', 
                             color = 'black', 
                             size = 0.3,
                             # alpha = 0.5
                             ) +
                geom_blank()
        # return the plot
            departures_point
    })
        
    # point plot - all cities - raw scores
    output$plot_point_raw <- renderPlot({
        # get variable values
            var_name <- ces_choices %>%
                filter(name == input$analysis_indicator_selection) %>%
                pull(ces_variable)
            measure_name <- input$analysis_indicator_selection
        # make plot
        raw_scores_point <- ggplot(data = if (input$analysis_aggregation_method == 'Area Weighted Average') {
                                                analysis_raw_scores
                                            } else if (input$analysis_aggregation_method == 'Nearest Centroid') {
                                                analysis_centroid_scores
                                            },
                                   mapping = aes(x = !!as.name(var_name),
                                                 y = holc_city)) +
            geom_jitter(mapping = aes(color = holc_grade),
                        height = 0.25) +
            scale_color_manual(values = alpha(c('green', 'blue', 'orange', 'red'), 0.3), name = 'HOLC Grade') +
            labs(x = glue('Aggregated {measure_name}'), 
                 y = 'City', 
                 title = glue('HOLC Polygon Aggregated {measure_name} ({input$analysis_aggregation_method})'),
                 subtitle = 'Black dots and lines represent mean +/- 1 standard deviation') +
            scale_y_discrete(limits = rev(levels(factor(analysis_raw_scores$holc_city)))) +
            # draw a point at the mean for each city
                # stat_summary(fun = mean, geom = 'point') + 
            # draw a vertical line at the mean for each city
                # stat_summary(fun = mean, 
                #              geom = 'errorbar',
                #              aes(xmax = ..x.., xmin = ..x..),
                #              width = 0.5, size = 0.7, linetype = "solid", color = 'black') +
            # draw the mean and standard deviation for each city (# of std deviations in the fun.args part)
                stat_summary(fun.data = mean_sdl, 
                             fun.args = list(mult = 1), 
                             geom = 'pointrange', 
                             color = 'black', 
                             size = 0.3,
                             # alpha = 0.5
                             ) +
            geom_blank()
        # return the plot
            raw_scores_point
    })
        
    # point plot - departures - by city (facet on cities)
    output$plot_departures_cities <- renderPlot({
        # get variable values
            var_name <- ces_choices %>% 
                filter(name == input$analysis_indicator_selection) %>% 
                pull(ces_variable)
            measure_name <- input$analysis_indicator_selection
        # make plot
            departures_point_city <- ggplot(data = if (input$analysis_aggregation_method == 'Area Weighted Average') {
                                                analysis_departure_scores
                                            } else if (input$analysis_aggregation_method == 'Nearest Centroid') {
                                                analysis_centroid_departure_scores
                                            }) +
                geom_point(aes(x = !!as.name(var_name),
                               y = holc_grade,
                               color = holc_grade)) +
                scale_color_manual(values = alpha(c('green', 'blue', 'orange', 'red'), 0.4),
                                   name = 'HOLC Grade') +
                scale_y_discrete(limits = rev(levels(factor(analysis_departure_scores$holc_grade)))) +
                facet_wrap(vars(holc_city), ncol = 4) +
                labs(x = glue('Aggregated {measure_name} Departure'), 
                     y = 'HOLC Grade',
                     title = glue('HOLC Polygon Aggregated {measure_name} ({input$analysis_aggregation_method}) Departure from Citywide Average'),
                     subtitle = 'Departure is aggregated score versus average score of all polygons in respective city') +
                geom_blank()
        # return the plot
            departures_point_city
    })
        
        
    # # point plot - raw scores - by city (facet on cities)
    # output$plot_raw_scores_cities <- renderPlot({
    #     # get variable values
    #         var_name <- ces_choices %>% 
    #             filter(name == input$analysis_indicator_selection) %>% 
    #             pull(ces_variable)
    #         measure_name <- input$analysis_indicator_selection
    #     # make plot
    #         raw_scores_point_city <- ggplot(data = if (input$analysis_aggregation_method == 'Area Weighted Average') {
                                            #     analysis_raw_scores
                                            # } else if (input$analysis_aggregation_method == 'Nearest Centroid') {
                                            #     analysis_centroid_scores
                                            # }) +
    #             geom_point(aes(x = !!as.name(var_name),
    #                            y = holc_grade,
    #                            color = holc_grade)) +
    #             scale_color_manual(values = alpha(c('green', 'blue', 'orange', 'red'), 0.4),
    #                                name = 'HOLC Grade') +
    #             scale_y_discrete(limits = rev(levels(factor(analysis_raw_scores$holc_grade)))) +
    #             facet_wrap(vars(holc_city), ncol = 4) +
    #             labs(x = measure_name,
    #                  y = 'HOLC Grade') +
    #             geom_blank()
    #     # return the plot
    #         raw_scores_point_city
    # })
   
    # plot average departure for each HOLC grade within each city
    output$plot_average_departures <- renderPlot({
        # get variable values
            var_name <- ces_choices %>% 
                filter(name == input$analysis_indicator_selection) %>% 
                pull(ces_variable)
            measure_name <- input$analysis_indicator_selection
        # compute average departures (for each HOLC grade within each city)
            analysis_departure_scores_summary <- if (input$analysis_aggregation_method == 'Area Weighted Average') {
                analysis_departure_scores
            } else if (input$analysis_aggregation_method == 'Nearest Centroid') {
                analysis_centroid_departure_scores
            }
            analysis_departure_scores_summary <- analysis_departure_scores_summary %>%
                group_by(holc_city, holc_grade) %>%
                # summarize(city_holc_dep_average = mean(!!as.name(var_name)),
                #           city_holc_dep_median = median(!!as.name(var_name))) %>%
                summarise(city_holc_dep_average = mean(!!as.name(var_name)),
                          city_holc_dep_median = median(!!as.name(var_name))) %>%
                ungroup() %>% 
                {.}
        # make plot
            holc_city_means_plot <- ggplot(analysis_departure_scores_summary) +
                geom_point(aes(x = city_holc_dep_average,
                               y = holc_city,
                               color = holc_grade)) +
                # geom_jitter(aes(x = city_holc_dep_average,
                #                 y = holc_city,
                #                 color = holc_grade),
                #             height = 0.15) +
                scale_color_manual(values = alpha(c('green', 'blue', 'orange', 'red'), 1.0),
                                   name = 'HOLC Grade') +
                scale_y_discrete(limits = rev(levels(factor(analysis_departure_scores_summary$holc_city)))) +
                labs(x = glue('Aggregated {measure_name} Average Departure'), 
                     y = 'City',
                     title = glue('Average HOLC Polygon Aggregated {measure_name} Departure from Citywide Average by HOLC Grade'),
                     subtitle = 'Departure is aggregated score versus average score of all polygons in respective city') +
                theme(legend.position = 'bottom') +
                geom_blank()
        # return the plot
            holc_city_means_plot
    })
    
# MAPS (ANALYSIS) ----
    # create a list of counties corresponding to the HOLC city maps
    cities_counties <- list('Fresno' = 'Fresno',
                            'Los Angeles' = c('Los Angeles', 'Orange'),
                            'Oakland' = 'Alameda',
                            'Sacramento' = c('Sacramento', 'Yolo'),
                            'San Diego' = 'San Diego',
                            'San Francisco' = 'San Francisco',
                            'San Jose' = 'Santa Clara',
                            'Stockton' = 'San Joaquin')
    
    redline_analysis_data <- reactive({
        return(redline_polygons %>%
                   filter(holc_city == input$analysis_city_selection))
    })
    
    ces_analysis_data <- reactive({
        counties_selected <- ca_counties %>% 
            filter(cnty_name %in% cities_counties[[input$analysis_city_selection]])
        return(ces3_poly()[counties_selected, ]
               # ces3_poly[redline_analysis_data(), ]
               # ces3_poly %>% 
               #     st_is_within_distance(y = redline_analysis_data(), dist = 10)
               # filter(nearby_city == input$analysis_city_selection))
        )
    })
    
    output$analysis_map_holc <- renderLeaflet({
        # get variable values
            # var_name <- ces_choices %>% 
            #     filter(name == input$analysis_indicator_selection) %>% 
            #     pull(ces_variable)
            # measure_name <- input$analysis_indicator_selection
            # city <- input$analysis_city_selection

        # specify the initial zoom level to use in the map
            bounds_city <- attributes(st_geometry(redline_analysis_data() %>% 
                                                      st_transform(crs = geographic_crs)))$bbox # have to convert to geographic coordinate system for leaflet
            
        # create the new (empty) map
            analysis_map_holc <- leaflet(options = leafletOptions(zoomControl = TRUE, 
                                                       dragging = TRUE))
        
        # Basemap Options
            # use default options
                # analysis_map_holc <- leaflet() %>%
                #     addTiles()
            # use custom options
                # basemap_options <- c('Esri.WorldTopoMap', 'CartoDB.Positron', 'Esri.WorldGrayCanvas','Esri.WorldImagery','Esri.WorldStreetMap')
                basemap_options <- c('CartoDB.Positron')
                for (provider in basemap_options) {
                    analysis_map_holc <- analysis_map_holc %>%
                        addProviderTiles(provider, group = provider)
                }
        
        # Create the color palette for the Redline scores
            redline_leaflet_pal <- colorFactor(palette = c('green', 'blue', 'gold1', 'red'), # 'YlOrBr'   # c("#FACD7B","#D1A149","#916714","#6B4703")
                                               domain = redline_polygons$holc_grade, 
                                               levels = c('A', 'B', 'C', 'D'))
        
        # add the legend
            analysis_map_holc <- analysis_map_holc %>% 
                addLegend(position = 'bottomright', 
                          pal = redline_leaflet_pal, 
                          values = c('A', 'B', 'C', 'D'), 
                          opacity = 1, 
                          layerId = 'redline_legend', 
                          group = 'Legend', 
                          title = paste0('HOLC Polygons'))
        
        # Add controls to select the basemap and layers
            analysis_map_holc <- analysis_map_holc %>% 
                addLayersControl(# baseGroups = basemap_options,
                                 overlayGroups = c('HOLC Polygons', 'Legend'),
                                 options = layersControlOptions(collapsed = TRUE, autoZIndex = TRUE)) 
        
        # Set the bounds of the map dynamically - initial view is based on the full extent of the selected city, after that the map is based on the most recent bounds when a new option is selected
            isolate(if (is.null(input$analysis_map_holc_bounds)) {
                analysis_map_holc <- analysis_map_holc %>%
                    fitBounds(lng1 = bounds_city[[1]],
                              lat1 = bounds_city[[2]],
                              lng2 = bounds_city[[3]],
                              lat2 = bounds_city[[4]])
            # }  else { 
            #     # maintain the current view
            #     analysis_map_holc <- analysis_map_holc %>%
            #         setView(lng = mean(c(input$analysis_map_holc_bounds$west, input$analysis_map_holc_bounds$east)),
            #                 lat = mean(c(input$analysis_map_holc_bounds$north, input$analysis_map_holc_bounds$south)),
            #                 zoom = input$analysis_map_holc_zoom)
            })
            
        # create a button to center the map on selected city
            analysis_map_holc <- analysis_map_holc %>% 
                addEasyButton(easyButton(
                    icon="fa-globe", title=glue('Zoom to {input$analysis_city_selection}'),
                    onClick=JS(paste0('function(btn, map){ map.fitBounds([[',
                                      round(bounds_city[[2]],4)-0.01, ', ',
                                      round(bounds_city[[1]],4)-0.01, '],[',
                                      round(bounds_city[[4]],4)+0.01, ', ',
                                      round(bounds_city[[3]],4)+0.01, ']]); }'))))
            
        # Add the HOLC (redline) polygons
            analysis_map_holc <- analysis_map_holc %>% 
                addPolygons(data = redline_analysis_data() %>% 
                                st_transform(crs = geographic_crs) %>% # have to convert to geographic coordinate system for leaflet
                                # filter(holc_grade %in% input$holc_rating_sites_filter) %>% 
                                {.},
                            color = 'black', # "#444444",
                            weight = 1.0,
                            smoothFactor = 1.0,
                            opacity = 1.0,
                            # fill = FALSE,
                            fillOpacity = 1.0,
                            # fillColor = 'lightblue',
                            fillColor = ~redline_leaflet_pal(holc_grade),
                            highlightOptions = highlightOptions(color = "white", weight = 2),#,bringToFront = TRUE
                            popup = ~paste0('<b>', '<u>', paste0('HOLC Assessed Area (', holc_year, ')'), '</u>', '</b>','<br/>',
                                            '<b>', 'City: ', '</b>', holc_city, '<br/>',
                                            '<b>', 'HOLC Name: ', '</b>', holc_name, '<br/>',
                                            '<b>', 'HOLC Grade (A-D): ', '</b>', holc_grade, '<br/>',
                                            '<b>', 'HOLC ID: ', '</b>', holc_id, '<br/>',
                                            '<b>', 'Area (1000 sq meters): ', '</b>', 
                                            format(x = area_calc_meters_sq / 1000, digits = 0, big.mark = ',', scientific = FALSE),
                                            '<br/>',
                                            # '<b>', paste0('HOLC Form Link (', year, '): '), '</b>', link, '</b>', 
                                            '<b>', 'HOLC Form Link: ', '</b>', 
                                            paste0('<a href = "', holc_link, '" ', 'target="_blank"> ', holc_link, ' </a>')# , '<br/>',
                                            # '<b>', 'Area Description Excerpts: ', '</b>', area_description_excerpts
                                            ),
                            group = 'HOLC Polygons',
                            label = ~glue('HOLC Polygon (Grade {holc_grade})')
                )
        
        # output the map object
            analysis_map_holc
    })
    
    
    output$analysis_map_overlap <- renderLeaflet({
        # get variable values
            # var_name <- ces_choices %>%
            #     filter(name == input$analysis_indicator_selection) %>%
            #     pull(ces_variable)
            # measure_name <- input$analysis_indicator_selection
            # city <- input$analysis_city_selection

        # specify the initial zoom level to use in the map
            bounds_city <- attributes(st_geometry(redline_analysis_data() %>% 
                                                      st_transform(crs = geographic_crs)))$bbox # have to convert to geographic coordinate system for leaflet
            
        # create the new (empty) map
            analysis_map_overlap <- leaflet(options = leafletOptions(zoomControl = FALSE, 
                                                       dragging = FALSE))
            
            analysis_map_overlap <- analysis_map_overlap %>% 
                addMapPane("ces_polygons", zIndex = 410) %>% 
                addMapPane("redline_polygons_pane", zIndex = 420)
        
        # Basemap Options
            # use default options
                # analysis_map_overlap <- leaflet() %>%
                #     addTiles()
            # use custom options
                # basemap_options <- c('Esri.WorldTopoMap', 'CartoDB.Positron', 'Esri.WorldGrayCanvas','Esri.WorldImagery','Esri.WorldStreetMap')
                basemap_options <- c('CartoDB.Positron')
                for (provider in basemap_options) {
                    analysis_map_overlap <- analysis_map_overlap %>%
                        addProviderTiles(provider, group = provider)
                }
        
        # Set the bounds of the map dynamically - initial view is based on the full extent of the selected city, after that the map is based on the most recent bounds when a new option is selected
            isolate(if (is.null(input$analysis_map_overlap_bounds)) {
                analysis_map_overlap <- analysis_map_overlap %>%
                    fitBounds(lng1 = bounds_city[[1]],
                              lat1 = bounds_city[[2]],
                              lng2 = bounds_city[[3]],
                              lat2 = bounds_city[[4]])
            } else { # maintain the current view
                analysis_map_overlap <- analysis_map_overlap %>%
                    setView(lng = mean(c(input$analysis_map_overlap_bounds$west, input$analysis_map_overlap_bounds$east)),
                            lat = mean(c(input$analysis_map_overlap_bounds$north, input$analysis_map_overlap_bounds$south)),
                            zoom = input$analysis_map_overlap_zoom)
            })
            
        # # create a button to center the map on selected city
        #     analysis_map_overlap <- analysis_map_overlap %>% 
        #         addEasyButton(easyButton(
        #             icon="fa-globe", title=glue('Zoom to {input$analysis_city_selection}'),
        #             onClick=JS(paste0('function(btn, map){ map.fitBounds([[',
        #                               round(bounds_city[[2]],4)-0.01, ', ',
        #                               round(bounds_city[[1]],4)-0.01, '],[',
        #                               round(bounds_city[[4]],4)+0.01, ', ',
        #                               round(bounds_city[[3]],4)+0.01, ']]); }'))))
            
        # Add CES polygons
            # create the color palette for the CES polygons
                parameter <- ces_choices %>%
                    filter(name == input$analysis_indicator_selection) %>%
                    pull(ces_variable)
                ces_pal_domain <- as.data.frame(ces3_poly() %>% st_drop_geometry()) %>% 
                    select(all_of(parameter)) %>% 
                    pull(all_of(parameter))
                # ces_fill_domain <- as.data.frame(ces_analysis_data() %>% st_drop_geometry()) %>% 
                #     select(all_of(parameter)) %>% 
                #     pull(all_of(parameter))
                # create the palette
                    ces_pal_color <- 'RdYlGn' # 'YlOrBr' #'Blues' #'Greys'
                    ces_leaflet_pal <- colorNumeric(
                        palette = ces_pal_color,
                        domain = ces_pal_domain,
                        reverse = TRUE
                    )
                # get the dataset
                    ces3_poly <- ces_analysis_data() %>% 
                        mutate(area_calc_meters_sq = st_area(.)) %>% 
                        drop_units() %>% 
                        st_transform(crs = geographic_crs) # %>% # have to convert to geographic coordinate system for leaflet,
                        # mutate(fill_variable = ces_fill_domain)
                # add polygons
                    analysis_map_overlap <- analysis_map_overlap %>% 
                        addPolygons(data = ces3_poly, # ces3_poly %>% filter(California_County == cities_counties[[input$city_selected_1]]), 
                                    options = pathOptions(pane = "ces_polygons"),
                                    color = 'black', # 'grey', # "#444444", 
                                    weight = 0.5, 
                                    smoothFactor = 1.0,
                                    opacity = 0.8, 
                                    fillOpacity = 0.8,
                                    # fillColor = ~colorNumeric('YlOrBr', Poll_pctl)(Poll_pctl), # view RColorBrewer palettes with: RColorBrewer::display.brewer.all()
                                    # fillColor = ~ces_leaflet_pal(ces_fill_domain),
                                    fillColor = ~ces_leaflet_pal(
                                        eval(as.symbol(
                                            ces_choices %>% 
                                                filter(name == input$analysis_indicator_selection) %>% 
                                                pull(ces_variable)
                                            ))), # ces_fill_domain),
                                    # fillColor = 'green',
                                    highlightOptions = highlightOptions(color = "white", weight = 2), # fill = TRUE, fillColor = "white"),#,bringToFront = TRUE
                                    popup = ~paste0('<b>', '<u>','CalEnviroScreen 3.0 (CES)', '</u>','</b>','<br/>',
                                                    '<b>', 'Census Tract: ', '</b>', 
                                                    census_tract, #eval(as.symbol(ces_field_names %>% filter(id == 'tract') %>% pull(ces_variable))),
                                                    '<br/>',
                                                    '<b>', 'Area (1000 sq meters): ', '</b>', 
                                                    format(x = area_calc_meters_sq / 1000, digits = 0, big.mark = ',', scientific = FALSE), 
                                                    '<br/>',
                                                    # '<b>', 'Location: ', '</b>', 
                                                    # nearby_city, # eval(as.symbol(ces_field_names %>% filter(id == 'City') %>% pull(ces_variable))),
                                                    # ', ', 
                                                    # california_county, # eval(as.symbol(ces_field_names %>% filter(id == 'California') %>% pull(ces_variable))),
                                                    # ' County, ', 
                                                    # zip, #eval(as.symbol(ces_field_names %>% filter(id == 'ZIP') %>% pull(ces_variable))),
                                                    # '<br/>',
                                                    # '<b>', 'Population (2010): ', '</b>', 
                                                    # population_2010, # eval(as.symbol(ces_field_names %>% filter(id == 'pop2010') %>% pull(ces_variable))),
                                                    # '<br/>',
                                                    
                                                    # SELECTED VARIABLE
                                                    #' '<b>', #'<u>',
                                                    #' 'Selected CES Score (and Percentile): ',
                                                    #' #'</u>', 
                                                    #' '</b>', '<br/>', '&emsp;',
                                                    '<b>', glue('{input$analysis_indicator_selection}: '), '</b>',
                                                    eval(as.symbol(ces_field_names %>% 
                                                                       filter(name == input$analysis_indicator_selection) %>%
                                                                       pull(ces_variable))),
                                                    '<br/>', # '&emsp;', 
                                                    '<b>', 
                                                    str_replace(string = input$analysis_indicator_selection,
                                                                                                      pattern = 'Score', 
                                                                                                      replacement = 'Percentile: '),
                                                    '</b>',
                                                    eval(as.symbol(ces_field_names %>%
                                                                           filter(name == str_replace(string = input$analysis_indicator_selection,
                                                                                                      pattern = 'Score', 
                                                                                                      replacement = 'Percentile')) %>%
                                                                           pull(ces_variable)))
                                    ),
                                    group = 'CalEnviroScreen',
                                    label = ~paste0('CES Polygon (', input$analysis_indicator_selection, ': ', 
                                                    eval(as.symbol(ces_field_names %>% 
                                                                       filter(name == input$analysis_indicator_selection) %>%
                                                                       pull(ces_variable))), 
                                                    ')')
                        )
                                # paste0(glue('Aggregated {input$analysis_indicator_selection}: '), # ({input$analysis_aggregation_method}): '),
                                #                     round(eval(as.symbol(ces_field_names %>%
                                #                                              filter(name == input$analysis_indicator_selection) %>%
                                #                                              pull(ces_variable))), 2)
                                #     )
            
            
            
            
        # Add the HOLC (redline) polygon outlines
            # Create the color palette for the Redline scores
                redline_leaflet_pal <- colorFactor(palette = c('green', 'blue', 'gold1', 'red'), # 'YlOrBr'   # c("#FACD7B","#D1A149","#916714","#6B4703")
                                                   domain = redline_polygons$holc_grade, 
                                                   levels = c('A', 'B', 'C', 'D'))
            # add polygons
            analysis_map_overlap <- analysis_map_overlap %>% 
                # addPolygons(data = redline_analysis_data() %>% 
                addPolylines(data = redline_analysis_data() %>% 
                                st_transform(crs = geographic_crs) %>% # have to convert to geographic coordinate system for leaflet
                                # filter(holc_grade %in% input$holc_rating_sites_filter) %>% 
                                {.},
                            options = pathOptions(pane = "redline_polygons_pane"),
                            color = ~redline_leaflet_pal(holc_grade), # 'black', # "#444444",
                            weight = 2.0,
                            smoothFactor = 1.0,
                            opacity = 1.0,
                            # fill = FALSE,
                            fillOpacity = 0, # input$redline_fill_1,
                            fillColor = 'lightgrey',
                            # fillColor = ~redline_leaflet_pal(holc_grade),
                            highlightOptions = highlightOptions(color = "white", weight = 2),#,bringToFront = TRUE
                            popup = ~paste0('<b>', '<u>', paste0('HOLC Assessed Area (', holc_year, ')'), '</u>', '</b>','<br/>',
                                            '<b>', 'City: ', '</b>', holc_city, '<br/>',
                                            '<b>', 'HOLC Name: ', '</b>', holc_name, '<br/>',
                                            '<b>', 'HOLC Grade (A-D): ', '</b>', holc_grade, '<br/>',
                                            '<b>', 'HOLC ID: ', '</b>', holc_id, '<br/>',
                                            '<b>', 'Area (1000 sq meters): ', '</b>', 
                                            format(x = area_calc_meters_sq / 1000, digits = 0, big.mark = ',', scientific = FALSE),
                                            '<br/>',
                                            # '<b>', paste0('HOLC Form Link (', year, '): '), '</b>', link, '</b>', 
                                            '<b>', 'HOLC Form Link: ', '</b>', 
                                            paste0('<a href = "', holc_link, '" ', 'target="_blank"> ', holc_link, ' </a>')# , '<br/>',
                                            # '<b>', 'Area Description Excerpts: ', '</b>', area_description_excerpts
                            ),
                            group = 'HOLC Polygons',
                            label = ~glue('HOLC Polygon (Grade {holc_grade})')
                )
            
        # add the legend
            analysis_map_overlap <- analysis_map_overlap %>% 
                addLegend(position = 'bottomright',
                          pal = ces_leaflet_pal,
                          values = ces_pal_domain, # ces3_poly$fill_variable,
                          opacity = 1,
                          layerId = 'ces_legend',
                          bins = 4,
                          group = 'Legend',
                          title = paste0('CalEnviroScreen'))
        
        # Add controls to select the basemap and layers
            analysis_map_overlap <- analysis_map_overlap %>% 
                addLayersControl(# baseGroups = basemap_options,
                                 overlayGroups = c('HOLC Polygons', 'CalEnviroScreen', 'Legend'),
                                 options = layersControlOptions(collapsed = TRUE, autoZIndex = TRUE)) 
        
        # output the map object
            analysis_map_overlap
    })

    
    output$analysis_map_intersection <- renderLeaflet({
        # get variable values
            # var_name <- ces_choices %>%
            #     filter(name == input$analysis_indicator_selection) %>%
            #     pull(ces_variable)
            # measure_name <- input$analysis_indicator_selection
            # city <- input$analysis_city_selection

        # specify the initial zoom level to use in the map
            bounds_city <- attributes(st_geometry(redline_analysis_data() %>% 
                                                      st_transform(crs = geographic_crs)))$bbox # have to convert to geographic coordinate system for leaflet
            
        # create the new (empty) map
            analysis_map_intersection <- leaflet(options = leafletOptions(zoomControl = FALSE, 
                                                       dragging = FALSE))
            
            analysis_map_intersection <- analysis_map_intersection %>% 
                addMapPane("ces_polygons", zIndex = 410) %>% 
                addMapPane("redline_polygons_pane", zIndex = 420) %>% 
                addMapPane('connecting_lines_pane', zIndex = 421) %>% 
                addMapPane('holc_centroids_pane', zIndex = 422) %>%
                addMapPane('ces_centroids_pane', zIndex = 423)

        
        # Basemap Options
            # use default options
                # analysis_map_intersection <- leaflet() %>%
                #     addTiles()
            # use custom options
                # basemap_options <- c('Esri.WorldTopoMap', 'CartoDB.Positron', 'Esri.WorldGrayCanvas','Esri.WorldImagery','Esri.WorldStreetMap')
                basemap_options <- c('CartoDB.Positron')
                for (provider in basemap_options) {
                    analysis_map_intersection <- analysis_map_intersection %>%
                        addProviderTiles(provider, group = provider)
                }
        
        # Set the bounds of the map dynamically - initial view is based on the full extent of the selected city, after that the map is based on the most recent bounds when a new option is selected
            isolate(if (is.null(input$analysis_map_intersection_bounds)) {
                analysis_map_intersection <- analysis_map_intersection %>%
                    fitBounds(lng1 = bounds_city[[1]],
                              lat1 = bounds_city[[2]],
                              lng2 = bounds_city[[3]],
                              lat2 = bounds_city[[4]])
            } else { # maintain the current view
                analysis_map_intersection <- analysis_map_intersection %>%
                    setView(lng = mean(c(input$analysis_map_intersection_bounds$west, input$analysis_map_intersection_bounds$east)),
                            lat = mean(c(input$analysis_map_intersection_bounds$north, input$analysis_map_intersection_bounds$south)),
                            zoom = input$analysis_map_intersection_zoom)
            })
            
        # # create a button to center the map on selected city
        #     analysis_map_intersection <- analysis_map_intersection %>% 
        #         addEasyButton(easyButton(
        #             icon="fa-globe", title=glue('Zoom to {input$analysis_city_selection}'),
        #             onClick=JS(paste0('function(btn, map){ map.fitBounds([[',
        #                               round(bounds_city[[2]],4)-0.01, ', ',
        #                               round(bounds_city[[1]],4)-0.01, '],[',
        #                               round(bounds_city[[4]],4)+0.01, ', ',
        #                               round(bounds_city[[3]],4)+0.01, ']]); }'))))
            
        
            
        # Add CES polygons
            # create the color palette for the CES polygons
                parameter <- ces_choices %>%
                    filter(name == input$analysis_indicator_selection) %>%
                    pull(ces_variable)
                ces_pal_domain <- as.data.frame(ces3_poly() %>% st_drop_geometry()) %>% 
                    select(all_of(parameter)) %>% 
                    pull(all_of(parameter))
                # ces_fill_domain <- as.data.frame(analysis_clipped_ces %>% st_drop_geometry()) %>% 
                #     select(all_of(parameter)) %>% 
                #     pull(all_of(parameter))
                # create the palette
                    ces_pal_color <- 'RdYlGn' # 'YlOrBr' #'Blues' #'Greys'
                    ces_leaflet_pal <- colorNumeric(
                        palette = ces_pal_color,
                        domain = ces_pal_domain,
                        reverse = TRUE
                    )
                # area weighted plot versus centroid plot
                if (input$analysis_aggregation_method == 'Area Weighted Average') { 
                # get the dataset
                    ces3_poly <- analysis_clipped_ces %>% 
                        filter(holc_city == input$analysis_city_selection) %>% 
                        mutate(area_calc_meters_sq = st_area(.)) %>% 
                        drop_units() %>% 
                        st_transform(crs = geographic_crs) # %>% # have to convert to geographic coordinate system for leaflet,
                        # mutate(fill_variable = ces_fill_domain)
                } else if (input$analysis_aggregation_method == 'Nearest Centroid') {
                    ces3_poly <- ces_analysis_data() %>% 
                        mutate(area_calc_meters_sq = st_area(.)) %>% 
                        drop_units() %>% 
                        st_transform(crs = geographic_crs) 
                } 
                # add polygons
                    analysis_map_intersection <- analysis_map_intersection %>% 
                        addPolygons(data = ces3_poly, # ces3_poly %>% filter(California_County == cities_counties[[input$city_selected_1]]), 
                                    options = pathOptions(pane = "ces_polygons"),
                                    color = 'black', # 'grey', # "#444444", 
                                    weight = 0.5,
                                    smoothFactor = 1.0,
                                    opacity = 0.8, 
                                    fillOpacity = 0.8,
                                    # fillColor = ~colorNumeric('YlOrBr', Poll_pctl)(Poll_pctl), # view RColorBrewer palettes with: RColorBrewer::display.brewer.all()
                                    fillColor = ~ces_leaflet_pal(
                                        eval(as.symbol(
                                            ces_choices %>% 
                                                filter(name == input$analysis_indicator_selection) %>% 
                                                pull(ces_variable)
                                            ))), # ces_fill_domain),
                                    # fillColor = 'green',
                                    highlightOptions = highlightOptions(color = "white", weight = 2), # fill = TRUE, fillColor = "white"),#, bringToFront = TRUE
                                    popup = ~paste0('<b>', '<u>', 
                                                    'CalEnviroScreen 3.0 (CES)', 
                                                    if(input$analysis_aggregation_method == 'Area Weighted Average') {
                                                        ' (Clipped)'
                                                        }, 
                                                    '</u>','</b>','<br/>',
                                                    '<b>', 'Census Tract: ', '</b>', 
                                                    census_tract, #eval(as.symbol(ces_field_names %>% filter(id == 'tract') %>% pull(ces_variable))),
                                                    '<br/>',
                                                    # '&emsp;', 
                                                    if(input$analysis_aggregation_method == 'Area Weighted Average') {
                                                        paste0('<b>', 'Area (1000 sq meters): ', '</b>', 
                                                               format(x = area_calc_meters_sq / 1000, digits = 0, big.mark = ',', scientific = FALSE), 
                                                               '<br/>')
                                                    }, 
                                                    # '<b>', 'Location: ', '</b>', 
                                                    # nearby_city, # eval(as.symbol(ces_field_names %>% filter(id == 'City') %>% pull(ces_variable))),
                                                    # ', ', 
                                                    # california_county, # eval(as.symbol(ces_field_names %>% filter(id == 'California') %>% pull(ces_variable))),
                                                    # ' County, ', 
                                                    # zip, #eval(as.symbol(ces_field_names %>% filter(id == 'ZIP') %>% pull(ces_variable))),
                                                    # '<br/>',
                                                    # '<b>', 'Population (2010): ', '</b>', 
                                                    # population_2010, # eval(as.symbol(ces_field_names %>% filter(id == 'pop2010') %>% pull(ces_variable))),
                                                    # '<br/>',
                                                    
                                                    # SELECTED VARIABLE
                                                    #' '<b>', #'<u>',
                                                    #' 'Selected CES Score (and Percentile): ',
                                                    #' #'</u>', 
                                                    #' '</b>', '<br/>', '&emsp;',
                                                    '<b>', glue('{input$analysis_indicator_selection}: '), '</b>',
                                                    eval(as.symbol(ces_field_names %>% 
                                                                       filter(name == input$analysis_indicator_selection) %>%
                                                                       pull(ces_variable))),
                                                    '<br/>', # '&emsp;', 
                                                    '<b>', 
                                                    str_replace(string = input$analysis_indicator_selection,
                                                                                                      pattern = 'Score', 
                                                                                                      replacement = 'Percentile: '),
                                                    '</b>',
                                                    eval(as.symbol(ces_field_names %>%
                                                                           filter(name == str_replace(string = input$analysis_indicator_selection,
                                                                                                      pattern = 'Score', 
                                                                                                      replacement = 'Percentile')) %>%
                                                                           pull(ces_variable)))
                                    ),
                                    group = 'CalEnviroScreen',
                                    label = ~paste0(if(input$analysis_aggregation_method == 'Area Weighted Average') {
                                                        'Clipped '
                                                        }, 'CES Polygon (', 
                                                    input$analysis_indicator_selection, ': ', 
                                                    eval(as.symbol(ces_field_names %>% 
                                                                       filter(name == input$analysis_indicator_selection) %>%
                                                                       pull(ces_variable))), 
                                                    ')')
                        ) 

        # Add the HOLC (redline) polygon outlines
            # Create the color palette for the Redline scores
                redline_leaflet_pal <- colorFactor(palette = c('green', 'blue', 'gold1', 'red'), # 'YlOrBr'   # c("#FACD7B","#D1A149","#916714","#6B4703")
                                                   domain = redline_polygons$holc_grade, 
                                                   levels = c('A', 'B', 'C', 'D'))
            # add polygons
            analysis_map_intersection <- analysis_map_intersection %>% 
                # addPolygons(data = redline_analysis_data() %>% 
                addPolylines(data = redline_analysis_data() %>% 
                                st_transform(crs = geographic_crs) %>% # have to convert to geographic coordinate system for leaflet
                                # filter(holc_grade %in% input$holc_rating_sites_filter) %>% 
                                {.},
                            options = pathOptions(pane = "redline_polygons_pane"),
                            color = ~redline_leaflet_pal(holc_grade), # 'black', # "#444444",
                            weight = 2.0,
                            smoothFactor = 1.0,
                            opacity = 1.0,
                            # fill = FALSE,
                            fill = FALSE,
                            # fillOpacity = 0, # input$redline_fill_1,
                            # fillColor = 'lightgrey',
                            # fillColor = ~redline_leaflet_pal(holc_grade),
                            highlightOptions = highlightOptions(color = "white", weight = 2),#,bringToFront = TRUE
                            popup = ~paste0('<b>', '<u>', paste0('HOLC Assessed Area (', holc_year, ')'), '</u>', '</b>','<br/>',
                                            '<b>', 'City: ', '</b>', holc_city, '<br/>',
                                            '<b>', 'HOLC Name: ', '</b>', holc_name, '<br/>',
                                            '<b>', 'HOLC Grade (A-D): ', '</b>', holc_grade, '<br/>',
                                            '<b>', 'HOLC ID: ', '</b>', holc_id, '<br/>',
                                            if(input$analysis_aggregation_method == 'Area Weighted Average') {
                                                paste0('<b>', 'Area (1000 sq meters): ', '</b>', 
                                                       format(x = area_calc_meters_sq / 1000, digits = 0, big.mark = ',', scientific = FALSE),
                                                       '<br/>')
                                            },
                                            # '<b>', paste0('HOLC Form Link (', year, '): '), '</b>', link, '</b>', 
                                            '<b>', 'HOLC Form Link: ', '</b>', 
                                            paste0('<a href = "', holc_link, '" ', 'target="_blank"> ', holc_link, ' </a>')# , '<br/>',
                                            # '<b>', 'Area Description Excerpts: ', '</b>', area_description_excerpts
                            ),
                            group = 'HOLC Polygons',
                            label = ~glue('HOLC Polygon (Grade {holc_grade})')
                )
            
        # add centroid info if selected
            # HOLC Centroids
            if (input$analysis_aggregation_method == 'Nearest Centroid') {
                analysis_map_intersection <- analysis_map_intersection %>%
                    addCircleMarkers(data = analysis_holc_centroids %>% 
                                         filter(holc_city == input$analysis_city_selection) %>% 
                                         st_transform(crs = geographic_crs),
                                     options = pathOptions(pane = 'holc_centroids_pane'),
                                     radius = 3,
                                     stroke = TRUE,
                                     color = 'black',
                                     weight = 0.5,
                                     opacity = 1.0,
                                     fill = TRUE,
                                     fillColor = ~redline_leaflet_pal(holc_grade), # 'black',
                                     fillOpacity = 1,
                                     popup = ~paste0('<b>', '<u>', paste0('HOLC Polygon Centroid'), '</u>', '</b>','<br/>',
                                            '<b>', 'City: ', '</b>', holc_city, '<br/>',
                                            # '<b>', 'HOLC Name: ', '</b>', holc_name, '<br/>',
                                            '<b>', 'HOLC Grade (A-D): ', '</b>', holc_grade, '<br/>',
                                            '<b>', 'HOLC ID: ', '</b>', holc_id, '<br/>'
                                            ),
                                     group = 'HOLC Centroids',
                                     label = ~glue('HOLC Centroid (Grade {holc_grade})')
                                     )
            }
            
            # CES Centroids
            if (input$analysis_aggregation_method == 'Nearest Centroid') {
                analysis_map_intersection <- analysis_map_intersection %>%
                    addCircleMarkers(data = analysis_ces_centroids[ces_analysis_data(), ] %>% 
                                         st_transform(crs = geographic_crs),
                                     options = pathOptions(pane = 'ces_centroids_pane'),
                                     radius = 3,
                                     stroke = TRUE,
                                     color = 'grey', # 'black', # 'darkgrey', # 'grey',
                                     weight = 0.5,
                                     opacity = 1.0,
                                     fill = TRUE,
                                     fillColor = 'grey', # 'black', # 'darkgrey', # 'grey',
                                     fillOpacity = 1,
                                     popup = ~paste0('<b>', '<u>', paste0('CES Polygon Centroid'), '</u>', '</b>','<br/>',
                                            '<b>', 'Census Tract: ', '</b>', census_tract, '<br/>',
                                            # SELECTED VARIABLE
                                                    '<b>', glue('{input$analysis_indicator_selection}: '), '</b>',
                                                    eval(as.symbol(ces_field_names %>% 
                                                                       filter(name == input$analysis_indicator_selection) %>%
                                                                       pull(ces_variable))),
                                                    '<br/>', # '&emsp;', 
                                                    '<b>', 
                                                    str_replace(string = input$analysis_indicator_selection,
                                                                                                      pattern = 'Score', 
                                                                                                      replacement = 'Percentile: '),
                                                    '</b>',
                                                    eval(as.symbol(ces_field_names %>%
                                                                           filter(name == str_replace(string = input$analysis_indicator_selection,
                                                                                                      pattern = 'Score', 
                                                                                                      replacement = 'Percentile')) %>%
                                                                           pull(ces_variable)))
                                            ),
                                     group = 'CES Centroids',
                                     label = ~paste0('CES Centroid (', 
                                                     input$analysis_indicator_selection, ': ',
                                                     eval(as.symbol(ces_field_names %>% 
                                                                       filter(name == input$analysis_indicator_selection) %>%
                                                                       pull(ces_variable))),
                                                     ')')
                                     )
            }
            
            # Connecting Lines
            if (input$analysis_aggregation_method == 'Nearest Centroid') {
                analysis_map_intersection <- analysis_map_intersection %>%
                    addPolylines(data = analysis_centroid_connecting_lines[ces_analysis_data(), ] %>% 
                                     st_transform(crs = geographic_crs),
                                 options = pathOptions(pane = 'connecting_lines_pane'),
                                 stroke = TRUE,
                                 color = 'grey', # 'black', # '#666666', # 'grey',
                                 weight = 2,
                                 opacity = 1.0,
                                 # fill = TRUE,
                                 # fillColor = 'black',
                                 # fillOpacity = 1,
                                 popup = ~paste0('<b>', paste0('HOLC-CES Centroid Match'), '</b>','<br/>'
                                 ),
                                 group = 'HOLC-CES Centroid Match',
                                 label = ~glue('HOLC-CES Centroid Match')
                    )
            }
            
        # add the legend
            analysis_map_intersection <- analysis_map_intersection %>% 
                addLegend(position = 'bottomright',
                          pal = ces_leaflet_pal,
                          values = ces_pal_domain, # ces3_poly$fill_variable,
                          opacity = 1,
                          layerId = 'ces_legend',
                          bins = 4,
                          group = 'Legend',
                          title = paste0('CalEnviroScreen'))
        
        # Add controls to select the basemap and layers
            analysis_map_intersection <- analysis_map_intersection %>% 
                addLayersControl(# baseGroups = basemap_options,
                                    overlayGroups = if (input$analysis_aggregation_method == 'Area Weighted Average') {
                                        c('HOLC Polygons', 'CalEnviroScreen', 'Legend')
                                    } else if (input$analysis_aggregation_method == 'Nearest Centroid') {
                                        c('HOLC Polygons', 'CalEnviroScreen', 'Legend', 'HOLC Centroids', 'CES Centroids', 'HOLC-CES Centroid Match')
                                    },
                                 options = layersControlOptions(collapsed = TRUE, autoZIndex = TRUE)) 
        
        # Hide some groups by default (can be turned on with the layers control box on the map)
            analysis_map_intersection <- analysis_map_intersection %>% 
                hideGroup(c('Legend')) #, 'HOLC Polygons'))
            
        # output the map object
            analysis_map_intersection
    })
    
    output$analysis_map_aggregated_scores <- renderLeaflet({
        # get variable values
            # var_name <- ces_choices %>%
            #     filter(name == input$analysis_indicator_selection) %>%
            #     pull(ces_variable)
            # measure_name <- input$analysis_indicator_selection
            # city <- input$analysis_city_selection

        # specify the initial zoom level to use in the map
            bounds_city <- attributes(st_geometry(redline_analysis_data() %>% 
                                                      st_transform(crs = geographic_crs)))$bbox # have to convert to geographic coordinate system for leaflet
            
        # create the new (empty) map
            analysis_map_aggregated_scores <- leaflet(options = leafletOptions(zoomControl = FALSE, 
                                                       dragging = FALSE))
            
            analysis_map_aggregated_scores <- analysis_map_aggregated_scores %>% 
                addMapPane("ces_polygons", zIndex = 410) %>% 
                addMapPane("redline_polygons_pane", zIndex = 420)
        
        # Basemap Options
            # use default options
                # analysis_map_aggregated_scores <- leaflet() %>%
                #     addTiles()
            # use custom options
                # basemap_options <- c('Esri.WorldTopoMap', 'CartoDB.Positron', 'Esri.WorldGrayCanvas','Esri.WorldImagery','Esri.WorldStreetMap')
                basemap_options <- c('CartoDB.Positron')
                for (provider in basemap_options) {
                    analysis_map_aggregated_scores <- analysis_map_aggregated_scores %>%
                        addProviderTiles(provider, group = provider)
                }
        
        # Set the bounds of the map dynamically - initial view is based on the full extent of the selected city, after that the map is based on the most recent bounds when a new option is selected
            isolate(if (is.null(input$analysis_map_aggregated_scores_bounds)) {
                analysis_map_aggregated_scores <- analysis_map_aggregated_scores %>%
                    fitBounds(lng1 = bounds_city[[1]],
                              lat1 = bounds_city[[2]],
                              lng2 = bounds_city[[3]],
                              lat2 = bounds_city[[4]])
            } else { # maintain the current view
                analysis_map_aggregated_scores <- analysis_map_aggregated_scores %>%
                    setView(lng = mean(c(input$analysis_map_aggregated_scores_bounds$west, input$analysis_map_aggregated_scores_bounds$east)),
                            lat = mean(c(input$analysis_map_aggregated_scores_bounds$north, input$analysis_map_aggregated_scores_bounds$south)),
                            zoom = input$analysis_map_aggregated_scores_zoom)
            })
            
        # # create a button to center the map on selected city
        #     analysis_map_aggregated_scores <- analysis_map_aggregated_scores %>% 
        #         addEasyButton(easyButton(
        #             icon="fa-globe", title=glue('Zoom to {input$analysis_city_selection}'),
        #             onClick=JS(paste0('function(btn, map){ map.fitBounds([[',
        #                               round(bounds_city[[2]],4)-0.01, ', ',
        #                               round(bounds_city[[1]],4)-0.01, '],[',
        #                               round(bounds_city[[4]],4)+0.01, ', ',
        #                               round(bounds_city[[3]],4)+0.01, ']]); }'))))
            
        
            
        # Add Area Weighted Average Polygons (Weighted Average of CES Scores)
            # create the color palette for the polygons
                parameter <- ces_choices %>%
                    filter(name == input$analysis_indicator_selection) %>%
                    pull(ces_variable)
                ces_pal_domain <- as.data.frame(ces3_poly() %>% st_drop_geometry()) %>% 
                    select(all_of(parameter)) %>% 
                    pull(all_of(parameter))
                # ces_fill_domain <- as.data.frame(analysis_clipped_ces %>% st_drop_geometry()) %>% 
                #     select(all_of(parameter)) %>% 
                #     pull(all_of(parameter))
                # create the palette
                    ces_pal_color <- 'RdYlGn' # 'YlOrBr' #'Blues' #'Greys'
                    ces_leaflet_pal <- colorNumeric(
                        palette = ces_pal_color,
                        domain = ces_pal_domain,
                        reverse = TRUE
                    )
                # get the dataset
                    ces3_poly <- if (input$analysis_aggregation_method == 'Area Weighted Average') {
                            analysis_raw_scores
                        } else if (input$analysis_aggregation_method == 'Nearest Centroid') {
                            analysis_centroid_scores
                        }
                    ces3_poly <- ces3_poly %>%
                        filter(holc_city == input$analysis_city_selection) %>% 
                        mutate(area_calc_meters_sq = st_area(.)) %>% 
                        drop_units() %>% 
                        st_transform(crs = geographic_crs) # %>% # have to convert to geographic coordinate system for leaflet,
                    # mutate(fill_variable = ces_fill_domain)
                # add polygons
                    analysis_map_aggregated_scores <- analysis_map_aggregated_scores %>% 
                        addPolygons(data = ces3_poly, # ces3_poly %>% filter(California_County == cities_counties[[input$city_selected_1]]), 
                                    options = pathOptions(pane = "ces_polygons"),
                                    color = 'grey', # 'grey', # "#444444", 
                                    weight = 0.5,
                                    smoothFactor = 1.0,
                                    opacity = 0.8, 
                                    fillOpacity = 0.8,
                                    # fillColor = ~colorNumeric('YlOrBr', Poll_pctl)(Poll_pctl), # view RColorBrewer palettes with: RColorBrewer::display.brewer.all()
                                    fillColor = ~ces_leaflet_pal(
                                        eval(as.symbol(
                                            ces_choices %>% 
                                                filter(name == input$analysis_indicator_selection) %>% 
                                                pull(ces_variable)
                                            ))), # ces_fill_domain),
                                    # fillColor = 'green',
                                    highlightOptions = highlightOptions(color = "white", weight = 2), # fill = TRUE, fillColor = "white"),#, bringToFront = TRUE
                                    popup = ~paste0('<b>', '<u>','HOLC Aggregated CalEnviroScreen (CES) Score (', 
                                                    input$analysis_aggregation_method, ')',
                                                    '</u>','</b>','<br/>',
                                                    # '<b>', 'Census Tract: ', '</b>', 
                                                    # census_tract, #eval(as.symbol(ces_field_names %>% filter(id == 'tract') %>% pull(ces_variable))),
                                                    # '<br/>',
                                                    # '&emsp;', 
                                                    # '<b>', 'Area (1000 sq m): ', '</b>',
                                                    # round(clipped_area / 1000, 0),
                                                    # '<br/>',
                                                    # '<b>', 'Location: ', '</b>', 
                                                    # nearby_city, # eval(as.symbol(ces_field_names %>% filter(id == 'City') %>% pull(ces_variable))),
                                                    # ', ', 
                                                    # california_county, # eval(as.symbol(ces_field_names %>% filter(id == 'California') %>% pull(ces_variable))),
                                                    # ' County, ', 
                                                    # zip, #eval(as.symbol(ces_field_names %>% filter(id == 'ZIP') %>% pull(ces_variable))),
                                                    # '<br/>',
                                                    # '<b>', 'Population (2010): ', '</b>', 
                                                    # population_2010, # eval(as.symbol(ces_field_names %>% filter(id == 'pop2010') %>% pull(ces_variable))),
                                                    # '<br/>',
                                                    
                                                    # HOLC POLYGON INFO
                                                    '<b>', 'City: ', '</b>', holc_city, '<br/>',
                                                    # '<b>', 'HOLC Name: ', '</b>', holc_name, '<br/>',
                                                    '<b>', 'HOLC Grade (A-D): ', '</b>', holc_grade, '<br/>',
                                                    '<b>', 'HOLC ID: ', '</b>', holc_id, '<br/>',
                                                    if(input$analysis_aggregation_method == 'Area Weighted Average') {
                                                        paste0(
                                                            '<b>', 'Area (1000 sq meters): ', '</b>', 
                                                            format(x = area_calc_meters_sq / 1000, digits = 0, big.mark = ',', scientific = FALSE), 
                                                            '<br/>')
                                                    },
                                                    
                                                    
                                                    # SELECTED VARIABLE
                                                    #' '<b>', #'<u>',
                                                    #' 'Selected CES Score (and Percentile): ',
                                                    #' #'</u>', 
                                                    #' '</b>', '<br/>', '&emsp;',
                                                    '<b>', glue('Aggregated {input$analysis_indicator_selection} ({input$analysis_aggregation_method}): '), '</b>',
                                                    round(eval(as.symbol(ces_field_names %>% 
                                                                       filter(name == input$analysis_indicator_selection) %>%
                                                                       pull(ces_variable))), 2)
                                    ),
                                    group = 'HOLC Aggregated Score',
                                    label = ~paste0('Aggregated ', input$analysis_indicator_selection, ': ', # ({input$analysis_aggregation_method}): '),
                                                    round(eval(as.symbol(ces_field_names %>%
                                                                             filter(name == input$analysis_indicator_selection) %>%
                                                                             pull(ces_variable))), 2)
                                                    )
                                    )

        # Add the HOLC (redline) polygon outlines
            # Create the color palette for the Redline scores
                redline_leaflet_pal <- colorFactor(palette = c('green', 'blue', 'gold1', 'red'), # 'YlOrBr'   # c("#FACD7B","#D1A149","#916714","#6B4703")
                                                   domain = redline_polygons$holc_grade, 
                                                   levels = c('A', 'B', 'C', 'D'))
            # add polygons
            analysis_map_aggregated_scores <- analysis_map_aggregated_scores %>% 
                # addPolygons(data = redline_analysis_data() %>% 
                addPolylines(data = redline_analysis_data() %>% 
                                st_transform(crs = geographic_crs) %>% # have to convert to geographic coordinate system for leaflet
                                # filter(holc_grade %in% input$holc_rating_sites_filter) %>% 
                                {.},
                            options = pathOptions(pane = "redline_polygons_pane"),
                            color = ~redline_leaflet_pal(holc_grade), # 'black', # "#444444",
                            weight = 2.0,
                            smoothFactor = 1.0,
                            opacity = 1.0,
                            # fill = FALSE,
                            fill = FALSE,
                            # fillOpacity = 0, # input$redline_fill_1,
                            # fillColor = 'lightgrey',
                            # fillColor = ~redline_leaflet_pal(holc_grade),
                            highlightOptions = highlightOptions(color = "white", weight = 2),#,bringToFront = TRUE
                            popup = ~paste0('<b>', '<u>', paste0('HOLC Assessed Area (', holc_year, ')'), '</u>', '</b>','<br/>',
                                            '<b>', 'City: ', '</b>', holc_city, '<br/>',
                                            '<b>', 'HOLC Name: ', '</b>', holc_name, '<br/>',
                                            '<b>', 'HOLC Grade (A-D): ', '</b>', holc_grade, '<br/>',
                                            '<b>', 'HOLC ID: ', '</b>', holc_id, '<br/>',
                                            if(input$analysis_aggregation_method == 'Area Weighted Average') {
                                                paste0(
                                                    '<b>', 'Area (1000 sq meters): ', '</b>', 
                                                    format(x = area_calc_meters_sq / 1000, digits = 0, big.mark = ',', scientific = FALSE), 
                                                    '<br/>')
                                            },
                                            # '<b>', paste0('HOLC Form Link (', year, '): '), '</b>', link, '</b>', 
                                            '<b>', 'HOLC Form Link: ', '</b>', 
                                            paste0('<a href = "', holc_link, '" ', 'target="_blank"> ', holc_link, ' </a>')# , '<br/>',
                                            # '<b>', 'Area Description Excerpts: ', '</b>', area_description_excerpts
                            ),
                            group = 'HOLC Polygons',
                            label = ~glue('HOLC Polygon (Grade {holc_grade})')
                )
            
        # add the legend
            analysis_map_aggregated_scores <- analysis_map_aggregated_scores %>% 
                addLegend(position = 'bottomright',
                          pal = ces_leaflet_pal,
                          values = ces_pal_domain, # ces3_poly$fill_variable,
                          opacity = 1,
                          layerId = 'ces_legend',
                          bins = 4,
                          group = 'Legend',
                          title = paste0('CalEnviroScreen'))
        
        # Add controls to select the basemap and layers
            analysis_map_aggregated_scores <- analysis_map_aggregated_scores %>% 
                addLayersControl(# baseGroups = basemap_options,
                                 overlayGroups = c('HOLC Polygons', 'HOLC Aggregated Score', 'Legend'),
                                 options = layersControlOptions(collapsed = TRUE, autoZIndex = TRUE)) 
            
        # Hide some groups by default (can be turned on with the layers control box on the map)
            analysis_map_aggregated_scores <- analysis_map_aggregated_scores %>% 
                hideGroup(c('Legend')) #, 'HOLC Polygons'))
        
        # output the map object
            analysis_map_aggregated_scores
    })
    
    # Observer to respond to zoom / pan of left side map and apply to other 3 maps
    # from: https://github.com/rstudio/leaflet/issues/347
        observe({
            coords_analysis <- input$analysis_map_holc_bounds
            if (!is.null(coords_analysis)) {
                proxy_analysis_1 <- leafletProxy('analysis_map_overlap') %>%
                    fitBounds(coords_analysis$west,
                              coords_analysis$south,
                              coords_analysis$east,
                              coords_analysis$north)

                proxy_analysis_2 <- leafletProxy('analysis_map_intersection') %>%
                    fitBounds(coords_analysis$west,
                              coords_analysis$south,
                              coords_analysis$east,
                              coords_analysis$north)

                proxy_analysis_3 <- leafletProxy('analysis_map_aggregated_scores') %>%
                    fitBounds(coords_analysis$west,
                              coords_analysis$south,
                              coords_analysis$east,
                              coords_analysis$north)
            }
        })
    
    
    # output$analysis_map_facet <- renderLeaflet({    
    analysis_map_facet <- function(map_grade, allow_zoom_pan, show_center_button) {
        # map_facet <- renderLeaflet({
            
        # get variable values
            # var_name <- ces_choices %>%
            #     filter(name == input$analysis_indicator_selection) %>%
            #     pull(ces_variable)
            # measure_name <- input$analysis_indicator_selection
            # city <- input$analysis_city_selection

        # specify the initial zoom level to use in the map
            bounds_city <- attributes(st_geometry(redline_analysis_data() %>% 
                                                      st_transform(crs = geographic_crs)))$bbox # have to convert to geographic coordinate system for leaflet
            
        # create the new (empty) map
            map_facet <- leaflet(options = leafletOptions(zoomControl = allow_zoom_pan, 
                                                          dragging = allow_zoom_pan))
            
            map_facet <- map_facet %>% 
                addMapPane("ces_polygons", zIndex = 410) %>% 
                addMapPane("redline_polygons_pane", zIndex = 420)
        
        # Basemap Options
            # use default options
                # map_facet <- leaflet() %>%
                #     addTiles()
            # use custom options
                # basemap_options <- c('Esri.WorldTopoMap', 'CartoDB.Positron', 'Esri.WorldGrayCanvas','Esri.WorldImagery','Esri.WorldStreetMap')
                basemap_options <- c('CartoDB.Positron')
                for (provider in basemap_options) {
                    map_facet <- map_facet %>%
                        addProviderTiles(provider, group = provider)
                }
        
        # Set the bounds of the map dynamically - initial view is based on the full extent of the selected city, after that the map is based on the most recent bounds when a new option is selected
            isolate(if (is.null(input$map_facet_bounds)) {
                map_facet <- map_facet %>%
                    fitBounds(lng1 = bounds_city[[1]],
                              lat1 = bounds_city[[2]],
                              lng2 = bounds_city[[3]],
                              lat2 = bounds_city[[4]])
            } # else { # maintain the current view
            #     map_facet <- map_facet %>%
            #         setView(lng = mean(c(input$map_facet_bounds$west, input$map_facet_bounds$east)),
            #                 lat = mean(c(input$map_facet_bounds$north, input$map_facet_bounds$south)),
            #                 zoom = input$map_facet_zoom)
            # }
            )
            
        # create a button to center the map on selected city
            if (show_center_button == TRUE) {
                map_facet <- map_facet %>%
                    addEasyButton(easyButton(
                        icon="fa-globe", title=glue('Zoom to {input$analysis_city_selection}'),
                        onClick=JS(paste0('function(btn, map){ map.fitBounds([[',
                                          round(bounds_city[[2]],4)-0.01, ', ',
                                          round(bounds_city[[1]],4)-0.01, '],[',
                                          round(bounds_city[[4]],4)+0.01, ', ',
                                          round(bounds_city[[3]],4)+0.01, ']]); }'))))
            }
            
        # Create the color palette for the Redline scores
            redline_leaflet_pal <- colorFactor(palette = c('green', 'blue', 'gold1', 'red'), # 'YlOrBr'   # c("#FACD7B","#D1A149","#916714","#6B4703")
                                               domain = redline_polygons$holc_grade, 
                                               levels = c('A', 'B', 'C', 'D'))
            
        # Add Area Weighted Average Polygons (Weighted Average of CES Scores)
            # create the color palette for the polygons
                parameter <- ces_choices %>%
                    filter(name == input$analysis_indicator_selection) %>%
                    pull(ces_variable)
                ces_pal_domain <- as.data.frame(ces3_poly() %>% st_drop_geometry()) %>% 
                    select(all_of(parameter)) %>% 
                    pull(all_of(parameter))
                # ces_fill_domain <- as.data.frame(analysis_clipped_ces %>% st_drop_geometry()) %>% 
                #     select(all_of(parameter)) %>% 
                #     pull(all_of(parameter))
                # create the palette
                    ces_pal_color <- 'RdYlGn' # 'YlOrBr' #'Blues' #'Greys'
                    ces_leaflet_pal <- colorNumeric(
                        palette = ces_pal_color,
                        domain = ces_pal_domain,
                        reverse = TRUE
                    )
                # get the dataset
                    ces3_poly <- if (input$analysis_aggregation_method == 'Area Weighted Average') {
                            analysis_raw_scores
                        } else if (input$analysis_aggregation_method == 'Nearest Centroid') {
                            analysis_centroid_scores
                        }
                    ces3_poly <- ces3_poly %>% 
                        filter(holc_grade == map_grade) %>% 
                        filter(holc_city == input$analysis_city_selection) %>% 
                        mutate(area_calc_meters_sq = st_area(.)) %>% 
                        drop_units() %>% 
                        st_transform(crs = geographic_crs) # %>% # have to convert to geographic coordinate system for leaflet,
                        # mutate(fill_variable = ces_fill_domain)
                # add polygons
                    map_facet <- map_facet %>% 
                        addPolygons(data = ces3_poly, # ces3_poly %>% filter(California_County == cities_counties[[input$city_selected_1]]), 
                                    options = pathOptions(pane = "ces_polygons"),
                                    color = ~redline_leaflet_pal(holc_grade), # 'black', # "#444444",
                                    weight = 1,
                                    smoothFactor = 1.0,
                                    opacity = 1.0,
                                    fillOpacity = 0.9,
                                    # fillColor = ~colorNumeric('YlOrBr', Poll_pctl)(Poll_pctl), # view RColorBrewer palettes with: RColorBrewer::display.brewer.all()
                                    fillColor = ~ces_leaflet_pal(
                                        eval(as.symbol(
                                            ces_choices %>% 
                                                filter(name == input$analysis_indicator_selection) %>% 
                                                pull(ces_variable)
                                            ))), # ces_fill_domain),
                                    # fillColor = 'green',
                                    highlightOptions = highlightOptions(color = "white", weight = 2), # fill = TRUE, fillColor = "white"),#, bringToFront = TRUE
                                    popup = ~paste0('<b>', '<u>',
                                                    'HOLC Aggregated CalEnviroScreen (CES) Score (', 
                                                    input$analysis_aggregation_method, ')',
                                                    '</u>','</b>','<br/>',
                                                    # '<b>', 'Census Tract: ', '</b>', 
                                                    # census_tract, #eval(as.symbol(ces_field_names %>% filter(id == 'tract') %>% pull(ces_variable))),
                                                    # '<br/>',
                                                    # '&emsp;', 
                                                    # '<b>', 'Area (1000 sq m): ', '</b>',
                                                    # round(clipped_area / 1000, 0),
                                                    # '<br/>',
                                                    # '<b>', 'Location: ', '</b>', 
                                                    # nearby_city, # eval(as.symbol(ces_field_names %>% filter(id == 'City') %>% pull(ces_variable))),
                                                    # ', ', 
                                                    # california_county, # eval(as.symbol(ces_field_names %>% filter(id == 'California') %>% pull(ces_variable))),
                                                    # ' County, ', 
                                                    # zip, #eval(as.symbol(ces_field_names %>% filter(id == 'ZIP') %>% pull(ces_variable))),
                                                    # '<br/>',
                                                    # '<b>', 'Population (2010): ', '</b>', 
                                                    # population_2010, # eval(as.symbol(ces_field_names %>% filter(id == 'pop2010') %>% pull(ces_variable))),
                                                    # '<br/>',
                                                    
                                                    # HOLC POLYGON INFO
                                                    '<b>', 'City: ', '</b>', holc_city, '<br/>',
                                                    # '<b>', 'HOLC Name: ', '</b>', holc_name, '<br/>',
                                                    '<b>', 'HOLC Grade (A-D): ', '</b>', holc_grade, '<br/>',
                                                    '<b>', 'HOLC ID: ', '</b>', holc_id, '<br/>',
                                                    if(input$analysis_aggregation_method == 'Area Weighted Average') {
                                                        paste0(
                                                            '<b>', 'Area (1000 sq meters): ', '</b>', 
                                                            format(x = area_calc_meters_sq / 1000, digits = 0, big.mark = ',', scientific = FALSE), 
                                                            '<br/>')
                                                    },
                                                    # SELECTED VARIABLE
                                                    #' '<b>', #'<u>',
                                                    #' 'Selected CES Score (and Percentile): ',
                                                    #' #'</u>', 
                                                    #' '</b>', '<br/>', '&emsp;',
                                                    '<b>', glue('Aggregated {input$analysis_indicator_selection} ({input$analysis_aggregation_method}): '), 
                                                    '</b>',
                                                    round(eval(as.symbol(ces_field_names %>% 
                                                                       filter(name == input$analysis_indicator_selection) %>%
                                                                       pull(ces_variable))), 2)
                                    ),
                                    group = 'HOLC Aggregated Score',
                                    label = ~paste0('Aggregated ', input$analysis_indicator_selection, ': ', # ({input$analysis_aggregation_method}): '),
                                                    round(eval(as.symbol(ces_field_names %>%
                                                                             filter(name == input$analysis_indicator_selection) %>%
                                                                             pull(ces_variable))), 2)
                                    )# ,
                                    # labelOptions = labelOptions(noHide = if(input$facet_map_labels == TRUE) {TRUE} else {FALSE},
                                    #                             textOnly = TRUE)
                        )
                    
        # add HOLC polygons
            map_facet <- map_facet %>% 
                # addPolygons(data = redline_analysis_data() %>% 
                addPolylines(data = redline_analysis_data() %>% 
                                 filter(holc_grade == map_grade) %>% 
                                st_transform(crs = geographic_crs) %>% # have to convert to geographic coordinate system for leaflet
                                # filter(holc_grade %in% input$holc_rating_sites_filter) %>% 
                                {.},
                            options = pathOptions(pane = "redline_polygons_pane"),
                            color = ~redline_leaflet_pal(holc_grade), # 'black', # "#444444",
                            weight = 2.0,
                            smoothFactor = 1.0,
                            opacity = 1.0,
                            # fill = FALSE,
                            fill = FALSE,
                            # fillOpacity = 0, # input$redline_fill_1,
                            # fillColor = 'lightgrey',
                            # fillColor = ~redline_leaflet_pal(holc_grade),
                            highlightOptions = highlightOptions(color = "white", weight = 2),#,bringToFront = TRUE
                            popup = ~paste0('<b>', '<u>', paste0('HOLC Assessed Area (', holc_year, ')'), '</u>', '</b>','<br/>',
                                            '<b>', 'City: ', '</b>', holc_city, '<br/>',
                                            '<b>', 'HOLC Name: ', '</b>', holc_name, '<br/>',
                                            '<b>', 'HOLC Grade (A-D): ', '</b>', holc_grade, '<br/>',
                                            '<b>', 'HOLC ID: ', '</b>', holc_id, '<br/>',
                                            if(input$analysis_aggregation_method == 'Area Weighted Average') {
                                                paste0(
                                                    '<b>', 'Area (1000 sq meters): ', '</b>', 
                                                    format(x = area_calc_meters_sq / 1000, digits = 0, big.mark = ',', scientific = FALSE), 
                                                    '<br/>')
                                            },
                                            # '<b>', paste0('HOLC Form Link (', year, '): '), '</b>', link, '</b>', 
                                            '<b>', 'HOLC Form Link: ', '</b>', 
                                            paste0('<a href = "', holc_link, '" ', 'target="_blank"> ', holc_link, ' </a>')# , '<br/>',
                                            # '<b>', 'Area Description Excerpts: ', '</b>', area_description_excerpts
                            ),
                            group = 'HOLC Polygons',
                            label = ~glue('HOLC Polygon (Grade {holc_grade})')
                )
            
        # add the legend
            map_facet <- map_facet %>% 
                addLegend(position = 'bottomright',
                          pal = ces_leaflet_pal,
                          values = ces_pal_domain, # ces3_poly$fill_variable,
                          opacity = 1,
                          layerId = 'ces_legend',
                          bins = 4,
                          group = 'Legend',
                          title = paste0('CalEnviroScreen'))
        
        # Add controls to select the basemap and layers
            map_facet <- map_facet %>% 
                addLayersControl(# baseGroups = basemap_options,
                                 overlayGroups = c('HOLC Polygons', 'HOLC Aggregated Score', 'Legend'),
                                 options = layersControlOptions(collapsed = TRUE, autoZIndex = TRUE)) 
            
        # Hide some groups by default (can be turned on with the layers control box on the map)
            map_facet <- map_facet %>% 
                hideGroup(c('Legend')) #, 'HOLC Polygons'))
        
        # output the map object
            return(map_facet)
            
    }

    output$analysis_map_facet_A <- renderLeaflet({analysis_map_facet(map_grade = 'A', allow_zoom_pan = TRUE, show_center_button = TRUE)})
    output$analysis_map_facet_B <- renderLeaflet({analysis_map_facet(map_grade = 'B', allow_zoom_pan = FALSE, show_center_button = FALSE)})
    output$analysis_map_facet_C <- renderLeaflet({analysis_map_facet(map_grade = 'C', allow_zoom_pan = FALSE, show_center_button = FALSE)})
    output$analysis_map_facet_D <- renderLeaflet({analysis_map_facet(map_grade = 'D', allow_zoom_pan = FALSE, show_center_button = FALSE)})

    # proxy to add / remove labels
        observe({
                # get the dataset
                    ces3_poly <- if (input$analysis_aggregation_method == 'Area Weighted Average') {
                            analysis_raw_scores
                        } else if (input$analysis_aggregation_method == 'Nearest Centroid') {
                            analysis_centroid_scores
                        }
                    ces3_poly <- ces3_poly %>% 
                        # filter(holc_grade == map_grade) %>% 
                        filter(holc_city == input$analysis_city_selection) %>% 
                        # mutate(area_calc_meters_sq = st_area(.)) %>% 
                        # drop_units() %>% 
                        st_transform(crs = geographic_crs) # %>% # have to convert to geographic coordinate system for leaflet,
                        # mutate(fill_variable = ces_fill_domain)
                    
            leafletProxy('analysis_map_facet_A') %>%
                clearMarkers() %>% 
                addLabelOnlyMarkers(data = ces3_poly %>% 
                                        filter(holc_grade == 'A') %>% 
                                        st_set_geometry(.$geom_centroid$geom) %>% 
                                        st_transform(crs = geographic_crs), 
                                    label = ~round(eval(as.symbol(ces_field_names %>%
                                                                      filter(name == input$analysis_indicator_selection) %>%
                                                                      pull(ces_variable))), 2),
                                    labelOptions = labelOptions(noHide = if(input$facet_map_labels == TRUE) {TRUE} else {FALSE},
                                                                textOnly = TRUE))
            leafletProxy('analysis_map_facet_B') %>%
                clearMarkers() %>% 
                addLabelOnlyMarkers(data = ces3_poly %>% 
                                        filter(holc_grade == 'B') %>% 
                                        st_set_geometry(.$geom_centroid$geom) %>% 
                                        st_transform(crs = geographic_crs), 
                                    label = ~round(eval(as.symbol(ces_field_names %>%
                                                                      filter(name == input$analysis_indicator_selection) %>%
                                                                      pull(ces_variable))), 2),
                                    labelOptions = labelOptions(noHide = if(input$facet_map_labels == TRUE) {TRUE} else {FALSE},
                                                                textOnly = TRUE))
            
            leafletProxy('analysis_map_facet_C') %>%
                clearMarkers() %>% 
                addLabelOnlyMarkers(data = ces3_poly %>% 
                                        filter(holc_grade == 'C') %>% 
                                        st_set_geometry(.$geom_centroid$geom) %>% 
                                        st_transform(crs = geographic_crs), 
                                    label = ~round(eval(as.symbol(ces_field_names %>%
                                                                      filter(name == input$analysis_indicator_selection) %>%
                                                                      pull(ces_variable))), 2),
                                    labelOptions = labelOptions(noHide = if(input$facet_map_labels == TRUE) {TRUE} else {FALSE},
                                                                textOnly = TRUE))
            
            leafletProxy('analysis_map_facet_D') %>%
                clearMarkers() %>% 
                addLabelOnlyMarkers(data = ces3_poly %>% 
                                        filter(holc_grade == 'D') %>% 
                                        st_set_geometry(.$geom_centroid$geom) %>% 
                                        st_transform(crs = geographic_crs), 
                                    label = ~round(eval(as.symbol(ces_field_names %>%
                                                                      filter(name == input$analysis_indicator_selection) %>%
                                                                      pull(ces_variable))), 2),
                                    labelOptions = labelOptions(noHide = if(input$facet_map_labels == TRUE) {TRUE} else {FALSE},
                                                                textOnly = TRUE))
        })
    

# Observer to respond to zoom / pan of upper left side map and apply to other 3 maps
    # from: https://github.com/rstudio/leaflet/issues/347
        observe({
            coords_analysis_facet <- input$analysis_map_facet_A_bounds
            if (!is.null(coords_analysis_facet)) {
                proxy_analysis_facet_1 <- leafletProxy('analysis_map_facet_B') %>%
                    fitBounds(coords_analysis_facet$west,
                              coords_analysis_facet$south,
                              coords_analysis_facet$east,
                              coords_analysis_facet$north)

                proxy_analysis_facet_2 <- leafletProxy('analysis_map_facet_C') %>%
                    fitBounds(coords_analysis_facet$west,
                              coords_analysis_facet$south,
                              coords_analysis_facet$east,
                              coords_analysis_facet$north)

                proxy_analysis_facet_3 <- leafletProxy('analysis_map_facet_D') %>%
                    fitBounds(coords_analysis_facet$west,
                              coords_analysis_facet$south,
                              coords_analysis_facet$east,
                              coords_analysis_facet$north)
            }
        })

}

shinyApp(ui, server)
