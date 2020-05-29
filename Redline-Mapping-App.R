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
            projected_crs <- 3310 # other options: 26910 -- see: https://epsg.io/3310 and https://epsg.io/26910
            geographic_crs <- 4326 # see: https://epsg.io/4326
    
    # initial selected city
        initial_selected_city <- 'Sacramento' 
    
# Load Packages ----
    # Shiny platform
        library(shiny) # the Shiny web application framework (https://shiny.rstudio.com) (https://cran.r-project.org/package=shiny)
    # General data analysis and transformation
        library(readr)
        library(readxl)
        library(dplyr)
        library(janitor)
        # library(gtools)
        library(stringr)
        library(lubridate)
        library(vroom)
        library(janitor)
        library(data.table)
        library(tidyselect)
        library(purrr)
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
    # workflow
        library(here)
    # shiny stuff
        library(shinycssloaders)
        library(DT)
        library(shinyWidgets)
        library(shinyjs)


# Read some static data into R ------------------------------------------------------------------------------------------------------------------
    # HOLC (Redline) Polygons
        # redline_polygons <- read_rds('data_prepared/redline_polygons.RDS')
        # st_crs(redline_polygons) <- 4326
        redline_polygons <- st_read('data_prepared/redline_polygons.gpkg')
        # transform to projected crs (for mapping and analysis it's best to use a projected CRS -- see: https://s3.amazonaws.com/files.zevross.com/workshops/spatial/slides/html/4-crs.html#31)
            redline_polygons <- redline_polygons %>% st_transform(crs = projected_crs)
            # st_crs(redline_polygons)
            
    # List of the CES parameter choices to plot
        ces_choices <- read_csv('data_prepared/ces_names.csv') %>% # manually prepared this file to make more descriptive names for the fields
            mutate(ces_variable = make_clean_names(name, 'parsed')) %>% 
            slice(8:64) # rows 8 to 64
        # ces_choices <- read_rds('data_prepared/ces_3_names.RDS') %>% 
        #     # filter(grepl(pattern = 'Percentile$', x = .$name)) %>% 
        #     slice(8:64) # rows 8 to 64
        
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
            # slice(1:50000) %>% 
            {.}
    # define choices for sources of CalEPA regulated site data
        site_source_choices <- c('None',
                                 'All Sites (Source: CalEPA Regulated Site Portal)', 
                                 'Select By Type (Source: CalEPA Geoserver)')
        
    # List of program types to select
        program_types <- fread('data_regulatory_actions/SiteEI.csv') %>%
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
        # inspections
            inspections_all_download <- fread('data_regulatory_actions/Evaluations.csv') %>%
                # inspections_all_download <- read_csv('data_regulatory_actions/Evaluations.zip') %>% 
                # inspections_all_download <- read_csv('data_regulatory_actions/Evaluations.csv') %>%
                # inspections_all_download <- vroom('data_regulatory_actions/Evaluations.csv') %>% 
                tibble() %>% 
                clean_names() %>%
                select(-dplyr::ends_with(as.character(0:9))) %>%
                mutate(eval_date = mdy(eval_date)) %>% 
                arrange(site_id, eval_date)
        # violations
            violations_all_download <- fread('data_regulatory_actions/Violations.csv') %>% #, guess_max = 1000, trim_ws = FALSE) %>%
                # violations_all_download <- read_csv('data_regulatory_actions/Violations.zip') %>% #, guess_max = 1000, trim_ws = FALSE) %>% 
                # violations_all_download <- read_csv('data_regulatory_actions/Violations.csv') %>% #, guess_max = 1000, trim_ws = FALSE) %>%
                # violations_all_download <- vroom('data_regulatory_actions/Violations.csv') %>% #, guess_max = 1000, trim_ws = FALSE) %>% 
                tibble() %>%
                clean_names() %>% 
                select(-dplyr::ends_with(as.character(0:9))) %>%
                mutate(violation_date = mdy(violation_date)) %>% 
                arrange(site_id, violation_date)        
        # enforcement actions
            enforcement_all_download <- fread('data_regulatory_actions/EA.csv') %>%
                # enforcement_all_download <- read_csv('data_regulatory_actions/Enforcements.zip') %>% 
                # enforcement_all_download <- read_csv('data_regulatory_actions/EA.csv') %>%
                # enforcement_all_download <- vroom('data_regulatory_actions/EA.csv') %>% 
                tibble() %>%
                clean_names() %>% 
                select(-dplyr::ends_with(as.character(0:9))) %>%
                mutate(enf_action_date = mdy(enf_action_date)) %>% 
                arrange(site_id, enf_action_date)
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

# ----------------------------------------------------------------------------------------------------------------------------------------------- #       
# ----------------------------------------------------------------------------------------------------------------------------------------------- #
# Define UI --------------------------------------------------------------------
ui <- navbarPage(title = "California's Redlined Communities", # theme = shinythemes::shinytheme('flatly'),
    # Maps / Tabular Data Tab ----
        tabPanel('Maps',
                 useShinyjs(),
            tags$head(tags$style(".buttonstyle{background-color:#f2f2f2;} .buttonstyle{color: black;}")), # define button style (background color and font color)
            # Sidebar layout with input and output definitions
            sidebarLayout(
                # Sidebar
                sidebarPanel(
                    # Inputs: 
                    selectInput(inputId = 'city_selected_1', 
                                label = 'Zoom To City:', 
                                choices = c(unique(redline_polygons$city)), 
                                selected = initial_selected_city), # 'All'
                    hr(style="border: 1px solid darkgrey"),
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
                    hr(style="border: 1px solid darkgrey"),
                    h4('Redlining Data:'),
                    tags$b('Filter For Sites Within HOLC Rated Polygons:'),
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
                    hr(style="border: 1px solid darkgrey"),
                    h4('Environmental, Public Health, & Socieconomic Data:'),
                    selectInput(inputId = 'ces_parameter', 
                        label = 'Select CalEnviroScreen (CES) Parameter:', 
                        choices = ces_choices$name, 
                        selected = ces_choices$name[1]),
                    # sliderInput(inputId = 'ces_score_range', 
                    #             label = 'Filter Sites By Score of Selected CES Parameter:', 
                    #             min = 0, max = 100, value = c(0,100)),
                    uiOutput('ces_range_filter'),
                    hr(style="border: 1px solid darkgrey"),
                    h4('CalEPA Regulatory Actions:'),
                    dateRangeInput2(inputId = "sites_date_range", 
                                    label = "Select Date Range For Inspections, Violations, & Enforcement Actions:*", 
                                    startview = "year", 
                                    minview = "months", 
                                    maxview = "decades", 
                                    format = 'yyyy-mm', 
                                    start = NULL, # as.Date("2010-01-01"),
                                    end = NULL, #as.Date(paste(year(Sys.Date()), month(Sys.Date()), 1, sep = '-')),
                                    min = as.Date("1900-01-01"),
                                    max = as.Date(paste(year(most_recent_reg_records), month(most_recent_reg_records), 1, sep = '-'))),
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
                    tags$em(p('**this filter resets whenever the other filters above are changed'))
                    # selectInput(inputId = 'program_type_1',
                    #             label = 'Filter Sites By Program Type:',
                    #             multiple = TRUE,
                    #             choices = program_types_distinct)
                    # hr(style="border: 1px solid darkgrey"),
                    # p(tags$b('NOTE:'), 'Use the left side map to pan/zoom, and use the button in the upper left corner of each map to toggle layers on or off.')
                ),
                # Main panel for displaying outputs 
                mainPanel(
                  fluidRow(
                      p(tags$b('NOTE:'), 'Use the left side map to pan/zoom, and use the button in the upper left corner of each map to toggle layers on or off.'),
                      p('Tabular data for the selected sites is available to view/download below the maps.'),
                      column(6, 
                             #tags$h4('Environmental / Public Health Indicators & Regulated Facilities:'),
                             tags$h4('CalEPA Data:'),
                             leafletOutput(outputId = 'map1', height = 800) %>% withSpinner(color="#0dc5c1")),
                      column(6, 
                             tags$h4('HOLC Rated (Redlined) Areas:'),
                             leafletOutput(outputId = 'map2', height = 800)%>% withSpinner(color="#0dc5c1"))
                      ),
                  fluidRow(
                      hr(style="border: 3px solid darkgrey"),
                      h4('Tabular Data For Selected Sites & Regulatory Actions:')
                      ),
                  fluidRow(
                      div(style="display:inline-block;vertical-align:top;",
                          p('Download all supporting regulatory data: ', style = 'display:inline'), HTML('&emsp;'),
                          downloadButton('downloadInspections', 'Inspection Data', class = "buttonstyle", style = 'display:inline'), HTML('&emsp;'),
                          downloadButton('downloadViolations', 'Violation Data', class = "buttonstyle", style = 'display:inline'), HTML('&emsp;'),
                          downloadButton('downloadEnforcements', 'Enforcement Data', class = "buttonstyle", style = 'display:inline'), HTML('&emsp;')
                  ),
                  fluidRow(
                      hr(),
                      downloadButton('download_summary', 'Download Data', class = "buttonstyle"),
                      DT::dataTableOutput('summary_table')
                  )
                  )
                )
            )
        ),
    # Analysis Tab ----
    tabPanel('Redline-CES Analysis',
             p('Working on it...')
    ),
    # Background Info Tab ----
        tabPanel('Background Info',
                 p('This draft tool displays California\'s Redlined communites, and helps to assess potential correlations between those policies and indicators of enviornmental and public health (e.g., 303d impaired water bodies, CalEnviroScreen scores), as well as facilities regulated by the CalEPA. More layers will be added in the future.'),
                 # Redlining History
                     h3('Redlining History'),
                     p('For more information on the history of Redline mapping and some previous studies of its effects, see:'),
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
server <- function(input, output) {
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
               condition = input$sites_source == site_source_choices[3])
      })

    # get reactive values (to isolate from each other and prevent map from completely rebuilding when an input is changed) 
        # regional board boundary containing the selected city ----
            rb_boundary <- reactive({
                withProgress(message = 'Downloading Data...', style = 'notification', value = 1, {
                    if (data_source_rb_bounds == 'local') {
                        rb_boundary_download <- st_read('data_prepared/rb_boundary_simplified.gpkg')
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
                #     rb_boundary_download <- read_rds('data_prepared/Regional_Board_Offices.RDS') %>% 
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
                    ces3_poly_download <- st_read('data_prepared/ces3_poly_simplified.gpkg')
                } else if (data_source_ces3 == 'remote') {
                # # get data
                #     ces3_poly_download <- read_rds('data_prepared/ces3_poly_download.RDS')
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
                        st_crs(ces3_poly_download) <- 3310 # the crs is incorrectly defined in the data returned by the api - have to reset it
                        ces3_poly_download <- ces3_poly_download %>% 
                            st_transform(crs = geographic_crs) # transform back to lat/lon coordinates
                    # check
                        # plot(ces3_poly_download$geometry, border = 'blue', col = 'grey')
                        # plot(rb_boundary$geometry, lwd = 3, add = TRUE)
                        # plot(redline_polygons$geometry, lwd = 3, add = TRUE, col = 'red', border = 'red')
                        # # plot(rb_boundary_simplify() %>% select(geometry), border = 'red', add = TRUE)
                    # revise column names
                        # ces3_poly_download <- ces3_poly_download %>% select(-CES2018_Rn)
                        col_names_original <- names(ces3_poly_download)
                        col_names_new <- read_csv('data_prepared/ces_names.csv')
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
                    # impaired_303d_poly_download <- read_rds('data_prepared/impaired_303d_poly_download.RDS')
                    impaired_303d_poly_download <- st_read('data_prepared/303d_polygons_simplified.gpkg')
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
                    # impaired_303d_lines_download <- read_rds('data_prepared/impaired_303d_lines_download_simplify_R1removed.RDS')
                    impaired_303d_lines_download <- st_read('data_prepared/303d_lines_R1removed_simplified.gpkg')
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
                        service_areas_download <- st_read('data_prepared/drinking_water_service_areas_simplified.gpkg')
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
                if (input$sites_source == site_source_choices[2]) {
                    # Create an sf object using the sites data from the saved flat file
                    cal_epa_sites_raw_download <- st_as_sf(calepa_reg_sites %>% filter(!is.na(latitude) & !is.na(longitude)),
                                                           coords = c('longitude', 'latitude'),
                                                           crs = 4326,
                                                           # agr = 'constant',
                                                           remove = FALSE)
                    # transform to projected crs (for mapping and analysis it's best to use a projected CRS -- see: https://s3.amazonaws.com/files.zevross.com/workshops/spatial/slides/html/4-crs.html#31)
                        cal_epa_sites_raw_download <- cal_epa_sites_raw_download %>% st_transform(crs = projected_crs)
                        # st_crs(cal_epa_sites_raw_download)
                    # return the object
                        return(cal_epa_sites_raw_download)
                } else if (input$sites_source == site_source_choices[3] & 
                           length(input$site_type_1) > 0) {    
                    withProgress(message = 'Getting Site Data', style = 'notification', value = 1, { # style = 'notification' 'old'
                    # })
                    # get CalEPA sites data from the geoserver api
                        # # get the coordinates of the simplified rb boundary containing the selected city and convert to geojson
                        #     rb_boundary_simplify_geojson <- rb_boundary_simplify() %>% sf_geojson()
                        # # extract coordinates
                        #     rb_boundary_coordinates <- rb_boundary_simplify_geojson %>%
                        #         str_sub(start = str_locate(string = ., pattern = 'coordinates\":') %>% as.data.frame() %>% pull(end) + 4,
                        #                 end = nchar(rb_boundary_simplify_geojson)-7)
                        # # format the coordinates for the api
                        #     rb_boundary_coordinates <- str_replace_all(string = rb_boundary_coordinates, pattern = '\\],\\[', replacement = '|')
                        #     rb_boundary_coordinates <- str_replace_all(string = rb_boundary_coordinates, pattern = ',', replacement = '%20')
                        #     rb_boundary_coordinates <- str_replace_all(string = rb_boundary_coordinates, pattern = '\\|', replacement = ',%20')
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
                            for (site_type in input$site_type_1) {
                                counter_sites <- counter_sites + 1
                                # url <- calepa_sites_typenames[[site_type]]
                                url_sites_api <- paste0('https://services.calepa.ca.gov/geoserver/calepa/',
                                                           'ows?service=WFS&version=1.0.0',
                                                           '&request=GetFeature',
                                                           '&typeName=', calepa_sites_typenames[[site_type]], # 'calepa:mv_fac_from_ciwqs', # calepa:mv_fac_from_geotracker # calepa:mv_fac_from_smarts
                                                           '&outputFormat=application%2Fjson'#,
                                                           # '&CQL_FILTER=INTERSECTS(fac_point,POLYGON((',
                                                           # rb_boundary_coordinates,
                                                           # ')))'
                                                        )
                                api_output <- readr::read_lines(url_sites_api)
                                json_list <- jsonlite::fromJSON(api_output)
                                df_api_result <- json_list$features
                                df_api_result <- df_api_result %>% mutate('data_source' = site_type)
                                # reformat the data frame (some of it is nested)
                                df_api_result <- bind_cols(df_api_result %>% select(id, data_source),
                                                           df_api_result$geometry,
                                                           df_api_result$properties)
                                if (counter_sites == 1) {
                                    cal_epa_sites_raw_download <- df_api_result
                                } else {
                                    cal_epa_sites_raw_download <- bind_rows(cal_epa_sites_raw_download, df_api_result)
                                }
                            }
                    })
                    # Create an sf object from the sites data
                    cal_epa_sites_raw_download <- st_as_sf(cal_epa_sites_raw_download %>% filter(!is.na(latitude) & !is.na(longitude)),
                                                         coords = c('longitude', 'latitude'),
                                                         crs = 4326,
                                                         # agr = 'constant',
                                                         remove = FALSE)
                    # rename some columns to match the data from the regulated site portal
                    cal_epa_sites_raw_download <- cal_epa_sites_raw_download %>% 
                        rename(site_name = facility_name)
                    # transform to projected crs (for mapping and analysis it's best to use a projected CRS -- see: https://s3.amazonaws.com/files.zevross.com/workshops/spatial/slides/html/4-crs.html#31)
                        cal_epa_sites_raw_download <- cal_epa_sites_raw_download %>% st_transform(crs = projected_crs)
                        # st_crs(cal_epa_sites_raw_download)
                        # return the object
                            return(cal_epa_sites_raw_download)
                } else {
                        return(tibble())
                    }
            })
            
    # REGULATORY ACTIONS
        # get the start and end date
            start_date <- reactive({
                start_date <- if (!is.na(input$sites_date_range[1])) {
                    as.Date(paste(year(input$sites_date_range[1]), # set to the first day of the selected month
                                  month(input$sites_date_range[1]),
                                  1,
                                  sep = '-'))
                } else {as.Date('0001-01-01')} # arbitrarily small date if no start date selected
                return(start_date)
            })
            end_date <- reactive({
                end_date <- if (!is.na(input$sites_date_range[2])) {
                    as.Date(paste(year(input$sites_date_range[2]),
                                  month(input$sites_date_range[2]),
                                  1,
                                  sep = '-'))
                } else {as.Date('9999-01-01')} # arbitrarily large date if no end date selected
                day(end_date) <- days_in_month(end_date) # set the end date to be the last day in the selected month, rather than the first
                return(end_date)
            })
            
    # summarize the inspections, violations, and enforcement actions for the selected period
        inspections_summary <- reactive({
            withProgress(message = 'Summarizing Data...', 
                         style = 'notification', 
                         value = 1, 
                         {
                             return(
                                 inspections_all_download %>% 
                                     # filter(eval_date >= start_date(),
                                     #        eval_date <= end_date()) %>%
                                     filter(site_id %in% (cal_epa_sites_raw() %>% 
                                                              pull(site_id))) %>%
                                     group_by(site_id) %>%
                                     summarize(inspections_count = n()) %>%
                                     {.}
                             )
                         })
        })
        # inspection_types <- reactive({
        #     return(
        #         inspections_all_download %>% 
        #             select(eval_type) %>% 
        #             distinct(eval_type) %>% 
        #             pull(eval_type)
        #     )
        # })
        violations_summary <- reactive({
            withProgress(message = 'Summarizing Data...', 
                         style = 'notification', 
                         value = 1, 
                         {
                             return(
                                 violations_all_download %>% 
                                     filter(violation_date >= start_date(),
                                            violation_date <= end_date()) %>%
                                     filter(site_id %in% (cal_epa_sites_raw() %>% 
                                                              pull(site_id))) %>%
                                     group_by(site_id) %>%
                                     summarize(violations_count = n()) %>%
                                     {.}
                             )
                         })
        })
        enforcement_summary <- reactive({
            withProgress(message = 'Summarizing Data...', 
                         style = 'notification', 
                         value = 1, 
                         {
                             return(
                                 enforcement_all_download %>% 
                                     filter(enf_action_date >= start_date(),
                                            enf_action_date <= end_date()) %>%
                                     filter(site_id %in% (cal_epa_sites_raw() %>% 
                                                              pull(site_id))) %>%
                                     group_by(site_id) %>%
                                     summarize(enforcements_count = n()) %>%
                                     {.}
                             )
                         })
        })
        
    # create an output text statement to report the most recent regulatory records available
        output$data_availability_msg <- renderText({
            paste0('*Most recent regulatory records available: ', 
                   most_recent_reg_records,
                   ''
            )
        })
        
    ###########################################################################################
    # summarize the inspections, violations, and/or enforcement records for the given time period, then join to the sites data
        cal_epa_sites_summarized <- reactive({
            withProgress(message = 'Summarizing Regulatory Action Data', 
                         style = 'notification', 
                         value = 1, { # style = 'notification' 'old'


        # join the regulatory data to the sites data
            cal_epa_sites_summarized_compute <- cal_epa_sites_raw()
            cal_epa_sites_summarized_compute <- cal_epa_sites_summarized_compute %>% 
                # left_join(inspections_count, by = c('site_id'))
                left_join(inspections_summary(), by = c('site_id'))
            cal_epa_sites_summarized_compute <- cal_epa_sites_summarized_compute %>% 
                # left_join(violations_count, by = c('site_id'))
                left_join(violations_summary(), by = c('site_id'))
            cal_epa_sites_summarized_compute <- cal_epa_sites_summarized_compute %>% 
                # left_join(enforcement_count, by = c('site_id')) %>%
                left_join(enforcement_summary(), by = c('site_id'))
        # replace NAs with zeros for the counts
            cal_epa_sites_summarized_compute <- cal_epa_sites_summarized_compute %>% 
                mutate(inspections_count = case_when(is.na(inspections_count) ~ 0L,
                                                     TRUE ~ inspections_count),
                       violations_count = case_when(is.na(violations_count) ~ 0L,
                                                    TRUE ~ violations_count),
                       enforcements_count = case_when(is.na(enforcements_count) ~ 0L,
                                                      TRUE ~ enforcements_count))
        })
        return(cal_epa_sites_summarized_compute)
        }) 
            

    # filter for sites... 
        # (1) with inspections, violations, and/or enforcement records > 0 if selected
        # (2) by CES polygon score
        # (3) by HOLC polygon rating
        cal_epa_sites_filtered_0 <- reactive({
            withProgress(message = 'Filtering Regulatory Action Data',
                         style = 'notification', value = 1, { # style = 'notification' 'old'
                             cal_epa_sites_filtered_0_compute <- cal_epa_sites_summarized()
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
                                         filter(enforcements_count > 0)
                                 }
                             # 2 - filter for sites by CES polygon score
                                # find the CES polygons that meet the selected criteria (range of values)
                                     parameter <- ces_choices %>% 
                                         filter(name == input$ces_parameter) %>% 
                                         pull(ces_variable)
                                     ces3_poly_filtered <- ces3_poly() %>% 
                                         filter(!!as.name(parameter) >= input$ces_range_filter[1]) %>%
                                         # filter(CES_3_Score >= input$ces_range_filter[1]) %>% 
                                         filter(!!as.name(parameter) <= input$ces_range_filter[2]) %>%
                                         # filter(CES_3_Score <= input$ces_range_filter[2]) %>%
                                         st_as_sf() %>% 
                                         {.}
                                 # filter for sites with CES polygons meeting the selected criteria
                                    cal_epa_sites_filtered_0_compute <- cal_epa_sites_filtered_0_compute[ces3_poly_filtered, ] # filter for sites within the selected ces polygons
                             # 3 - filter for sites by HOLC rating
                                if (input$holc_rating_sites_filter_on_off == TRUE) {
                                    cal_epa_sites_filtered_0_compute <- cal_epa_sites_filtered_0_compute[redline_polygons %>% 
                                                                                                             filter(holc_grade %in% input$holc_rating_sites_filter) %>% 
                                                                                                             filter(city %in% input$holc_city_sites_filter), ]
                                }
                         })
            return(cal_epa_sites_filtered_0_compute)
        })

    # filter for sites by program type if selected
        cal_epa_sites_filtered <- reactive({
            withProgress(message = 'Filtering Regulatory Action Data', 
                         style = 'notification', value = 1, { # style = 'notification' 'old'
                        cal_epa_sites_filtered_compute <- cal_epa_sites_filtered_0()
                        # if program type filters are selected, filter for the selected program types
                             if (!is.null(input$program_type_1)) {
                                 program_types_filter <- program_types %>% 
                                     filter(ei_description %in% input$program_type_1)
                                 cal_epa_sites_filtered_compute <- cal_epa_sites_filtered_compute %>% 
                                     filter(site_id %in% program_types_filter$site_id)
                             }
                         })
            return(cal_epa_sites_filtered_compute)
        })

    # create a dynamic input for program type
        output$program_type_1 <- renderUI({
            selectInput(inputId = 'program_type_1',
                        label = '**Filter Sites By Program Type:**',
                        multiple = TRUE,
                        choices = if (length(input$site_type_1) > 0 | input$sites_source == site_source_choices[3]) {
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
        # revise the data frame
            summary_table_df <- reactive({ # eventReactive(input$site_type_1, {
                if(input$sites_source == site_source_choices[1]) { 
                    shiny::showNotification("No data", type = "error")
                    NULL
                } else if (input$sites_source == site_source_choices[3] & length(input$site_type_1) == 0) {
                    shiny::showNotification("No data", type = "error")
                    NULL
                } else {
                    return( 
                        cal_epa_sites_filtered() %>% 
                            st_join(redline_polygons %>% select(-city)) %>% 
                            select(site_id, site_name, address,
                                   city, state, zip_code, 
                                   inspections_count, violations_count,
                                   enforcements_count, data_source, 
                                   latitude, longitude,
                                   holc_grade) %>% 
                            # rename(!!paste0('inspections_', col_name_custom()) := 'inspections_count') %>% 
                            st_drop_geometry() %>% 
                            mutate(reg_action_filter_start_date = if (is.na(input$sites_date_range[1]) | # this adds a new column showing the start date filter
                                                           is.null(input$sites_date_range[1])) {
                                NA
                                } else {
                                    input$sites_date_range[1]
                                    },
                                reg_action_filter_end_date = if (is.na(input$sites_date_range[2]) | # this adds a new column showing the end date filter
                                                      is.null(input$sites_date_range[2])) {
                                    NA
                                    } else {
                                        min(
                                            as.Date(paste0(year(input$sites_date_range[2]), '-',
                                                           month(input$sites_date_range[2]), '-',
                                                           days_in_month(input$sites_date_range[2]))),
                                            most_recent_reg_records
                                        )
                                        }) %>% 
                            rename(holc_polygon_grade = holc_grade) %>% 
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
                summary_table_df(),
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
        # get the bounds of the HOLC (redline) polygons for the selected city
            # bounds_1 <- attributes(st_geometry(redline_polygons %>% filter(city == input$city_selected_1)))$bbox
            bounds_1 <- attributes(st_geometry(redline_polygons %>% 
                                                   st_transform(crs = geographic_crs) %>% # have to convert to geographic coordinate system for leaflet
                                                   filter(city == initial_selected_city)))$bbox

        
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
        
        # create a button to center the map on CA
            bounds_ca <- attributes(st_geometry(rb_boundary() %>%
                                       st_transform(crs = geographic_crs)))$bbox # have to convert to geographic coordinate system for leaflet
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

        # Add the 303d polygons
            # add polygons
                l_map1 <- l_map1 %>% addPolygons(data = impaired_303d_poly() %>% st_transform(crs = geographic_crs), # have to convert to geographic coordinate system for leaflet
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
            # add polylines
                l_map1 <- l_map1 %>% addPolylines(data = impaired_303d_lines() %>% 
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
                                                  group = '303d Listed Waters')

        
        # add the region boundary
            l_map1 <- l_map1 %>% addPolygons(data = rb_boundary() %>% 
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
                                             popup = ~paste0('<b>', '<u>', 'Regional Board Boundary', '</u>', '</b>','<br/>',
                                                             '<b>', 'Region Name: ', '</b>', RB_NAME, '<br/>',
                                                             '<b>', 'Region Number: ', '</b>', RB_OFF),
                                             group = 'Regional Board Boundary'
            )
        
        # add the drinking water provider service areas
            # add polygons
                l_map1 <- l_map1 %>% 
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
                                                                    'HOLC Polygons', 
                                                                    '303d Listed Waters', 
                                                                    'CalEPA Regulated Sites',
                                                                    'Drinking Water Provider Service Areas',
                                                                    'Regional Board Boundary',
                                                                    'Legend'),
                                                  options = layersControlOptions(collapsed = TRUE,
                                                                                 autoZIndex = TRUE))
        # Hide some groups by default (can be turned on with the layers control box on the map)
            l_map1 <- l_map1 %>% hideGroup(c('Drinking Water Provider Service Areas')) #, 'HOLC Polygons')) 
                
        # output the map object
            l_map1
        })
    })
    
    # Use a separate observer to recreate some parts of the map as they are updated, without re-drawing the entire map ----
        # # create a button to re-center the map - NOT WORKING!!!!!!!!!
        #     observe({
        #         bounds_1 <- attributes(st_geometry(redline_polygons %>% 
        #                                                st_transform(crs = geographic_crs) %>% # have to convert to geographic coordinate system for leaflet,
        #                                                filter(city == input$city_selected_1)))$bbox
        #         leafletProxy('map1') %>% 
        #             addEasyButton(easyButton(
        #                 icon="fa-globe", title="Center Map on Selected City",
        #                 onClick=JS(paste0('function(btn, map){ map.fitBounds([[',
        #                                   round(bounds_1[[2]],4)-0.01, ', ',
        #                                   round(bounds_1[[1]],4)-0.01, '],[',
        #                                   round(bounds_1[[4]],4)+0.01, ', ',
        #                                   round(bounds_1[[3]],4)+0.01, ']]); }'))))
        #     })
    
    
        # HOLC rated (redlined) areas
            observe({
                withProgress(message = 'Drawing Map', value = 1, style = 'notification', {
                # Create the color palette for the Redline scores
                redline_leaflet_pal <- colorFactor(palette = c('green', 'blue', 'yellow', 'red'), # 'YlOrBr'   # c("#FACD7B","#D1A149","#916714","#6B4703")
                                                   domain = redline_polygons$holc_grade, 
                                                   levels = c('A', 'B', 'C', 'D'))
                leafletProxy('map1') %>%
                    clearGroup('HOLC Polygons') %>%
                    addPolygons(data = redline_polygons %>% 
                                    st_transform(crs = geographic_crs) %>% # have to convert to geographic coordinate system for leaflet,
                                    # filter(holc_grade %in% input$holc_rating_sites_filter) %>% 
                                    {.},
                                options = pathOptions(pane = "redline_polygons"),
                                color = ~redline_leaflet_pal(holc_grade), # 'black', # "#444444",
                                weight = 2.0,
                                smoothFactor = 1.0,
                                opacity = 1.0,
                                # fill = FALSE,
                                fillOpacity = 0, # input$redline_fill_1,
                                fillColor = 'lightgrey',
                                # fillColor = ~redline_leaflet_pal(holc_grade),
                                highlightOptions = highlightOptions(color = "white", weight = 2),#,bringToFront = TRUE
                                popup = ~paste0('<b>', '<u>', 'HOLC Rated Polygon', '</u>', '</b>','<br/>',
                                                '<b>', 'City: ', '</b>', city, '<br/>',
                                                '<b>', 'Name: ', '</b>', name, '<br/>',
                                                '<b>', 'Grade (A-D): ', '</b>', holc_grade),
                                group = 'HOLC Polygons'
                                )
                })
            })
        
        # CalEPA sites
            observe({
                withProgress(message = 'Drawing Map', value = 1, style = 'notification', {
                if (length(input$site_type_1) > 0 | input$sites_source == site_source_choices[2]) {
                    cal_epa_sites <- cal_epa_sites_filtered() %>% 
                        st_transform(crs = geographic_crs) # have to convert to geographic coordinate system for leaflet
                    # # if the option is selected, filter for sites with violations and/or enforcement actions
                    #     if ('Violations' %in% input$show_violations_enforcement) {
                    #         cal_epa_sites <- cal_epa_sites %>% filter(violations_count > 0)
                    #     }
                    #     if ('Enforcement Actions' %in% input$show_violations_enforcement) {
                    #         cal_epa_sites <- cal_epa_sites %>% filter(enforcements_count > 0)
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
                                         popup = ~paste0('<b>', '<u>', 'CalEPA Regulated Site', '</u>','</b>','<br/>',
                                                         # '<b>', '<u>', 'Site Information:', '</u>', '</b>','<br/>',
                                                         '<b>', 'Name: ', '</b>', site_name,'<br/>',
                                                         '<b>', 'ID: ', '</b>', site_id,'<br/>',
                                                         '<b>', 'Address: ', '</b>', address, '<br/>',
                                                         '<b>', 'City: ', '</b>', city, '<br/>',
                                                         #<b>', 'County: ', '</b>', County,'<br/>',
                                                         '<b>', 'Zip Code: ', '</b>', zip_code, '<br/>',
                                                         '<b>', 'Source: ', '</b>', data_source, '<br/>',
                                                         '<b>', 'Regulatory Actions (for selected time period):', '</b>','<br/>',
                                                         '<b>', HTML('&nbsp;'), HTML('&nbsp;'), ' - Number of Inspection Records: ', '</b>', inspections_count, '<br/>', # chr(149),
                                                         '<b>', HTML('&nbsp;'), HTML('&nbsp;'), ' - Number of Violation Records: ', '</b>', violations_count, '<br/>', # chr(149),
                                                         '<b>', HTML('&nbsp;'), HTML('&nbsp;'), ' - Number of Enforcement Records: ', '</b>', enforcements_count # chr(149),
                                         ),
                                         group = 'CalEPA Regulated Sites')
                } else {
                    leafletProxy('map1') %>%
                        clearGroup('CalEPA Regulated Sites')
                }
                })
            })
            
        # CalEnvironScreen Polygons 
            observe({
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
                                  values = ces3_poly$fill_variable,
                                  opacity = 1,
                                  layerId = 'ces_legend',
                                  bins = 4,
                                  group = 'Legend',
                                  title = paste0('CalEnviroScreen'))
                })
            })

        
    
    
    # MAP 2 -----------------------------------------------------------------------------------------------------------------#    
    output$map2 <- renderLeaflet({
        # get the bounds of the HOLC (redline) polygons for the selected city
            #bounds_2 <- attributes(st_geometry(redline_polygons %>% filter(city == input$city_selected_1)))$bbox
            bounds_2 <- attributes(st_geometry(redline_polygons %>% 
                                                   st_transform(crs = geographic_crs) %>% # have to convert to geographic coordinate system for leaflet,
                                                   filter(city == initial_selected_city)))$bbox
                                                    

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
                          title = paste0('HOLC Polygons'))
        
        # Add controls to select the basemap and layers
            l_map2 <- l_map2 %>% addLayersControl(baseGroups = basemap_options,
                                                 overlayGroups = c('HOLC Polygons', 'Legend'),
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
        # HOLC (redline) polygons
            observe({
                #input$city_selected_1 # to re-draw polygons when city is changed
                # Create the color palette for the Redline scores
                redline_leaflet_pal <- colorFactor(palette = c('green', 'blue', 'yellow', 'red'), # 'YlOrBr'   # c("#FACD7B","#D1A149","#916714","#6B4703")
                                                   domain = redline_polygons$holc_grade, 
                                                   levels = c('A', 'B', 'C', 'D'))
                leafletProxy("map2") %>% 
                    clearShapes() %>% 
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
                                popup = ~paste0('<b>', '<u>', 'HOLC Polygon', '</u>', '</b>','<br/>',
                                                '<b>', 'City: ', '</b>', city, '<br/>',
                                                '<b>', 'Name: ', '</b>', name, '<br/>',
                                                '<b>', 'Grade (A-D): ', '</b>', holc_grade),
                                group = 'HOLC Polygons'
                    )
            })


# Helper functions ---------------------------------------------------------------------------------------------------------#
    # Center map1 and map2 on change in city selection
        observeEvent(input$city_selected_1, {
            if (input$city_selected_1 == 'All') {
                redline_selected_1 <- redline_polygons %>% 
                    st_transform(crs = geographic_crs) # have to convert to geographic coordinate system for leaflet,
            } else {
                redline_selected_1 <- redline_polygons %>% 
                    st_transform(crs = geographic_crs) %>% # have to convert to geographic coordinate system for leaflet
                    filter(city == input$city_selected_1)
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
        