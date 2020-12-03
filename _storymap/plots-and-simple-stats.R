library(sf)
library(ggplot2)
library(dplyr)
library(here)
library(glue)
library(readr)
library(janitor)
library(tidyr)
library(stringr)
library(tmap)
library(tmaptools)
library(ceramic)

# read data ----
df_departure_scores <- st_read(here('data_processed-analysis',
                                    'departure-area_weighted_scores.gpkg'))
df_raw_scores <- st_read(here('data_processed-analysis',
                              'holc_area_weighted_scores.gpkg'))
ces_choices <- read_csv(here('data_processed', 
                             'ces_names.csv')) %>% # manually processed this file to make more descriptive names for the fields
    mutate(ces_variable = make_clean_names(name)) %>% #, 'parsed')) %>% 
    slice(8:64) %>%  # rows 8 to 64
    filter(type != 'other?')

# ces_field_names <- read_csv(here('data_processed', 'ces_names.csv')) %>% 
#     mutate(ces_variable = make_clean_names(name)) #, 'parsed'))

# map data
projected_crs <- 3310 # see: https://epsg.io/3310 
redline_polygons <- st_read(here('data_processed', 'redline_polygons.gpkg'), quiet = TRUE)
ces3_poly <- st_read(here('data_processed', 'ces3_poly.gpkg'), quiet = TRUE)
    ces3_poly <- ces3_poly %>% 
        rename('ces_city' = 'nearby_city')
ces_redline_intersection <- st_read(here('data_processed-analysis', 'ces_clipped_to_holc_bounds.gpkg'))
holc_hulls <- st_read(here('data_processed', 'holc_map_hulls.gpkg'))

# plots ----
# generic CES departure scores boxplot function
fn_departures_boxplot <- function(plot_var) {
    measure_name <- ces_choices %>% filter(ces_variable == plot_var) %>% pull(name)
    ggplot(data = df_departure_scores, 
                             mapping = aes(x = holc_grade, 
                                           y = !!as.name(plot_var))) +
    geom_boxplot(aes(fill = holc_grade), notch = TRUE, outlier.shape = NA) +
    scale_fill_manual(values = alpha(c('green', 'blue', 'yellow', 'red'), 0.6)) +
    geom_jitter(color='black', size=0.6, alpha=0.5, width = 0.2) +
    scale_x_discrete(limits = rev(levels(factor(df_departure_scores$holc_grade)))) +
    coord_flip() + # ylim = c(axis_min, axis_max)) +
    # NOTE: if not flipping the coordinates, use coord_cartesian(ylim = c(axis_min, axis_max))
    theme(legend.position = "none") +
    labs(x = 'HOLC Grade', 
         y = glue('Aggregated {measure_name} (Relative to Respective City-Wide Average Score)'),
         title = glue('Aggregated CES Scores for Neighborhoods Assessed by the HOLC (Area Weighted Average Aggregation)'), # Departure from Citywide Average, by HOLC Grade'),
         subtitle = 'Each point represents a neighborhood in the HOLC maps, and colors reflect the shading used in the HOLC maps to represent each grade') +
    geom_blank()
}

# create boxplots for overall CES score, pollution burden group, and popluation characteristics group
plot_box_ces <- fn_departures_boxplot(plot_var = 'ces_3_score')
plot_box_pollution_burden <- fn_departures_boxplot(plot_var = 'pollution_burden_group_score')
plot_box_population_char <- fn_departures_boxplot(plot_var = 'population_characteristics_group_score')


# point plot - all cities - raw scores
fn_plot_point_raw <- function(plot_var) {
        measure_name <- ces_choices %>% 
            filter(ces_variable == plot_var) %>% 
            pull(name)
        # make plot
        raw_scores_point <- ggplot(data = df_raw_scores,
                                   mapping = aes(x = !!as.name(plot_var),
                                                 y = holc_city)) +
            geom_jitter(mapping = aes(color = holc_grade),
                        height = 0.25) +
            scale_color_manual(values = alpha(c('green', 'blue', 'orange', 'red'), 0.3), name = 'HOLC Grade') +
            labs(x = glue('Aggregated {measure_name}'), 
                 y = 'City', 
                 title = glue('Aggregated {measure_name} for Neighborhoods Assessed by the HOLC (Area Weighted Average Aggregation)'),
                 subtitle = 'Each colored point represents a neighborhood in the HOLC maps, black dots and lines represent mean score +/- 1 standard deviation') +
            scale_y_discrete(limits = rev(levels(factor(df_raw_scores$holc_city)))) +
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
                             size = 0.5,
                             # alpha = 0.5
                             ) +
            geom_blank()
    }

plot_point_raw <- fn_plot_point_raw(plot_var = 'ces_3_score')



# simple stats ----
# deciles <- ntile(df_departure_scores %>% 
#                      st_drop_geometry() %>% 
#                      pull(ces_3_score), 
#                  10)

departure_scores_dec <- df_departure_scores %>% 
    st_drop_geometry() %>% 
    mutate(dec = ntile(ces_3_score, 10)) %>% 
    select(holc_grade, ces_3_score, dec) %>% 
    count(holc_grade, dec) %>% 
    mutate(holc_grade = factor(holc_grade, 
                               levels = rev(c('A', 'B', 'C', 'D')))) %>% 
    mutate(dec = factor(dec))


deciles_plot <- ggplot(data = departure_scores_dec) +
    geom_bar(mapping = aes(x = dec, y = n, fill = holc_grade), stat = 'identity') +
    scale_fill_manual(values = alpha(rev(c('green', 'blue', 'orange', 'red')), 0.8), name = 'HOLC Rating') +
    # scale_y_discrete(limits = levels(factor(df_departure_scores$holc_grade))) +
    scale_x_discrete(limits = rev(levels(departure_scores_dec$dec))) +
    labs(x = 'CES Score Decile (of 868 HOLC Neighborhoods',
         y = 'Number of HOLC-Assessed Neighborhoods',
         title = 'CES Score Deciles for HOLC-Assessed Neighborhoods') +
    coord_flip() +
    guides(fill = guide_legend(reverse = TRUE))

deciles_plot_comparison <- ggplot(data = departure_scores_dec %>% count(holc_grade, wt = n)) +
    geom_bar(mapping = aes(x = '', y = n, fill = holc_grade), stat = 'identity') +
    scale_fill_manual(values = alpha(rev(c('green', 'blue', 'orange', 'red')), 0.8), name = 'HOLC Grade') +
    # scale_y_discrete(limits = levels(factor(df_departure_scores$holc_grade))) +
    # scale_x_discrete(limits = rev(levels(departure_scores_dec$dec))) +
    xlab('CES Score Decile (of 868 HOLC Neighborhoods)') + 
    ylab('Number of HOLC Rated Neighborhoods') +
    coord_flip() +
    guides(fill = guide_legend(reverse = TRUE))

# percentage in top and bottom deciles
departure_scores_dec %>% filter(dec == 1) %>% mutate(pct = n / sum(n) * 100)
departure_scores_dec %>% filter(dec == 10) %>% mutate(pct = n / sum(n) * 100)
departure_scores_dec %>% count(holc_grade, wt = n) %>% mutate(pct = n / sum(n) * 100) %>% arrange(desc(holc_grade))

mean_scores_by_grade <- df_departure_scores %>% 
    st_drop_geometry() %>% 
    group_by(holc_grade) %>% 
    summarize(across(.fns = mean)) %>% 
    select(ends_with(c('grade', 'score')))

mean_scores_by_grade_metric <- mean_scores_by_grade %>% 
    pivot_longer(values_to = 'score', names_to = 'metric', cols = -c(holc_grade)) %>% 
    arrange(metric, score) %>% 
    filter(!metric %in% c('ces_3_score', 
                       'pollution_burden_group_score', 
                       'population_characteristics_group_score'))

# analyze the order of mean scores by HOLC grade for each indicator
holc_grade_orders <- list()
j <- 1
holc_grade_orders <- c()
for (i in seq(1, 77, by = 4)) {
    # print(j)
    holc_grade_orders[j] <- glue_collapse(c(mean_scores_by_grade_metric$holc_grade[i:(i+3)]))
  # names(holc_grade_orders)[i] <- mean_scores_by_grade_metric$metric[i]
    j <- j + 1
}
holc_grade_orders
sum(str_detect(holc_grade_orders, 'ABCD')) # 17
sum(str_detect(holc_grade_orders, 'A..D')) # 18
sum(str_detect(holc_grade_orders, 'DCBA')) # 2
# SUMMARY: mean scores by holc grade for 17 of the 20 indicators follow 
# the A-B-C-D trend (best to worst score), 

# for 18 of the 20 A has the best score and D has the worst - 1 (traffic) 
# has B and C reversed), 

# 2 of 20 (ozone and drinking water) follow the opposite 
# trend (best to worst is D-C-B-A)


# dot plot by indicator
# make plot
departures_point_city <- ggplot(data = mean_scores_by_grade_metric) +
    geom_point(aes(x = score,
                   y = '',
                   color = holc_grade)) +
    scale_color_manual(values = alpha(c('green', 'blue', 'orange', 'red'), 0.9),
                       name = 'HOLC Grade') +
    # scale_y_discrete(limits = rev(levels(factor(mean_scores_by_grade_metric$holc_grade)))) +
    facet_wrap(~ metric, ncol = 4, scale = 'free') +
    theme(axis.title.y = NULL) +
    labs(x = glue('Mean Aggregated Departure Score'), 
         y = 'HOLC Grade',
         title = glue('HOLC Polygon Aggregated Indicator Scores (Area Weighted Average)'),
         subtitle = 'Scores are aggregated score versus average score of all polygons in respective city') +
    geom_blank()

# scale the scores (from 0 to 1)
scaled_scores <- mean_scores_by_grade_metric %>% 
    pivot_wider(names_from = holc_grade, values_from = score)
scaled_scores <- scaled_scores %>% 
    mutate(scaled_A = (A - pmin(A,B,C,D)) / (pmax(A,B,C,D) - pmin(A,B,C,D)),
           scaled_B = (B - pmin(A,B,C,D)) / (pmax(A,B,C,D) - pmin(A,B,C,D)),
           scaled_C = (C - pmin(A,B,C,D)) / (pmax(A,B,C,D) - pmin(A,B,C,D)),
           scaled_D = (D - pmin(A,B,C,D)) / (pmax(A,B,C,D) - pmin(A,B,C,D))
    )
# plot
df_scaled_scores_plot <- scaled_scores %>% 
                                   select(starts_with(c('metric', 'scaled'))) %>% 
                                   pivot_longer(cols = -metric, names_to = 'holc_grade') %>% 
    mutate(metric = str_remove(string = metric, pattern = '_score')) %>% 
    mutate(metric = str_replace_all(string = metric, pattern = '2_5', replacement = '2.5')) %>% 
    mutate(metric = str_replace_all(string = metric, pattern = '_', replacement = ' ')) %>% 
    mutate(holc_grade = str_remove(string = holc_grade, pattern = 'scaled_'))
scaled_scores_plot <- ggplot(df_scaled_scores_plot) +
    geom_point(aes(x = value,
                   y = metric,
                   color = holc_grade)) +
    scale_color_manual(values = alpha(c('green', 'blue', 'orange', 'red'), 1.0),
                       name = 'HOLC Grade') +
    scale_y_discrete(limits = rev(levels(factor(df_scaled_scores_plot$metric)))) +
    labs(x = glue('Mean CES Score (Scaled from 0-1)'), 
         y = 'Indicator',
         title = glue('Average HOLC Polygon Aggregated Departure from Citywide Average by HOLC Grade'),
         subtitle = 'Departure is aggregated score versus average score of all polygons in respective city') +
    theme(legend.position = 'bottom', axis.text.x = element_blank()) +
    geom_blank()


# Maps ----
basemap <- function(city_selected) {
    centroid <- st_centroid(holc_hulls %>% filter(city == city_selected) %>% st_transform(4326))
    centroid_lon <- (centroid %>% st_geometry())[[1]][1]
    centroid_lat <- (centroid %>% st_geometry())[[1]][2]
    pt <- cbind(centroid_lon, centroid_lat)
    # get a basemap (See: https://github.com/mtennekes/tmap/issues/185)
        Sys.setenv(MAPBOX_API_KEY = Sys.getenv('mapbox_api_key'))
        map_background <- cc_location(pt, buffer = 10000, verbose = FALSE)
        map_background
}


holc_map <- function(city_selected) {
        background <- basemap(city_selected)
        # create map
        map_redline <- tm_shape(background) + 
            tm_rgb(alpha = 0.5) + 
            tm_shape(redline_polygons %>% 
                         filter(holc_city == city_selected), is.master = TRUE) + 
            tm_polygons('holc_grade', 
                        palette = c('green', 'blue', 'yellow', 'red'), 
                        title = 'HOLC Grade') + 
            tm_layout(legend.bg.color = 'white', 
                      legend.bg.alpha = 0.7, 
                      legend.frame = TRUE)
        map_redline
    }




ces3_poly_map <- function(ces_id, ces_measure_name, city_selected) {
    background <- basemap(city_selected)
    # plot the CES polygons
    map_ces <- tm_shape(background) + 
        tm_rgb(alpha = 0.5) + 
        # tm_shape(ces3_poly %>% 
        #              filter(ces_city %in% (ces_redline_intersection %>% 
        #                                        filter(holc_city == city) %>% 
        #                                        pull(ces_city)))
        # ) +
        tm_shape(ces3_poly[holc_hulls %>% filter(city == city_selected),]) + 
        # filter(ces_city == 'Sacramento' | ces_city == 'West Sacramento')) + 
        tm_polygons(col = ces_id, 
                    title = ces_measure_name, 
                    border.alpha = 1) + 
        tm_shape(ces_redline_intersection %>% 
                     filter(holc_city == city_selected), 
                 is.master = TRUE) +
        tm_borders(lwd = 0, alpha = 0) +
        # tm_shape(redline_polygons %>% filter(holc_city == 'Sacramento')) +
        # tm_borders(lwd = 2, col = 'black') +
        tm_shape(redline_polygons %>% 
                     filter(holc_city == city_selected, holc_grade == 'D')) + 
        tm_borders(lwd = 2, col = 'red') + 
        tm_shape(redline_polygons %>% 
                     filter(holc_city == city_selected, holc_grade == 'C')) + 
        tm_borders(lwd = 2, col = 'gold2') + 
        tm_shape(redline_polygons %>% 
                     filter(holc_city == city_selected, holc_grade == 'B')) + 
        tm_borders(lwd = 2, col = 'blue') + 
        tm_shape(redline_polygons %>% 
                     filter(holc_city == city_selected, holc_grade == 'A')) + 
        tm_borders(lwd = 2, col = 'green') +
        # tm_text('holc_grade', size = 0.5) + 
        tm_layout(legend.bg.color = 'white', legend.bg.alpha = 0.7, legend.frame = TRUE)
    map_ces
}
 
map_ces <- ces3_poly_map(ces_id = 'ces_3_score', 
                         ces_measure_name = 'CES 3 Score', 
                         city_selected = 'Stockton')


ces_redline_overlap_map <- function(ces_id, ces_measure_name, city_selected) {
        background <- basemap(city_selected)
        map_overlap <- tm_shape(background) + 
            tm_rgb(alpha = 0.5) + 
            # tm_shape(ces3_poly %>% filter(ces_city == 'Sacramento')) + 
            # tm_shape(ces3_poly %>% 
            #              filter(ces_city %in% (ces_redline_intersection %>% 
            #                                        filter(holc_city == city_selected) %>% 
            #                                        pull(ces_city)))
            # ) +
            tm_shape(ces3_poly[holc_hulls %>% filter(city == city_selected),]) +
            tm_borders(lwd = 1) + 
            tm_shape(ces_redline_intersection %>% filter(holc_city == city_selected), is.master = TRUE) + 
            tm_polygons(col = ces_id, title = ces_measure_name, border.alpha = 0) + 
            # tm_shape(redline_polygons %>% filter(holc_city == 'Sacramento')) + 
            # tm_borders(lwd = 2, col = 'black') + 
            tm_shape(redline_polygons %>% 
                       filter(holc_city == city_selected, holc_grade == 'D')) + 
            tm_borders(lwd = 2, col = 'red') + 
            tm_shape(redline_polygons %>% 
                       filter(holc_city == city_selected, holc_grade == 'C')) + 
            tm_borders(lwd = 2, col = 'gold2') + 
            tm_shape(redline_polygons %>% 
                       filter(holc_city == city_selected, holc_grade == 'B')) + 
            tm_borders(lwd = 2, col = 'blue') + 
            tm_shape(redline_polygons %>% 
                       filter(holc_city == city_selected, holc_grade == 'A')) + 
            tm_borders(lwd = 2, col = 'green') + 
            #tm_text('holc_grade', size = 0.5) + 
            tm_layout(legend.bg.color = 'white', legend.bg.alpha = 0.7, legend.frame = TRUE)
        map_overlap
}


holc_scores_map <- function(ces_id, ces_measure_name, city_selected) {
        background <- basemap(city_selected)
        map_holc_scores <- tm_shape(background) + 
            tm_rgb(alpha = 0.5) + 
            # tm_shape(ces3_poly %>% filter(ces_city == 'Sacramento')) + 
            # tm_shape(ces3_poly %>% 
            #              filter(ces_city %in% (ces_redline_intersection %>% 
            #                                        filter(holc_city == city_selected) %>% 
            #                                        pull(ces_city)))
            # ) +
            tm_shape(ces3_poly[holc_hulls %>% filter(city == city_selected),]) +
            tm_borders(lwd = 1) + 
            tm_shape(df_raw_scores %>% filter(holc_city == city_selected), is.master = TRUE) + 
            tm_polygons(col = ces_id, title = ces_measure_name, border.alpha = 0) + 
            # tm_shape(redline_polygons %>% filter(holc_city == 'Sacramento')) + 
            # tm_borders(lwd = 2, col = 'black') + 
            tm_shape(redline_polygons %>% 
                       filter(holc_city == city_selected, holc_grade == 'D')) + 
            tm_borders(lwd = 2, col = 'red') + 
            tm_shape(redline_polygons %>% 
                       filter(holc_city == city_selected, holc_grade == 'C')) + 
            tm_borders(lwd = 2, col = 'gold2') + 
            tm_shape(redline_polygons %>% 
                       filter(holc_city == city_selected, holc_grade == 'B')) + 
            tm_borders(lwd = 2, col = 'blue') + 
            tm_shape(redline_polygons %>% 
                       filter(holc_city == city_selected, holc_grade == 'A')) + 
            tm_borders(lwd = 2, col = 'green') + 
            #tm_text('holc_grade', size = 0.5) + 
            tm_layout(legend.bg.color = 'white', legend.bg.alpha = 0.7, legend.frame = TRUE)
        map_holc_scores
    }

map_redline <- holc_map(city_selected = 'Stockton')

map_ces <- ces3_poly_map(ces_id = 'ces_3_score', 
                         ces_measure_name = 'CES 3 Score', 
                         city_selected = 'Stockton')

map_overlap <- ces_redline_overlap_map(ces_id = 'ces_3_score', 
                                       ces_measure_name = 'CES 3 Score', 
                                       city_selected = 'Stockton')

map_holc_scores <- holc_scores_map(ces_id = 'ces_3_score', 
                                       ces_measure_name = 'CES 3 Score', 
                                       city_selected = 'Stockton')


# tmap_arrange(map_redline, map_ces, map_overlap, nrow = 1)
tmap_arrange(map_redline, map_ces, map_overlap, map_holc_scores, nrow = 1)
# tmap_arrange(map_ces, map_holc_scores, nrow = 1)
# tmap_arrange(map_redline, map_holc_scores, nrow = 1)

