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
library(patchwork)
library(forcats)


# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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



# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# box plots ----
# generic CES departure scores boxplot function
fn_departures_boxplot <- function(plot_var) {
    measure_name <- ces_choices %>% filter(ces_variable == plot_var) %>% pull(name)
    ggplot(data = df_departure_scores, 
                             mapping = aes(x = holc_grade, 
                                           y = !!as.name(plot_var))) +
    geom_boxplot(aes(fill = holc_grade), notch = TRUE, outlier.shape = NA) +
    scale_fill_manual(values = alpha(c('green', 'blue', 'yellow', 'red'), 0.6), 
                      labels = c('A (Best)', 'B (Desirable)', 'C (Declining)', 'D (Hazardous)')) +
    geom_jitter(color='black', size=0.6, alpha=0.5, width = 0.2) +
    scale_x_discrete(limits = rev(levels(factor(df_departure_scores$holc_grade)))) +
    coord_flip() + # ylim = c(axis_min, axis_max)) +
    # NOTE: if not flipping the coordinates, use coord_cartesian(ylim = c(axis_min, axis_max))
    # theme(legend.position = "none") +
    labs(x = 'HOLC Grade', 
         y = glue('{measure_name} (Relative to Respective City-Wide Average Score)'),
         title = glue('{measure_name} Scores for California Neighborhoods Assessed by the HOLC in the 1930s'), # Departure from Citywide Average, by HOLC Grade'),
         subtitle = 'Each point represents a neighborhood in the HOLC maps',
         fill = 'HOLC Grade') +
    geom_blank()
}

# create boxplots for overall CES score, pollution burden group, and popluation characteristics group
plot_box_ces <- fn_departures_boxplot(plot_var = 'ces_3_score')
    ggsave(filename = here('_storymap', 'storymap_images', 'box_ces_score.png'), 
           plot = plot_box_ces, width = 10, height = 4.5, dpi = 125)

plot_box_pollution_burden <- fn_departures_boxplot(plot_var = 'pollution_burden_group_score')
    ggsave(filename = here('_storymap', 'storymap_images', 'box_pollution_burden_group.png'), 
           plot = plot_box_pollution_burden, width = 10, height = 4.5, dpi = 125)
plot_box_population_char <- fn_departures_boxplot(plot_var = 'population_characteristics_group_score')
    ggsave(filename = here('_storymap', 'storymap_images', 'box_population_char_group.png'), 
           plot = plot_box_population_char, width = 10, height = 4.5, dpi = 125)


# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# point plots ----
# point plot - grouped by city - raw scores
fn_plot_point_raw <- function(plot_var, show_sd, show_titles, show_legend, fixed_x) {
        measure_name <- ces_choices %>% 
            filter(ces_variable == plot_var) %>% 
            pull(name)
        # make plot
        raw_scores_point <- ggplot(data = df_raw_scores %>% 
                                       mutate(holc_city = fct_reorder(holc_city, !!as.name(plot_var))),
                                   mapping = aes(x = !!as.name(plot_var),
                                                 y = holc_city)) +
            geom_jitter(mapping = aes(color = holc_grade),
                        height = 0.25) +
            scale_color_manual(values = alpha(c('green', 'blue', 'orange', 'red'), 0.3), 
                               name = 'HOLC Grade',
                               labels = c('A (Best)', 'B (Desirable)', 'C (Declining)', 'D (Hazardous)')) +
            # scale_y_discrete(limits = rev(levels(factor(df_raw_scores$holc_city)))) +
            # draw a point at the mean for each city
            # stat_summary(fun = mean, geom = 'point') + 
            # draw a vertical line at the mean for each city
            # stat_summary(fun = mean, 
            #              geom = 'errorbar',
            #              aes(xmax = ..x.., xmin = ..x..),
            #              width = 0.5, size = 0.7, linetype = "solid", color = 'black') +
            # draw the mean and standard deviation for each city (# of std deviations in the fun.args part)
            # xlab(glue('{measure_name}')) +
            # xlab(glue('{measure_name} (\u2013Increasing Disadvantage\u2192)')) +
            xlab(glue('{measure_name} (Increasing Disadvantage\u2192)')) +
            ylab('City') +
            geom_blank()
        
        if (fixed_x) {
            raw_scores_point <- raw_scores_point +
                scale_x_continuous(breaks = seq(0, 100, 20), limits = c(0, 90))
        }
        
        if (show_sd == TRUE) {
            raw_scores_point <- raw_scores_point + 
                stat_summary(fun.data = mean_sdl,
                             fun.args = list(mult = 1),
                             geom = 'pointrange',
                             color = 'black',
                             size = 0.5,
                             # alpha = 0.5
                ) +
                stat_summary(# fun.data = "mean_cl_normal", 
                    fun = 'mean',
                    aes(shape="Mean +/- 1 SD"), 
                    colour = "black",
                    size = 2.5,
                    geom="point") +
                scale_shape_manual("", values=c("Mean +/- 1 SD"=c(19)))
                # stat_summary(# fun.data = "mean_cl_normal", 
                #     fun = 'sd',
                #     # aes(shape="City Average"), 
                #     colour = "black",
                #     # size = 2.5,
                #     geom="line") +
        } else {
            raw_scores_point <- raw_scores_point + 
                stat_summary(# fun.data = "mean_cl_normal", 
                    fun = 'mean',
                    aes(shape="City Average"), 
                    colour = "black",
                    size = 4.5,
                    geom="point", 
                    stroke = 3) +
                scale_shape_manual("", values=c("City Average"=124)) # 19
        }
            
    if (show_titles == TRUE) {
        if (show_sd == FALSE) {
            raw_scores_point <- raw_scores_point +
                labs(title = glue('{measure_name} for Neighborhoods in California Cities Assessed by the HOLC in the 1930s'),
                     caption = glue('Note: Higher {str_replace(measure_name, "Score", "score")}s indicate greater pollution burden and/or population vulnerability'),
                     subtitle = glue('Each colored point represents a neighborhood in the HOLC maps, and black lines represent the average {measure_name} \nof all neighborhoods assessed by the HOLC in the given city')) # 'and lines represent mean score +/- 1 standard deviation') +

        } else {
            raw_scores_point <- raw_scores_point +
                labs(title = glue('{measure_name} for Neighborhoods in California Cities Assessed by the HOLC in the 1930s'),
                     caption = glue('Note: Higher {str_replace(measure_name, "Score", "score")}s indicate greater pollution burden and/or population vulnerability'),
                     subtitle = glue('Each colored point represents a neighborhood in the HOLC maps, and black dots/lines represent the mean {measure_name} \n+/- 1 standard deviation of all neighborhoods assessed by the HOLC in the given city')
                     ) # 'and lines represent mean score +/- 1 standard deviation') +

        }
    }
        
    if (show_legend) {
        raw_scores_point <- raw_scores_point + 
            theme(legend.position = 'right')
    } else {
        raw_scores_point <- raw_scores_point +
            theme(legend.position = 'none')
    }
}

plot_point_raw <- fn_plot_point_raw(plot_var = 'ces_3_score', 
                                    show_sd = TRUE, 
                                    show_titles = TRUE, 
                                    show_legend = TRUE, 
                                    fixed_x = FALSE)
ggsave(filename = here('_storymap', 'storymap_images', 'point_raw_score_by_city.png'), 
       plot = plot_point_raw, width = 10, height = 4.5, dpi = 125)

plot_point_raw_noSD <- fn_plot_point_raw(plot_var = 'ces_3_score', 
                                         show_sd = FALSE, 
                                         show_titles = TRUE, 
                                         show_legend = TRUE, 
                                         fixed_x = FALSE)
ggsave(filename = here('_storymap', 'storymap_images', 'point_raw_score_by_city_noSD.png'), 
       plot = plot_point_raw_noSD, width = 10, height = 4.5, dpi = 125)




# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# point plot with city averages for each HOLC grade 
    df_raw_scores_summary <- df_raw_scores %>%
        st_drop_geometry() %>% 
        group_by(holc_city, holc_grade) %>%
        summarise(city_holc_average = mean(ces_3_score),
                  city_holc_median = median(ces_3_score)) %>%
        ungroup() %>% 
        {.}
    df_raw_scores_city_summary <- df_raw_scores %>%
        st_drop_geometry() %>% 
        group_by(holc_city) %>%
        summarise(city_holc_average = mean(ces_3_score),
                  city_holc_median = median(ces_3_score)) %>%
        ungroup() %>% 
        {.}
    
# make plot
fn_plot_point_city_avgs <- function(plot_var, show_titles, show_legend, fixed_x) {  
    measure_name <- ces_choices %>% 
            filter(ces_variable == plot_var) %>% 
            pull(name)
        # make plot
    holc_city_means_plot <- ggplot() + 
        geom_point(data = df_raw_scores_city_summary %>% 
                       mutate(holc_city = 
                                  fct_reorder(holc_city, 
                                              city_holc_average)),
                   mapping = aes(x = city_holc_average,
                                 y = holc_city,
                                 shape = "City Average"
                   ),
                   color = 'black',
                   size = 4.5,
                   alpha = 1,
                   stroke = 3) +
        geom_point(data = df_raw_scores_summary,
                   aes(x = city_holc_average,
                       y = holc_city,
                       color = holc_grade), 
                   size = 3.0, 
                   alpha = 0.7) +
        # geom_jitter(aes(x = city_holc_dep_average,
        #                 y = holc_city,
        #                 color = holc_grade),
        #             height = 0.15) +
        scale_color_manual(values = alpha(c('green', 'blue', 'orange', 'red'), 1.0),
                           name = 'HOLC Grade',
                           labels = c('A (Best)', 'B (Desirable)', 'C (Declining)', 'D (Hazardous)')) +
        # scale_y_discrete(limits = rev(levels(factor(df_raw_scores_summary$holc_city)))) +
        # theme(legend.position = 'bottom') +
        # stat_summary(# fun.data = "mean_cl_normal", 
        #     fun = 'mean',
        #     aes(shape="Average Score"), 
        #     colour = "black",
        #     size = 2.5,
        #     geom="point") +
        scale_shape_manual("", values=c("City Average"=124)) +
        xlab(glue('Average {measure_name} (Increasing Disadvantage\u2192)')) + 
        ylab('City') +
        geom_blank()
    
    if (fixed_x) {
        holc_city_means_plot <- holc_city_means_plot +
            scale_x_continuous(breaks = seq(0, 100, 20), limits = c(0, 90))
    }        
    
    if (show_titles) {
        holc_city_means_plot <- holc_city_means_plot +
            labs(title = glue('Average {measure_name} by City and HOLC Grade for Neighborhoods in California Cities Assessed \nby the HOLC in the 1930s'),
                 caption = glue('Note: Higher {str_replace(measure_name, "Score", "score")}s indicate greater pollution burden and/or population vulnerability'),
                 subtitle = glue('Black lines represent the average {measure_name} of all neighborhoods assessed by the HOLC in the given city')
            )
    }
    
    if (show_legend) {
        holc_city_means_plot <- holc_city_means_plot + 
            theme(legend.position = 'right')
    } else {
        holc_city_means_plot <- holc_city_means_plot +
            theme(legend.position = 'none')
    }
}

plot_ces_avgScore_cityAndHOLC <- fn_plot_point_city_avgs(plot_var = 'ces_3_score', 
                                                         show_titles = TRUE, 
                                                         show_legend = TRUE,
                                                         fixed_x = FALSE)
ggsave(filename = here('_storymap', 'storymap_images', 'point_raw_score_by_city_and_grade.png'), 
       plot = plot_ces_avgScore_cityAndHOLC, width = 10, height = 4.5, dpi = 125)

# PATCHWORK
plot_point_sxs <- fn_plot_point_raw(plot_var = 'ces_3_score', 
                                    show_sd = FALSE, 
                                    show_titles = FALSE, 
                                    show_legend = FALSE,
                                    fixed_x = TRUE) + 
    # labs(subtitle = 'Colored dots represent individual neighborhoods \nassessed by the HOLC') + 
    fn_plot_point_city_avgs(plot_var = 'ces_3_score', 
                            show_titles = FALSE, 
                            show_legend = TRUE, 
                            fixed_x = TRUE) +
    labs(y = '') +
    # labs(subtitle = 'Colored dots represent average CES 3 scores \nfor neighborhood of each HOLC grade') +
    plot_annotation(title = 'CES 3 Score by Neighborhood (Left) and Average CES 3 Score by HOLC Grade (Right) for Neighborhoods in \nCalifonia Cities Assessed by the HOLC in the 1930s', 
                    caption = 'Note: Higher CES scores indicate greater pollution burden and/or population vulnerability',
                    subtitle = 'Black lines represent the average CES 3 score for all neighborhoods assessed by the HOLC in the given city')
ggsave(filename = here('_storymap', 'storymap_images', 'point_city_side_x_side.png'), 
       plot = plot_point_sxs, width = 10, height = 4.5, dpi = 125)

plot_point_stacked <- fn_plot_point_raw(plot_var = 'ces_3_score', 
                                    show_sd = FALSE, 
                                    show_titles = FALSE, 
                                    show_legend = FALSE,
                                    fixed_x = TRUE) / 
    fn_plot_point_city_avgs(plot_var = 'ces_3_score', 
                            show_titles = FALSE, 
                            show_legend = TRUE, 
                            fixed_x = TRUE)
ggsave(filename = here('_storymap', 'storymap_images', 'point_city_stacked.png'), 
       plot = plot_point_stacked, width = 10, height = 4.5, dpi = 125)



# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# point plot grouped by HOLC grade - alternative to box plot
fn_plot_point_by_grade <- function(plot_var) {
    measure_name <- ces_choices %>% 
        filter(ces_variable == plot_var) %>% 
        pull(name)
    # make plot
    raw_scores_point <- ggplot(data = df_departure_scores,
                               mapping = aes(x = !!as.name(plot_var),
                                             y = holc_grade)) +
        geom_jitter(mapping = aes(color = holc_grade),
                    height = 0.25) +
        scale_color_manual(values = alpha(c('green', 'blue', 'orange', 'red'), 0.3), name = 'HOLC Grade') +
        labs(x = glue('{measure_name} (Relative to Respective City-Wide Average Score)'), 
             y = 'HOLC Grade', 
             title = glue('{measure_name} Grouped by HOLC Grade for California Neighborhoods Assessed by the HOLC in the 1930s'),
             subtitle = 'Each colored point represents a neighborhood in the HOLC maps') +
        scale_y_discrete(limits = rev(levels(factor(df_raw_scores$holc_grade)))) +
        # draw a point at the mean for each city
        # stat_summary(fun = mean, geom = 'point') + 
        # draw a vertical line at the mean for each city
        # stat_summary(fun = mean, 
        #              geom = 'errorbar',
        #              aes(xmax = ..x.., xmin = ..x..),
        #              width = 0.5, size = 0.7, linetype = "solid", color = 'black') +
        # draw the mean and standard deviation for each city (# of std deviations in the fun.args part)
        stat_summary(fun = 'mean',
                     # fun.data = 'mean', # mean_sdl, 
                     #fun.args = list(mult = 1), 
                     geom = 'point', # 'pointrange', 
                     color = 'black', 
                     size = 2.0, # 0.5, 
                     aes(shape="Average Score"),
                     # show.legend = TRUE,
                     # alpha = 0.5
        ) +
        scale_shape_manual("", values=c("Average Score"=19)) +
        geom_blank()
}

plot_point_grade <- fn_plot_point_by_grade(plot_var = 'ces_3_score')
ggsave(filename = here('_storymap', 'storymap_images', 'point_departure_score_by_grade.png'), 
       plot = plot_point_grade, width = 10, height = 4.5, dpi = 125)



# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Deciles plot / stats ----
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
    scale_fill_manual(values = alpha(rev(c('green', 'blue', 'orange', 'red')), 0.8), 
                      name = 'HOLC Rating',
                      labels = rev(c('A (Best)', 'B (Desirable)', 'C (Declining)', 'D (Hazardous)'))) +
    # scale_y_discrete(limits = levels(factor(df_departure_scores$holc_grade))) +
    scale_x_discrete(limits = rev(levels(departure_scores_dec$dec))) +
    labs(x = 'Adjusetd CES 3 Score Decile \n(\u2190Increasing Disadvantage)',
         y = 'Number of HOLC Rated Neighborhoods', 
         caption = 'Note: Pollution burden and/or population vulnerability increases with increasing decile number (from 1 to 10)',
         title = 'Number of HOLC Rated Neighborhoods by Adjusted CES 3 Score Decile for All California Cities \nAssessed by the HOLC in the 1930s') +
         
    # title = 'HOLC Ratings of Neighborhoods in each CES 3 Score Decile for Neighborhoods in California Cities Assessed by the HOLC in the 1930s') +
         # title = 'HOLC Rating of Neighborhoods in California by CES 3 Score Decile') + # 'CES 3 Score Deciles for HOLC-Assessed Neighborhoods in California') +
    coord_flip() +
    guides(fill = guide_legend(reverse = TRUE))
ggsave(filename = here('_storymap', 'storymap_images', 'bar_deciles.png'), 
       plot = deciles_plot, width = 10, height = 4.5, dpi = 125)

# deciles_plot_comparison <- ggplot(data = departure_scores_dec %>% count(holc_grade, wt = n)) +
#     geom_bar(mapping = aes(x = '', y = n, fill = holc_grade), stat = 'identity') +
#     scale_fill_manual(values = alpha(rev(c('green', 'blue', 'orange', 'red')), 0.8), name = 'HOLC Grade') +
#     # scale_y_discrete(limits = levels(factor(df_departure_scores$holc_grade))) +
#     # scale_x_discrete(limits = rev(levels(departure_scores_dec$dec))) +
#     xlab('CES Score Decile (of 868 HOLC Neighborhoods)') + 
#     ylab('Number of HOLC Rated Neighborhoods') +
#     coord_flip() +
#     guides(fill = guide_legend(reverse = TRUE))

# stats about deciles
# percentage in top and bottom deciles
departure_scores_dec %>% filter(dec == 1) %>% mutate(pct = n / sum(n) * 100)
departure_scores_dec %>% filter(dec == 10) %>% mutate(pct = n / sum(n) * 100)
departure_scores_dec %>% count(holc_grade, wt = n) %>% mutate(pct = n / sum(n) * 100) %>% arrange(desc(holc_grade))



# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# analyze the order of mean scores by HOLC grade for each indicator ----
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



# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# make dot plot by indicator ----
    plot_indicators_dot_facet <- ggplot(data = mean_scores_by_grade_metric %>% 
                                        mutate(metric = str_replace_all(metric, '_', ' ')) %>% 
                                        mutate(metric = str_replace(metric, '2 5', '2.5'))) +
        geom_point(aes(x = score,
                       y = '',
                       color = holc_grade)) +
        scale_color_manual(values = alpha(c('green', 'blue', 'orange', 'red'), 0.9),
                           name = 'HOLC Grade') +
        # scale_y_discrete(limits = rev(levels(factor(mean_scores_by_grade_metric$holc_grade)))) +
        facet_wrap(~ metric, ncol = 4, scale = 'free') +
        theme(axis.title.y = NULL) +
        labs(x = glue('Mean Aggregated Departure Score'), 
             y = '') + # 'HOLC Grade'),
             # title = glue('HOLC Polygon Aggregated Indicator Scores (Area Weighted Average)'),
             #subtitle = 'Scores are aggregated score versus average score of all polygons in respective city') +
        geom_blank()
    ggsave(filename = here('_storymap', 'storymap_images', 'indicators_dot_facet.png'), 
           plot = plot_indicators_dot_facet, width = 10, height = 4.5, dpi = 125)

# MAKE A SCALED PLOT OF THE INDICATOR SCORES ----
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
    plot_indicators_scaled <- ggplot(df_scaled_scores_plot) +
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
    ggsave(filename = here('_storymap', 'storymap_images', 'indicators_dot_scaled.png'), 
               plot = plot_indicators_scaled, width = 10, height = 4.5, dpi = 125)
    
    
    
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# MAPS ----
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
map_combined <- tmap_arrange(map_redline, map_ces, map_overlap, map_holc_scores, nrow = 1)
# tmap_arrange(map_ces, map_holc_scores, nrow = 1)
# tmap_arrange(map_redline, map_holc_scores, nrow = 1)
tmap_save(tm = map_combined, 
          filename = here('_storymap', 'storymap_images', 'map_combined.png'), 
          width = 10, height = 4.0, dpi = 100)
