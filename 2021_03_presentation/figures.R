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


# load data ----
df_departure_scores <- st_read(here('data_processed-analysis',
                                    'departure-area_weighted_scores.gpkg'))
df_raw_scores <- st_read(here('data_processed-analysis',
                              'holc_area_weighted_scores.gpkg'))

# departure scores using the nearest centroid approach
df_raw_scores_centroid <- st_read(here('data_processed-analysis', 
                                       'holc_centroid_scores.gpkg'))

df_departure_scores_centroid <- st_read(here('data_processed-analysis', 
                                             'departure-centroid_scores.gpkg'))

ces_choices <- read_csv(here('data_processed', 
                             'ces_names.csv')) %>% 
    mutate(ces_variable = make_clean_names(name)) %>% #, 'parsed')) %>% 
    slice(8:64) %>%  # rows 8 to 64
    filter(type != 'other?')




# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# plot - translation method comparison
plot_method_comparison <- ggplot() +
    geom_point(aes(x = df_raw_scores$ces_3_score,
                   y = df_raw_scores_centroid$ces_3_score,
                   color = df_raw_scores$holc_grade)) + 
    geom_smooth(aes(x = df_raw_scores$ces_3_score,
                    y = df_raw_scores_centroid$ces_3_score), 
                method = 'lm') + 
    scale_color_manual(values = alpha(c('green', 'blue', 'yellow', 'red'), 0.5), 
                       name = 'HOLC Grade',
                       labels = c('A (Best)', 'B (Desirable)', 'C (Declining)', 'D (Hazardous)')) +
    labs(x = 'CalEnviroScreen Score (Area Weighted Average)',
         y = 'CalEnviroScreen Score (Nearest Centroid)'
    ) +
    geom_abline(slope = 1, linetype = 'dashed') +
    coord_fixed()
    
plot_method_comparison

ggsave(filename = here('2021_03_presentation', 'images', 'method_comparison.png'), 
       plot = plot_method_comparison, width = 5, height = 4, dpi = 125)




# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# generic CES departure scores boxplot function
fn_departures_boxplot <- function(plot_var) {
    measure_name <- ces_choices %>% filter(ces_variable == plot_var) %>% pull(name)
    ggplot(data = df_departure_scores, 
                             mapping = aes(x = holc_grade, 
                                           y = !!as.name(plot_var))) +
    geom_boxplot(aes(fill = holc_grade), notch = FALSE, outlier.shape = NA) + # notch = FALSE
    scale_fill_manual(values = alpha(c('green', 'blue', 'yellow', 'red'), 0.6), 
                      labels = c('A (Best)', 'B (Desirable)', 'C (Declining)', 'D (Hazardous)')) +
    geom_jitter(color='black', size=0.6, alpha=0.5, width = 0.2) +
    scale_x_discrete(limits = rev(levels(factor(df_departure_scores$holc_grade)))) +
    coord_flip() + # ylim = c(axis_min, axis_max)) +
    # NOTE: if not flipping the coordinates, use coord_cartesian(ylim = c(axis_min, axis_max))
    # theme(legend.position = "none") +
    labs(x = 'HOLC Grade', 
         y = glue('{measure_name} Relative to Respective City-Wide Average Score (Increasing Disadvantage\u2192)'),
         title = glue('{measure_name} for Neighborhoods in California Cities Assessed by the HOLC in the 1930s'), # Departure from Citywide Average, by HOLC Grade'),
         subtitle = 'Each point represents a neighborhood in the HOLC maps',
         caption = glue('Note: Higher {str_replace(measure_name, "Score", "score")}s indicate greater pollution burden and/or population vulnerability'),
         fill = 'HOLC Grade') +
    geom_blank()
}

plot_box_ces <- fn_departures_boxplot(plot_var = 'ces_3_score')

ggsave(filename = here('2021_03_presentation', 'images', 'box_ces_score.png'), 
       plot = plot_box_ces, width = 10, height = 4.5, dpi = 125)
    
# display the plot
plot_box_ces



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

# plot_point_raw <- fn_plot_point_raw(plot_var = 'ces_3_score', 
#                                     show_sd = TRUE, 
#                                     show_titles = TRUE, 
#                                     show_legend = TRUE, 
#                                     fixed_x = FALSE)
# ggsave(filename = here('2021_03_presentation', 'images', '2-results_by_city.png'), 
#        plot = plot_point_raw, width = 10, height = 4.5, dpi = 125)

plot_point_raw_noSD <- fn_plot_point_raw(plot_var = 'ces_3_score', 
                                         show_sd = FALSE, 
                                         show_titles = TRUE, 
                                         show_legend = TRUE, 
                                         fixed_x = FALSE)
ggsave(filename = here('2021_03_presentation', 'images', 'point_raw_score_by_city_noSD.png'), 
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
ggsave(filename = here('2021_03_presentation', 'images', '3-results_avg_by_city_and_grade.png'), 
       plot = plot_ces_avgScore_cityAndHOLC, width = 10, height = 4.5, dpi = 125)





##################################################3
# point plot - grouped by city - departure scores
fn_plot_point_departure <- function(plot_var, show_sd, show_titles, show_legend, fixed_x) {
        measure_name <- ces_choices %>% 
            filter(ces_variable == plot_var) %>% 
            pull(name)
        # make plot
        departure_scores_point <- ggplot(data = df_departure_scores %>% 
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
            xlab(glue('Adjusted {measure_name} (Increasing Disadvantage\u2192)')) +
            ylab('City') +
            geom_blank()
        
        if (fixed_x) {
            departure_scores_point <- departure_scores_point +
                scale_x_continuous(breaks = seq(0, 100, 20), limits = c(0, 90))
        }
        
        if (show_sd == TRUE) {
            departure_scores_point <- departure_scores_point + 
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
            departure_scores_point <- departure_scores_point + 
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
            departure_scores_point <- departure_scores_point +
                labs(title = glue('Adjusted {measure_name} for Neighborhoods in California Cities Assessed by the HOLC in the 1930s'),
                     caption = glue('Note: Higher {str_replace(measure_name, "Score", "score")}s indicate greater pollution burden and/or population vulnerability'),
                     subtitle = glue('Each colored point represents a neighborhood in the HOLC maps, and black lines represent the average adjusted {measure_name} \nof all neighborhoods assessed by the HOLC in the given city')) # 'and lines represent mean score +/- 1 standard deviation') +

        } else {
            departure_scores_point <- departure_scores_point +
                labs(title = glue('Adjusted {measure_name} for Neighborhoods in California Cities Assessed by the HOLC in the 1930s'),
                     caption = glue('Note: Higher {str_replace(measure_name, "Score", "score")}s indicate greater pollution burden and/or population vulnerability'),
                     subtitle = glue('Each colored point represents a neighborhood in the HOLC maps, and black dots/lines represent the adjusted mean {measure_name} \n+/- 1 standard deviation of all neighborhoods assessed by the HOLC in the given city')
                     ) # 'and lines represent mean score +/- 1 standard deviation') +

        }
    }
        
    if (show_legend) {
        departure_scores_point <- departure_scores_point + 
            theme(legend.position = 'right')
    } else {
        departure_scores_point <- departure_scores_point +
            theme(legend.position = 'none')
    }
}

# plot_point_departure <- fn_plot_point_departure(plot_var = 'ces_3_score', 
#                                                 show_sd = TRUE, 
#                                                 show_titles = TRUE, 
#                                                 show_legend = TRUE, 
#                                                 fixed_x = FALSE)
plot_point_departure_noSD <- fn_plot_point_departure(plot_var = 'ces_3_score', 
                                                     show_sd = FALSE, 
                                                     show_titles = TRUE, 
                                                     show_legend = TRUE, 
                                                     fixed_x = FALSE)
ggsave(filename = here('2021_03_presentation', 'images', 'plot_point_departure_noSD.png'), 
       plot = plot_point_departure_noSD, width = 10, height = 4.5, dpi = 125)




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



# # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# # make dot plot by indicator ----
#     plot_indicators_dot_facet <- ggplot(data = mean_scores_by_grade_metric %>% 
#                                         mutate(metric = str_replace_all(metric, '_', ' ')) %>% 
#                                         mutate(metric = str_replace(metric, '2 5', '2.5'))) +
#         geom_point(aes(x = score,
#                        y = '',
#                        color = holc_grade)) +
#         scale_color_manual(values = alpha(c('green', 'blue', 'orange', 'red'), 0.9),
#                            name = 'HOLC Grade') +
#         # scale_y_discrete(limits = rev(levels(factor(mean_scores_by_grade_metric$holc_grade)))) +
#         facet_wrap(~ metric, ncol = 4, scale = 'free') +
#         theme(axis.title.y = NULL) +
#         labs(x = glue('Mean Aggregated Departure Score'), 
#              y = '') + # 'HOLC Grade'),
#              # title = glue('HOLC Polygon Aggregated Indicator Scores (Area Weighted Average)'),
#              #subtitle = 'Scores are aggregated score versus average score of all polygons in respective city') +
#         geom_blank()
#     ggsave(filename = here('_storymap', 'storymap_images', 'indicators_dot_facet.png'), 
#            plot = plot_indicators_dot_facet, width = 10, height = 4.5, dpi = 125)

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
    ggsave(filename = here('2021_03_presentation', 'images', 'indicators_dot_scaled.png'), 
               plot = plot_indicators_scaled, width = 10, height = 4.5, dpi = 125)
    