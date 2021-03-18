library(sf)
library(ggplot2)
library(here)
library(dplyr)
library(readr)
library(janitor)
library(glue)
library(stringr)

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

# plot translation method comparison

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
