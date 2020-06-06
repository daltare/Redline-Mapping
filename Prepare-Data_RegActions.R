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
            mutate(site_id = factor(site_id)) %>%
            {.}
            # check (NOTE: no missing coordinate data in the flat file)
                # range(calepa_reg_sites$latitude)
                # range(calepa_reg_sites$longitude)


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
                mutate(site_id = factor(site_id, levels = calepa_reg_sites$site_id)) %>%
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
                relocate(action, .after = action_date)
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
                mutate(site_id = factor(site_id, levels = calepa_reg_sites$site_id)) %>%
                {.}  
            violations_summary <- violations_all_download %>% 
                select(site_id, violation_date, citation, 
                       violation_division, violation_program, violation_source) %>% 
                mutate(action = 'violation') %>% 
                rename(action_date = violation_date,
                       type = citation,
                       division = violation_division,
                       program = violation_program,
                       source = violation_source) %>% 
                relocate(action, .after = action_date)
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
                mutate(site_id = factor(site_id, levels = calepa_reg_sites$site_id)) %>%
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
                relocate(action, .after = action_date)

        rm(list = c('inspections_all_download', 'violations_all_download', 'enforcement_all_download'))            
        gc()
        
    reg_actions_all <- bind_rows(inspections_summary, violations_summary, enforcement_summary)    
        rm(list = c('inspections_summary', 'violations_summary', 'enforcement_summary'))            
        gc()
        
       
    reg_actions_all <- reg_actions_all %>% left_join(calepa_reg_sites)
        rm('calepa_reg_sites')
        gc()
        
    # join the CES data to get the CES score
        
    # join the redline data to get the HOLC grade
        
    # save file to csv
        write_csv(x = reg_actions_all, 
                  path = here('data_prepared',
                              'regulatory_actions.csv'))

# test (to be done in app)
    tic()
    reg_actions_summary <- reg_actions_all %>% 
        # filter(action_date >= as.Date('2018-07-01'), action_date <= as.Date('2019-06-30')) %>% 
        group_by(site_id, action, site_name, 
                 address, city, zip_code, 
                 county, latitude, longitude, 
                 state, data_source) %>% 
        summarize(count = n()) %>% 
        pivot_wider(names_from = action, values_from = count) %>% 
        rename(inspections_count = inspection,
               violations_count = violation,
               enforcement_actions_count = enf_action) %>% 
        relocate(inspections_count, violations_count, enforcement_actions_count, .after = site_id) %>%
        ungroup() %>%
        {.}
    toc()
    
    # replace NAs with zeros
    tic()
        reg_actions_summary <- reg_actions_summary %>% mutate_if(is.integer, ~replace(., is.na(.), 0))
    toc()

    # find sites in the sites dataset that don't have any regulatory actions and add a zero record to make sure they are included
        tf <- calepa_reg_sites$site_id %in% reg_actions_summary$site_id
        sum(tf)
        sum(!tf)
        
        missing_sites <- calepa_reg_sites %>% 
            filter(!(site_id %in% reg_actions_summary$site_id)) %>% 
            mutate(inspections_count = 0,
                   violations_count = 0,
                   enforcement_actions_count = 0)
        
    # bind the sites with no reg actions to the dataset (this is the dataset to use)
        reg_actions_revised <- bind_rows(reg_actions_summary, missing_sites) %>% 
            arrange(site_id)
            View(reg_actions_revised)
    