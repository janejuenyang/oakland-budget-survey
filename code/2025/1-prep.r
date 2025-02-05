################################################################################
# purpose: prep data for fy25-27 oakland, ca resident budget priorities survey
# last edited: feb 4, 2025
# TODO: 
# 1. handle "other" and demographic questions
# 2. create script for weighting
# 3. create script for visualizing
################################################################################

#### load packages and utility functions ####
# load packages
library(tidyverse)
library(googlesheets4)
library(janitor)
library(ggrepel)
library(ggtext)
library(paletteer)
library(scales)
library(gt)
library(fs)
library(janitor)
library(skimr)
library(polyglotr)

# load utility functions
files_utilities <- dir_ls("code/utilities")
map(files_utilities, source)

#### import data ####
# the responses are saved in google sheets that are publicly available
# so no authentication into google drive is needed
gs4_deauth()

# note: to view the google sheets of raw data, append the sheet ids to the
#   base url "https://docs.google.com/spreadsheets/d/"
# question and response maps
ssid_survey <- as_sheets_id("1GAEqgWN8lUrdYzCS4jI3F4-Uncw5je1PPrI8K3IvVNw")
d_qmap <- read_sheet(ssid_survey, sheet = "questions")
d_rmap <- read_sheet(ssid_survey, sheet = "response_options")
d_rmap_long <- d_rmap %>% pivot_longer(cols = starts_with("r_"))

# english survey
ssid_en <- as_sheets_id("1ndq2wPKhRmZyfpY7266vrZOsl1lzZpaUUSgAtbJCzhw")
d_survey_raw_en <- read_sheet(ssid_en)

# spanish survey
ssid_es <- as_sheets_id("1-852noYDoPOoRzPE5UQemP6sR5BkHVgwgt081pbNJd0")
d_survey_raw_es <- read_sheet(ssid_es)

# chinese survey
ssid_zh <- as_sheets_id("1dFOjTR1A87weC3rGDdU2JyU58twO4TyI4V2im_PuhhM")
d_survey_raw_zh <- read_sheet(ssid_zh)

# create dataset of trend questions
# reference data: p10 and p13 of 
#   https://cao-94612.s3.us-west-2.amazonaws.com/documents/FY-2022-23-Oakland-Budget-Priorities-Survey-Presentation.pdf
l_years <- c(2000, 2002, 2005, 2015, 2017, 2018, 2020, 2022)
l_q2_responses <- c("Excellent", "Good", "Fair", "Poor", "Don't know")
l_q3_responses <- c("Strongly approve", "Somewhat approve", 
    "Somewhat disapprove", "Strongly disapprove", "Don't know")

d_place_to_live <- tibble(
    question = "q2",
    question_text = "Generally speaking, how would you rate Oakland as a place 
        to live?",
    year = rep(l_years, times = length(l_q2_responses)) %>% sort(),
    response = rep(l_q2_responses, times = length(l_years)),
    pct = c(
        0.18, 0.47, 0.28, 0.06, 0.01, # 2000
        0.19, 0.45, 0.27, 0.08, 0.01, # 2002
        0.19, 0.42, 0.30, 0.08, 0.01, # 2005
        0.26, 0.44, 0.21, 0.09, 0.00, # 2015; "fair" reduced by 0.01 (rounding)
        0.27, 0.43, 0.21, 0.08, 0.01, # 2017
        0.16, 0.48, 0.27, 0.08, 0.01, # 2018
        0.13, 0.46, 0.26, 0.14, 0.01, # 2020
        0.10, 0.44, 0.30, 0.15, 0.01  # 2022
    )
)

d_approval <- tibble(
    question = "q3",
    question_text = " Do you approve or disapprove of the overall job being
        done by Oakland City government in providing services to the people who 
        live here?",
    year = rep(l_years, times = length(l_q3_responses)) %>% sort(),
    response = rep(l_q3_responses, times = length(l_years)),
    pct = c( # note: all rounded to nearest 1%
        0.04, 0.30, 0.49, 0.13, 0.04, # 2000
        0.05, 0.35, 0.42, 0.14, 0.04, # 2002
        0.02, 0.28, 0.49, 0.18, 0.03, # 2005
        0.04, 0.28, 0.44, 0.18, 0.06, # 2015
        0.04, 0.31, 0.41, 0.20, 0.04, # 2017
        0.01, 0.23, 0.50, 0.22, 0.04, # 2018
        0.06, 0.35, 0.24, 0.27, 0.08, # 2020
        0.03, 0.27, 0.29, 0.34, 0.07  # 2022
    )
)

# save all raw survey data
save(d_survey_raw_en, d_survey_raw_es, d_survey_raw_zh, 
     file = "data/2025/raw/survey_raw.RData")

#### identify questions that require special handling in surveys ####
# identify multi-select questions with list of question IDs with their options
q_multi <- list(
    "q5" = d_rmap_long %>% filter(question == "q5") %>% pull(value), 
    "q6" = d_rmap_long %>% filter(question == "q6") %>% pull(value)
)

# identify demographic questions for segmenting results
# exclude "district" (handled separately)
q_demographics <- rep(paste0("q", 15:19))

# identify district question
q_district <- "q14"

# identify questions with "other" options
q_other <- c("q5", "q6", "q7", "q11", "q17", "q18")

# identify free-form response questions
q_freeform <- c("q4", "q13")

#### prepare chinese responses ####
d_survey_pp_zh <- d_survey_raw_zh %>% 
    preprocess_survey_data(
        district_qid = q_district, 
        language_code = "zh", 
        multi_select_qids = q_multi
    )

d_survey_totranslate_zh <- d_survey_pp_zh %>%
    filter(!is.na(response) & question %in% q_freeform)

d_survey_translated_zh <- d_survey_totranslate_zh %>%
    mutate(freeform_translation = translate_column(response, source_lang = "zh-CN")) %>%
    select(response_id, freeform_translation)

d_survey_zh <- d_survey_pp_zh %>%
    left_join(d_survey_translated_zh, by = "response_id") %>%
    mutate(r_en = if_else(
        question %in% q_freeform,
        freeform_translation,
        r_en
    )) %>%
    select(-freeform_translation)
    
#### prepare spanish responses ####
d_survey_pp_es <- d_survey_raw_es %>% 
    preprocess_survey_data(
        district_qid = q_district, 
        language_code = "es", 
        multi_select_qids = q_multi
    )

d_survey_totranslate_es <- d_survey_pp_es %>%
    filter(!is.na(response) & question %in% q_freeform)

d_survey_translated_es <- d_survey_totranslate_es %>%
    mutate(freeform_translation = translate_column(response, source_lang = "es")) %>%
    select(response_id, freeform_translation)

d_survey_es <- d_survey_pp_es %>%
    left_join(d_survey_translated_es, by = "response_id") %>%
    mutate(r_en = if_else(
        question %in% q_freeform,
        freeform_translation,
        r_en
    )) %>%
    select(-freeform_translation)

#### prepare english responses ####
d_survey_en <- d_survey_raw_en %>% 
    # make sure district question comes in as a character vector, not list
    mutate(`14. Which district do you live in?` = as.character(`14. Which district do you live in?`)) %>%
    preprocess_survey_data(
        district_qid = q_district, 
        language_code = "en", 
        multi_select_qids = q_multi
    ) %>%
    # match columns to spanish and chinese processed results
    rename(r_en = response_en) %>%
    mutate(
        r_zh = NA,
        r_es = NA,
    ) %>%
    select(timestamp:r_en, r_zh, r_es, response_id)

#### combine survey responses into single dataframe ####
d_survey <- bind_rows(d_survey_zh, d_survey_es, d_survey_en)

# save combined response dataframe as .csv and .Rdata with other dataframes
save(d_survey, d_place_to_live, d_approval,d_qmap, d_rmap,
     file = "data/2025/processed/survey_processed.RData")
write_csv(d_survey, file = "data/2025/processed/survey_processed.csv")

# save question and response maps as .csv
write_csv(d_qmap, file = "data/2025/processed/qmap.csv")
write_csv(d_rmap, file = "data/2025/processed/rmap.csv")
