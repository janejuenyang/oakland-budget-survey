################################################################################
# purpose: prep data for fy25-27 oakland, ca resident budget priorities survey
# last edited: mar 1, 2025
################################################################################

#### load packages and utility functions ####
# load packages
library(tidyverse)
library(googlesheets4)
library(janitor)
library(fs)
library(skimr)
library(polyglotr)

# load utility functions
source("code/utilities/structure-survey-data.r")
source("code/utilities/translate.r")

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

d_place_to_live_historical <- tibble(
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

d_approval_historical <- tibble(
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
     d_place_to_live_historical, d_approval_historical,
     file = "data/2025/raw/survey_raw.RData")

#### identify questions that require special handling in surveys ####
# identify residency screener question
q_screener <- "q1"

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
# structure survey data
d_survey_pp_zh <- d_survey_raw_zh %>% 
    preprocess_survey_data(
        screener_qid = q_screener,
        district_qid = q_district, 
        language_code = "zh", 
        multi_select_qids = q_multi
    )

# translate free-form responses using Google Translation
d_survey_totranslate_zh <- d_survey_pp_zh %>%
    filter(!is.na(response) & question %in% q_freeform)

d_survey_translated_zh <- d_survey_totranslate_zh %>%
    mutate(freeform_translation = translate_column(response, source_lang = "zh-CN")) %>%
    select(response_id, freeform_translation)

# combine translated free-form responses with rest of survey data
d_survey_zh <- d_survey_pp_zh %>%
    left_join(d_survey_translated_zh, by = "response_id") %>%
    mutate(r_en = if_else(
        question %in% q_freeform,
        freeform_translation,
        r_en
    )) %>%
    select(-freeform_translation)
    
#### prepare spanish responses ####
# structure survey data
d_survey_pp_es <- d_survey_raw_es %>% 
    preprocess_survey_data(
        screener_qid = q_screener,
        district_qid = q_district, 
        language_code = "es", 
        multi_select_qids = q_multi
    )

# translate free-form responses using Google Translation
d_survey_totranslate_es <- d_survey_pp_es %>%
    filter(!is.na(response) & question %in% q_freeform)

d_survey_translated_es <- d_survey_totranslate_es %>%
    mutate(freeform_translation = translate_column(response, source_lang = "es")) %>%
    select(response_id, freeform_translation)

# combine translated free-form responses with rest of survey data
d_survey_es <- d_survey_pp_es %>%
    left_join(d_survey_translated_es, by = "response_id") %>%
    mutate(r_en = if_else(
        question %in% q_freeform,
        freeform_translation,
        r_en
    )) %>%
    select(-freeform_translation)

#### prepare english responses ####
# structure survey data
d_survey_en <- d_survey_raw_en %>% 
    # make sure district question comes in as a character vector, not list
    mutate(`14. Which district do you live in?` = as.character(`14. Which district do you live in?`)) %>%
    preprocess_survey_data(
        screener_qid = q_screener,
        district_qid = q_district, 
        language_code = "en", 
        multi_select_qids = q_multi
    ) %>%
    # match columns to spanish and chinese survey processed results
    rename(r_en = response_en) %>%
    mutate(
        r_zh = NA,
        r_es = NA,
        # consolidate final question verbiage with pre-finalized question verbiage
        # context note: before wide-scale outreach on the survey, the BAC circulated
        #               a draft for feedback on the questions from various organizations
        #               and networks. as a result, the raw data includes some options
        #               with slight variations in the wording.
        r_en = case_when(
            question == "q7" & 
                r_en == "Expand the use of civilian teams, instead of police, to respond to calls where no threat or harm" ~
                "Use more civilian staff - not sworn police officers - to respond to calls",
            question == "q7" &
                r_en == "Use of civilian staff - not sworn police officers - to respond to calls" ~
                "Use more civilian staff - not sworn police officers - to respond to calls",
            question == "q8" & 
                r_en == "Reduce funding for cultural programs" ~
                "Reduce funding for cultural programs and art organizations",
            question == "q9" & 
                r_en == "Slow down investments to increase accessibility and safety of sidewalks" ~
                "Reduce accessibility and safety of sidewalks",
            question == "q9" & 
                r_en == "Slow down street repaving and traffic light improvements" ~
                "Reduce street paving and traffic light improvements",
            question == "q10" & 
                r_en == "Defer improvements and maintenance for libraries" ~
                "Reduce improvements and maintenance for libraries",
            question == "q10" & 
                r_en == "Defer improvements and maintenance frequency for parks and recreational facilities" ~
                "Reduce parks and recreational facilities maintenance",
            question == "q10" & 
                r_en == "Increase fees for youth programming such as camps" ~
                "Reduce youth programming such as after school programs and summer camps",
            # one case -- original range was narrower
            question == "q15" & 
                r_en == "3-5 years" ~
                "4-7 years",
            # one case -- original range was broader
            question == "q15" & 
                r_en == "5-10 years" ~
                "4-7 years",
            TRUE ~ r_en
        )
    ) %>%
    select(timestamp:r_en, r_zh, r_es, response_id)

#### create final processed data ####
# combine survey responses into single dataframe
d_survey_combined <- bind_rows(d_survey_zh, d_survey_es, d_survey_en) %>%
    filter(
        # remove rows for unanswered questions
        !is.na(response)
    )

# categorize responses to questions with "other" response option
# and include separate column for full provided response
d_survey_other_responses_grouped <- d_survey_combined %>%
    mutate(
        other_detail = if_else(
            question %in% q_other & !(r_en %in% d_rmap$r_en[d_rmap$question %in% q_other]),
            r_en,
            NA_character_
        ),
        r_en = if_else(
            question %in% q_other & !(r_en %in% d_rmap$r_en[d_rmap$question %in% q_other]),
            "Other Response",
            r_en
        ),
        is_writein = case_when(
            !is.na(is_writein) ~ is_writein,
            !is.na(other_detail) & r_en == "Other Response" ~ TRUE,
            TRUE ~ FALSE
        )
    )

# pivot demographic responses for easier population segment analysis
d_survey <- d_survey_other_responses_grouped %>%
    # create a copy of questions and r_en to pivot from
    mutate(
        pivot_names = question,
        pivot_values = r_en
    ) %>%
    pivot_wider(
        names_from = pivot_names,
        values_from = pivot_values
    ) %>%
    select(timestamp:other_detail, all_of(q_demographics)) %>%
    group_by(respondent_id) %>%
    fill(all_of(q_demographics), .direction = "updown") %>%
    ungroup() %>%
    mutate(across(all_of(q_demographics), ~replace_na(., "No Response"))) %>%
    # final clean-up and translation district names
    mutate(
        # handle missing responses
        district = if_else(
            district == "NULL" | is.na(district), 
            "No Response", 
            district
        ),
        # translate to English
        district = str_replace(district, "Distrito", "District"),
        district = str_replace(district, "第", "District"),
        district = str_replace(district, " 区", ""),
    ) %>%
    mutate(
        # flag responses from people living outside (collected on paper, entered
        # after online responses were closed)
        is_unhoused_respondent = timestamp > ymd("2025-02-06")
    ) %>%
    # rename demographic columns
    rename(
        oakland_tenure = q15,
        age = q16,
        race_ethnicity = q17,
        gender = q18,
        education = q19
    ) %>%
    # reorder columns
    select(
        timestamp, respondent_id, is_unhoused_respondent, question, response, 
        is_writein, other_detail, response_id, survey_language, starts_with("r_"),
        district, oakland_tenure, age, race_ethnicity, gender, education,
        ends_with("_group")
    )

# save combined response dataframe as .csv and .Rdata with other dataframes
save(d_survey, d_place_to_live_historical, d_approval_historical,d_qmap, d_rmap,
     file = "data/2025/processed/survey_processed.RData")
write_csv(d_survey, file = "data/2025/processed/survey_processed.csv")

# save question and response maps as .csv
write_csv(d_qmap, file = "data/2025/processed/qmap.csv")
write_csv(d_rmap, file = "data/2025/processed/rmap.csv")
