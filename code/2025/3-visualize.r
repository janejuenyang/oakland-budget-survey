################################################################################
# purpose: visualize weighted responses
# last edited: feb 22, 2025
################################################################################

#### load packages, utility functions, and data ####
# load packages
library(tidyverse)
library(skimr)
library(patchwork)
library(scales)
library(ggtext)
library(paletteer)

# load utility functions
source("code/utilities/ggplot-settings.r")
source("code/utilities/plot-survey-data.r")

# load data
load("data/2025/processed/survey_weighted.Rdata")

# define variables to segment results by
segment_vars <- c("district", "oakland_tenure", "age", "age_group", "gender",
                  "race_ethnicity","race_group", "education", "education_group")

#### create overall summaries by question ####
# create vector of questions
q <- d_qmap$question

# calculate summary tables per question that is already by-respondent
q %>%
    walk(
        ~assign(paste0("s_", .x),
            summarize_distributions(
                df = d_survey %>% filter(question == .x), 
                group_var = "r_en",
                wt_var = "wt",
                count_type = "n"
            ),
            envir = .GlobalEnv
        )
    )

# for multi-select questions, re-do summary according to distinct respondents
q_multiselect <- c("q5", "q6")
q_multiselect %>%
    walk(
        ~assign(paste0("s_", .x),
                summarize_distributions(
                    df = d_survey %>% filter(question == .x), 
                    group_var = "r_en",
                    wt_var = "wt",
                    count_type = "n_distinct"
                ),
                envir = .GlobalEnv
        )
    )

#### demographics ####
# create vector of demographic categories
demographic_vars <- names(d_respondents %>% select(-respondent_id, -wt))

# calculate summary tables per demographic category
demographic_vars %>%
    walk(
        ~assign(paste0("s_", .x),
                summarize_distributions(
                    df = d_respondents, 
                    group_var = .x
                ),
                envir = .GlobalEnv
        )
    )

# add census bureau percent distributions to each summary table
demographic_vars %>%
    walk(~{
        table_name <- paste0("s_", .x)
        target_dist <- distributions[[.x]]
        
        get(table_name) %>%
            mutate(census_pct = target_dist[!!sym(.x)]) %>%
            assign(table_name, ., envir = .GlobalEnv)
    })

# map titles against the variable names
demographic_titles <- c(
    district = "District",
    age_group = "Age",
    gender = "Gender",
    race_group = "Race/Ethnicity", 
    education_group = "Education"
)

# plot
demographic_plots <- demographic_vars %>%
    # name vector elements
    set_names() %>%
    # create list of demographic summary tables
    map(~get(paste0("s_", .x))) %>%
    # for each summary table, create bar chart
    map2(demographic_vars, ~ggplot(.x) +
        # wide, light blue bar for raw count of respondents
        geom_col(
            aes(
                x = !!sym(.y), 
                y = raw_pct, 
                fill = "Raw"
            ), 
            width = 0.8
        ) +
        # narrower dark blue bar for weighed responses
        geom_col(
            aes(
                x = !!sym(.y), 
                y = weighted_pct, 
                fill = "Weighted"), 
            width = 0.5,
            alpha = 0.8,
        ) +
        # add horizontal lines showing the census distributions
        geom_segment(
            aes(
                x = as.numeric(!!sym(.y)) - 0.45,
                xend = as.numeric(!!sym(.y)) + 0.45,
                y = census_pct,
                yend = census_pct,
                linetype = "Census"
            ),
            color = "black",
            linewidth = 1
        ) +
        scale_fill_manual(
            values = c(
                "Weighted" = "darkblue", 
                "Raw" = "lightblue"
            )
        ) +
        # format axes and title
        scale_x_discrete(label = function(x) str_wrap(x, width = 10)) +
        scale_y_continuous(labels = percent) +
        labs(title = demographic_titles[.y]) +
        theme(
            legend.position = "top",
            legend.background = element_rect(fill = "seashell", linewidth = 0),
            legend.margin = margin(4, 4, 4, 4)
        ) +
        guides(
            fill = guide_legend(title = NULL),
            linetype = guide_legend(title = NULL)
        )
    )

combined_demographic_plots <- wrap_plots(demographic_plots) +
    plot_layout(ncol = 2, guides = "collect") +
    plot_layout(guides = "collect") +
    plot_annotation(
        theme = theme(
            legend.position = "top",
            legend.justification = "right",
        )
    ); combined_demographic_plots

# save plot
ggsave(
    plot = combined_demographic_plots, 
    filename = "output/2025/demographics.png",
    width = 14,
    height = 7,
    units = "in",
    dpi = 300
)

#### longitudinal ####
# define order of responses
l_years <- c(2000, 2002, 2005, 2015, 2017, 2018, 2020, 2022, 2025)
l_q2_responses <- c("Excellent", "Good", "Don't know", "Fair", "Poor")
l_q3_responses <- c("Strongly approve", "Somewhat approve", 
                    "Don't know",
                    "Somewhat disapprove", "Strongly disapprove")

# append 2025 data
q2_text <- "Generally speaking, how would you rate Oakland as a place to live?"
s_q2_2025 <- s_q2 %>%
    mutate(
        question = "q2",
        question_text = q2_text,
        year = 2025
    ) %>%
    select(question, question_text, year, response = r_en, pct = weighted_pct) %>%
    add_row(
        question = "q2",
        question_text = q2_text,
        year = 2025,
        response = "Don't know",
        pct = 0
    )

d_place_to_live <- bind_rows(d_place_to_live_historical, s_q2_2025) %>%
    mutate(
        question_text = str_squish(question_text),
        response = fct_relevel(response, l_q2_responses)
    )

q3_text <- "Do you approve or disapprove of the overall job being done by Oakland City government in providing services to the people who live here?"
s_q3_2025 <- s_q3 %>%
    mutate(
        question = "q3",
        question_text = q3_text,
        year = 2025
    ) %>%
    select(question, question_text, year, response = r_en, pct = weighted_pct) %>%
    add_row(
        question = "q3",
        question_text = q3_text,
        year = 2025,
        response = "Don't know",
        pct = 0
    )

d_approval <- bind_rows(d_approval_historical, s_q3_2025) %>%
    mutate(
        question_text = str_squish(question_text),
        response = fct_relevel(response, l_q3_responses)
    )

# create and save overall stacked bar chart showing change over time
longitudinal_plots <- map(list(d_place_to_live, d_approval), plot_stacked_trend)
q_longitudinal <- c("q2", "q3")

walk2(longitudinal_plots, q_longitudinal, ~ggsave(
    filename = paste0("output/2025/", .y, ".png"),
    plot = .x,
    width = 14,
    height = 7,
    units = "in",
    dpi = 300
))

# create and save 2025's segmented results
q_segment_combos <- expand_grid(q = q_longitudinal, segment = segment_vars) 

pwalk(q_segment_combos,
      ~assign(paste0("s_", .x, "_by_", .y),
              summarize_distributions(
                  df = d_survey %>% filter(question == .x),
                  group_var = "r_en",
                  segment_var = .y
              ),
              envir = .GlobalEnv
      )
)

# create segmented plots
walk(q_longitudinal, ~plot_all_segments(.x, segment_vars))

#### overall bar charts ####
# create vector of questions to plot as bar
q_bar <- paste0("q", seq(5, 12))

# create overall plots of each
bar_plots <- q_bar %>% 
    map(~get(paste0("s_", .x))) %>%
    map(~plot_distribution(.x))

# save plots
walk2(bar_plots, q_bar, ~ggsave(
    filename = paste0("output/2025/", .y, ".png"),
    plot = .x,
    width = 5,
    height = 5,
    units = "in",
    dpi = 300
))

#### bar charts by segment, with overall reference ####
# create all combinations of questions and segments
q_segment_combos <- expand_grid(q = q_bar, segment = segment_vars) 

# create segmented summary tables
pwalk(q_segment_combos,
    ~assign(paste0("s_", .x, "_by_", .y),
        summarize_distributions(
            df = d_survey %>% filter(question == .x),
            group_var = "r_en",
            segment_var = .y
        ),
        envir = .GlobalEnv
    )
)

# create segmented plots
walk(q_bar, ~plot_all_segments(.x, segment_vars))


#### weighted vs. unweighted (overall and by segment) ####

#### unweighted subgroups (district, age group, people living outside) ####
#### save objects ####
save.image("data/2025/visualized/survey_viz.Rdata")
