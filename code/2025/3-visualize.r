################################################################################
# purpose: visualize weighted responses
# last edited: feb 7, 2025
################################################################################

#### load packages, utility functions, and data ####
# load packages
library(tidyverse)
library(skimr)
library(patchwork)
library(scales)
library(ggtext)

# load utility functions
source("code/utilities/ggplot-settings.r")
source("code/utilities/plot-survey-data.r")

# load data
load("data/2025/processed/survey_weighted.Rdata")

#### create overall summaries by question ####
# create vector of questions
q <- d_qmap$question

# calculate summary tables per question
q %>%
    walk(
        ~assign(paste0("s_", .x),
            summarize_distributions(
                df = d_survey %>% filter(question == .x), 
                group_var = "r_en"
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
            legend.background = element_rect(color = "black", linewidth = 0.5),
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
    )

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
# for initial discussion draft, included table only
s_q2
s_q3

# TODO: make alluvial charts for overall
# TODO: segment by 

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
# define segments
segment_vars <- c("district", "oakland_tenure", "age", "age_group", "gender",
                  "race_ethnicity","race_group", "education", "education_group")

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

#### save objects ####
save.image("data/2025/visualized/survey_viz.Rdata")
