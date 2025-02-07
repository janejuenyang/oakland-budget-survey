################################################################################
# purpose: visualize weighted responses
# last edited: feb 6, 2025
# analyze by:
#   total
#   segment: district
#   segment: age - youth, adults, older adults
#   segment: tenure living in Oakland
#   segment: gender
#   segment: race/ethnicity
#   segment: education
################################################################################

#### load packages, utility functions, and data ####
# load packages
library(tidyverse)
library(skimr)
library(patchwork)
library(ggrepel)
library(ggtext)
library(paletteer)
library(scales)
library(gt)

# load utility functions
source("code/utilities/ggplot-settings.r")
source("code/utilities/plot-survey-data.r")

# load data
load("data/2025/processed/survey_weighted.Rdata")

#### demographics ####
# create vector of demographic categories
demographic_vars <- names(d_respondents %>% select(-respondent_id, -wt))

# create summary tables of unweighted and weighted respondents for each 
# demographic category. save each summary table to an object with name 
# pattern `s_variable`
demographic_vars %>%
    walk(
        ~assign(paste0("s_", .x),
                summarize_distributions(df = d_respondents, group_var = .x),
            envir = .GlobalEnv)
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
# TODO: make pretty charts; for initial discussion draft, included table only
q_longitudinal <- c("q2", "q3")

#### great place x budget impact ####

#### tough choices and sales tax ####