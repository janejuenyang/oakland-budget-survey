################################################################################
# purpose: define functions for commonly-used plots 
# last edited: feb 6, 2025
################################################################################

#' Summarize Distributions
#'
#' @param df Data frame containing survey responses
#' @param group_var String naming the grouping variable
#' @param wt_var String naming the weight variable, defaults to "wt"
#' @return Tibble with counts and weighted sums by group
#' @examples
#' summarize_distributions(survey_df, "age_group")
#' summarize_distributions(survey_df, "education", "sample_weight")
summarize_distributions <- function(df, group_var, segment_var = NULL, wt_var = "wt") {
    group_sym <- sym(group_var)
    wt_sym <- sym(wt_var)
    
    if (is.null(segment_var)) {
        res <- df %>%
            group_by(!!group_sym) %>%
            summarize(
                raw_count = n(),
                weighted_count = sum(!!wt_sym),
                .groups = "drop"
            ) %>%
            mutate(
                raw_pct = raw_count/sum(raw_count),
                weighted_pct = weighted_count/sum(weighted_count)
            )
    } else {
        segment_sym <- sym(segment_var)
        
        res <- df %>%
            group_by(!!segment_sym, !!group_sym) %>%
            summarize(
                raw_count = n(),
                weighted_count = sum(!!wt_sym),
                .groups = "drop"
            ) %>%
            group_by(!!segment_sym) %>%
            mutate(
                raw_pct = raw_count/sum(raw_count),
                weighted_pct = weighted_count/sum(weighted_count),
            ) %>%
            ungroup()
    }
    
    return(res)
}

#' Plot a distribution using a column chart
#' 
#' @param df A data frame containing the variables to plot
#' @param x Character. Name of the x-axis variable
#' @param y Character. Name of the y-axis variable 
#' @param threshold Numeric. Threshold below which labels appear outside bars (default 0.10)
#' @param title Optional character. Plot title
#' @param subtitle Optional character. Plot subtitle
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' df <- data.frame(
#'   r_en = letters[1:5],
#'   weighted_pct = c(0.2, 0.3, 0.15, 0.25, 0.1)
#' )
#' plot_distribution(df)
plot_distribution <- function(df, x = "r_en", y = "weighted_pct",
    threshold = 0.10, title = NULL, subtitle = NULL) {
    
    # create symbols for tidy evaluation
    x_sym <- sym(x)
    y_sym <- sym(y)
    
    # arrange data for presentation
    df <- df %>%
        mutate(
            # sort data by y value
            !!x_sym := fct_reorder(!!x_sym, !!y_sym),
            # add label position logic
            label_pos = if_else(!!y_sym < threshold, 
                                !!y_sym + 0.01,  # outside
                                !!y_sym - 0.01),  # inside
            label_hjust = if_else(!!y_sym < threshold,
                                  -0.1,  # outside
                                  1.1)   # inside
        )
    
    # plot
    res <- ggplot(
            data = df, 
            mapping = aes(
                x = !!x_sym,
                y = !!y_sym,
            )
        ) +
        # bar chart
        geom_col(fill = "lightblue") +
        # text label of percent, positioned inside the bar by default
        # but outside for very small values
        geom_text(
            aes(
                label = scales::percent(!!y_sym, accuracy = 1),
                y = label_pos,
                hjust = label_hjust
            ),
            size = 3.5
        ) +
        # format axes and title
        scale_x_discrete(label = function(x) str_wrap(x, width = 30)) +
        scale_y_continuous(
            labels = percent,
            # expand y-axis slightly to accommodate outside labels
            expand = expansion(mult = c(0, 0.1))
        ) +
        theme(axis.text.x = element_blank(), 
              axis.ticks.x = element_blank()) + 
        labs(title = title, subtitle = subtitle) +
        coord_flip()
    
    return(res)
}

#' Plot faceted distribution of survey responses with reference lines
#' 
#' @param df Data frame containing survey responses and segments
#' @param x Character string naming the x-axis variable, defaults to "r_en"
#' @param y Character string naming the y-axis variable, defaults to "weighted_pct"
#' @param segment_var Character string naming the segmentation variable
#' @param threshold Numeric value for label position threshold, defaults to 0.10
#' @param title Optional plot title
#' @param subtitle Optional plot subtitle
#' @return A ggplot object
#' @import ggplot2 dplyr stringr scales
#' @export
plot_distribution_segmented <- function(df, x = "r_en", y = "weighted_pct",
    segment_var, threshold = 0.10, title = NULL, subtitle = NULL) {
    
    x_sym <- sym(x)
    y_sym <- sym(y)
    segment_sym <- sym(segment_var)
    
    # prepare data
    plot_data <- df %>%
        mutate(
            !!x_sym := fct_reorder(!!x_sym, !!y_sym),
            label_pos = if_else(!!y_sym < threshold, 
                                !!y_sym + 0.01,
                                !!y_sym - 0.01),
            label_hjust = if_else(!!y_sym < threshold,
                                  -0.1,
                                  1.1)
        )
    
    # get overall distribution for reference
    overall_dist <- plot_data %>%
        group_by(!!x_sym) %>%
        summarize(
            reference_pct = mean(!!y_sym),
            .groups = "drop"
        )
    
    # calculate number of facets for layout
    n_facets <- n_distinct(plot_data[[segment_var]])
    n_cols <- min(2, n_facets)
    
    # plot
    ggplot(plot_data, aes(x = !!x_sym, y = !!y_sym)) +
        # bars
        geom_col(fill = "lightblue") +
        # labels
        geom_text(
            aes(
                label = percent(!!y_sym, accuracy = 1),
                y = label_pos,
                hjust = label_hjust
            ),
            size = 3
        ) +
        # Reference line segments
        geom_segment(
            data = overall_dist,
            # aes(x = 0, xend = reference_pct,
            #     y = as.numeric(!!x_sym), yend = as.numeric(!!x_sym)),
            aes(
                x = as.numeric(!!x_sym) - 0.45,
                xend = as.numeric(!!x_sym) + 0.45,
                y = reference_pct,
                yend = reference_pct,
                linetype = "overall"
            ),
            color = "black"
        ) +
        # facet by segment categories
        facet_wrap(
            vars(!!segment_sym), 
            scales = "free_y",
            ncol = n_cols,
            strip.position = "top"
        ) +
        # format
        theme(
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            strip.text = element_text(size = 8),
            # remove y-axis text from interior facets
            axis.text.y = element_text(size = 8),
            panel.spacing.x = unit(1, "lines")
        ) +
        scale_x_discrete(label = function(x) str_wrap(x, width = 30)) +
        scale_y_continuous(
            labels = percent,
            expand = expansion(mult = c(0, 0.1))
        ) +
        labs(title = title, subtitle = subtitle) +
        coord_flip()
}

#' Generate, save, and store plots for all segment combinations of a question
#' 
#' @param question_id Character string identifying the survey question
#' @param segment_vars Character vector of segmentation variable names
#' @param env Environment to store plots in, defaults to global environment
#' @return NULL, called for side effects (saving plots)
#' @import purrr ggplot2
#' @export
plot_all_segments <- function(question_id, segment_vars, env = .GlobalEnv) {
    plots <- walk(segment_vars, function(seg_var) {
        data_name <- paste0("s_", question_id, "_by_", seg_var)
        plot_data <- get(data_name)
        
        # plot
        p <- plot_distribution_segmented(
            df = plot_data,
            segment_var = seg_var,
            title = paste("Response Distribution by", str_to_title(seg_var)),
        )
        
        ggsave(
            filename = paste0("output/2025/", question_id, "_by_", seg_var, ".png"),
            plot = p,
            width = 12,
            height = 8
        )
        
        # assign plot to environment with descriptive name
        plot_name <- paste0("g_", question_id, "_by_", seg_var)
        assign(plot_name, p, envir = env)
        
        p
    })
    
    # Name list elements
    names(plots) <- paste0("g_", question_id, "_by_", segment_vars)
    
    plots
}
