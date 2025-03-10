################################################################################
# purpose: define functions for commonly-used plots 
# last edited: mar 1, 2025
################################################################################

# TODO: tidy up roxygen2 documentation

#' Summarize Distributions
#'
#' @param df Data frame containing survey responses
#' @param group_var String naming the grouping variable
#' @param segment_var String naming the segmentation variable (optional)
#' @param wt_var String naming the weight variable, defaults to "wt"
#' @param count_type String specifying count type ("n" or "n_distinct")
#' @return Tibble with counts and weighted sums by group
#' @examples
#' summarize_distributions(survey_df, "age_group")
#' summarize_distributions(survey_df, "education", wt_var = "sample_weight")
summarize_distributions <- function(
        df, 
        group_var, 
        segment_var = NULL, 
        wt_var = "wt",
        count_type = c("n", "n_distinct")
    ) {
    # evaluate provided parameters
    count_type <- match.arg(count_type)
    
    # calculate number of respondents
    n_respondents <- length(unique(df$respondent_id))

    # # calculate summary
    if (is.null(segment_var)) {
        res <- df |>
            group_by(across(all_of(group_var))) |>
            summarise(
                raw_count = if_else(count_type == "n", n(), n_distinct(respondent_id)),
                weighted_count = sum(get(wt_var)),
                .groups = "drop"
            ) |>
            mutate(
                raw_pct = raw_count/if_else(count_type == "n", sum(raw_count),
                    n_respondents),
                weighted_pct = weighted_count/if_else(count_type == "n", 
                    sum(weighted_count), n_respondents)
            )
    } else {
        res <- df |>
            group_by(across(all_of(c(segment_var, group_var)))) |>
            summarise(
                raw_count = if_else(count_type == "n", n(), n_distinct(respondent_id)),
                weighted_count = sum(get(wt_var)),
                .groups = "drop"
            ) |>
            group_by(across(all_of(segment_var))) |>
            mutate(
                raw_pct = raw_count/if_else(count_type == "n", sum(raw_count),
                    n_respondents),
                weighted_pct = weighted_count/if_else(count_type == "n", 
                    sum(weighted_count), n_respondents)
            ) |>
            ungroup()
    }
    
    return(res)
}

# Plot stacked area chart showing change in categories over time
plot_stacked_trend <- function(df, x = "year", y = "pct", 
    fill = "response", x_breaks = l_years){
    
    # create symbols for tidy evaluation
    x_sym <- sym(x)
    y_sym <- sym(y)
    fill_sym <- sym(fill)
    
    # plot
    res <- ggplot(
        d = df,
        mapping = aes(
            x = !!x_sym,
            y = !!y_sym
        )) +
        # stacked bar chart 
        geom_area(mapping = aes(fill = !!fill_sym)) +
        scale_fill_paletteer_d(palette = "NineteenEightyR::miami2") +
        # vertical lines to distinguish years
        geom_vline(
            xintercept = x_breaks,
            color = "white",
            linewidth = 0.5
        ) + 
        # format axes
        scale_y_continuous(labels = percent) +
        scale_x_continuous(breaks = x_breaks) +
        theme(legend.title = element_blank())

    # return plot
    return(res)
}

#' Create distribution summaries for survey responses and save to global environment
#' 
#' @param data Data frame containing survey responses
#' @param filter_var Character string of column name to subset data to
#' @param group_var Character string of column name to group by
#' @param wt_var Column containing weights, default "wt" 
#' @param count_type Type of count to compute, default "n"
#' @export
summarize_subset_distributions <- function(
        data,
        filter_var,
        group_var,
        wt_var = "wt",
        count_type = c("n", "n_distinct")) {
    
    # evaluate provided parameters
    filter_sym <- sym(filter_var)
    #group_sym <- sym(group_var)
    #wt_sym <- sym(wt_var)
    count_type <- match.arg(count_type)
    
    subsets <- data %>%
        pull(!!filter_var) %>%
        unique()
    
    subsets %>%
        walk(function(f) {
            q %>%
                walk(
                    ~assign(
                        paste0("s_", .x, "_", make_clean_names(f)),
                        summarize_distributions(
                            df = data %>% 
                                filter(
                                    question == .x,
                                    !!sym(filter_sym) == f
                                ),
                            wt_var = wt_var,
                            group_var = group_var,
                            count_type = count_type
                        ),
                        envir = .GlobalEnv
                    )
                )
        })
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
    plot_data <- df |>
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
    overall_dist <- plot_data |>
        group_by(across(all_of(x))) |>
        dplyr::summarize(
            reference_pct = weighted.mean(get(y), weighted_count),
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
        coord_flip() +
        theme(
            legend.title = element_blank(),
            legend.position = "top",
            legend.margin = margin(0, 0, 0, 0),
            legend.justification.top = "left",
            legend.justification.left = "top",
        )
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
            height = 12
        )
        
        # assign plot to environment with descriptive name
        plot_name <- paste0("g_", question_id, "_by_", seg_var)
        assign(plot_name, p, envir = env)
        
        p
    })
    
    # name list elements
    names(plots) <- paste0("g_", question_id, "_by_", segment_vars)
    
    # return objects saved in environment
    return(plots)
}

#' Extract grouping variable from dataframe name
#'
#' @param df_name Name of the dataframe (string)
#' @return String containing the grouping variable name or NULL if no match
#' @examples
#' extract_group_var("s_q6_by_race_ethnicity")  # returns "race_ethnicity"
#' extract_group_var("s_q6_response")           # returns NULL
extract_group_var <- function(df_name) {
    # match pattern after "by_"
    match <- regexpr("by_(.+)$", df_name, perl = TRUE)
    if (match != -1) {
        group_var <- substr(df_name, match + 3, nchar(df_name))
        return(group_var)
    }
    return(NULL)
}

#' Create a side-by-side comparison bar chart of raw vs weighted percentages
#'
#' @param df A dataframe containing columns: r_en (response text), raw_pct (raw percentages),
#'        and weighted_pct (weighted percentages)
#' @param obj_name Optional name of the object, used for saved file name
#' @param facet Logical; if TRUE, attempts to extract faceting variable from dataframe name
#' @param facet_label Optional label for facet strips (default: prettified facet variable name)
#' @param title Optional plot title (default: "Raw vs Weighted Percentages")
#' @param colors Optional vector of two colors for the bars (default: c("#1f77b4", "#ff7f0e"))
#'
#' @return A ggplot2 object containing the side-by-side bar chart
#' @export
#'
#' @import ggplot2
#' @import tidyr
#' @import ggtext
#'
#' @examples
#' s_q6_by_race_ethnicity <- data.frame(
#'   r_en = c("Response A", "Response B"),
#'   raw_pct = c(0.50, 0.50),
#'   weighted_pct = c(0.54, 0.46),
#'   race_ethnicity = c("Group 1", "Group 2")
#' )
#' # Will automatically detect and use race_ethnicity as facet variable
#' plot_pct_comparison(s_q6_by_race_ethnicity, facet = TRUE)
plot_pct_comparison <- function(df, obj_name = NULL, 
                                facet = FALSE, facet_label = NULL,
                                title = "Raw vs Weighted Percentages", 
                                colors = c("#1f77b4", "#ff7f0e")) {
    
    # get name of df object
    df_name <- if(!is.null(obj_name)) obj_name else deparse(substitute(df))
    
    # extract facet variable if faceting is requested
    facet_var <- NULL
    if (facet) {
        facet_var <- extract_group_var(df_name)
        
        if (is.null(facet_var)) {
            warning("Could not extract faceting variable from dataframe name. Expected pattern: '*by_variable'")
            facet <- FALSE
        } else if (!facet_var %in% names(df)) {
            warning(sprintf("Extracted faceting variable '%s' not found in dataframe", facet_var))
            facet <- FALSE
        }
    }
    
    # create a copy of the dataframe with only the columns we need
    plot_df <- df[, c("r_en", "raw_pct", "weighted_pct")]
    if (facet) {
        plot_df[[facet_var]] <- df[[facet_var]]
    }
    
    # reshape data from wide to long format
    plot_data <- pivot_longer(
        plot_df,
        cols = c("raw_pct", "weighted_pct"),
        names_to = "type",
        values_to = "percentage"
    )
    
    # create more readable labels
    plot_data$type <- factor(
        plot_data$type,
        levels = c("raw_pct", "weighted_pct"),
        labels = c("Raw", "Weighted")
    )
    
    # create the base plot
    p <- ggplot(plot_data, aes(y = r_en, x = percentage, fill = type)) +
        geom_bar(stat = "identity", position = "dodge", width = 0.7) +
        geom_richtext(
            aes(
                label = percent(percentage, accuracy = 0.1),
                group = type
            ),
            position = position_dodge(width = 0.7),
            hjust = -0.1,
            size = 3.5,
            fill = NA,
            label.color = NA
        ) +
        scale_fill_manual(values = colors) +
        scale_x_continuous(labels = scales::percent) +
        labs(
            title = title,
            fill = NULL
        ) +
        theme(
            legend.position = "top",
            legend.justification = "left",
            legend.direction = "horizontal",
        )
    
    # add faceting if requested and variable was successfully extracted
    if (facet) {
        p <- p + facet_wrap(
            as.formula(paste("~", facet_var)),
            strip.position = "top"
        ) +
            theme(
                strip.text = element_text(face = "bold"),
                panel.spacing = unit(2, "lines")
            )
    }
    
    # save plot
    ggsave(
        filename = paste0("output/2025/", str_remove(df_name, "s_"), 
            "_raw-v-wt.png"),
        plot = p,
        width = 12,
        height = 12,
        units = "in", 
        limitsize = FALSE
    )
    
    # return plot, saved in environment
    plot_name <- paste0("g_", str_remove(df_name, "s_"), "_raw-v-wt")
    assign(plot_name, p, envir = .GlobalEnv)
    return(p)
}
