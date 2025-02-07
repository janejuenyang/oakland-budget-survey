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
summarize_distributions <- function(df, group_var, wt_var = "wt") {
    group_sym <- sym(group_var)
    wt_sym <- sym(wt_var)
    
    res <- df %>%
        group_by(!!group_sym) %>%
        summarize(
            raw_count = n(),
            weighted_count = sum(!!wt_sym),
        ) %>%
        ungroup() %>%
        mutate(
            raw_pct = raw_count/sum(raw_count),
            weighted_pct = weighted_count/sum(weighted_count)
        )
    
    return(res)
}
