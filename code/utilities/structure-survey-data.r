################################################################################
# purpose: structure survey data for easy analysis
# last edited: feb 4, 2025
################################################################################

#' @title Extract question number from survey column name
#' @description
#' Given text that includes a number followed by a period, this function
#' returns the digit(s) before the first period.
#' @param text A string that includes a number followed by a period
#' @returns A string vector of extracted digits
#' @examples 
#' extract_question_number("q1. What is your favorite color?") # returns "1"
extract_question_number <- function(text) {
    #   (\\d+) capture one or more digits
    #   \\. followed by a literal period
    match <- str_extract(text, "(\\d+)\\.")
  
    # remove the period
    number <- str_remove(match, "\\.")
    
    return(number)
}

#' @title Create new column names for survey dataframe
#' @description
#' Generates a vector of column names including timestamp, and q1, q2, ...
#' @param df A dataframe
#' @returns A dataframe with renamed columns
#' @examples 
#' d <- tibble(
#'     timestamp = Sys.time(), 
#'     `1. What is 1+1?` = "2", 
#'     `2. What color is the sky?` = "blue"
#'     )
#' rename_questions(d)
#' # A tibble: 1 × 3
#' # timestamp           q1    q2   
#' # <dttm>              <chr> <chr>
#' # 1 2025-01-19 20:23:22 2     blue 
rename_questions <- function(df) {
    # validate input dataframe
    if (!is.data.frame(df)) {
        cli::cli_abort(
            "Input {.arg df} must be a dataframe, not {.type {df}}"
        )
    }
    
    # get the current names of dataframe columns
    cols_raw <- names(df)[2:length(names(df))]
    
    # pull out the question numbers
    cols_number <- map_chr(cols_raw, extract_question_number)
    
    # create new column names, appending a timestamp field at the top
    cols_new <- c("timestamp", paste("q", cols_number, sep = ""))
    
    # rename columns of dataframe to c(timestamp, q1, q2, ...)
    names(df) <- cols_new
    
    # return dataframe with renamed columns
    return(df)
}

#' Generate a unique ID for responses with a prefix and zero-padded row number
#' 
#' @param prefix A string to use as the ID prefix
#' @param row_number A numeric vector representing the row number
#' @param total_width The total width of the numeric portion (default 5)
#' @return A string combining the prefix and zero-padded number
#' 
#' @examples
#' create_id("CUST", 1)      # Returns "CUST00001"
#' create_id("ORD", 42, 3)   # Returns "ORD042"
create_id <- function(prefix, row_numbers, total_width = 5) {
    # input validation for prefix
    if (!is.character(prefix) || length(prefix) != 1) {
        stop("Prefix must be a single character string")
    }
    
    # input validation for row_number
    if (!is.vector(row_numbers)) {
        stop("Row numbers must be numeric")
    }
    
    # input validation for total_width
    if (!is.numeric(total_width) || total_width < 1) {
        stop("Total width must be a positive number")
    }
    
    # check if any row_number has more digits than total_width
    if (any(nchar(as.character(row_numbers)) > total_width)) {
        warning("Some row numbers exceed specified width. IDs will be longer than expected.")
    }
    
    # create the formatted number with leading zeros
    padded_numbers <- sprintf(paste0("%0", total_width, "d"), row_numbers)
    
    # combine prefix and padded number
    id <- paste0(prefix, padded_numbers)
    
    return(id)
}

#' @title Extract text that is not enclosed within parentheses
#' @description
#' Given text that includes parenthetical language, this function
#' returns the text that are not enclosed within parentheses
#' @param text A string
#' @returns A string of all non-parenthetical language
#' @examples 
#' # Test cases to demonstrate different scenarios
#' examples <- c(
#'     "Hello (world) there",                    # Basic case
#'     "Multiple (first) words (second) here",   # Multiple parentheses
#'     "No parentheses here",                    # No parentheses
#'     "Nested (outer (inner) outer) text",      # Nested parentheses
#'     "Text (note) ends (here)"                 # Parentheses at the end
#' )
#' 
#' # Apply our function to each example
#' results <- map_chr(examples, extract_outside_parentheses)
extract_outside_parentheses <- function(text) {
    # extract text that is not within innermost parentheticals
    pattern <- "[（(][^）)]*[）)]" 

    # remove the parenthetical content and any trailing white space
    cleaned_text <- str_replace_all(text, pattern, "") %>%
        str_trim()
    
    return(cleaned_text)
}

#' @title Standardize column of district names
#' @description
#' Given text that includes a district number with the council member names 
#' in parentheses
#' @param df A dataframe
#' @param question_id A string of the column name that has district data
#' @returns A dataframe with a new column of standardized district names
#' @examples 
#' d <- tibble(
#'     district_raw = c("District 1 (Kalb)", "District 2 (Bas)", "District 2 (Kaplan)"),
#'     n = 1:3
#' )
#' standardize_districts(d, "district_raw")
standardize_districts <- function(df, question_id) {
    # validate input dataframe
    if (!is.data.frame(df)) {
        cli::cli_abort(
            "Input {.arg df} must be a dataframe, not {.type {df}}"
        )
    }
    
    # check if the column exists in the dataframe
    if (!{{ question_id }} %in% names(df)) {
        stop(paste("Column", question_id, "not found in dataframe"))
    }

    # remove council member names
    df_res <- df %>%
        mutate(district = extract_outside_parentheses(.data[[question_id]]))

    # return dataframe with updated district column
    return(df_res)
}

#' @title Identify chosen multi-select response options
#' @description
#' Given a string containing the multi-selected 
#' @param response_text A string with comma-delimited selected responses
#' @param options A vector of strings that represent the possible options
#' @returns A vector of logical values indicating whether an option was selected
#' @examples 
#' options <- c(
#'     "Robust small business economy",
#'     "Reduced maintenance (e.g. more potholes, more vandalism, more 
#'          illegal dumping, reduced cleanliness, reduced sidewalk and 
#'          bike accessibility)",
#'     "Feeling safe when walking and/or leaving my car alone"
#'     )
#' response <- "Reduced maintenance (e.g. more potholes, more vandalism, 
#'      more illegal dumping, reduced cleanliness, reduced sidewalk and 
#'      bike accessibility), Feeling safe when walking and/or leaving my 
#'      car alone"
#' check_responses(response, options) # returns FALSE  TRUE  TRUE
check_responses <- function(response_text, options) {
    # handle NA values
    if (is.na(response_text)) {
        return(rep(FALSE, length(options)))
    }
    
    # return a logical vector of whether response text is in options
    # use fixed() to ensure exact string matching
    res <- map_lgl(options, ~str_detect(response_text, fixed(.x)))
    return(res)
}

#' @title Separate multi-select responses into separate rows
#' @description
#' Given a vector of strings that may include comma-delimited responses
#' for a multiple-choice survey question, reshape the data so that there is
#' a single selected option per row
#' @param df A dataframe
#' @param col A string with the name of the column that includes the 
#' multi-select question responses
#' @param options A vector of strings that represent the possible options
#' @returns A lengthened dataframe with one selected option per row
#' @examples 
#' options <- c(
#'     "Robust small business economy",
#'     "Reduced maintenance (e.g. more potholes, more vandalism, more illegal dumping, reduced cleanliness, reduced sidewalk and bike accessibility)",
#'     "Feeling safe when walking and/or leaving my car alone"
#' )
#' 
#' df <- tibble(
#'     respondent_id = 1:3,
#'     responses = c(
#'         "Robust small business economy, Reduced maintenance (e.g. more potholes, more vandalism, more illegal dumping, reduced cleanliness, reduced sidewalk and bike accessibility), Feeling safe when walking and/or leaving my car alone",
#'         "Reduced maintenance (e.g. more potholes, more vandalism, more illegal dumping, reduced cleanliness, reduced sidewalk and bike accessibility), Feeling safe when walking and/or leaving my car alone",
#'         "Robust small business economy"
#'     )
#' )
#' 
#' separate_multi_select(df, "responses", options)
separate_multi_select <- function(df, col, options) {
    # validate input dataframe
    if (!is.data.frame(df)) {
        cli::cli_abort(
            "Input {.arg df} must be a dataframe, not {.type {df}}"
        )
    }
    
    # check if the column exists in the dataframe
    if (!{{ col }} %in% names(df)) {
        stop(paste("Column", col, "not found in dataframe"))
    }
    
    # validate options input is vector
    if (!is.vector(options)) {
        stop("Argument `options` must be a vector")
    }
    
    # transform data
    df_res <- df %>%
        # process each row individually
        rowwise() %>%
        mutate(
            # create list column of logical vectors showing selected options
            selections = list(check_responses(get(col), options))
        ) %>% 
        # expand the selections into individual rows
        unnest_longer(selections) %>%
        # add the corresponding option text
        mutate(
            {{col}} := options[row_number() %% length(options) + 
                (row_number() %% length(options) == 0) * length(options)]
        ) %>%
        # keep only the selected options
        filter(selections) %>%
        select(-selections)

    # return expanded survey dataframe    
    return(df_res)
}

# TODO: manage "other" responses

#' @title Reshape survey data from wide to long format
#' #' @description
#' Given a dataframe of survey data in wide format, pivot into long format
#' with the columns `respondent_id`, `timestamp`, `question`, `response`
#' @param df A dataframe
#' @returns Survey data in long format
elongate_survey_data <- function(df) {
    # input datatype validation
    if (!is.data.frame(df)) {
        stop("Argument `df` must be a dataframe")
    }
    
    # reshape into three columns (timestamp, question, response)
    df_long <- df %>%
        pivot_longer(
            cols = c(-timestamp, -respondent_id, -district),
            names_to = "question", 
            values_to = "response"
        )
    
    # return long dataframe
    return(df_long)
}

#' Map Non-English Survey Responses to English Equivalents
#' 
#' @description
#' Takes a dataframe containing survey responses in various languages and maps them
#' to their English equivalents using a reference mapping table. This function is
#' particularly useful for standardizing multilingual survey data analysis.
#' 
#' @param df A dataframe containing at least two columns:
#'   \itemize{
#'     \item question: The survey question identifier
#'     \item response: The response in the original language
#'   }
#' @param language_code Character string specifying the source language.
#'   Must be one of:
#'   \itemize{
#'     \item "en": English (returns original responses)
#'     \item "es": Spanish
#'     \item "zh": Chinese
#'   }
#'
#' @return A dataframe with all original columns plus:
#'   \itemize{
#'     \item response_en: The English equivalent of the response
#'   }
#'   For non-English inputs, this is achieved through a left join with the mapping
#'   table 'd_rmap'.
#'
#' @details
#' The function relies on a global mapping table 'd_rmap' that should contain
#' the following columns:
#' \itemize{
#'   \item question: Question identifier matching the input df
#'   \item r_es: Spanish response text
#'   \item r_zh: Chinese response text
#'   \item response_en: English equivalent response text
#' }
#'
#' @examples
#' # English responses (returns original)
#' survey_en <- tibble(
#'   question = c("q1", "q2"),
#'   response = c("Yes", "No")
#' )
#' map_en_responses(survey_en, "en")
#'
#' # Spanish responses
#' survey_es <- tibble(
#'   question = c("q1", "q2"),
#'   response = c("Sí", "No")
#' )
#' map_en_responses(survey_es, "es")
map_en_responses <- function(df, language_code = c("en", "es", "zh")) {
    # validate input dataframe
    if (!is.data.frame(df)) {
        cli::cli_abort(
            "Input {.arg df} must be a dataframe, not {.type {df}}"
        )
    }
    
    # validate required columns exist
    required_cols <- c("question", "response")
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
        cli::cli_abort(
            c("Missing required columns in input dataframe:",
              "x" = "{.val {missing_cols}}")
        )
    }
    
    # match and validate language_code argument
    language_code <- match.arg(language_code)
    
    # check if mapping table exists in environment when needed
    if (language_code != "en" && !exists("d_rmap")) {
        cli::cli_abort(
            "Mapping table {.val d_rmap} not found in environment"
        )
    }
    
    # create column of equivalent responses in english
    if (language_code == "en") {
        df_res <- df %>% mutate(response_en = response)
    } else if (language_code == "es") {
        df_res <- df %>% 
            left_join(
                d_rmap, 
                by = c("question" = "question", "response" = "r_es")
            ) %>%
            mutate(r_es = response)
    } else if (language_code == "zh") {
        df_res <- df %>% 
            left_join(
                d_rmap, 
                by = c("question" = "question", "response" = "r_zh")
            ) %>%
            mutate(r_zh = response)
    }
    
    # return expanded dataset
    return(df_res)
}

#' Preprocess Survey Data with Multi-language and Multi-select Support
#' 
#' @description
#' Provides end-to-end preprocessing of survey data, including handling multiple 
#' languages, district standardization, and multi-select questions. The function 
#' performs the following steps in order:
#' 1. Renames columns to standardized format (q1, q2, etc.)
#' 2. Creates unique response IDs
#' 3. Standardizes district information
#' 4. Converts data to long format
#' 5. Maps non-English responses to English
#' 6. Processes multi-select questions
#'
#' @param df A dataframe containing raw survey responses, with the first column 
#'   being a timestamp and subsequent columns being survey questions
#' @param screener_qid String identifying the question column containing whether 
#'   respondent lives in Oakland (e.g., "q1")
#' @param district_qid String identifying the question column containing district 
#'   information (e.g., "q2")
#' @param language_code Character string specifying the source language.
#'   Must be one of: "en" (English), "es" (Spanish), or "zh" (Chinese)
#' @param multi_select_questions Named list where:
#'   - Names are question IDs (e.g., "q5")
#'   - Values are vectors of possible options for that question
#'
#' @return A dataframe in long format with columns:
#'   \itemize{
#'     \item respondent_id: Unique identifier for each respondent
#'     \item timestamp: Time the response was recorded
#'     \item question: Question identifier (q1, q2, etc.)
#'     \item response: Original response text
#'     \item district: Standardized district name (if applicable)
#'     \item survey_language: Language of the original response
#'     \item response_en: English translation of response (if applicable)
#'   }
#'
#' @examples
#' # Define multi-select questions and their options
#' multi_select_qs <- list(
#'   "q5" = c(
#'     "Robust small business economy",
#'     "Reduced maintenance",
#'     "Feeling safe when walking"
#'   ),
#'   "q8" = c(
#'     "Option A",
#'     "Option B",
#'     "Option C"
#'   )
#' )
#'
#' # Process English survey data
#' processed_data <- preprocess_survey_data(
#'   df = survey_data,
#'   screener_qid = "q2"
#'   district_qid = "q2",
#'   language_code = "en",
#'   multi_select_questions = multi_select_qs
#' )
preprocess_survey_data <- function(
    df, 
    screener_qid,
    district_qid, 
    language_code = c("en", "es", "zh"),
    multi_select_qids = NULL
) {
    # validate df input is dataframe
    if (!is.data.frame(df)) {
        cli::cli_abort(
            "Input {.arg df} must be a dataframe, not {.type {df}}"
        )
    }
    
    # validate language_code is one of available options
    language_code <- match.arg(language_code)
    
    # validate district_qid
    if (!is.character(district_qid) || length(district_qid) != 1) {
        cli::cli_abort(
            "Argument {.arg district_qid} must be a single character string"
        )
    }
    
    # validate multi_select_questions structure if provided
    if (!is.null(multi_select_qids)) {
        if (!is.list(multi_select_qids) || is.null(names(multi_select_qids))) {
            cli::cli_abort(
                "Argument {.arg multi_select_qids} must be a named list"
            )
        }
        
        # Verify each element is a character vector
        invalid_elements <- !map_lgl(multi_select_qids, is.character)
        if (any(invalid_elements)) {
            cli::cli_abort(c(
                "Invalid options in {.arg multi_select_qids}:",
                "x" = "Each element must be a character vector",
                "i" = "Check elements: {.val {names(multi_select_qids)[invalid_elements]}}"
            ))
        }
    }

    # standardize question names
    df_renamed <- rename_questions(df)
    
    # remove responses from respondents who do not live in Oakland
    df_screened <- df_renamed %>%
        filter(.data[[screener_qid]] %in% c("Yes", "Sí", "是的"))
    
    # create response IDs and standardize districts
    df_processed <- df_screened %>% 
        mutate(respondent_id = create_id(
            prefix = language_code,
            row_number = row_number()
        )) %>%
        standardize_districts(question_id = district_qid)
    
    # convert to long format
    df_long <- df_processed %>% elongate_survey_data()
    
    # map non-English responses to English
    df_translated <- df_long %>%
        mutate(
            survey_language = case_when(
                language_code == "en" ~ "English",
                language_code == "sp" ~ "Spanish",
                language_code == "zh" ~ "Chinese"
            )
        ) %>%
        map_en_responses(language_code = language_code)
    
    # process multi-select questions if provided
    if (!is.null(multi_select_qids)) {
        # function to process a single multi-select question
        process_one_question <- function(df, qid, options) {
            # only process rows for this specific question
            df_question <- df %>%
                filter(.data$question == qid)
            
            # Process multi-select responses for this question
            df_expanded <- separate_multi_select(
                df_question, 
                col = "response", 
                options = options
            )
            
            # Return combined data
            bind_rows(
                df %>% filter(.data$question != qid),
                df_expanded
            )
        }
        
        # process each multi-select question sequentially
        df_multi <- reduce2(
            .x = names(multi_select_qids),
            .y = multi_select_qids,
            .f = process_one_question,
            .init = df_translated
        )
    } else {
        df_multi <- df_translated
    }
    
    # add a response id that is unique to each row in restructured data
    df_res <- df_multi %>%
        group_by(respondent_id) %>%
        mutate(response_id = paste(
            respondent_id, 
            question, 
            row_number(), 
            sep = "-")
        ) %>%
        ungroup()
    
    # return processed data
    return(df_res)
}