################################################################################
# purpose: programmatically translate Spanish and Chinese responses to English
# last edited: feb 3, 2025
# TODO: add roxygen2 documentation
################################################################################

# function to translate a vector of text - works with any source language
translate_column <- function(text_vector, source_lang) {
    map_chr(
        text_vector,
        ~if (is.na(.x) || nchar(trimws(.x)) == 0) {
            .x  # Return NA or empty string as-is
        } else {
            tryCatch({
                translated <- google_translate(
                    text = .x,
                    source_lang = source_lang,
                    target_lang = "en"
                )
                translated
            }, error = function(e) {
                warning("Translation error for text '", .x, "': ", e$message)
                NA_character_
            })
        }
    )
}
