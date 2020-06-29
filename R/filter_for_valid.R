#' Filter for valid concepts
#' @export

filter_for_valid <-
        function(.data) {
            
            .data %>%
                filter_invalid_reason(value = NA_character_)

        }
