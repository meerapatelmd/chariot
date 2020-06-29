#' Filter the invalid_reason Field
#' @importFrom dplyr filter_at
#' @export

filter_invalid_reason <-
        function(.data, 
                 value,
                 invert = FALSE) {
            
            col <- grep("invalid_reason", colnames(.data), value = TRUE)
            
            if (length(col) > 1) {
                    cols <- paste(col, collapse = ", ")
                    warning("More than 1 column filtered: ", cols)
            }
            
            if (invert) {
                
                .data %>%
                    dplyr::filter_at(vars(contains("invalid_reason")),
                                     any_vars(!(. %in% value)))
                
                
            } else {
            
                .data %>%
                    dplyr::filter_at(vars(contains("invalid_reason")),
                                     any_vars(. %in% value))
                
            }

        }
