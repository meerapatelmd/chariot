#' Filter the standard_concept field
#' @importFrom dplyr filter_at
#' @export

filter_standard_concept <-
        function(.data, 
                 value,
                 invert = FALSE) {
            
            standard_concept_col <- grep("standard_concept", colnames(.data), value = TRUE)
            
            if (length(standard_concept_col) > 1) {
                    standard_concept_cols <- paste(standard_concept_col, collapse = ", ")
                    warning("More than 1 standard_concept filtered: ", standard_concept_cols)
            }
            
            if (invert) {
                
                .data %>%
                    dplyr::filter_at(vars(contains("standard_concept")),
                                     any_vars(!(. %in% value)))
                
                
            } else {
            
                .data %>%
                    dplyr::filter_at(vars(contains("standard_concept")),
                                     any_vars(. %in% value))
                
            }

        }
