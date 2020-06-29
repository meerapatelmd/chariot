#' Filter Vocabulary
#' @importFrom dplyr filter_at
#' @export

filter_vocab <-
        function(.data, 
                 value,
                 invert = FALSE) {
            
            col <- grep("vocabulary_id", colnames(.data), value = TRUE)
            
            if (length(col) > 1) {
                    cols <- paste(col, collapse = ", ")
                    warning("More than 1 column filtered: ", cols)
            }
            
            if (invert) {
                
                .data %>%
                    dplyr::filter_at(vars(contains("vocabulary_id")),
                                     any_vars(!(. %in% value)))
                
                
            } else {
            
                .data %>%
                    dplyr::filter_at(vars(contains("vocabulary_id")),
                                     any_vars(. %in% value))
                
            }

        }
