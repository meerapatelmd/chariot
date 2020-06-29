#' Filter Domain
#' @importFrom dplyr filter_at
#' @export

filter_domain <-
        function(.data, 
                 value,
                 invert = FALSE) {
            
            col <- grep("domain_id", colnames(.data), value = TRUE)
            
            if (length(col) > 1) {
                    cols <- paste(col, collapse = ", ")
                    warning("More than 1 column filtered: ", cols)
            }
            
            if (invert) {
                
                .data %>%
                    dplyr::filter_at(vars(contains("domain_id")),
                                     any_vars(!(. %in% value)))
                
                
            } else {
            
                .data %>%
                    dplyr::filter_at(vars(contains("domain_id")),
                                     any_vars(. %in% value))
                
            }

        }
