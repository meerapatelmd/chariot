#' Mutate all concept_ids to integer
#' @description Takes all the fields with "concept_id" in their names and converts to integer
#' @import dplyr
#' @export

ids_to_integer <- 
                function(.data) {
                    
                    .data %>%
                        dplyr::mutate_at(vars(contains("concept_id")),
                                         as.integer)
                    
                }