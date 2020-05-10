#' Merge OMOP concept elements into a single string
#' @description All elements of the CONCEPT table are included except for the dates.
#' @param concept_dataframe output from concept table
#' @param into name of the column that the new combined string will be in
#' @param suffix if the omop concept element column names are different from the standard by a suffix, include it so it can point to the correct set of columns
#' @import dplyr
#' @import tidyr
#' @export

merge_concepts <-
            function(concept_dataframe, into, suffix = NULL) {
                
                into <- dplyr::enquo(into)
                
                column_names <- paste0(c("concept_id",
                                         "concept_name",
                                         "domain_id",
                                         "vocabulary_id",
                                         "concept_class_id",
                                         "standard_concept",
                                         "concept_code",
                                         "valid_start_date",
                                         "valid_end_date",
                                         "invalid_reason"),
                                       suffix)
                
                
                date_columns <- grep("valid.*date", colnames(concept_dataframe), value = TRUE)
                
                if (length(date_columns) > 0) {
                    dplyr::bind_cols(concept_dataframe,
                                     concept_dataframe %>%
                                         dplyr::select(all_of(column_names)) %>%
                                         dplyr::mutate_at(vars(contains("standard_concept")), function(x) ifelse(is.na(x), "N", x)) %>%
                                         dplyr::mutate_at(vars(contains("standard_concept")), function(x) paste0("[", x, "]")) %>%
                                         dplyr::mutate_at(vars(contains("invalid_reason")), function(x) ifelse(is.na(x), "[V]", paste0("[", x, "]"))) %>%
                                         tidyr::unite(col = vocabulary, contains("vocabulary_id"), contains("concept_code"), sep = " ") %>%
                                         dplyr::mutate_at(vars(contains("domain_id"), 
                                                               contains("vocabulary"), 
                                                               contains("concept_class_id")), function(x) paste0("[", x, "]")) %>%
                                         dplyr::select_at(vars(!matches("valid.*date"))) %>%
                                         tidyr::unite(col = !!into,
                                                      contains("invalid_reason"),
                                                      contains("standard_concept"),
                                                      contains("concept_id"),
                                                      contains("concept_name"),
                                                      contains("vocabulary"),
                                                      contains("domain_id"),
                                                      contains("concept_class_id"),
                                                      sep = " ")
                    )
                } else {
                    column_names <- grep("valid.*date", column_names, value = TRUE, invert = TRUE)
                    dplyr::bind_cols(concept_dataframe,
                                     concept_dataframe %>%
                                         dplyr::select(all_of(column_names)) %>%
                                         dplyr::mutate_at(vars(contains("standard_concept")), function(x) ifelse(is.na(x), "N", x)) %>%
                                         dplyr::mutate_at(vars(contains("standard_concept")), function(x) paste0("[", x, "]")) %>%
                                         dplyr::mutate_at(vars(contains("invalid_reason")), function(x) ifelse(is.na(x), "[V]", paste0("[", x, "]"))) %>%
                                         tidyr::unite(col = vocabulary, contains("vocabulary_id"), contains("concept_code"), sep = " ") %>%
                                         dplyr::mutate_at(vars(contains("domain_id"), contains("vocabulary"), contains("concept_class_id")), function(x) paste0("[", x, "]")) %>%
                        tidyr::unite(col = !!into,
                                     contains("invalid_reason"),
                                     contains("standard_concept"),
                                     contains("concept_id"),
                                     contains("concept_name"),
                                     contains("vocabulary"),
                                     contains("domain_id"),
                                     contains("concept_class_id"),
                                     sep = " "))
                }
            }


