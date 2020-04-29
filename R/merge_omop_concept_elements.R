






merge_omop_concept_elements <-
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
                
                bind_cols(concept_dataframe,
                concept_dataframe %>%
                    dplyr::select(all_of(column_names)) %>%
                    dplyr::mutate_at(vars(contains("standard_concept")), function(x) ifelse(is.na(x), "N", x)) %>%
                    dplyr::mutate_at(vars(contains("standard_concept")), function(x) paste0("[", x, "]")) %>%
                    dplyr::mutate_at(vars(contains("invalid_reason")), function(x) ifelse(is.na(x), "[V]", paste0("[", x, "]"))) %>%
                    tidyr::unite(col = vocabulary, contains("vocabulary_id"), contains("concept_code"), sep = " ") %>%
                    dplyr::mutate_at(vars(contains("domain_id"), contains("vocabulary"), contains("concept_class_id")), function(x) paste0("[", x, "]")) %>%
                    dplyr::select(-contains("_date")) %>%
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
