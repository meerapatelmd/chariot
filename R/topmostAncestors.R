





topmostAncestors <-
        function(.data,
                 column = NULL,
                 ...) {

                if (is.null(column)) {
                        column <- colnames(.data)[1]
                }

                leftJoinForAncestors(.data = .data,
                                           descendant_id_column = column) %>%
                                        dplyr::select(all_of(column),
                                                      ancestor_concept_id,
                                                      contains("levels")) %>%
                        tidyr::pivot_longer(cols = contains("levels"),
                                            names_to = "levels_type",
                                            values_to = "level",
                                            values_drop_na = TRUE) %>%
                        dplyr::select(-levels_type) %>%
                        dplyr::group_by_at(vars(!!column)) %>%
                        dplyr::arrange(desc(level), .by_group = TRUE) %>%
                        dplyr::filter_at(vars(level), any_vars(. == max(., na.rm = TRUE))) %>%
                        dplyr::select(hemonc_id,
                                      topmost_ancestor_id = ancestor_concept_id) %>%
                        dplyr::distinct() %>%
                        leftJoinConcept(column = "topmost_ancestor_id")
        }
