





leftJoinRelatives <-
        function(.data,
                 athena_schema = "public",
                 id_column = NULL,
                 whereLevelIn = NULL,
                 whereLevelType = NULL,
                 render_sql = TRUE,
                 conn = NULL) {



                ancestors <-
                        leftJoinForAncestors(.data = .data,
                                             athena_schema = athena_schema,
                                             descendant_id_column = id_column,
                                             whereLevelIn = whereLevelIn,
                                             whereLevelType = whereLevelType,
                                             render_sql = render_sql,
                                             conn = conn)

                descendants <-
                        leftJoinForDescendants(.data = .data,
                                               athena_schema = athena_schema,
                                               ancestor_id_column = id_column,
                                               whereLevelIn = whereLevelIn,
                                               whereLevelType = whereLevelType,
                                               render_sql = render_sql,
                                               conn = conn)

                final <- list(A = ancestors,
                              D = descendants) %>%
                                rubix::map_names_set(function(x) x %>%
                                                             rubix::rename_all_remove(pattern = "ancestor_|descendant_")) %>%
                        dplyr::bind_rows(.id = "relative_type") %>%
                        rubix::rename_at_prefix(concept_id,
                                                concept_name,
                                                domain_id,
                                                vocabulary_id,
                                                concept_class_id,
                                                standard_concept,
                                                concept_code,
                                                valid_start_date,
                                                valid_end_date,
                                                invalid_reason,
                                                prefix = "relative_") %>%
                        dplyr::select(all_of(colnames(.data)),
                                      relative_type,
                                      min_levels_of_separation,
                                      max_levels_of_separation,
                                            relative_concept_id,
                                            relative_concept_name,
                                            relative_domain_id,
                                            relative_vocabulary_id,
                                            relative_concept_class_id,
                                            relative_standard_concept,
                                            relative_concept_code,
                                            relative_valid_start_date,
                                            relative_valid_end_date,
                                            relative_invalid_reason,
                                      dplyr::everything())

                return(final)



        }
