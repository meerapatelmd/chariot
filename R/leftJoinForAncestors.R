#' Left Join a Dataframe to the Concept Ancestor Table
#' @importFrom rlang list2
#' @export




leftJoinForAncestors <-
        function(.data,
                 athena_schema = "public",
                 descendant_id_column = NULL,
                 whereLevelIn = NULL,
                 whereLevelType = NULL,
                 override_cache = FALSE,
                 print_sql = TRUE) {

                        if (!is.null(whereLevelIn) && length(whereLevelType) != 1) {


                                warning("No 'whereLevelType'. Defaulting to 'max'")
                                whereLevelType <- "max"

                        }

                        if (!is.null(whereLevelIn)) {
                                if (whereLevelType == "max") {
                                        whereAthenaField <- "max_levels_of_separation"
                                } else {
                                        whereAthenaField <- "min_levels_of_separation"
                                }


                                ancestors <-
                                        leftJoin(.data = .data,
                                                 column = descendant_id_column,
                                                 athena_schema = athena_schema,
                                                 athena_table = "concept_ancestor",
                                                 athena_column = "descendant_concept_id",
                                                 where_athena_col = whereAthenaField,
                                                 where_athena_col_in = whereLevelIn,
                                                 override_cache = override_cache,
                                                 print_sql = print_sql)

                                ancestors_detail <-
                                        leftJoinConcept(ancestors %>%
                                                                dplyr::select(ancestor_concept_id),
                                                        athena_schema = athena_schema,
                                                        synonyms = FALSE,
                                                        override_cache = override_cache) %>%
                                        dplyr::select(-ancestor_concept_id) %>%
                                        rubix::rename_all_with_prefix("ancestor_")

                                final_ancestors <-
                                        dplyr::left_join(ancestors,
                                                         ancestors_detail,
                                                         by = "ancestor_concept_id") %>%
                                        dplyr::select(-descendant_concept_id)


                        } else {

                                ancestors <-
                                        leftJoin(.data = .data,
                                                 column = descendant_id_column,
                                                 athena_schema = athena_schema,
                                                 athena_table = "concept_ancestor",
                                                 athena_column = "descendant_concept_id",
                                                 override_cache = override_cache,
                                                 print_sql = print_sql)

                                ancestors_detail <-
                                        leftJoinConcept(ancestors %>%
                                                                dplyr::select(ancestor_concept_id),
                                                        athena_schema = athena_schema,
                                                        synonyms = FALSE,
                                                        override_cache = override_cache) %>%
                                        dplyr::select(-ancestor_concept_id) %>%
                                        rubix::rename_all_with_prefix("ancestor_")

                                final_ancestors <-
                                        dplyr::left_join(ancestors,
                                                         ancestors_detail,
                                                         by = "ancestor_concept_id") %>%
                                        dplyr::select(-descendant_concept_id)




                        }
                                        return(final_ancestors)

        }
