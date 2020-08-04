#' Left Join a Dataframe to the Concept Ancestor Table
#' @importFrom rlang list2
#' @export

leftJoinForDescendants <-
        function(.data,
                 athena_schema = "public",
                 ancestor_id_column = NULL,
                 whereLevelIn = NULL,
                 whereLevelType = NULL,
                 render_sql = TRUE,
                 conn = NULL) {

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


                                descendants <-
                                        leftJoin(.data = .data,
                                                 column = ancestor_id_column,
                                                 athena_schema = athena_schema,
                                                 athena_table = "concept_ancestor",
                                                 athena_column = "ancestor_concept_id",
                                                 where_athena_col = whereAthenaField,
                                                 where_athena_col_in = whereLevelIn,
                                                 render_sql = render_sql,
                                                 conn = conn)

                        } else {

                                descendants <-
                                        leftJoin(.data = .data,
                                                 column = ancestor_id_column,
                                                 athena_schema = athena_schema,
                                                 athena_table = "concept_ancestor",
                                                 athena_column = "ancestor_concept_id",
                                                 render_sql = render_sql,
                                                 conn = conn)
                        }



                                descendants_detail <-
                                        leftJoinConcept(descendants %>%
                                                                dplyr::select(descendant_concept_id),
                                                        athena_schema = athena_schema,
                                                        render_sql = render_sql,
                                                        conn = conn,
                                                        synonyms = FALSE) %>%
                                        dplyr::select(-descendant_concept_id) %>%
                                        rubix::rename_all_with_prefix("descendant_") %>%
                                        dplyr::distinct()


                                final_descendants <-
                                        dplyr::left_join(descendants,
                                                         descendants_detail,
                                                         by = "descendant_concept_id") %>%
                                        dplyr::select(-ancestor_concept_id)


                                return(final_descendants)

        }
