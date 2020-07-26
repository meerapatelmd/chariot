#' Query descendants for a given concept_id
#' @inheritParams write_sql_for_descendants
#' @export

query_descendants <-
    function(ancestor_concept_ids,
             max_levels_of_separation = NULL,
             plot = FALSE) {

            .Deprecated(new = "queryDescendants")

            sql <- write_sql_for_descendants(ancestor_concept_ids = ancestor_concept_ids,
                                             max_levels_of_separation = max_levels_of_separation)

            resultset <- query_athena(sql)

            if (plot == FALSE) {
                return(resultset)
            } else {
                parent <-
                query_concept_id(ancestor_concept_ids) %>%
                    merge_concepts(into = "parent",
                                   shorthand = TRUE) %>%
                    dplyr::select(parent) %>%
                    dplyr::distinct() %>%
                    unlist()

                output <-
                resultset %>%
                    merge_concepts(into = "child", shorthand = TRUE) %>%
                    rubix::mutate_if_not_exist(column_name = "parent",
                                                        value = parent) %>%
                    dplyr::select(parent, child) %>%
                    dplyr::distinct() %>%
                    dplyr::filter(parent != child)


                ggenealogy::plotAncDes(output$parent[1], output)

            }

    }
