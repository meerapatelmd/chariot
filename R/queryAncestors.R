#' Query ancestors for a given concept_id
#' @inheritParams write_sql_for_ancestors
#' @export

queryAncestors <-
    function(descendant_concept_ids,
             schema,
             min_levels_of_separation = NULL,
             max_levels_of_separation = NULL,
             ...) {


            sql_statement <- renderQueryAncestors(descendant_concept_ids = descendant_concept_ids,
                                                  schema = schema,
                                                  min_levels_of_separation = min_levels_of_separation,
                                                  max_levels_of_separation = max_levels_of_separation)

            query_athena(sql_statement = sql_statement,
                         ...)

    }
