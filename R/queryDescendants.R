#' Query descendants for a given concept_id
#' @export

queryDescendants <-
    function(ancestor_concept_ids,
             schema,
             min_levels_of_separation = NULL,
             max_levels_of_separation = NULL,
             ...) {


            sql_statement <- renderQueryDescendants(ancestor_concept_ids = ancestor_concept_ids,
                                                    schema = schema,
                                                    min_levels_of_separation = min_levels_of_separation,
                                                    max_levels_of_separation = max_levels_of_separation)
            query_athena(sql_statement = sql_statement,
                         ...)

    }
