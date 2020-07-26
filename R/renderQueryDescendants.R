#' Render SQL to Query for Descendants
#' @import pg13
#' @export

renderQueryDescendants <-
        function(ancestor_concept_ids,
                 schema,
                 min_levels_of_separation = NULL,
                 max_levels_of_separation = NULL) {


                sql_statement <-
                        stringr::str_remove_all(
                                pg13::buildQuery(schema = schema,
                                                 tableName = "concept_ancestor",
                                                 whereInField = "ancestor_concept_id",
                                                 whereInVector = ancestor_concept_ids,
                                                 caseInsensitive = FALSE),
                                pattern = "[;]{1}$")

                if (!is.null(min_levels_of_separation)) {

                        sql_statement <-
                                pg13::concatWhereConstructs(sql_statement,
                                                            pg13::constructWhereIn(field = "min_levels_of_separation",
                                                                                   vector = min_levels_of_separation))

                }

                if (!is.null(max_levels_of_separation)) {

                        sql_statement <-
                                pg13::concatWhereConstructs(sql_statement,
                                                            pg13::constructWhereIn(field = "max_levels_of_separation",
                                                                                   vector = max_levels_of_separation))

                }

                pg13::terminateBuild(sql_statement = sql_statement)


        }
