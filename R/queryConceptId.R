#' Lookup a concept id in Athena
#' @importFrom seagull write_query_where_in
#' @export

queryConceptId <-
    function(concept_ids,
             schema,
             ...) {


                            sql <-
                            pg13::buildQuery(schema = schema,
                                             tableName = "concept",
                                             whereInField = "concept_id",
                                             whereInVector = concept_ids,
                                             caseInsensitive = FALSE)

                            query_athena(sql,
                                         ...)
    }
