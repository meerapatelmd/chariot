#' Get all relationships for a concept_id or a vector of concept_ids
#' @inheritParams write_sql_to_get_relationships
#' @export

query_relationships <-
    function(concept_ids) {
        sql_statement <-
            write_sql_to_get_relationships(concept_ids = concept_ids)
        resultset <- query_athena(sql_statement = sql_statement)
        return(resultset)
    }