#' Query all the relationships for a vocabulary id
#' @export


query_vocab_relationships <-
    function(vocabulary_id) {
                    sql <-
                        write_sql_all_vocab_relationships(vocabulary_id = vocabulary_id)
                    resultset <- query_athena(sql_statement = sql)
                    return(resultset)
    }
