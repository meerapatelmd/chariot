#' Query all "Maps to" relationships of a concept_id
#' @inheritParams write_sql_for_maps_to
#' @importFrom dplyr full_join
#' @importFrom dplyr rename_all
#' @importFrom seagull write_query_where_in
#' @export

query_maps_to <-
    function(concept_id) {
        sql <- write_sql_for_maps_to(concept_id = concept_id)
        
        resultset <- query_athena(sql_statement = sql)
        
        sql <- seagull::write_query_where_in(table_name = "concept",
                                             column_name = "concept_id",
                                             where_in_vector = resultset$concept_id_2)
        
        resultset2 <- query_athena(sql_statement = sql) %>%
                            dplyr::rename_all(function(x) paste0(x, "_2"))
        
        output <- dplyr::full_join(x = resultset,
                                   y = resultset2,
                                   by = c("concept_id_2" = "concept_id_2"))
        
        return(output)
        
    }