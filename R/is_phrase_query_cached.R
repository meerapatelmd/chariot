#' Is the query_phrase cached?
#' @export

is_phrase_query_cached <- 
    function(phrase,
             type,
             limit = NULL,
             case_insensitive = TRUE,
             where_col = NULL,
             where_col_in = NULL) {
        
        
                sql_statement <- 
                    phrase_sql(phrase = phrase,
                               type = type,
                               limit = limit,
                               case_insensitive = case_insensitive,
                               where_col = where_col,
                               where_col_in = where_col_in)
                
                cached_data <- 
                    load_athena_query(sql_statement = sql_statement)
                
                if (!is.null(cached_data)) {
                    
                        TRUE
                    
                } else {
                    
                    FALSE
                }
        
    }