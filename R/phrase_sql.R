#' Construct SQL for query_phrase
#' @description Also serves as the cache key
#' @import seagull
#' @export

phrase_sql <- 
    function(phrase, 
             type, 
             schema = NULL,
             limit = NULL,
             case_insensitive = TRUE,
             where_col = NULL,
             where_col_in = NULL) {

        
        
        if ((any(
            !is.null(where_col),
            !is.null(where_col_in)))) {
            
            
            if (any(
                is.null(where_col),
                is.null(where_col_in))) {
                
                stop('to filter all `where_` parameters require an input or all must be left NULL.')
                
            }
        }
        
        if (is.null(schema)) {
            
            schema <- "public"
        }
        
        
        if (case_insensitive == FALSE) {
            
            if (type == "exact") {
                
                sql_statement <- paste0("SELECT * FROM ", schema, ".concept WHERE concept_name = '", phrase, "'")
                
                if (!is.null(where_col)) {
                    
                    if (is.character(where_col_in)) {
                        
                        where_col_in <- seagull::write_where_in_string(where_in_vector = where_col_in)
                        
                    } else {
                        
                        where_col_in <- seagull::write_where_in_string(where_in_vector = where_col_in, as_character = FALSE)
                    }
                    
                    sql_statement <- paste0(sql_statement, " AND ", where_col, " IN ", where_col_in, "")
                    
                }
                
                
                if (!is.null(limit)) {
                    
                    sql_statement <- paste0(sql_statement, " LIMIT ", limit)
                    
                }
                
                
            } else if (type == "like") {
                
                sql_statement <- paste0("SELECT * FROM ", schema, ".concept WHERE concept_name LIKE '%", phrase, "%'")
                
                if (!is.null(where_col)) {
                    
                    if (is.character(where_col_in)) {
                        where_col_in <- seagull::write_where_in_string(where_in_vector = where_col_in)
                    } else {
                        where_col_in <- seagull::write_where_in_string(where_in_vector = where_col_in, as_character = FALSE)
                    }
                    
                    sql_statement <- paste0(sql_statement, " AND ", where_col, " IN ", where_col_in, "")
                    
                }
                
                
                if (!is.null(limit)) {
                    
                    sql_statement <- paste0(sql_statement, " LIMIT ", limit)
                    
                }
            }
            
        } else {
            if (type == "exact") {
                
                sql_statement <- paste0("SELECT * FROM ", schema, ".concept WHERE LOWER(concept_name) = '", tolower(phrase), "'")
                
                if (!is.null(where_col)) {
                    
                    if (is.character(where_col_in)) {
                        where_col_in <- seagull::write_where_in_string(where_in_vector = where_col_in)
                    } else {
                        where_col_in <- seagull::write_where_in_string(where_in_vector = where_col_in, as_character = FALSE)
                    }
                    
                    sql_statement <- paste0(sql_statement, " AND ", where_col, " IN ", where_col_in, "")
                    
                }
                
                
                if (!is.null(limit)) {
                    
                    sql_statement <- paste0(sql_statement, " LIMIT ", limit)
                    
                }
                
            } else if (type == "like") {
                sql_statement <- paste0("SELECT * FROM ", schema, ".concept WHERE LOWER(concept_name) LIKE '%", tolower(phrase), "%'")
                
                if (!is.null(where_col)) {
                    
                    if (is.character(where_col_in)) {
                        where_col_in <- seagull::write_where_in_string(where_in_vector = where_col_in)
                    } else {
                        where_col_in <- seagull::write_where_in_string(where_in_vector = where_col_in, as_character = FALSE)
                    }
                    
                    sql_statement <- paste0(sql_statement, " AND ", where_col, " IN ", where_col_in, "")
                    
                }
                
                
                if (!is.null(limit)) {
                    
                    sql_statement <- paste0(sql_statement, " LIMIT ", limit)
                    
                }
            }
            
        }
        
        return(sql_statement)
        
        
    }