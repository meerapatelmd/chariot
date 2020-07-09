#' Query based on search terms that does not write to catalogue
#' @param ... vector of phrases to collectively feed into the LIKE sql statement
#' @return resultset as a dataframe with all column types as character and trimmed white space
#' @export

query_phrase <-
        function(phrase, 
                 type = c("like", "exact"), 
                 limit = NULL,
                 case_insensitive = TRUE,
                 return_valid_only = TRUE,
                 cache_resultset = TRUE,
                 override_cache = FALSE,
                 where_col = NULL,
                 where_col_in = NULL,
                 omop = FALSE,
                 omop_schema = "omop_vocabulary") {
            
            
            if (length(type) != 1) {

                    warning('input parameter "type" missing and defaulting to "type" = "exact"')
                    type <- "exact"

            }
            # 
            # 
            # if ((any(
            #           !is.null(where_col),
            #           !is.null(where_col_in)))) {
            #     
            #     
            #     if (any(
            #             is.null(where_col),
            #             is.null(where_col_in))) {
            #     
            #                 stop('to filter all `where_` parameters require an input or all must be left NULL.')
            #     
            #     }
            # }
            #    
            # 
            # if (case_insensitive == FALSE) {
            #     
            #             if (type == "exact") {
            #                 
            #                         sql_statement <- paste0("SELECT * FROM concept WHERE concept_name = '", phrase, "'")
            #                         
            #                         if (!is.null(where_col)) {
            #                             
            #                                 if (is.character(where_col_in)) {
            #                                     
            #                                         where_col_in <- seagull::write_where_in_string(where_in_vector = where_col_in)
            #                                     
            #                                 } else {
            #                                     
            #                                         where_col_in <- seagull::write_where_in_string(where_in_vector = where_col_in, as_character = FALSE)
            #                                 }
            #                             
            #                                 sql_statement <- paste0(sql_statement, " AND ", where_col, " IN ", where_col_in, "")
            #                             
            #                         }
            #                         
            #                         
            #                         if (!is.null(limit)) {
            #                             
            #                                     sql_statement <- paste0(sql_statement, " LIMIT ", limit)
            #                             
            #                         }
            #     
            #                         
            #     } else if (type == "like") {
            #         
            #             sql_statement <- paste0("SELECT * FROM concept WHERE concept_name LIKE '%", phrase, "%'")
            #         
            #         if (!is.null(where_col)) {
            #             
            #             if (is.character(where_col_in)) {
            #                 where_col_in <- seagull::write_where_in_string(where_in_vector = where_col_in)
            #             } else {
            #                 where_col_in <- seagull::write_where_in_string(where_in_vector = where_col_in, as_character = FALSE)
            #             }
            #             
            #             sql_statement <- paste0(sql_statement, " AND ", where_col, " IN ", where_col_in, "")
            #             
            #         }
            #         
            #         
            #         if (!is.null(limit)) {
            #             
            #             sql_statement <- paste0(sql_statement, " LIMIT ", limit)
            #             
            #         }
            #     }
            # 
            # } else {
            #     if (type == "exact") {
            #         
            #         sql_statement <- paste0("SELECT * FROM concept WHERE LOWER(concept_name) = '", tolower(phrase), "'")
            #         
            #         if (!is.null(where_col)) {
            #             
            #             if (is.character(where_col_in)) {
            #                 where_col_in <- seagull::write_where_in_string(where_in_vector = where_col_in)
            #             } else {
            #                 where_col_in <- seagull::write_where_in_string(where_in_vector = where_col_in, as_character = FALSE)
            #             }
            #             
            #             sql_statement <- paste0(sql_statement, " AND ", where_col, " IN ", where_col_in, "")
            #             
            #         }
            #         
            #         
            #         if (!is.null(limit)) {
            #             
            #             sql_statement <- paste0(sql_statement, " LIMIT ", limit)
            #             
            #         }
            #         
            #     } else if (type == "like") {
            #         sql_statement <- paste0("SELECT * FROM concept WHERE LOWER(concept_name) LIKE '%", tolower(phrase), "%'")
            #         
            #         if (!is.null(where_col)) {
            #             
            #             if (is.character(where_col_in)) {
            #                 where_col_in <- seagull::write_where_in_string(where_in_vector = where_col_in)
            #             } else {
            #                 where_col_in <- seagull::write_where_in_string(where_in_vector = where_col_in, as_character = FALSE)
            #             }
            #             
            #             sql_statement <- paste0(sql_statement, " AND ", where_col, " IN ", where_col_in, "")
            #             
            #         }
            #         
            #         
            #         if (!is.null(limit)) {
            #             
            #             sql_statement <- paste0(sql_statement, " LIMIT ", limit)
            #             
            #         }
            #     }
            #     
            # }
            
            
            sql_statement <- 
                phrase_sql(phrase = phrase,
                           type = type,
                           limit = limit,
                           case_insensitive = case_insensitive,
                           where_col = where_col,
                           where_col_in = where_col_in)
            
            if (omop) {
                
                resultset <- 
                            fantasia::query_omop(sql_statement = sql_statement,
                                                 schema = omop_schema,
                                                 override_cache = override_cache)
                
                
                
                
            } else {
            
                resultset <- query_athena(sql_statement = sql_statement,
                                          cache_resultset = cache_resultset,
                                          override_cache = override_cache)
            
            }
            
            if (return_valid_only) {
                resultset %>%
                    return_valid()
            } else {
                return(resultset)
            }
              
        }
