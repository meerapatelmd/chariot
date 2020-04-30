#' Query based on search terms in the concept synonym table
#' @param phrase vector of phrases to collectively feed into the LIKE sql statement
#' @return resultset as a dataframe with all column types
#' @importFrom seagull write_query_where_in
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @export

query_phrase_synonym <-
        function(phrase, limit = NULL, type = c("like", "exact"), case_insensitive = TRUE) {
            if (case_insensitive == FALSE) {
                if (type == "exact") {
                    if (is.null(limit)) {
                        sql_statement <- paste0("SELECT * FROM public.concept_synonym WHERE concept_synonym_name = '", phrase, "';")
                    } else {
                        sql_statement <- paste0("SELECT * FROM public.concept_synonym WHERE concept_synonym_name = '", phrase, "' ")
                        sql_statement <- paste0(sql_statement, "LIMIT ", limit, ";")
                    }
                    
                } else if (type == "like") {
                    if (is.null(limit)) {
                        sql_statement <- paste0("SELECT * FROM public.concept_synonym WHERE concept_synonym_name LIKE '%", phrase, "%';")
                    } else {
                        sql_statement <- paste0("SELECT * FROM public.concept_synonym WHERE concept_synonym_name LIKE '%", phrase, "%' ")
                        sql_statement <- paste0(sql_statement, "LIMIT ", limit, ";")
                    }
                }
                
                resultset <- query_athena(sql_statement = sql_statement)
                
                sql_statement <- seagull::write_query_where_in(table_name = "concept",
                                                               column_name = "concept_id",
                                                               where_in_vector = resultset$concept_id)
                
                output <- query_athena(sql_statement)
                output <- 
                    output %>%
                    dplyr::left_join(resultset %>%
                                         dplyr::select(-language_concept_id))
                return(output)
            } else {
                if (type == "exact") {
                    if (is.null(limit)) {
                        sql_statement <- paste0("SELECT * FROM public.concept_synonym WHERE LOWER(concept_synonym_name) = '", tolower(phrase), "';")
                    } else {
                        sql_statement <- paste0("SELECT * FROM public.concept_synonym WHERE LOWER(concept_synonym_name) = '", tolower(phrase), "' ")
                        sql_statement <- paste0(sql_statement, "LIMIT ", limit, ";")
                    }
                    
                } else if (type == "like") {
                    if (is.null(limit)) {
                        sql_statement <- paste0("SELECT * FROM public.concept_synonym WHERE LOWER(concept_synonym_name) LIKE '%", tolower(phrase), "%';")
                    } else {
                        sql_statement <- paste0("SELECT * FROM public.concept_synonym WHERE LOWER(concept_synonym_name) LIKE '%", tolower(phrase), "%' ")
                        sql_statement <- paste0(sql_statement, "LIMIT ", limit, ";")
                    }
                }
                
                resultset <- query_athena(sql_statement = sql_statement)
                
                sql_statement <- seagull::write_query_where_in(table_name = "concept",
                                                               column_name = "concept_id",
                                                               where_in_vector = resultset$concept_id)
                
                output <- query_athena(sql_statement)
                output <- 
                    output %>%
                    dplyr::left_join(resultset %>%
                                         dplyr::select(-language_concept_id))
                return(output)
            }
               
        }
