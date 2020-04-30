#' Query based on search terms that does not write to catalogue
#' @param ... vector of phrases to collectively feed into the LIKE sql statement
#' @return resultset as a dataframe with all column types as character and trimmed white space
#' @export

query_phrase <-
        function(phrase, limit = NULL, type = c("like", "exact"), case_insensitive = TRUE) {
            if (case_insensitive == FALSE) {
                if (type == "exact") {
                    if (is.null(limit)) {
                        sql_statement <- paste0("SELECT * FROM public.concept WHERE concept_name = '", phrase, "';")
                    } else {
                        sql_statement <- paste0("SELECT * FROM public.concept WHERE concept_name = '", phrase, "' ")
                        sql_statement <- paste0(sql_statement, "LIMIT ", limit, ";")
                    }
                    
                } else if (type == "like") {
                    if (is.null(limit)) {
                        sql_statement <- paste0("SELECT * FROM public.concept WHERE concept_name LIKE '%", phrase, "%';")
                    } else {
                        sql_statement <- paste0("SELECT * FROM public.concept WHERE concept_name LIKE '%", phrase, "%' ")
                        sql_statement <- paste0(sql_statement, "LIMIT ", limit, ";")
                    }
                }

            } else {
                if (type == "exact") {
                    if (is.null(limit)) {
                        sql_statement <- paste0("SELECT * FROM public.concept WHERE LOWER(concept_name) = '", tolower(phrase), "';")
                    } else {
                        sql_statement <- paste0("SELECT * FROM public.concept WHERE LOWER(concept_name) = '", tolower(phrase), "' ")
                        sql_statement <- paste0(sql_statement, "LIMIT ", limit, ";")
                    }
                    
                } else if (type == "like") {
                    if (is.null(limit)) {
                        sql_statement <- paste0("SELECT * FROM public.concept WHERE LOWER(concept_name) LIKE '%", tolower(phrase), "%';")
                    } else {
                        sql_statement <- paste0("SELECT * FROM public.concept WHERE LOWER(concept_name) LIKE '%", tolower(phrase), "%' ")
                        sql_statement <- paste0(sql_statement, "LIMIT ", limit, ";")
                    }
                }
                
            }
            
            resultset <- query_athena(sql_statement = sql_statement)
            return(resultset)
              
        }
