#' Query based on search terms that does not write to catalogue
#' @param ... vector of codes to collectively feed into the LIKE sql statement
#' @return resultset as a dataframe with all column types as character and trimmed white space
#' @importFrom seagull connect_to_local_postgres
#' @export

query_code <-
        function(code, limit = NULL, type = c("like", "exact")) {
                if (type == "exact") {
                        if (is.null(limit)) {
                                sql_statement <- paste0("SELECT * FROM public.concept WHERE concept_code = '", code, "';")
                        } else {
                                sql_statement <- paste0("SELECT * FROM public.concept WHERE concept_code = '", code, "' ")
                                sql_statement <- paste0(sql_statement, "LIMIT ", limit, ";")
                        }

                } else if (type == "like") {
                        if (is.null(limit)) {
                                sql_statement <- paste0("SELECT * FROM public.concept WHERE concept_code LIKE '%", code, "%';")
                        } else {
                                sql_statement <- paste0("SELECT * FROM public.concept WHERE concept_code LIKE '%", code, "%' ")
                                sql_statement <- paste0(sql_statement, "LIMIT ", limit, ";")
                        }
                }
            
                resultset <- query_athena(sql_statement = sql_statement)
                return(resultset)
        }
