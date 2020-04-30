#' Query based on search terms that does not write to catalogue
#' @param ... vector of codes to collectively feed into the LIKE sql statement
#' @return resultset as a dataframe with all column types as character and trimmed white space
#' @export

query_where_in <-
        function(column_name, inclusion_vector, limit = NULL) {

                        if (is.null(limit)) {
                                sql_statement <- paste0("SELECT * FROM public.concept WHERE ", column_name, " IN (", paste(paste0("'", inclusion_vector, "'"), collapse = ", "), ");")
                        } else {
                                sql_statement <- paste0("SELECT * FROM public.concept WHERE ", column_name, " IN (", paste(paste0("'", inclusion_vector, "'"), collapse = ", "), ")")
                                sql_statement <- paste0(sql_statement, "LIMIT ", limit, ";")
                        }


                resultset <- query_athena(sql_statement = sql_statement)
                return(resultset)
        }
