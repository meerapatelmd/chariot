#' Query more than 1 search terms
#' @param ... vector of terms to query
#' @return resultset as a dataframe with all column types as character and trimmed white space
#' @export

query_phrases <-
        function(..., limit = NULL) {
                Args <- list(...)
                        if (is.null(limit)) {
                                for (i in 1:length(Args)) {
                                        if (i == 1) {
                                                sql_statement <- paste0("SELECT * FROM public.concept WHERE concept_name LIKE '%", Args[[1]], "%'")
                                        } else {
                                                sql_statement <- paste0(sql_statement,
                                                                        paste0(" AND concept_name LIKE '%", Args[[i]], "%'"))
                                        }
                                }
                                sql_statement <- paste0(sql_statement, ";")
                        } else {
                                for (i in 1:length(Args)) {
                                        if (i == 1) {
                                                sql_statement <- paste0("SELECT * FROM public.concept WHERE concept_name LIKE '%", Args[[1]], "%'")
                                        } else {
                                                sql_statement <- paste0(sql_statement,
                                                                        paste0(" AND concept_name LIKE '%", Args[[i]], "%'"))
                                        }
                                }
                                sql_statement <- paste0(sql_statement, "LIMIT ", limit, ";")
                        }

                resultset <- query_athena(sql_statement = sql_statement)
                return(resultset)
        }
