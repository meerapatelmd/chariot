#' Query based on search terms that does not write to catalogue
#' @param ... vector of phrases to collectively feed into the LIKE sql statement
#' @return resultset as a dataframe with all column types as character and trimmed white space
#' @importFrom mySeagull connect_to_local_postgres
#' @import DBI
#' @importFrom rubix call_mr_clean
#' @import dplyr
#' @export

query_string_as_vector <-
        function(string, split = " ", limit = NULL) {
                Args <- strsplit(string, split = split)
                
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
