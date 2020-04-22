#' Query based on search terms in the concept synonym table
#' @param phrase vector of phrases to collectively feed into the LIKE sql statement
#' @return resultset as a dataframe with all column types
#' @importFrom mySeagull connect_to_local_postgres
#' @import DBI
#' @importFrom rubix call_mr_clean
#' @import dplyr
#' @export

query_phrase_synonym <-
        function(phrase, limit = NULL, type = c("like", "exact")) {
                if (type == "exact") {
                        if (is.null(limit)) {
                                sql_statement <- paste0("SELECT * FROM public.concept_synonym WHERE concept_name = '", phrase, "';")
                        } else {
                                sql_statement <- paste0("SELECT * FROM public.concept_synonym WHERE concept_name = '", phrase, "' ")
                                sql_statement <- paste0(sql_statement, "LIMIT ", limit, ";")
                        }

                } else if (type == "like") {
                        if (is.null(limit)) {
                                sql_statement <- paste0("SELECT * FROM public.concept_synonym WHERE concept_name LIKE '%", phrase, "%';")
                        } else {
                                sql_statement <- paste0("SELECT * FROM public.concept_synonym WHERE concept_name LIKE '%", phrase, "%' ")
                                sql_statement <- paste0(sql_statement, "LIMIT ", limit, ";")
                        }
                }

                conn_to_athena <- mySeagull::connect_to_local_postgres(dbname = "athena")
                resultset <- DBI::dbGetQuery(conn = conn_to_athena,
                                             statement = sql_statement)
                DBI::dbDisconnect(conn_to_athena)
                return(resultset)
        }
