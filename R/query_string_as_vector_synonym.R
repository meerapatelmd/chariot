#' Query based on search terms that does not write to catalogue
#' @param ... vector of phrases to collectively feed into the LIKE sql statement
#' @return resultset as a dataframe with all column types as character and trimmed white space
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @export

query_string_as_vector_synonym <-
        function(string, split = " ", limit = NULL, case_insensitive = TRUE, verbose = TRUE) {
            
                Args <- strsplit(string, split = split) %>%
                            unlist()
                
                #Removing terminal punctuation
                Args <- stringr::str_remove_all(Args, pattern = "[[:punct:]]{1}$")
                
                if (verbose) {
                    secretary::typewrite(crayon::bold("Vector:"))
                    Args %>%
                        purrr::map(secretary::typewrite, tabs = 1)
                }
                
                if (case_insensitive == FALSE) {
                    if (is.null(limit)) {
                        for (i in 1:length(Args)) {
                            if (i == 1) {
                                sql_statement <- paste0("SELECT * FROM public.concept_synonym WHERE concept_synonym_name LIKE '%", Args[[1]], "%'")
                            } else {
                                sql_statement <- paste0(sql_statement,
                                                        paste0(" AND concept_synonym_name LIKE '%", Args[[i]], "%'"))
                            }
                        }
                        sql_statement <- paste0(sql_statement, ";")
                    } else {
                        for (i in 1:length(Args)) {
                            if (i == 1) {
                                sql_statement <- paste0("SELECT * FROM public.concept_synonym WHERE concept_synonym_name LIKE '%", Args[[1]], "%'")
                            } else {
                                sql_statement <- paste0(sql_statement,
                                                        paste0(" AND concept_synonym_name LIKE '%", Args[[i]], "%'"))
                            }
                        }
                        sql_statement <- paste0(sql_statement, "LIMIT ", limit, ";")
                    }
                } else {
                    Args <- lapply(Args, tolower)
                    if (is.null(limit)) {
                        for (i in 1:length(Args)) {
                            if (i == 1) {
                                sql_statement <- paste0("SELECT * FROM public.concept_synonym WHERE LOWER(concept_synonym_name) LIKE '%", Args[[1]], "%'")
                            } else {
                                sql_statement <- paste0(sql_statement,
                                                        paste0(" AND LOWER(concept_synonym_name) LIKE '%", Args[[i]], "%'"))
                            }
                        }
                        sql_statement <- paste0(sql_statement, ";")
                    } else {
                        for (i in 1:length(Args)) {
                            if (i == 1) {
                                sql_statement <- paste0("SELECT * FROM public.concept_synonym WHERE LOWER(concept_synonym_name) LIKE '%", Args[[1]], "%'")
                            } else {
                                sql_statement <- paste0(sql_statement,
                                                        paste0(" AND LOWER(concept_synonym_name) LIKE '%", Args[[i]], "%'"))
                            }
                        }
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


