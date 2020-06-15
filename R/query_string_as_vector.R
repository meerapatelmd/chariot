#' Query based on search terms that does not write to catalogue
#' @param ... vector of phrases to collectively feed into the LIKE sql statement
#' @return resultset as a dataframe with all column types as character and trimmed white space
#' @export

query_string_as_vector <-
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
                } else {
                    Args <- lapply(Args, tolower)
                    if (is.null(limit)) {
                        for (i in 1:length(Args)) {
                            if (i == 1) {
                                sql_statement <- paste0("SELECT * FROM public.concept WHERE LOWER(concept_name) LIKE '%", Args[[1]], "%'")
                            } else {
                                sql_statement <- paste0(sql_statement,
                                                        paste0(" AND LOWER(concept_name) LIKE '%", Args[[i]], "%'"))
                            }
                        }
                        sql_statement <- paste0(sql_statement, ";")
                    } else {
                        for (i in 1:length(Args)) {
                            if (i == 1) {
                                sql_statement <- paste0("SELECT * FROM public.concept WHERE LOWER(concept_name) LIKE '%", Args[[1]], "%'")
                            } else {
                                sql_statement <- paste0(sql_statement,
                                                        paste0(" AND LOWER(concept_name) LIKE '%", Args[[i]], "%'"))
                            }
                        }
                        sql_statement <- paste0(sql_statement, "LIMIT ", limit, ";")
                    }
                }

                resultset <- query_athena(sql_statement = sql_statement)
                return(resultset)
        }


