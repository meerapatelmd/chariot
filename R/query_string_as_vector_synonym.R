#' Query based on search terms that does not write to catalogue
#' @param ... vector of phrases to collectively feed into the LIKE sql statement
#' @return resultset as a dataframe with all column types as character and trimmed white space
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @export

query_string_as_vector_synonym <-
        function(string, split = " |[[:punct:]]", limit = NULL, case_insensitive = TRUE, verbose = TRUE, override_cache = FALSE) {

                .Deprecated(new = "queryPhraseStringSynonym")

                Args <- strsplit(string, split = split) %>%
                            unlist() %>%
                            centipede::no_blank()

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

                #print(sql_statement)
                resultset <- query_athena(sql_statement = sql_statement, override_cache = override_cache)
                print(resultset)

                output <- left_join_concept(resultset %>%
                                                dplyr::select(concept_synonym_name = concept_id),
                                            include_synonyms = FALSE,
                                            override_cache = override_cache)

                return(output)
        }


