#' Perform a Global Query on a Single Phrase
#' @export

queryPhrase <-
        function(schema,
                 phrase,
                 split,
                 caseInsensitive = TRUE,
                 concept_table_search = c("exact", "string", "like"),
                 concept_synonym_table_search = c("exact", "string", "like"),
                 conn = NULL,
                 render_sql = TRUE,
                 cache_resultset = TRUE,
                 override_cache = FALSE,
                 verbose = FALSE) {

                if (any(!(concept_table_search %in% c("exact", "string", "like")))) {
                        stop("'concept_table_search' can only be one of c('exact', 'string', 'like')")
                }

                if (any(!(concept_synonym_table_search %in% c("exact", "string", "like")))) {
                        stop("'concept_synonym_table_search' can only be one of c('exact', 'string', 'like')")
                }

                concept_resultsets <- list()

                while (length(concept_table_search) > 0) {

                        search <- concept_table_search[1]

                        if (search == "exact") {

                                concept_resultsets[[1+length(concept_resultsets)]] <-
                                        queryPhraseExact(schema = schema,
                                                         phrase = phrase,
                                                         caseInsensitive = caseInsensitive,
                                                         conn = conn,
                                                         render_sql = render_sql,
                                                         cache_resultset = cache_resultset,
                                                         override_cache = override_cache,
                                                         verbose = verbose)

                        } else if (search == "like") {

                                concept_resultsets[[1+length(concept_resultsets)]] <-
                                        queryPhraseLike(schema = schema,
                                                         phrase = phrase,
                                                         caseInsensitive = caseInsensitive,
                                                         conn = conn,
                                                         render_sql = render_sql,
                                                         cache_resultset = cache_resultset,
                                                         override_cache = override_cache,
                                                         verbose = verbose)

                        } else if (search == "string") {

                                concept_resultsets[[1+length(concept_resultsets)]] <-
                                        queryPhraseString(schema = schema,
                                                        phrase = phrase,
                                                        split = split,
                                                        caseInsensitive = caseInsensitive,
                                                        conn = conn,
                                                        render_sql = render_sql,
                                                        cache_resultset = cache_resultset,
                                                        override_cache = override_cache,
                                                        verbose = verbose)


                        }

                        names(concept_resultsets)[length(concept_resultsets)] <- search
                        concept_table_search <- concept_table_search[-1]
                        Sys.sleep(.2)
                }

                while (length(concept_synonym_table_search) > 0) {

                        search <- concept_synonym_table_search[1]

                        if (search == "exact") {

                                concept_resultsets[[1+length(concept_resultsets)]] <-
                                        queryPhraseExactSynonym(schema = schema,
                                                         phrase = phrase,
                                                         caseInsensitive = caseInsensitive,
                                                         conn = conn,
                                                         render_sql = render_sql,
                                                         cache_resultset = cache_resultset,
                                                         override_cache = override_cache,
                                                         verbose = verbose)

                        } else if (search == "like") {

                                concept_resultsets[[1+length(concept_resultsets)]] <-
                                        queryPhraseLikeSynonym(schema = schema,
                                                        phrase = phrase,
                                                        caseInsensitive = caseInsensitive,
                                                        conn = conn,
                                                        render_sql = render_sql,
                                                        cache_resultset = cache_resultset,
                                                        override_cache = override_cache,
                                                        verbose = verbose)

                        } else if (search == "string") {

                                concept_resultsets[[1+length(concept_resultsets)]] <-
                                        queryPhraseStringSynonym(schema = schema,
                                                          phrase = phrase,
                                                          split = split,
                                                          caseInsensitive = caseInsensitive,
                                                          conn = conn,
                                                          render_sql = render_sql,
                                                          cache_resultset = cache_resultset,
                                                          override_cache = override_cache,
                                                          verbose = verbose)


                        }

                        names(concept_resultsets)[length(concept_resultsets)] <- paste0("synonym_", search)
                        concept_synonym_table_search <- concept_synonym_table_search[-1]
                        Sys.sleep(.2)
                }


                return(concept_resultsets)

        }
