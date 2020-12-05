#' @title
#' Search for a Code in the Concept Table
#'
#' @inheritParams queryAthena
#' @inheritParams grepl
#'
#' @return resultset as a dataframe with all column types as character and trimmed white space
#' @export
#' @rdname searchCode

searchCode <-
        function(code_pattern,
                 ignore.case = FALSE,
                 perl = FALSE,
                 fixed = FALSE,
                 useBytes = FALSE,
                 schema,
                 limit = NULL,
                 verbose = FALSE,
                 conn = NULL,
                 conn_fun,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 render_sql = FALSE,
                 sleepTime = 1) {


                        code <- stringr::str_replace_all(string = code_pattern,
                                                         pattern = "[^0-9A-Za-z]{1,}",
                                                         replacement = "%")

                        sql_statement <-
                                pg13::buildQueryLike(schema = schema,
                                                     tableName = "concept",
                                                     whereLikeField = "concept_code",
                                                     whereLikeValue = code,
                                                     caseInsensitive = TRUE,
                                                     limit_n = limit)

                        resultset <- queryAthena(sql_statement = sql_statement,
                                            conn = conn,
                                            conn_fun = conn_fun,
                                            cache_only = cache_only,
                                            skip_cache = skip_cache,
                                            override_cache = override_cache,
                                            render_sql = render_sql,
                                            verbose = verbose,
                                            sleepTime = sleepTime)

                        resultset %>%
                                dplyr::filter_at(dplyr::vars(concept_code),
                                                 dplyr::all_vars(grepl(pattern = code_pattern,
                                                                       x = .,
                                                                       ignore.case = ignore.case,
                                                                       perl = perl,
                                                                       fixed = fixed,
                                                                       useBytes = useBytes)))
        }


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param schema PARAM_DESCRIPTION
#' @param phrase PARAM_DESCRIPTION
#' @param split PARAM_DESCRIPTION
#' @param caseInsensitive PARAM_DESCRIPTION, Default: TRUE
#' @param concept_table_search PARAM_DESCRIPTION, Default: c("exact", "string", "like")
#' @param concept_synonym_table_search PARAM_DESCRIPTION, Default: c("exact", "string", "like")
#' @param conn PARAM_DESCRIPTION, Default: NULL
#' @param cache_only PARAM_DESCRIPTION, Default: FALSE
#' @param skip_cache PARAM_DESCRIPTION, Default: FALSE
#' @param override_cache PARAM_DESCRIPTION, Default: FALSE
#' @param render_sql PARAM_DESCRIPTION, Default: FALSE
#' @param verbose PARAM_DESCRIPTION, Default: FALSE
#' @param sleepTime PARAM_DESCRIPTION, Default: 1
#' @return OUTPUT_DESCRIPTION
#' @rdname searchPhrase
#' @export
#' @family query functions
searchPhrase <-
        function(schema,
                 phrase,
                 split,
                 caseInsensitive = TRUE,
                 concept_table_search = c("exact", "string", "like"),
                 concept_synonym_table_search = c("exact", "string", "like"),
                 conn = NULL,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 render_sql = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {

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
                                        searchPhraseExact(schema = schema,
                                                         phrase = phrase,
                                                         caseInsensitive = caseInsensitive,
                                                         conn = conn,
                                                         render_sql = render_sql,
                                                         cache_resultset = cache_resultset,
                                                         override_cache = override_cache,
                                                         verbose = verbose)

                        } else if (search == "like") {

                                concept_resultsets[[1+length(concept_resultsets)]] <-
                                        searchPhraseLike(schema = schema,
                                                        phrase = phrase,
                                                        caseInsensitive = caseInsensitive,
                                                        conn = conn,
                                                        render_sql = render_sql,
                                                        cache_resultset = cache_resultset,
                                                        override_cache = override_cache,
                                                        verbose = verbose)

                        } else if (search == "string") {

                                concept_resultsets[[1+length(concept_resultsets)]] <-
                                        searchPhraseString(schema = schema,
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
                                        searchPhraseExactSynonym(schema = schema,
                                                                phrase = phrase,
                                                                caseInsensitive = caseInsensitive,
                                                                conn = conn,
                                                                render_sql = render_sql,
                                                                cache_resultset = cache_resultset,
                                                                override_cache = override_cache,
                                                                verbose = verbose)

                        } else if (search == "like") {

                                concept_resultsets[[1+length(concept_resultsets)]] <-
                                        searchPhraseLikeSynonym(schema = schema,
                                                               phrase = phrase,
                                                               caseInsensitive = caseInsensitive,
                                                               conn = conn,
                                                               render_sql = render_sql,
                                                               cache_resultset = cache_resultset,
                                                               override_cache = override_cache,
                                                               verbose = verbose)

                        } else if (search == "string") {

                                concept_resultsets[[1+length(concept_resultsets)]] <-
                                        searchPhraseStringSynonym(schema = schema,
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





#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param schema PARAM_DESCRIPTION
#' @param phrase PARAM_DESCRIPTION
#' @param case_insensitive PARAM_DESCRIPTION
#' @param conn PARAM_DESCRIPTION, Default: NULL
#' @param cache_only PARAM_DESCRIPTION, Default: FALSE
#' @param skip_cache PARAM_DESCRIPTION, Default: FALSE
#' @param override_cache PARAM_DESCRIPTION, Default: FALSE
#' @param render_sql PARAM_DESCRIPTION, Default: FALSE
#' @param verbose PARAM_DESCRIPTION, Default: FALSE
#' @param sleepTime PARAM_DESCRIPTION, Default: 1
#' @return OUTPUT_DESCRIPTION
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname searchPhraseExact
#' @export
#' @family query functions
#' @importFrom SqlRender render
searchPhraseExact <-
        function(schema,
                 phrase,
                 case_insensitive,
                 conn = NULL,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 render_sql = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {


                if (case_insensitive) {

                        sql_statement <-
                                SqlRender::render(
                                        "
                                SELECT *
                                FROM @schema.@table c
                                WHERE LOWER(c.concept_name) = LOWER('@phrase')
                                ",
                                        schema = schema,
                                        phrase = phrase,
                                        table = "concept"
                                )


                } else {

                        sql_statement <-
                                SqlRender::render(
                                        "
                                SELECT *
                                FROM @schema.@table c
                                WHERE c.concept_name = '@phrase'
                                ",
                                        schema = schema,
                                        phrase = phrase,
                                        table = "concept"
                                )

                }


                queryAthena(sql_statement = sql_statement,
                            conn = conn,
                            cache_only = cache_only,
                            skip_cache = skip_cache,
                            override_cache = override_cache,
                            render_sql = render_sql,
                            verbose = verbose,
                            sleepTime = sleepTime)
        }






#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param schema PARAM_DESCRIPTION
#' @param case_insensitive PARAM_DESCRIPTION, Default: TRUE
#' @param phrase PARAM_DESCRIPTION
#' @param conn PARAM_DESCRIPTION, Default: NULL
#' @param cache_only PARAM_DESCRIPTION, Default: FALSE
#' @param skip_cache PARAM_DESCRIPTION, Default: FALSE
#' @param override_cache PARAM_DESCRIPTION, Default: FALSE
#' @param render_sql PARAM_DESCRIPTION, Default: FALSE
#' @param verbose PARAM_DESCRIPTION, Default: FALSE
#' @param sleepTime PARAM_DESCRIPTION, Default: 1
#' @return OUTPUT_DESCRIPTION
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname searchPhraseExactSynonym
#' @export
#' @family query functions
#' @importFrom SqlRender render
searchPhraseExactSynonym <-
        function(schema,
                 case_insensitive = TRUE,
                 phrase,
                 conn = NULL,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 render_sql = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {

                if (case_insensitive) {

                        sql_statement <-
                                SqlRender::render(
                                        "
                                SELECT *
                                FROM @schema.@table c
                                WHERE LOWER(c.concept_synonym_name) = LOWER('@phrase')
                                ",
                                        schema = schema,
                                        phrase = phrase,
                                        table = "concept_synonym"
                                )


                } else {

                        sql_statement <-
                                SqlRender::render(
                                        "
                                SELECT *
                                FROM @schema.@table c
                                WHERE c.concept_synonym_name = '@phrase'
                                ",
                                        schema = schema,
                                        phrase = phrase,
                                        table = "concept_synonym"
                                )

                }


                queryAthena(sql_statement = sql_statement,
                            conn = conn,
                            cache_only = cache_only,
                            skip_cache = skip_cache,
                            override_cache = override_cache,
                            render_sql = render_sql,
                            verbose = verbose,
                            sleepTime = sleepTime)


        }





#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param schema PARAM_DESCRIPTION
#' @param phrase PARAM_DESCRIPTION
#' @param caseInsensitive PARAM_DESCRIPTION
#' @param conn PARAM_DESCRIPTION, Default: NULL
#' @param cache_only PARAM_DESCRIPTION, Default: FALSE
#' @param skip_cache PARAM_DESCRIPTION, Default: FALSE
#' @param override_cache PARAM_DESCRIPTION, Default: FALSE
#' @param render_sql PARAM_DESCRIPTION, Default: FALSE
#' @param verbose PARAM_DESCRIPTION, Default: FALSE
#' @param sleepTime PARAM_DESCRIPTION, Default: 1
#' @return OUTPUT_DESCRIPTION
#' @seealso
#'  \code{\link[pg13]{buildQueryLike}}
#' @rdname searchPhraseLike
#' @export
#' @family query functions
#' @importFrom pg13 buildQueryLike
searchPhraseLike <-
        function(schema,
                 phrase,
                 caseInsensitive,
                 conn = NULL,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 render_sql = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {

                sql_statement <-
                        pg13::buildQueryLike(tableName = "concept",
                                             schema = schema,
                                             whereLikeField = "concept_name",
                                             whereLikeValue = phrase,
                                             caseInsensitive = caseInsensitive)


                queryAthena(sql_statement = sql_statement,
                            conn = conn,
                            cache_only = cache_only,
                            skip_cache = skip_cache,
                            override_cache = override_cache,
                            render_sql = render_sql,
                            verbose = verbose,
                            sleepTime = sleepTime)
        }






#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param schema PARAM_DESCRIPTION
#' @param caseInsensitive PARAM_DESCRIPTION, Default: TRUE
#' @param phrase PARAM_DESCRIPTION
#' @param conn PARAM_DESCRIPTION, Default: NULL
#' @param cache_only PARAM_DESCRIPTION, Default: FALSE
#' @param skip_cache PARAM_DESCRIPTION, Default: FALSE
#' @param override_cache PARAM_DESCRIPTION, Default: FALSE
#' @param render_sql PARAM_DESCRIPTION, Default: FALSE
#' @param verbose PARAM_DESCRIPTION, Default: FALSE
#' @param sleepTime PARAM_DESCRIPTION, Default: 1
#' @return OUTPUT_DESCRIPTION
#' @rdname searchPhraseLikeSynonym
#' @export
#' @family query functions
searchPhraseLikeSynonym <-
        function(schema,
                 caseInsensitive = TRUE,
                 phrase,
                 conn = NULL,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 render_sql = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {

                sql_statement <-
                        rendersearchPhraseLikeSynonym(schema = schema,
                                                     phrase = phrase,
                                                     caseInsensitive = caseInsensitive)


                queryAthena(sql_statement = sql_statement,
                            conn = conn,
                            cache_only = cache_only,
                            skip_cache = skip_cache,
                            override_cache = override_cache,
                            render_sql = render_sql,
                            verbose = verbose,
                            sleepTime = sleepTime)

        }






#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param schema PARAM_DESCRIPTION
#' @param caseInsensitive PARAM_DESCRIPTION, Default: TRUE
#' @param phrase PARAM_DESCRIPTION
#' @param split PARAM_DESCRIPTION, Default: ' '
#' @param conn PARAM_DESCRIPTION, Default: NULL
#' @param cache_only PARAM_DESCRIPTION, Default: FALSE
#' @param skip_cache PARAM_DESCRIPTION, Default: FALSE
#' @param override_cache PARAM_DESCRIPTION, Default: FALSE
#' @param render_sql PARAM_DESCRIPTION, Default: FALSE
#' @param verbose PARAM_DESCRIPTION, Default: FALSE
#' @param sleepTime PARAM_DESCRIPTION, Default: 1
#' @return OUTPUT_DESCRIPTION
#' @seealso
#'  \code{\link[pg13]{buildQueryString}}
#' @rdname searchPhraseString
#' @export
#' @family query functions
#' @importFrom pg13 buildQueryString
searchPhraseString <-
        function(schema,
                 caseInsensitive = TRUE,
                 phrase,
                 split = " ",
                 conn = NULL,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 render_sql = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {

                sql_statement <-
                        pg13::buildQueryString(schema = schema,
                                               tableName = "concept",
                                               whereLikeField = "concept_name",
                                               string=phrase,
                                               split=split,
                                               caseInsensitive = caseInsensitive)

                queryAthena(sql_statement = sql_statement,
                            conn = conn,
                            cache_only = cache_only,
                            skip_cache = skip_cache,
                            override_cache = override_cache,
                            render_sql = render_sql,
                            verbose = verbose,
                            sleepTime = sleepTime)

        }






#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param schema PARAM_DESCRIPTION
#' @param caseInsensitive PARAM_DESCRIPTION, Default: TRUE
#' @param phrase PARAM_DESCRIPTION
#' @param split PARAM_DESCRIPTION, Default: ' '
#' @param conn PARAM_DESCRIPTION, Default: NULL
#' @param cache_only PARAM_DESCRIPTION, Default: FALSE
#' @param skip_cache PARAM_DESCRIPTION, Default: FALSE
#' @param override_cache PARAM_DESCRIPTION, Default: FALSE
#' @param render_sql PARAM_DESCRIPTION, Default: FALSE
#' @param verbose PARAM_DESCRIPTION, Default: FALSE
#' @param sleepTime PARAM_DESCRIPTION, Default: 1
#' @return OUTPUT_DESCRIPTION
#' @seealso
#'  \code{\link[pg13]{buildQueryString}}
#'  \code{\link[dplyr]{rename}},\code{\link[dplyr]{filter}},\code{\link[dplyr]{select}}
#'  \code{\link[rubix]{group_by_unique_aggregate}}
#' @rdname searchPhraseStringSynonym
#' @export
#' @family query functions
#' @importFrom pg13 buildQueryString
#' @importFrom dplyr rename filter select
#' @importFrom rubix group_by_unique_aggregate
searchPhraseStringSynonym <-
        function(schema,
                 caseInsensitive = TRUE,
                 phrase,
                 split = " ",
                 conn = NULL,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 render_sql = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {

                sql_statement <-
                        pg13::buildQueryString(schema = schema,
                                               tableName = "concept_synonym",
                                               whereLikeField = "concept_synonym_name",
                                               string=phrase,
                                               split=split,
                                               caseInsensitive = caseInsensitive)

                output1 <-
                        queryAthena(sql_statement = sql_statement,
                                    conn = conn,
                                    cache_only = cache_only,
                                    skip_cache = skip_cache,
                                    override_cache = override_cache,
                                    render_sql = render_sql,
                                    verbose = verbose,
                                    sleepTime = sleepTime) %>%
                        dplyr::rename(concept_synonym_id = concept_id)



                leftJoinConcept(output1,
                                column = "concept_synonym_id",
                                athena_schema = schema,
                                conn = conn,
                                render_sql = render_sql) %>%
                        dplyr::filter(concept_name != concept_synonym_name) %>%
                        dplyr::select(-concept_synonym_id) %>%
                        rubix::group_by_unique_aggregate(concept_id,
                                                         concept_name,
                                                         domain_id,
                                                         vocabulary_id,
                                                         concept_class_id,
                                                         standard_concept,
                                                         concept_code,
                                                         valid_start_date,
                                                         valid_end_date,
                                                         invalid_reason,
                                                         agg.col = concept_synonym_name)

        }
