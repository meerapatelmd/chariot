#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param vocabSchema PARAM_DESCRIPTION
#' @param vocabulary_id PARAM_DESCRIPTION
#' @param domain_id PARAM_DESCRIPTION
#' @param concept_class_id PARAM_DESCRIPTION
#' @param standard_concept PARAM_DESCRIPTION
#' @param invalid_reason PARAM_DESCRIPTION, Default: 'NULL'
#' @param lowered_match PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname exact_phrase_search_ff
#' @export
#' @importFrom SqlRender render

exact_phrase_search_ff <-
        function(vocabulary_id,
                 domain_id,
                 concept_class_id,
                 standard_concept,
                 invalid_reason = "NULL",
                 lowered_match = TRUE) {

                where_clause <- make_where_clause(vocabulary_id = vocabulary_id,
                                                  domain_id = domain_id,
                                                  concept_class_id = concept_class_id,
                                                  standard_concept = standard_concept,
                                                  invalid_reason = invalid_reason)


                if (!is.null(where_clause)) {

                        if (lowered_match) {
                                sql_statement <-
                                        SqlRender::render(
                                                "
                                                WITH params AS (
                                                        SELECT DISTINCT concept_id
                                                        FROM @vocabSchema.concept
                                                        WHERE @where_clause
                                                )
                                                SELECT DISTINCT
                                                        c.*,
                                                        cs.concept_synonym_name
                                                FROM @vocabSchema.concept_synonym cs
                                                INNER JOIN params p
                                                ON p.concept_id = cs.concept_id
                                                LEFT JOIN @vocabSchema.concept c
                                                ON c.concept_id = cs.concept_id
                                                WHERE LOWER('@phrase') = LOWER(cs.concept_synonym_name)
                                                ",
                                                where_clause = where_clause
                                        )


                        } else {

                                sql_statement <-
                                SqlRender::render(
                                        "
                                                WITH params AS (
                                                        SELECT DISTINCT concept_id
                                                        FROM @vocabSchema.concept
                                                        WHERE @where_clause
                                                )
                                                SELECT DISTINCT
                                                        c.*,
                                                        cs.concept_synonym_name
                                                FROM @vocabSchema.concept_synonym cs
                                                INNER JOIN params p
                                                ON p.concept_id = cs.concept_id
                                                LEFT JOIN @vocabSchema.concept c
                                                ON c.concept_id = cs.concept_id
                                                WHERE '@phrase' = cs.concept_synonym_name
                                                ",
                                        where_clause = where_clause
                                )

                        }

                } else {

                        if (lowered_match) {

                                sql_statement <-
                                        SqlRender::render(
                                                "
                                                SELECT DISTINCT
                                                        c.*,
                                                        cs.concept_synonym_name
                                                FROM @vocabSchema.concept_synonym cs
                                                LEFT JOIN @vocabSchema.concept c
                                                ON c.concept_id = cs.concept_id
                                                WHERE LOWER('@phrase') = LOWER(cs.concept_synonym_name)
                                                "
                                        )



                        } else {

                                sql_statement <-
                                        SqlRender::render(
                                                "
                                                SELECT DISTINCT
                                                        c.*,
                                                        cs.concept_synonym_name
                                                FROM @vocabSchema.concept_synonym cs
                                                LEFT JOIN @vocabSchema.concept c
                                                ON c.concept_id = cs.concept_id
                                                WHERE '@phrase' = cs.concept_synonym_name
                                                "
                                        )



                        }
                }


                function(phrase,
                         conn = NULL,
                         vocabSchema,
                         cache_only = FALSE,
                         skip_cache = FALSE,
                         override_cache = FALSE,
                         render_sql = TRUE,
                         verbose = TRUE,
                         sleepTime = 1) {


                        queryAthena(
                                SqlRender::render(
                                        sql_statement,
                                        vocabSchema = vocabSchema,
                                        phrase = phrase
                                ),
                                conn = conn,
                                cache_only = cache_only,
                                skip_cache = skip_cache,
                                override_cache = override_cache,
                                render_sql = render_sql,
                                verbose = verbose,
                                sleepTime = sleepTime
                        )
                }


        }



#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param vocabSchema PARAM_DESCRIPTION
#' @param vocabulary_id PARAM_DESCRIPTION
#' @param domain_id PARAM_DESCRIPTION
#' @param concept_class_id PARAM_DESCRIPTION
#' @param standard_concept PARAM_DESCRIPTION
#' @param invalid_reason PARAM_DESCRIPTION, Default: 'NULL'
#' @param lowered_match PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname like_phrase_search_ff
#' @export
#' @importFrom SqlRender render


like_phrase_search_ff <-
        function(vocabulary_id,
                 domain_id,
                 concept_class_id,
                 standard_concept,
                 invalid_reason = "NULL",
                 lowered_match = TRUE) {

                where_clause <- make_where_clause(vocabulary_id = vocabulary_id,
                                                  domain_id = domain_id,
                                                  concept_class_id = concept_class_id,
                                                  standard_concept = standard_concept,
                                                  invalid_reason = invalid_reason)


                if (!is.null(where_clause)) {

                        if (lowered_match) {
                                sql_statement <-
                                        SqlRender::render(
                                                "
                                                WITH params AS (
                                                        SELECT DISTINCT concept_id
                                                        FROM @vocabSchema.concept
                                                        WHERE @where_clause
                                                )
                                                SELECT
                                                        c.*,
                                                        cs.concept_synonym_name
                                                FROM @vocabSchema.concept_synonym cs
                                                INNER JOIN params p
                                                ON p.concept_id = cs.concept_id
                                                LEFT JOIN @vocabSchema.concept c
                                                ON c.concept_id = cs.concept_id
                                                WHERE LOWER(cs.concept_synonym_name) LIKE LOWER('%@phrase%')
                                                ORDER BY length(cs.concept_synonym_name)
                                                ",
                                                where_clause = where_clause
                                        )


                        } else {

                                sql_statement <-

                                SqlRender::render(
                                        "
                                                WITH params AS (
                                                        SELECT DISTINCT concept_id
                                                        FROM @vocabSchema.concept
                                                        WHERE @where_clause
                                                )
                                                SELECT
                                                        c.*,
                                                        cs.concept_synonym_name
                                                FROM @vocabSchema.concept_synonym cs
                                                INNER JOIN params p
                                                ON p.concept_id = cs.concept_id
                                                LEFT JOIN @vocabSchema.concept c
                                                ON c.concept_id = cs.concept_id
                                                WHERE cs.concept_synonym_name LIKE '%@phrase%'
                                                ORDER BY length(cs.concept_synonym_name)
                                                ",
                                        where_clause = where_clause
                                )

                        }

                } else {

                        if (lowered_match) {

                                sql_statement <-
                                        SqlRender::render(
                                                "
                                                SELECT c.*,
                                                        cs.concept_synonym_name
                                                FROM @vocabSchema.concept_synonym cs
                                                LEFT JOIN @vocabSchema.concept c
                                                ON c.concept_id = cs.concept_id
                                                WHERE LOWER(cs.concept_synonym_name) LIKE LOWER('%@phrase%')
                                                ORDER BY length(cs.concept_synonym_name)
                                                "
                                        )



                        } else {

                                sql_statement <-
                                        SqlRender::render(
                                                "
                                                SELECT
                                                        c.*,
                                                        cs.concept_synonym_name
                                                FROM @vocabSchema.concept_synonym cs
                                                LEFT JOIN @vocabSchema.concept c
                                                ON c.concept_id = cs.concept_id
                                                WHERE cs.concept_synonym_name LIKE '%@phrase%'
                                                ORDER BY length(cs.concept_synonym_name)
                                                "
                                        )



                        }
                }


                function(phrase,
                         conn = NULL,
                         vocabSchema,
                         cache_only = FALSE,
                         skip_cache = FALSE,
                         override_cache = FALSE,
                         render_sql = TRUE,
                         verbose = TRUE,
                         sleepTime = 1) {


                        queryAthena(
                                SqlRender::render(
                                        sql_statement,
                                        vocabSchema = vocabSchema,
                                        phrase = phrase
                                ),
                                conn = conn,
                                cache_only = cache_only,
                                skip_cache = skip_cache,
                                override_cache = override_cache,
                                render_sql = render_sql,
                                verbose = verbose,
                                sleepTime = sleepTime
                        )
                }


        }



#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param vocabulary_id PARAM_DESCRIPTION
#' @param domain_id PARAM_DESCRIPTION
#' @param concept_class_id PARAM_DESCRIPTION
#' @param standard_concept PARAM_DESCRIPTION
#' @param invalid_reason PARAM_DESCRIPTION, Default: 'NULL'
#' @param lowered_match PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname tokenized_phrase_search_ff
#' @export
#' @importFrom SqlRender render


tokenized_phrase_search_ff <-
        function(vocabulary_id,
                 domain_id,
                 concept_class_id,
                 standard_concept,
                 invalid_reason = "NULL",
                 lowered_match = TRUE) {

                where_clause <- make_where_clause(vocabulary_id = vocabulary_id,
                                                  domain_id = domain_id,
                                                  concept_class_id = concept_class_id,
                                                  standard_concept = standard_concept,
                                                  invalid_reason = invalid_reason)


                if (!is.null(where_clause)) {

                        if (lowered_match) {
                                sql_statement <-
                                        SqlRender::render(
                                                "
                                                WITH params AS (
                                                        SELECT DISTINCT concept_id
                                                        FROM @vocabSchema.concept
                                                        WHERE @where_clause
                                                )
                                                SELECT
                                                        c.*,
                                                        cs.concept_synonym_name
                                                FROM @vocabSchema.concept_synonym cs
                                                INNER JOIN params p
                                                ON p.concept_id = cs.concept_id
                                                LEFT JOIN @vocabSchema.concept c
                                                ON c.concept_id = cs.concept_id
                                                WHERE @like_token_clause
                                                ",
                                                where_clause = where_clause
                                        )


                        } else {

                                sql_statement <-

                                SqlRender::render(
                                        "
                                                WITH params AS (
                                                        SELECT DISTINCT concept_id
                                                        FROM @vocabSchema.concept
                                                        WHERE @where_clause
                                                )
                                                SELECT
                                                        c.*,
                                                        cs.concept_synonym_name
                                                FROM @vocabSchema.concept_synonym cs
                                                INNER JOIN params p
                                                ON p.concept_id = cs.concept_id
                                                LEFT JOIN @vocabSchema.concept c
                                                ON c.concept_id = cs.concept_id
                                                WHERE @like_token_clause
                                                ",
                                        where_clause = where_clause
                                )

                        }

                } else {

                        if (lowered_match) {

                                sql_statement <-
                                        SqlRender::render(
                                                "
                                                SELECT c.*,
                                                        cs.concept_synonym_name
                                                FROM @vocabSchema.concept_synonym cs
                                                LEFT JOIN @vocabSchema.concept c
                                                ON c.concept_id = cs.concept_id
                                                WHERE @like_token_clause
                                                "
                                        )



                        } else {

                                sql_statement <-
                                        SqlRender::render(
                                                "
                                                SELECT
                                                        c.*,
                                                        cs.concept_synonym_name
                                                FROM @vocabSchema.concept_synonym cs
                                                LEFT JOIN @vocabSchema.concept c
                                                ON c.concept_id = cs.concept_id
                                                WHERE @like_token_clause
                                                "
                                        )



                        }
                }


                function(phrase,
                         split = " |[[:punct:]]",
                         conn = NULL,
                         vocabSchema,
                         cache_only = FALSE,
                         skip_cache = FALSE,
                         override_cache = FALSE,
                         render_sql = TRUE,
                         verbose = TRUE,
                         sleepTime = 1) {


                        lowered_match <- lowered_match


                        tokens <-
                                strsplit(x = phrase,
                                         split = split) %>%
                                        unlist() %>%
                                        trimws(which = "both")


                        if (lowered_match) {
                                like_token_clause <-
                                paste0("LOWER(cs.concept_synonym_name) LIKE LOWER('%", tokens, "%')") %>%
                                                paste(collapse = " AND ")

                        } else {
                                like_token_clause <-
                                               paste0("cs.concept_synonym_name LIKE '%", tokens, "%'") %>%
                                                       paste(collapse = " AND ")
                        }


                        queryAthena(
                                SqlRender::render(
                                        sql_statement,
                                        vocabSchema = vocabSchema,
                                        like_token_clause = like_token_clause
                                ),
                                conn = conn,
                                cache_only = cache_only,
                                skip_cache = skip_cache,
                                override_cache = override_cache,
                                render_sql = render_sql,
                                verbose = verbose,
                                sleepTime = sleepTime
                        )
                }


        }



#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param vocabulary_id PARAM_DESCRIPTION
#' @param domain_id PARAM_DESCRIPTION
#' @param concept_class_id PARAM_DESCRIPTION
#' @param standard_concept PARAM_DESCRIPTION
#' @param invalid_reason PARAM_DESCRIPTION, Default: 'NULL'
#' @param lowered_match PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname ordered_phrase_search_ff
#' @export
#' @importFrom SqlRender render


ordered_phrase_search_ff <-
        function(vocabulary_id,
                 domain_id,
                 concept_class_id,
                 standard_concept,
                 invalid_reason = "NULL",
                 lowered_match = TRUE) {

                where_clause <- make_where_clause(vocabulary_id = vocabulary_id,
                                                  domain_id = domain_id,
                                                  concept_class_id = concept_class_id,
                                                  standard_concept = standard_concept,
                                                  invalid_reason = invalid_reason)


                if (!is.null(where_clause)) {

                        if (lowered_match) {
                                sql_statement <-
                                        SqlRender::render(
                                                "
                                                WITH params AS (
                                                        SELECT DISTINCT concept_id
                                                        FROM @vocabSchema.concept
                                                        WHERE @where_clause
                                                )
                                                SELECT
                                                        c.*,
                                                        cs.concept_synonym_name
                                                FROM @vocabSchema.concept_synonym cs
                                                INNER JOIN params p
                                                ON p.concept_id = cs.concept_id
                                                LEFT JOIN @vocabSchema.concept c
                                                ON c.concept_id = cs.concept_id
                                                WHERE LOWER(cs.concept_synonym_name) LIKE LOWER('%@phrase%')
                                                ",
                                                where_clause = where_clause
                                        )


                        } else {

                                sql_statement <-

                                        SqlRender::render(
                                                "
                                                WITH params AS (
                                                        SELECT DISTINCT concept_id
                                                        FROM @vocabSchema.concept
                                                        WHERE @where_clause
                                                )
                                                SELECT
                                                        c.*,
                                                        cs.concept_synonym_name
                                                FROM @vocabSchema.concept_synonym cs
                                                INNER JOIN params p
                                                ON p.concept_id = cs.concept_id
                                                LEFT JOIN @vocabSchema.concept c
                                                ON c.concept_id = cs.concept_id
                                                WHERE cs.concept_synonym_name LIKE '%@phrase%'
                                                ",
                                                where_clause = where_clause
                                        )

                        }

                } else {

                        if (lowered_match) {

                                sql_statement <-
                                        SqlRender::render(
                                                "
                                                SELECT c.*,
                                                        cs.concept_synonym_name
                                                FROM @vocabSchema.concept_synonym cs
                                                LEFT JOIN @vocabSchema.concept c
                                                ON c.concept_id = cs.concept_id
                                                WHERE LOWER(cs.concept_synonym_name) LIKE LOWER('%@phrase%')
                                                "
                                        )



                        } else {

                                sql_statement <-
                                        SqlRender::render(
                                                "
                                                SELECT
                                                        c.*,
                                                        cs.concept_synonym_name
                                                FROM @vocabSchema.concept_synonym cs
                                                LEFT JOIN @vocabSchema.concept c
                                                ON c.concept_id = cs.concept_id
                                                WHERE cs.concept_synonym_name LIKE '%@phrase%'
                                                "
                                        )



                        }
                }


                function(phrase,
                         wildcard = " {1,}|[[:punct:]]{1,}",
                         conn = NULL,
                         vocabSchema,
                         cache_only = FALSE,
                         skip_cache = FALSE,
                         override_cache = FALSE,
                         render_sql = TRUE,
                         verbose = TRUE,
                         sleepTime = 1) {


                        phrase <-
                        stringr::str_replace_all(string = phrase,
                                                 pattern = wildcard,
                                                 replacement = "%")



                        queryAthena(
                                SqlRender::render(
                                        sql_statement,
                                        vocabSchema = vocabSchema,
                                        phrase = phrase
                                ),
                                conn = conn,
                                cache_only = cache_only,
                                skip_cache = skip_cache,
                                override_cache = override_cache,
                                render_sql = render_sql,
                                verbose = verbose,
                                sleepTime = sleepTime
                        )
                }


        }



