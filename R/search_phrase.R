#' @title
#' Search Exact Matches to a Phrase
#' @description
#' Search for an exact phrase in the Concept Synonym table.
#' @param phrase String to search.
#' @param case_insensitive Should the search ignore case?, Default: TRUE
#' @param vocab_schema OMOP Vocabulary schema, Default: 'omop_vocabulary'
#' @inheritParams queryAthena
#' @return
#' Tibble of all the matching Concept Table fields and an added `concept_synonyms`
#'  field of a pipe-separated aggregate of all the synonyms, including
#'  `concept_name`, in the Concept Synonym table.
#' @example inst/example/search_phrase.R
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname search_exact_phrase
#' @export
#' @importFrom SqlRender render
search_exact_phrase <-
        function(phrase,
                 case_insensitive = TRUE,
                 vocab_schema = "omop_vocabulary",
                 conn,
                 conn_fun = "connectAthena()",
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 cache_resultset = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE,
                 verbose = TRUE,
                 sleepTime = 1) {


                if (case_insensitive) {
                        sql_statement <-
                                SqlRender::render(
                                        "
                        SELECT c.*, STRING_AGG(cs.concept_synonym_name, '|') AS concept_synonyms
                        FROM @vocab_schema.concept_synonym cs
                        INNER JOIN @vocab_schema.concept c
                        ON c.concept_id = cs.concept_id
                        WHERE LOWER(cs.concept_synonym_name) = LOWER('@phrase')
                        GROUP BY c.concept_id,
                                c.concept_name,
                                c.domain_id,
                                c.vocabulary_id,
                                c.concept_class_id,
                                c.standard_concept,
                                c.concept_code,
                                c.valid_start_date,
                                c.valid_end_date,
                                c.invalid_reason;
                        ",
                                        vocab_schema = vocab_schema,
                                        phrase = phrase
                                )


                } else {
                sql_statement <-
                SqlRender::render(
                        "
                        SELECT c.*, STRING_AGG(cs.concept_synonym_name, '|') AS concept_synonyms
                        FROM @vocab_schema.concept_synonym cs
                        INNER JOIN @vocab_schema.concept c
                        ON c.concept_id = cs.concept_id
                        WHERE cs.concept_synonym_name = '@phrase'
                        GROUP BY c.concept_id,
                                c.concept_name,
                                c.domain_id,
                                c.vocabulary_id,
                                c.concept_class_id,
                                c.standard_concept,
                                c.concept_code,
                                c.valid_start_date,
                                c.valid_end_date,
                                c.invalid_reason;
                        ",
                        vocab_schema = vocab_schema,
                        phrase = phrase
                )
                }


                queryAthena(
                        sql_statement = sql_statement,
                        conn = conn,
                        conn_fun = conn_fun,
                        cache_only = cache_only,
                        skip_cache = skip_cache,
                        override_cache = override_cache,
                        cache_resultset = cache_resultset,
                        render_sql = render_sql,
                        render_only = render_only,
                        verbose = verbose,
                        sleepTime = sleepTime
                )
        }



#' @title
#' Search Concepts that Contain a Phrase
#' @description
#' Search for concepts that contain a phrase in the Concept Synonym table.
#' @inherit search_exact_phrase example
#' @inheritParams search_exact_phrase
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname search_like_phrase
#' @export
#' @importFrom SqlRender render
search_like_phrase <-
        function(phrase,
                 case_insensitive = TRUE,
                 vocab_schema = "omop_vocabulary",
                 conn,
                 conn_fun = "connectAthena()",
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 cache_resultset = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE,
                 verbose = TRUE,
                 sleepTime = 1) {


                if (case_insensitive) {
                        sql_statement <-
                                SqlRender::render(
                                        "
                        SELECT c.*, STRING_AGG(cs.concept_synonym_name, '|') AS concept_synonyms
                        FROM @vocab_schema.concept_synonym cs
                        INNER JOIN @vocab_schema.concept c
                        ON c.concept_id = cs.concept_id
                        WHERE LOWER(cs.concept_synonym_name) LIKE LOWER('%@phrase%')
                        GROUP BY c.concept_id,
                                c.concept_name,
                                c.domain_id,
                                c.vocabulary_id,
                                c.concept_class_id,
                                c.standard_concept,
                                c.concept_code,
                                c.valid_start_date,
                                c.valid_end_date,
                                c.invalid_reason;
                        ",
                                        vocab_schema = vocab_schema,
                                        phrase = phrase
                                )


                } else {
                        sql_statement <-
                                SqlRender::render(
                                        "
                        SELECT c.*, STRING_AGG(cs.concept_synonym_name, '|') AS concept_synonyms
                        FROM @vocab_schema.concept_synonym cs
                        INNER JOIN @vocab_schema.concept c
                        ON c.concept_id = cs.concept_id
                        WHERE cs.concept_synonym_name LIKE '%@phrase%'
                        GROUP BY c.concept_id,
                                c.concept_name,
                                c.domain_id,
                                c.vocabulary_id,
                                c.concept_class_id,
                                c.standard_concept,
                                c.concept_code,
                                c.valid_start_date,
                                c.valid_end_date,
                                c.invalid_reason;
                        ",
                                        vocab_schema = vocab_schema,
                                        phrase = phrase
                                )
                }


                queryAthena(
                        sql_statement = sql_statement,
                        conn = conn,
                        conn_fun = conn_fun,
                        cache_only = cache_only,
                        skip_cache = skip_cache,
                        override_cache = override_cache,
                        cache_resultset = cache_resultset,
                        render_sql = render_sql,
                        render_only = render_only,
                        verbose = verbose,
                        sleepTime = sleepTime
                )
        }


#' @title
#' Search Concepts that Start With a Phrase
#' @description
#' Search for concepts that start with a phrase in the Concept Synonym table.
#' @inherit search_exact_phrase example
#' @inheritParams search_exact_phrase
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname search_starts_with_phrase
#' @export
#' @importFrom SqlRender render
search_starts_with_phrase <-
        function(phrase,
                 case_insensitive = TRUE,
                 vocab_schema = "omop_vocabulary",
                 conn,
                 conn_fun = "connectAthena()",
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 cache_resultset = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE,
                 verbose = TRUE,
                 sleepTime = 1) {


                if (case_insensitive) {
                        sql_statement <-
                                SqlRender::render(
                                        "
                        SELECT c.*, STRING_AGG(cs.concept_synonym_name, '|') AS concept_synonyms
                        FROM @vocab_schema.concept_synonym cs
                        INNER JOIN @vocab_schema.concept c
                        ON c.concept_id = cs.concept_id
                        WHERE LOWER(cs.concept_synonym_name) LIKE LOWER('@phrase%')
                        GROUP BY c.concept_id,
                                c.concept_name,
                                c.domain_id,
                                c.vocabulary_id,
                                c.concept_class_id,
                                c.standard_concept,
                                c.concept_code,
                                c.valid_start_date,
                                c.valid_end_date,
                                c.invalid_reason;
                        ",
                                        vocab_schema = vocab_schema,
                                        phrase = phrase
                                )


                } else {
                        sql_statement <-
                                SqlRender::render(
                                        "
                        SELECT c.*, STRING_AGG(cs.concept_synonym_name, '|') AS concept_synonyms
                        FROM @vocab_schema.concept_synonym cs
                        INNER JOIN @vocab_schema.concept c
                        ON c.concept_id = cs.concept_id
                        WHERE cs.concept_synonym_name LIKE '@phrase%'
                        GROUP BY c.concept_id,
                                c.concept_name,
                                c.domain_id,
                                c.vocabulary_id,
                                c.concept_class_id,
                                c.standard_concept,
                                c.concept_code,
                                c.valid_start_date,
                                c.valid_end_date,
                                c.invalid_reason;
                        ",
                                        vocab_schema = vocab_schema,
                                        phrase = phrase
                                )
                }


                queryAthena(
                        sql_statement = sql_statement,
                        conn = conn,
                        conn_fun = conn_fun,
                        cache_only = cache_only,
                        skip_cache = skip_cache,
                        override_cache = override_cache,
                        cache_resultset = cache_resultset,
                        render_sql = render_sql,
                        render_only = render_only,
                        verbose = verbose,
                        sleepTime = sleepTime
                )
        }



#' @title
#' Search Concepts that End With a Phrase
#' @description
#' Search for concepts that end with a phrase in the Concept Synonym table.
#' @inherit search_exact_phrase example
#' @inheritParams search_exact_phrase
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname search_ends_with_phrase
#' @export
#' @importFrom SqlRender render
search_ends_with_phrase <-
        function(phrase,
                 case_insensitive = TRUE,
                 vocab_schema = "omop_vocabulary",
                 conn,
                 conn_fun = "connectAthena()",
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 cache_resultset = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE,
                 verbose = TRUE,
                 sleepTime = 1) {


                if (case_insensitive) {
                        sql_statement <-
                                SqlRender::render(
                                        "
                        SELECT c.*, STRING_AGG(cs.concept_synonym_name, '|') AS concept_synonyms
                        FROM @vocab_schema.concept_synonym cs
                        INNER JOIN @vocab_schema.concept c
                        ON c.concept_id = cs.concept_id
                        WHERE LOWER(cs.concept_synonym_name) LIKE LOWER('%@phrase')
                        GROUP BY c.concept_id,
                                c.concept_name,
                                c.domain_id,
                                c.vocabulary_id,
                                c.concept_class_id,
                                c.standard_concept,
                                c.concept_code,
                                c.valid_start_date,
                                c.valid_end_date,
                                c.invalid_reason;
                        ",
                                        vocab_schema = vocab_schema,
                                        phrase = phrase
                                )


                } else {
                        sql_statement <-
                                SqlRender::render(
                                        "
                        SELECT c.*, STRING_AGG(cs.concept_synonym_name, '|') AS concept_synonyms
                        FROM @vocab_schema.concept_synonym cs
                        INNER JOIN @vocab_schema.concept c
                        ON c.concept_id = cs.concept_id
                        WHERE cs.concept_synonym_name LIKE '%@phrase'
                        GROUP BY c.concept_id,
                                c.concept_name,
                                c.domain_id,
                                c.vocabulary_id,
                                c.concept_class_id,
                                c.standard_concept,
                                c.concept_code,
                                c.valid_start_date,
                                c.valid_end_date,
                                c.invalid_reason;
                        ",
                                        vocab_schema = vocab_schema,
                                        phrase = phrase
                                )
                }


                queryAthena(
                        sql_statement = sql_statement,
                        conn = conn,
                        conn_fun = conn_fun,
                        cache_only = cache_only,
                        skip_cache = skip_cache,
                        override_cache = override_cache,
                        cache_resultset = cache_resultset,
                        render_sql = render_sql,
                        render_only = render_only,
                        verbose = verbose,
                        sleepTime = sleepTime
                )
        }



#' @title
#' Search Concepts that Contain all Parts of a Phrase
#' @description
#' Search for concepts that contain all the fragments of a phrase created by the
#' `split` argument in the Concept Synonym table.
#' @inheritParams strsplit
#' @inherit search_exact_phrase example
#' @inheritParams search_exact_phrase
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname search_split_phrase
#' @export
#' @importFrom SqlRender render
search_split_phrase <-
        function(phrase,
                 split = " |[[:punct:]]",
                 case_insensitive = TRUE,
                 vocab_schema = "omop_vocabulary",
                 conn,
                 conn_fun = "connectAthena()",
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 cache_resultset = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE,
                 verbose = TRUE,
                 sleepTime = 1) {

                args <- unlist(strsplit(phrase, split = split))
                args <- trimws(args, which = "both")
                args <- args[!(args %in% c(""))]
                args <- paste0("%", args, "%")

                if (case_insensitive) {
                        sql_statement <- SqlRender::render(
                                                        "
                                                        SELECT c.*, STRING_AGG(cs.concept_synonym_name, '|') AS concept_synonyms
                                                        FROM @schema.concept_synonym cs
                                                        INNER JOIN @schema.concept c
                                                        ON c.concept_id  = cs.concept_id
                                                        WHERE @where_clauses
                                                        GROUP BY
                                                                c.concept_id,
                                                                c.concept_name,
                                                                c.domain_id,
                                                                c.vocabulary_id,
                                                                c.concept_class_id,
                                                                c.standard_concept,
                                                                c.concept_code,
                                                                c.valid_start_date,
                                                                c.valid_end_date,
                                                                c.invalid_reason;",
                                                           schema = vocab_schema,
                                                           where_clauses = paste(sprintf("LOWER(cs.concept_synonym_name) LIKE LOWER('%s')", args),
                                                                                 collapse = "\n\t\t\t\t\t\t\t\tAND "))
                }
                else {
                        sql_statement <- SqlRender::render(
                                "
                                                        SELECT c.*, STRING_AGG(cs.concept_synonym_name, '|') AS concept_synonyms
                                                        FROM @schema.concept_synonym cs
                                                        INNER JOIN @schema.concept c
                                                        ON c.concept_id  = cs.concept_id
                                                        WHERE @where_clauses
                                                        GROUP BY
                                                                c.concept_id,
                                                                c.concept_name,
                                                                c.domain_id,
                                                                c.vocabulary_id,
                                                                c.concept_class_id,
                                                                c.standard_concept,
                                                                c.concept_code,
                                                                c.valid_start_date,
                                                                c.valid_end_date,
                                                                c.invalid_reason;",
                                schema = vocab_schema,
                                where_clauses = paste(sprintf("cs.concept_synonym_name LIKE '%s'", args),
                                                      collapse = "\n\t\t\t\t\t\t\t\tAND "))
                }

                queryAthena(
                        sql_statement = sql_statement,
                        conn = conn,
                        conn_fun = conn_fun,
                        cache_only = cache_only,
                        skip_cache = skip_cache,
                        override_cache = override_cache,
                        cache_resultset = cache_resultset,
                        render_sql = render_sql,
                        render_only = render_only,
                        verbose = verbose,
                        sleepTime = sleepTime
                )

        }
