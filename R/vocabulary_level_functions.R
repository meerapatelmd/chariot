#' @title
#' Vocabulary Level Queries
#'
#' @description
#' This family of functions explores the OMOP Vocabulary at the Source Vocabulary level.
#'
#' @name vocabulary_level_functions
#' @keywords internal
NULL




lookup_vocabulary_relationships <-
        function(vocabulary_id,
                 conn = NULL,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 render_sql = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {

                vocabulary_id <- paste0("'", vocabulary_id, "'")

                queryAthena(sql_statement =
                                    SqlRender::render(
                                            "SELECT DISTINCT
                                                        c.vocabulary_id AS vocabulary_id_1,
                                                        c.concept_class_id AS concept_class_id_1,
                                                        cr.relationship_id
                                                        FROM public.concept c
                                                        LEFT JOIN public.concept_relationship cr
                                                        ON cr.concept_id_1 = c.concept_id
                                                        LEFT JOIN public.concept c2
                                                        ON c2.concept_id = cr.concept_id_2
                                                        WHERE c.vocabulary_id IN (@vocabulary_id)
                                                                AND c.invalid_reason IS NULL
                                                                AND cr.invalid_reason IS NULL",
                                            vocabulary_id = vocabulary_id),
                            conn = conn,
                            cache_only = cache_only,
                            skip_cache = skip_cache,
                            override_cache = override_cache,
                            cache_resultset = cache_resultset,
                            render_sql = render_sql,
                            verbose = verbose,
                            sleepTime = sleepTime)

        }



query_vocabulary_relationships <-
        function(vocabulary_id_1,
                 vocabulary_id_2 = NULL,
                 conn = NULL,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 cache_resultset = TRUE,
                 render_sql = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {

                vocabulary_id_1 <- paste0("'", vocabulary_id_1, "'")

                if (is.null(vocabulary_id_2)) {

                        vocabulary_id_2 <- vocabulary_id_1

                } else {

                        vocabulary_id_2 <- paste0("'", vocabulary_id_2, "'")

                }

                queryAthena(sql_statement =
                                SqlRender::render(
                                        "SELECT DISTINCT
                                                        c.vocabulary_id AS vocabulary_id_1,
                                                        c.concept_class_id AS concept_class_id_1,
                                                        cr.relationship_id,
                                                        c2.vocabulary_id AS vocabulary_id_2,
                                                        c2.concept_class_id AS concept_class_id_2
                                                        FROM public.concept c
                                                        LEFT JOIN public.concept_relationship cr
                                                        ON cr.concept_id_1 = c.concept_id
                                                        LEFT JOIN public.concept c2
                                                        ON c2.concept_id = cr.concept_id_2
                                                        WHERE c.vocabulary_id IN (@vocabulary_id_1)
                                                                AND c.invalid_reason IS NULL
                                                                AND c2.vocabulary_id IN (@vocabulary_id_2)
                                                                AND c2.invalid_reason IS NULL
                                                                AND cr.invalid_reason IS NULL",
                                                vocabulary_id_1 = vocabulary_id_1,
                                                vocabulary_id_2 = vocabulary_id_2
                                ),
                            conn = conn,
                            cache_only = cache_only,
                            skip_cache = skip_cache,
                            override_cache = override_cache,
                            cache_resultset = cache_resultset,
                            render_sql = render_sql,
                            verbose = verbose,
                            sleepTime = sleepTime)

        }



query_all_vocabulary_relationship <-
        function(vocabulary_id,
                 conn = NULL,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 cache_resultset = TRUE,
                 render_sql = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {

                vocabulary_id <- paste0("'", vocabulary_id, "'")


                queryAthena(sql_statement =
                                    SqlRender::render(
                                            "SELECT DISTINCT
                                                        c.vocabulary_id AS vocabulary_id_1,
                                                        c.concept_class_id AS concept_class_id_1,
                                                        cr.relationship_id,
                                                        c2.vocabulary_id AS vocabulary_id_2,
                                                        c2.concept_class_id AS concept_class_id_2
                                                        FROM public.concept c
                                                        LEFT JOIN public.concept_relationship cr
                                                        ON cr.concept_id_1 = c.concept_id
                                                        LEFT JOIN public.concept c2
                                                        ON c2.concept_id = cr.concept_id_2
                                                        WHERE c.vocabulary_id IN (@vocabulary_id)
                                                                AND c.invalid_reason IS NULL
                                                                AND c2.invalid_reason IS NULL
                                                                AND cr.invalid_reason IS NULL",
                                            vocabulary_id = vocabulary_id
                                    ),
                            conn = conn,
                            cache_only = cache_only,
                            skip_cache = skip_cache,
                            override_cache = override_cache,
                            cache_resultset = cache_resultset,
                            render_sql = render_sql,
                            verbose = verbose,
                            sleepTime = sleepTime)

        }
