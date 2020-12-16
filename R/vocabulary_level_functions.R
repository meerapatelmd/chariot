#' @title
#' Vocabulary Level Queries
#'
#' @description
#' This family of functions explores the OMOP Vocabulary at the Source Vocabulary level. Lookup functions find details about the given vocabulary while the Query functions query for the inter- and intra-relationships between vocabularies. The concept_class_id for the vocabulary is retained to provide a layer of granularity in the resultsets.
#'
#' @name vocabulary_level_functions
#' @keywords internal
NULL

#' @title
#' Lookup a Source Vocabulary's Relationships
#'
#' @details
#' Lookup all of a Source Vocabulary's non-null relationships in the Concept Relationship Table.
#'
#' @inherit vocabulary_level_functions description
#'
#' @inheritParams queryAthena
#' @param vocabulary_id         Vector of 1 or more `vocabulary_id` from the OMOP Vocabulary
#'
#' @seealso
#'  \code{\link[SqlRender]{render}}
#'
#' @rdname lookup_vocabulary_relationships
#' @export
#' @importFrom SqlRender render


lookup_vocabulary_relationships <-
        function(vocabulary_id,
                 conn = NULL,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 render_sql = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {

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
                                                        WHERE c.vocabulary_id IN ('@vocabulary_id')
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

#' @title
#' Query a Source Vocabulary's Relationships to Other Vocabularies
#'
#' @details
#' Query for the Source Vocabulary's valid relationships in the Concept Relationship Table and all of the OMOP Source Vocabulary and Concept Classes that the relationships are to. For a resultset for relationships within the vocabulary, see \code{\link{lookup_intravocabulary_relationship}}.
#'
#' @inherit vocabulary_level_functions description
#'
#' @inheritParams lookup_vocabulary_relationships
#'
#' @seealso
#'  \code{\link[SqlRender]{render}}
#'
#' @rdname lookup_intervocabulary_relationship
#'
#' @export
#' @importFrom SqlRender render

lookup_intervocabulary_relationship <-
        function(vocabulary_id,
                 conn = NULL,
                 vocab_schema = "omop_vocabulary",
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 cache_resultset = TRUE,
                 render_sql = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {


                queryAthena(sql_statement =
                                    SqlRender::render(
                                            "SELECT DISTINCT
                                                        c.vocabulary_id AS vocabulary_id_1,
                                                        c.concept_class_id AS concept_class_id_1,
                                                        cr.relationship_id,
                                                        c2.vocabulary_id AS vocabulary_id_2,
                                                        c2.concept_class_id AS concept_class_id_2
                                                        FROM @vocab_schema.concept c
                                                        LEFT JOIN @vocab_schema.concept_relationship cr
                                                        ON cr.concept_id_1 = c.concept_id
                                                        LEFT JOIN @vocab_schema.concept c2
                                                        ON c2.concept_id = cr.concept_id_2
                                                        WHERE c.vocabulary_id IN ('@vocabulary_id')
                                                                AND c2.vocabulary NOT IN ('@vocabulary_id')
                                                                AND c.invalid_reason IS NULL
                                                                AND c2.invalid_reason IS NULL
                                                                AND cr.invalid_reason IS NULL",
                                            vocab_schema = vocab_schema,
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



#' @title
#' Query a Source Vocabulary's Relationships
#'
#' @details
#' Query for the Source Vocabulary's non-null relationships in the Concept Relationship Table and all of the OMOP Source Vocabulary and Concept Classes that the relationships are to. For a resultset that filters for relationships to other vocabularies, see \code{\link{query_intervocabulary_relationship}}.
#'
#' @inherit vocabulary_level_functions description
#'
#' @inheritParams lookup_vocabulary_relationships
#'
#' @seealso
#'  \code{\link[SqlRender]{render}}
#'
#' @rdname lookup_intravocabulary_relationship
#'
#' @export
#' @importFrom SqlRender render

lookup_intravocabulary_relationship <-
        function(vocabulary_id,
                 conn = NULL,
                 vocab_schema = "omop_vocabulary",
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 cache_resultset = TRUE,
                 render_sql = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {


                queryAthena(sql_statement =
                                    SqlRender::render(
                                            "SELECT DISTINCT
                                                        c.vocabulary_id AS vocabulary_id_1,
                                                        c.concept_class_id AS concept_class_id_1,
                                                        cr.relationship_id,
                                                        c2.vocabulary_id AS vocabulary_id_2,
                                                        c2.concept_class_id AS concept_class_id_2
                                                        FROM @vocab_schema.concept c
                                                        LEFT JOIN @vocab_schema.concept_relationship cr
                                                        ON cr.concept_id_1 = c.concept_id
                                                        LEFT JOIN @vocab_schema.concept c2
                                                        ON c2.concept_id = cr.concept_id_2
                                                        WHERE c.vocabulary_id IN ('@vocabulary_id')
                                                                AND c2.vocabulary IN ('@vocabulary_id')
                                                                AND c.invalid_reason IS NULL
                                                                AND c2.invalid_reason IS NULL
                                                                AND cr.invalid_reason IS NULL",
                                            vocab_schema = vocab_schema,
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







#' @title
#' Query for a Source Vocabulary's Relationships to Other Source Vocabularies
#'
#' @details
#' Query for the Source Vocabulary's non-null relationships in the Concept Relationship Table to a second target set of OMOP Source Vocabulary and its Concept Classes. For a resultset that does not filters on both ends of the relationship, see \code{\link{query_all_vocabulary_relationships}}.
#'
#' @inherit vocabulary_level_functions description
#'
#' @inheritParams queryAthena
#' @param vocabulary_id_1         Vector of 1 or more `vocabulary_id`
#' @param vocabulary_id_2         Vector of 1 or more target `vocabulary_id`. If NULL, the target vocabulary is set to itself. Default: NULL.
#'
#' @seealso
#'  \code{\link[SqlRender]{render}}
#'
#' @rdname query_vocabulary_relationships
#'
#' @export
#' @importFrom SqlRender render

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

