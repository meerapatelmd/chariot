#' @title
#' Lookup UCUM Unit Concepts
#' @description
#' List all UCUM Unit Concepts from the Concept Table. To list UCUM Units specifically
#' related to the measurement of Time and Rate, see \code{\link{lookup_ucum_time}}.
#' To list the UCUM Units used to process the Drug Strength table, see
#' \code{\link{ds_lookup_ucum}}. The resultset is never cached.
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname lookup_ucum
#' @export
#' @importFrom SqlRender render


lookup_ucum <-
        function(conn,
                 conn_fun,
                 vocab_schema = "omop_vocabulary") {
                queryAthena(SqlRender::render(
                        "
                                        SELECT *
                                        FROM @vocab_schema.concept
                                        WHERE domain_id = 'Unit'
                                                AND vocabulary_id = 'UCUM'
                                                AND concept_class_id = 'Unit'
                                                AND invalid_reason IS NULL
                                        ;
                                        ",
                        vocab_schema = vocab_schema
                ),
                conn = conn,
                cache_only = FALSE,
                skip_cache = TRUE,
                override_cache = FALSE,
                render_sql = FALSE,
                verbose = FALSE,
                sleepTime = 1
                )
        }

#' @title
#' List UCUM Concepts Related to Time
#'
#' @description
#' List all UCUM Unit Concepts from the Concept Table that are related to
#' measurement of Time based on a broad regex match to the phrases "hour",
#' "minute", "second", "day", "month", "week", and "year". To list all UCUM
#' Units, see \code{\link{lookup_ucum}}. To list the UCUM Units used to process
#' the Drug Strength table, see \code{\link{ds_lookup_ucum}}This resultset is
#' never cached.
#'
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname lookup_ucum_time
#' @export
#' @importFrom SqlRender render


lookup_ucum_time <-
        function(conn,
                 conn_fun,
                 vocab_schema = "omop_vocabulary") {


                queryAthena(SqlRender::render(
                        "
                                        SELECT *
                                        FROM @vocab_schema.concept
                                        WHERE domain_id = 'Unit'
                                                AND vocabulary_id = 'UCUM'
                                                AND concept_class_id = 'Unit'
                                                AND invalid_reason IS NULL
                                                AND (LOWER(concept_name) LIKE '%hour%'
                                                        OR LOWER(concept_name) LIKE '%minute%'
                                                        OR LOWER(concept_name) LIKE '%second%'
                                                        OR LOWER(concept_name) LIKE '%day%'
                                                        OR LOWER(concept_name) LIKE '%week%'
                                                        OR LOWER(concept_name) LIKE '%month%'
                                                        OR LOWER(concept_name) LIKE '%year%')
                                        ;
                                        ",
                        vocab_schema = vocab_schema
                ),
                conn = conn,
                cache_only = FALSE,
                skip_cache = TRUE,
                override_cache = FALSE,
                render_sql = FALSE,
                verbose = FALSE,
                sleepTime = 1
                )
        }
