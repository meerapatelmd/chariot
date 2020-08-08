#' @title
#' Get all OMOP Domain
#' @return
#' A data frame of unique `domain_id`, `vocabulary_id`, and `concept_class_id` combinations from the Concept Table.
#' @seealso
#'  \code{\link[pg13]{buildQuery}}
#' @rdname getConceptClasses
#' @export
#' @importFrom pg13 buildQuery

getDomain <-
        function(schema = NULL,
                 verbose = FALSE,
                 cache_resultset = TRUE,
                 override_cache = FALSE,
                 conn = NULL,
                 render_sql = FALSE,
                 sleepTime = 1,
                 ...) {


                sql_statement <-
                pg13::buildQuery(fields = c("domain_id"),
                                 distinct = TRUE,
                                 schema = schema,
                                 tableName = "concept")

                queryAthena(sql_statement = sql_statement,
                             verbose = verbose,
                             cache_resultset = cache_resultset,
                             override_cache = override_cache,
                             conn = conn,
                             render_sql = render_sql,
                             sleepTime = sleepTime,
                             ...)
        }
