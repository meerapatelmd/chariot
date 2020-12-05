#' @title Query the Athena Postgres Database
#' @description
#' By default, this function queries a local database named "Athena". If a connection object is passed into the function, the database of the connection object is queried instead. The caching feature is only available when using the built-in connection to Athena.
#'
#' @param sql_statement         SQL query
#' @param cache_only            Loads from the cache and does not query the database. A NULL object is returned if a resultset was not cached.
#' @param skip_cache            Skip the caching altogether and directly query the database.
#' @param override_cache        If TRUE, the cache will not be loaded and will be overwritten by a new query. For override_cache to take effect, skip_cache should be FALSE.
#' @param conn                  Connection object. If provided, diverts queries to the connection instead of the local Athena instance without caching features.
#' @param render_sql            If TRUE, the SQL will be printed back in the console prior to execution. Default: FALSE
#' @param verbose               If TRUE, prints loading and querying operations messages to the console. Default: FALSE
#' @param sleepTime             Argument for `Sys.sleep()` in between queries to allow for halting function execution, especially in cases where other chariot functions are executing multiple queries in succession and require cancellation.
#' @param cache_resultset       (deprecated) If TRUE, the resultset from the query will first be loaded from the cache. The query will be executed if a cached resultset is not retrieved for this particular query, after which the resultset will be cached. If FALSE, Athena or conn will be directly queried without any caching operations.
#'
#' @return a [tibble][tibble::tibble-package]
#'
#' @seealso
#'  \code{\link[rlang]{parse_expr}}
#'  \code{\link[pg13]{is_conn_open}},\code{\link[pg13]{query}},\code{\link[pg13]{cacheQuery}},\code{\link[pg13]{loadCachedQuery}}
#'  \code{\link[secretary]{c("typewrite", "typewrite")}},\code{\link[secretary]{character(0)}}
#'  \code{\link[tibble]{as_tibble}}
#' @rdname queryAthena
#' @export
#' @family query functions
#' @importFrom rlang parse_expr
#' @importFrom pg13 is_conn_open query cacheQuery loadCachedQuery
#' @importFrom secretary typewrite magentaTxt
#' @importFrom tibble as_tibble

queryAthena <-
        function(sql_statement,
                 conn = NULL,
                 conn_fun = "connectAthena()",
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 cache_resultset = TRUE,
                 render_sql = TRUE,
                 verbose = TRUE,
                 sleepTime = 1) {


                if (!missing(conn_fun)) {

                        conn <- eval(expr = rlang::parse_expr(x = conn_fun))
                        on.exit(expr = dcAthena(conn = conn,
                                                verbose = verbose),
                                add = TRUE,
                                after = TRUE)

                }

                if (is.null(conn)) {

                        conn <- connectAthena(verbose = verbose)
                        on.exit(dcAthena(conn = conn,
                                         verbose = verbose))


                }

                if (!.hasSlot(conn, name = "jConnection")) {

                        stop('conn object must be a Database Connector JDBC Connection')

                }


                if (!pg13::is_conn_open(conn)) {

                        stop("`conn` object has closed connection.")

                }

                db <- get_conn_db(conn = conn)

                if (skip_cache) {

                                if (verbose) {
                                        secretary::typewrite(secretary::magentaTxt("Skipping cache..."))
                                }

                                resultset <- pg13::query(conn = conn,
                                                         sql_statement = sql_statement,
                                                         verbose = verbose,
                                                         render_sql = render_sql)

                        } else {

                                if (override_cache) {

                                        if (verbose) {
                                                secretary::typewrite(secretary::magentaTxt("Overriding cache... Querying Athena..."))
                                        }

                                        resultset <- pg13::query(conn = conn,
                                                                 sql_statement = sql_statement,
                                                                 verbose = verbose,
                                                                 render_sql = render_sql)

                                        if (verbose) {
                                                secretary::typewrite(secretary::magentaTxt("Caching resultset..."))
                                        }

                                        pg13::cacheQuery(resultset,
                                                         sqlQuery = sql_statement,
                                                         db = db)


                                } else {

                                        if (verbose) {
                                                secretary::typewrite(secretary::magentaTxt("Loading Cache..."))
                                                secretary::typewrite(secretary::magentaTxt("Cached SQL:"), sql_statement)
                                        }


                                        resultset <- tryCatch(pg13::loadCachedQuery(sqlQuery = sql_statement,
                                                                                    db = db),
                                                              error = function(e) NULL)

                                        if (!cache_only) {

                                                if (is.null(resultset)) {


                                                        if (verbose) {
                                                                secretary::typewrite(secretary::magentaTxt("No cached resultset found... querying Athena..."))
                                                        }

                                                        Sys.sleep(time = sleepTime)
                                                        resultset <- pg13::query(conn = conn,
                                                                                 sql_statement = sql_statement,
                                                                                 verbose = verbose,
                                                                                 render_sql = render_sql)

                                                        if (verbose) {
                                                                secretary::typewrite(secretary::magentaTxt("Caching resultset..."))
                                                        }

                                                        pg13::cacheQuery(resultset,
                                                                         sqlQuery = sql_statement,
                                                                         db = db)

                                                }


                                        } else {

                                                if (verbose) {

                                                        secretary::typewrite(secretary::magentaTxt("Cached resultset found..."))

                                                }
                                        }

                                }
                        }


                tibble::as_tibble(resultset)

        }



#' @title
#' Query Ancestors
#'
#' @inheritParams queryAthena
#' @return a [tibble][tibble::tibble-package]
#'
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname queryAncestors
#' @export
#' @family query functions
#' @importFrom SqlRender render

queryAncestors <-
    function(descendant_concept_ids,
             schema,
             min_levels_of_separation = NULL,
             max_levels_of_separation = NULL,
             conn = NULL,
             cache_only = FALSE,
             skip_cache = FALSE,
             override_cache = FALSE,
             render_sql = TRUE,
             verbose = TRUE,
             sleepTime = 1) {

            if (is.null(conn)) {

                    schema <- "public"

            }


            sql_statement <- "SELECT *
                                FROM @schema.concept_ancestor ca
                                WHERE ca.descendant_concept_id IN (@descendant_concept_ids);"

            sql_statement <-
                    SqlRender::render(sql = sql_statement,
                                      schema = schema,
                                      descendant_concept_ids = descendant_concept_ids)

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
#' @inheritParams queryAthena
#' @return a [tibble][tibble::tibble-package]
#' @seealso
#'  \code{\link[pg13]{buildQuery}}
#' @rdname queryCode
#' @export
#' @family query functions
#' @importFrom pg13 buildQuery

queryCode <-
        function(code,
                 schema,
                 caseInsensitive = TRUE,
                 limit = NULL,
                 verbose = FALSE,
                 conn = NULL,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 render_sql = FALSE,
                 sleepTime = 1) {


                sql_statement <-
                                pg13::buildQuery(schema = schema,
                                                 tableName = "concept",
                                                 whereInField = "concept_code",
                                                 whereInVector = code,
                                                 caseInsensitive = caseInsensitive,
                                                 n = limit,
                                                 n_type = "limit")


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
#' @inheritParams queryAthena
#' @return a [tibble][tibble::tibble-package]
#' @rdname queryConceptClassRelationships
#' @export
#' @family query functions

queryConceptClassRelationships <-
    function(vocabulary_id_1,
             vocabulary_id_2 = NULL,
             schema = NULL,
             conn = NULL,
             cache_only = FALSE,
             skip_cache = FALSE,
             override_cache = FALSE,
             render_sql = FALSE,
             verbose = FALSE,
             sleepTime = 1) {

                        if (is.null(schema)) {

                                schema <- "public"

                        }

                        sql_statement <- renderConceptClassRelationships(vocabulary_id_1 = vocabulary_id_1,
                                                                        vocabulary_id_2 = vocabulary_id_2,
                                                                        schema = schema)


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
#' @inheritParams queryAthena
#' @return a [tibble][tibble::tibble-package]
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname queryConceptId
#' @export
#' @family query functions
#' @importFrom SqlRender render
queryConceptId <-
    function(concept_ids,
             schema,
             conn = NULL,
             cache_only = FALSE,
             skip_cache = FALSE,
             override_cache = FALSE,
             render_sql = FALSE,
             verbose = FALSE,
             sleepTime = 1) {


                            # sql <-
                            # pg13::buildQuery(schema = schema,
                            #                  tableName = "concept",
                            #                  whereInField = "concept_id",
                            #                  whereInVector = concept_ids,
                            #                  caseInsensitive = FALSE)

                            sql <-
                                    SqlRender::render("SELECT *
                                                        FROM @schema.concept c
                                                        WHERE c.concept_id IN (@concept_ids)
                                                      ",
                                                        schema = schema,
                                                      concept_ids = concept_ids)


                            queryAthena(sql_statement = sql,
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
#' @param ancestor_concept_ids PARAM_DESCRIPTION
#' @param schema PARAM_DESCRIPTION
#' @param min_levels_of_separation PARAM_DESCRIPTION, Default: NULL
#' @param max_levels_of_separation PARAM_DESCRIPTION, Default: NULL
#' @inheritParams queryAthena
#' @return a [tibble][tibble::tibble-package]
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname queryDescendants
#' @export
#' @family query functions
#' @importFrom SqlRender render

queryDescendants <-
    function(ancestor_concept_ids,
             schema,
             min_levels_of_separation = NULL,
             max_levels_of_separation = NULL,
             conn = NULL,
             cache_only = FALSE,
             skip_cache = FALSE,
             override_cache = FALSE,
             render_sql = FALSE,
             verbose = FALSE,
             sleepTime = 1) {
            sql_statement <-
                    SqlRender::render(
                            "
                            SELECT
                                c.concept_id AS ancestor_concept_id,
                                c.concept_name AS ancestor_concept_name,
                                c.domain_id AS ancestor_domain_id,
                                c.vocabulary_id AS ancestor_vocabulary_id,
                                c.concept_class_id AS ancestor_concept_class_id,
                                c.standard_concept AS ancestor_standard_concept,
                                c.concept_code AS ancestor_concept_code,
                                c.valid_start_date AS ancestor_valid_start_date,
                                c.valid_end_date AS ancestor_valid_end_date,
                                c.invalid_reason AS ancestor_invalid_reason,
                                ca.min_levels_of_separation,
                                ca.max_levels_of_separation,
                                c2.concept_id AS descendant_concept_id,
                                c2.concept_name AS descendant_concept_name,
                                c2.domain_id AS descendant_domain_id,
                                c2.vocabulary_id AS descendant_vocabulary_id,
                                c2.concept_class_id AS descendant_concept_class_id,
                                c2.standard_concept AS descendant_standard_concept,
                                c2.concept_code AS descendant_concept_code,
                                c2.valid_start_date AS descendant_valid_start_date,
                                c2.valid_end_date AS descendant_valid_end_date,
                                c2.invalid_reason AS descendant_invalid_reason
                            FROM @schema.concept_ancestor ca
                            INNER JOIN @schema.concept c
                            ON c.concept_id = ca.ancestor_concept_id
                            INNER JOIN @schema.concept c2
                            ON c2.concept_id = ca.descendant_concept_id
                            WHERE
                                ca.ancestor_concept_id IN (@ancestor_concept_ids)
                                AND c.invalid_reason IS NULL
                                AND c2.invalid_reason IS NULL
                            ;",
                            schema = schema,
                            ancestor_concept_ids = ancestor_concept_ids
                    )

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
#' @param concept_id_1s PARAM_DESCRIPTION
#' @param schema PARAM_DESCRIPTION
#' @inheritParams queryAthena
#' @return a [tibble][tibble::tibble-package]
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname queryRelationships
#' @export
#' @family query functions
#' @importFrom SqlRender render

queryRelationships <-
        function(concept_id_1s,
                 schema,
                 relationship_ids = NULL,
                 conn = NULL,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 render_sql = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {


                if (is.null(relationship_ids)) {
                sql_statement <-
                        SqlRender::render(
                                "
                            SELECT
                                c.concept_id AS concept_id_1,
                                c.concept_name AS concept_name_1,
                                c.domain_id AS domain_id_1,
                                c.vocabulary_id AS vocabulary_id_1,
                                c.concept_class_id AS concept_class_id_1,
                                c.standard_concept AS standard_concept_1,
                                c.concept_code AS concept_code_1,
                                c.valid_start_date AS valid_start_date_1,
                                c.valid_end_date AS valid_end_date_1,
                                c.invalid_reason AS invalid_reason_1,
                                cr.relationship_id,
                                c2.concept_id AS concept_id_2,
                                c2.concept_name AS concept_name_2,
                                c2.domain_id AS domain_id_2,
                                c2.vocabulary_id AS vocabulary_id_2,
                                c2.concept_class_id AS concept_class_id_2,
                                c2.standard_concept AS concept_2,
                                c2.concept_code AS concept_code_2,
                                c2.valid_start_date AS valid_start_date_2,
                                c2.valid_end_date AS valid_end_date_2,
                                c2.invalid_reason AS invalid_reason_2
                            FROM @schema.concept_relationship cr
                            INNER JOIN @schema.concept c
                            ON c.concept_id = cr.concept_id_1
                            INNER JOIN @schema.concept c2
                            ON c2.concept_id = cr.concept_id_2
                            WHERE
                                cr.concept_id_1 IN (@concept_id_1s)
                                AND c.invalid_reason IS NULL
                                AND cr.invalid_reason IS NULL
                                AND c2.invalid_reason IS NULL
                            ;",
                                schema = schema,
                                concept_id_1s = concept_id_1s
                        )
                } else {

                        relationship_ids <- paste0("'", tolower(relationship_ids), "'")

                        sql_statement <-
                                SqlRender::render(
                                        "
                            SELECT
                                c.concept_id AS concept_id_1,
                                c.concept_name AS concept_name_1,
                                c.domain_id AS domain_id_1,
                                c.vocabulary_id AS vocabulary_id_1,
                                c.concept_class_id AS concept_class_id_1,
                                c.standard_concept AS standard_concept_1,
                                c.concept_code AS concept_code_1,
                                c.valid_start_date AS valid_start_date_1,
                                c.valid_end_date AS valid_end_date_1,
                                c.invalid_reason AS invalid_reason_1,
                                cr.relationship_id,
                                c2.concept_id AS concept_id_2,
                                c2.concept_name AS concept_name_2,
                                c2.domain_id AS domain_id_2,
                                c2.vocabulary_id AS vocabulary_id_2,
                                c2.concept_class_id AS concept_class_id_2,
                                c2.standard_concept AS standard_concept_2,
                                c2.concept_code AS concept_code_2,
                                c2.valid_start_date AS valid_start_date_2,
                                c2.valid_end_date AS valid_end_date_2,
                                c2.invalid_reason AS invalid_reason_2
                            FROM @schema.concept_relationship cr
                            INNER JOIN @schema.concept c
                            ON c.concept_id = cr.concept_id_1
                            INNER JOIN @schema.concept c2
                            ON c2.concept_id = cr.concept_id_2
                            WHERE
                                cr.concept_id_1 IN (@concept_id_1s)
                                AND LOWER(cr.relationship_id) IN (@relationship_ids)
                                AND c.invalid_reason IS NULL
                                AND cr.invalid_reason IS NULL
                                AND c2.invalid_reason IS NULL
                            ;",
                                        schema = schema,
                                        concept_id_1s = concept_id_1s,
                                        relationship_ids = relationship_ids
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
#' @param concept_id PARAM_DESCRIPTION
#' @param schema PARAM_DESCRIPTION, Default: NULL
#' @param language_concept_id PARAM_DESCRIPTION, Default: 4180186
#' @param conn PARAM_DESCRIPTION, Default: NULL
#' @param cache_only PARAM_DESCRIPTION, Default: FALSE
#' @param skip_cache PARAM_DESCRIPTION, Default: FALSE
#' @param override_cache PARAM_DESCRIPTION, Default: FALSE
#' @param render_sql PARAM_DESCRIPTION, Default: FALSE
#' @param verbose PARAM_DESCRIPTION, Default: FALSE
#' @param sleepTime PARAM_DESCRIPTION, Default: 1
#' @return OUTPUT_DESCRIPTION
#' @seealso
#'  \code{\link[dplyr]{select}}
#' @rdname querySynonyms
#' @export
#' @family query functions
#' @importFrom dplyr select
querySynonyms <-
        function(concept_id,
                 schema = NULL,
                 language_concept_id = 4180186,
                 conn = NULL,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 render_sql = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {

                sql_statement <- renderSynonyms(concept_id = concept_id,
                                                schema = schema,
                                                language_concept_id = language_concept_id)

                queryAthena(sql_statement = sql_statement,
                            conn = conn,
                            cache_only = cache_only,
                            skip_cache = skip_cache,
                            override_cache = override_cache,
                            render_sql = render_sql,
                            verbose = verbose,
                            sleepTime = sleepTime) %>%
                        dplyr::select(concept_synonym_name) %>%
                        unlist()
        }



#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param vocabulary_id PARAM_DESCRIPTION
#' @param verbose PARAM_DESCRIPTION, Default: FALSE
#' @param conn PARAM_DESCRIPTION, Default: NULL
#' @param cache_only PARAM_DESCRIPTION, Default: FALSE
#' @param skip_cache PARAM_DESCRIPTION, Default: FALSE
#' @param override_cache PARAM_DESCRIPTION, Default: FALSE
#' @param render_sql PARAM_DESCRIPTION, Default: FALSE
#' @param sleepTime PARAM_DESCRIPTION, Default: 1
#' @return OUTPUT_DESCRIPTION
#' @seealso
#'  \code{\link[SqlRender]{render}},\code{\link[SqlRender]{readSql}}
#' @rdname queryVocabularyRelationships
#' @export
#' @family query functions
#' @importFrom SqlRender render readSql
queryVocabularyRelationships <-
    function(vocabulary_id,
             verbose = FALSE,
             conn = NULL,
             cache_only = FALSE,
             skip_cache = FALSE,
             override_cache = FALSE,
             render_sql = FALSE,
             sleepTime = 1) {

                        base <- system.file(package = "chariot")
                        path <- paste0(base, "/sql/vocabularyRelationship.sql")

                        sql_statement <-
                                SqlRender::render(SqlRender::readSql(sourceFile = path),
                                                  vocabulary_id = vocabulary_id)

                        queryAthena(sql_statement = sql_statement,
                                    conn = conn,
                                    cache_only = cache_only,
                                    skip_cache = skip_cache,
                                    override_cache = override_cache,
                                    render_sql = render_sql,
                                    verbose = verbose,
                                    sleepTime = sleepTime)
    }


