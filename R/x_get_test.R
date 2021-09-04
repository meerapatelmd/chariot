#' @title
#' Get Test Data
#' @description
#' Get test data for further experimentation for functions in this or other
#' packages.
#' @rdname get_test_data
#' @export
#' @importFrom rlang parse_expr
#' @importFrom SqlRender render

get_test_data <-
        function(conn,
                 conn_fun = "connectAthena()",
                 fields = "*",
                 schema = "omop_vocabulary",
                 table = "concept",
                 limit = 100,
                 verbose = TRUE,
                 render_sql = TRUE) {

                if (missing(conn)) {

                        conn <- eval(rlang::parse_expr(conn_fun))
                        on.exit(expr = dcAthena(conn = conn),
                                add = TRUE,
                                after = TRUE)
                }
#'
#'                 if (!missing(...)) {
#'
#'                         Args  <- list(...)
#'
#'                         nms <- names(Args)
#'                         vals <- unname(Args)
#'
#'                         output <- list()
#'
#'                         for (i in seq_along(vals)) {
#'
#'                                 output[[i]] <- list()
#'
#'                                 val <- vals[[i]]
#'
#'                                 if ("NULL" %in% val) {
#'
#'                                         output[[i]][[1]] <-
#'                                                 paste0(nms[i], " IS NULL")
#'
#'                                 }
#'
#'
#'                                 val <- val[!(val %in% c("NULL"))]
#'
#'
#'                                 if (length(val)>0) {
#'
#'                                         if (is.character(val)) {
#'
#'                                                 val <- paste0("'", val, "'")
#'
#'                                         }
#'
#'
#'                                         output[[i]][[2]] <-
#'                                                 SqlRender::render("@nm IN (@val)", nm = nms[i], val = val)
#'
#'
#'                                 }
#'
#'                                 output[[i]] <-
#'                                         unlist(output[[i]]) %>%
#'                                         paste(collapse = " OR ")
#'
#'
#'                                 output[[i]] <-
#'                                         paste0("(", output[[i]], ")")
#'
#'
#'                         }
#'
#'                         where_clause1 <-
#'                                 paste0(unlist(output) %>%
#'                                                paste(collapse = " AND "))
#'
#'                 } else {
#'
#'                         where_clause1 <- NULL
#'
#'
#'
#'                 }
#'
#'
#'                 function(...,
#'                          schema,
#'                          fields = NULL,
#'                          limit = 100,
#'                          cache_only = FALSE,
#'                          skip_cache = FALSE,
#'                          override_cache = TRUE,
#'                          verbose = TRUE,
#'                          render_sql = TRUE) {
#'
#'                         if (missing(schema)) {
#'
#'                                 stop("`schema` is required")
#'
#'                         }
#'
#'
#'                         if (!missing(...)) {
#'
#'                                 Args  <- list(...)
#'
#'                                 nms <- names(Args)
#'                                 vals <- unname(Args)
#'
#'                                 output <- list()
#'
#'                                 for (i in seq_along(vals)) {
#'
#'                                         output[[i]] <- list()
#'
#'                                         val <- vals[[i]]
#'
#'                                         if ("NULL" %in% val) {
#'
#'                                                 output[[i]][[1]] <-
#'                                                         paste0(nms[i], " IS NULL")
#'
#'                                         }
#'
#'
#'                                         val <- val[!(val %in% c("NULL"))]
#'
#'
#'                                         if (length(val)>0) {
#'
#'                                                 if (is.character(val)) {
#'
#'                                                         val <- paste0("'", val, "'")
#'
#'                                                 }
#'
#'
#'                                                 output[[i]][[2]] <-
#'                                                         SqlRender::render("@nm IN (@val)", nm = nms[i], val = val)
#'
#'
#'                                         }
#'
#'                                         output[[i]] <-
#'                                                 unlist(output[[i]]) %>%
#'                                                 paste(collapse = " OR ")
#'
#'
#'                                         output[[i]] <-
#'                                                 paste0("(", output[[i]], ")")
#'
#'
#'                                 }
#'
#'                                 where_clause2 <-
#'                                         paste0(unlist(output) %>%
#'                                                        paste(collapse = " AND "))
#'
#'                         } else {
#'
#'                                 where_clause2 <- NULL
#'
#'
#'
#'                         }
#'
#'                         where_clause <- paste0("WHERE ", c(where_clause1,
#'                                                            where_clause2) %>%
#'                                                        paste(collapse = " AND "))
#'
#'
#'
#'                         if (is.null(fields)) {
#'
#'                                 fields <-
#'                                         pg13::ls_fields(conn = conn,
#'                                                         table = table,
#'                                                         schema = schema,
#'                                                         verbose = verbose,
#'                                                         render_sql = render_sql)
#'
#'
#'                         }
#'
#'
#'                         select_statement <-
#'                                 paste0(fields, " AS ", "test_", fields) %>%
#'                                 paste(collapse = ",\n")
#'
#'
                        sql_statement <-
                                SqlRender::render(
                                        "
                                        SELECT @fields
                                        FROM @schema.@table
                                        ORDER BY RANDOM()
                                        LIMIT @limit
                                        ",
                                        fields = fields,
                                        schema = schema,
                                        table = table,
                                        limit = limit
                                )
#'

                        queryAthena(
                          conn = conn,
                          sql_statement = sql_statement,
                          override_cache = TRUE,
                          verbose = verbose,
                          render_sql = render_sql
                        )
#'
#'
#'                 }
#'

        }
#'
#'
#' @title
#' Get Unfiltered Test Concepts
#' @description
#' Get random rows from the Concept table without any additional filters.
#' @seealso
#'  \code{\link[rlang]{parse_expr}}
#' @rdname get_test_concepts
#' @export
#' @importFrom rlang parse_expr
#' @example inst/example/get_test.R
get_test_concepts <-
        function(conn,
                 conn_fun = "connectAthena()",
                 schema = "omop_vocabulary",
                 limit = 100,
                 verbose = TRUE,
                 render_sql = TRUE) {

                if (missing(conn)) {

                        conn <- eval(rlang::parse_expr(conn_fun))
                        on.exit(expr = dcAthena(conn = conn),
                                add = TRUE,
                                after = TRUE)
                }

                get_test_data(
                        conn = conn,
                        fields = "*",
                        schema = schema,
                        table = "concept",
                        limit = limit,
                        verbose = verbose,
                        render_sql = render_sql
                )
        }
#'
#'

#' @title
#' Get Test Drug Concepts
#' @description
#' Get random records of RxNorm and RxNorm Extension Concepts that are:
#' \enumerate{
#'   \item Valid,
#'   \item Not a Class,
#'   \item Belong to the Drug domain
#' }
#' @seealso
#'  \code{\link[rlang]{parse_expr}}
#'  \code{\link[SqlRender]{render}}
#' @rdname get_test_drug_concepts
#' @export
#' @importFrom rlang parse_expr
#' @importFrom SqlRender render
#' @example inst/example/get_test.R
get_test_drug_concepts <-
        function(conn,
                 conn_fun = "connectAthena()",
                 schema = "omop_vocabulary",
                 limit = 100,
                 verbose = TRUE,
                 render_sql = TRUE) {

                if (missing(conn)) {

                        conn <- eval(rlang::parse_expr(conn_fun))
                        on.exit(expr = dcAthena(conn = conn),
                                add = TRUE,
                                after = TRUE)
                }

                sql_statement <-
                        SqlRender::render(
                                "
                                SELECT *
                                FROM @schema.concept c
                                WHERE
                                  c.invalid_reason IS NULL
                                    AND c.vocabulary_id IN ('RxNorm', 'RxNorm Extension')
                                    AND c.standard_concept <> 'C'
                                    AND c.domain_id = 'Drug'
                                ORDER BY RANDOM()
                                LIMIT @limit
                                ",
                                schema = schema,
                                limit = limit
                        )

                queryAthena(
                        conn = conn,
                        sql_statement = sql_statement,
                        override_cache = TRUE,
                        verbose = verbose,
                        render_sql = render_sql
                )
        }

#' @title
#' Get Test Drug Classes
#' @description
#' Get random records of ATC and HemOnc Drug Classes that are:
#' \enumerate{
#'   \item Valid,
#'   \item Class,
#'   \item Belong to the Drug domain
#' }
#' @seealso
#'  \code{\link[rlang]{parse_expr}}
#'  \code{\link[SqlRender]{render}}
#' @rdname get_test_drug_classes
#' @export
#' @importFrom rlang parse_expr
#' @importFrom SqlRender render
#' @example inst/example/get_test.R

get_test_drug_classes <-
        function(conn,
                 conn_fun = "connectAthena()",
                 schema = "omop_vocabulary",
                 limit = 100,
                 verbose = TRUE,
                 render_sql = TRUE) {

                if (missing(conn)) {

                        conn <- eval(rlang::parse_expr(conn_fun))
                        on.exit(expr = dcAthena(conn = conn),
                                add = TRUE,
                                after = TRUE)
                }

                sql_statement <-
                        SqlRender::render(
                                "
                                SELECT *
                                FROM @schema.concept c
                                WHERE
                                  c.invalid_reason IS NULL
                                    AND c.vocabulary_id IN ('ATC', 'HemOnc')
                                    AND c.standard_concept = 'C'
                                    AND c.domain_id = 'Drug'
                                ORDER BY RANDOM()
                                LIMIT @limit
                                ",
                                schema = schema,
                                limit = limit
                        )

                queryAthena(
                        conn = conn,
                        sql_statement = sql_statement,
                        override_cache = TRUE,
                        verbose = verbose,
                        render_sql = render_sql
                )

        }
