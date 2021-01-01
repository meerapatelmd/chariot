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
#' @importFrom secretary typewrite magentaTxt
#' @importFrom tibble as_tibble

queryAthena <-
  function(sql_statement,
           conn,
           conn_fun = "connectAthena()",
           skip_cache = FALSE,
           override_cache = FALSE,
           cache_only = FALSE,
           cache_resultset = TRUE,
           render_sql = TRUE,
           render_only = FALSE,
           verbose = TRUE,
           sleepTime = 1) {


    if (skip_cache) {
      if (verbose) {
        secretary::typewrite(secretary::magentaTxt("Skipping cache..."))
      }

      resultset <- pg13::query(
        conn = conn,
       # conn_fun = conn_fun,
        sql_statement = sql_statement,
        verbose = verbose,
        render_sql = render_sql,
        render_only = render_only)

        return(tibble::as_tibble(resultset))

    }

    if (override_cache) {
        if (verbose) {
          secretary::typewrite(secretary::magentaTxt("Overriding cache... Querying Athena..."))
        }

        resultset <- pg13::query(
          conn = conn,
         # conn_fun = conn_fun,
          sql_statement = sql_statement,
          verbose = verbose,
          render_sql = render_sql,
          render_only = render_only
        )


        if (verbose) {
          secretary::typewrite(secretary::magentaTxt("Caching resultset..."))
        }

        lowLevelCache(
          data = resultset,
          query = sql_statement
        )

        return(tibble::as_tibble(resultset))
      }


    if (cache_only) {

      if (verbose) {
        secretary::typewrite(secretary::magentaTxt("Loading Cache..."))
        secretary::typewrite(secretary::magentaTxt("Cached SQL:"), sql_statement)
      }


      resultset <- lowLevelLoadCache(query = sql_statement)
      return(tibble::as_tibble(resultset))

    }

    if (verbose) {
      secretary::typewrite(secretary::magentaTxt("Loading Cache..."))
      secretary::typewrite(secretary::magentaTxt("Cached SQL:"), sql_statement)
    }


    resultset <- lowLevelLoadCache(query = sql_statement)
    if (is.null(resultset)) {
        if (verbose) {
          secretary::typewrite(secretary::magentaTxt("No cached resultset found... querying Athena..."))
        }

      Sys.sleep(time = sleepTime)
      resultset <- pg13::query(
        conn = conn,
       # conn_fun = conn_fun,
        sql_statement = sql_statement,
        verbose = verbose,
        render_sql = render_sql,
        render_only = render_only
      )


      if (verbose) {
        secretary::typewrite(secretary::magentaTxt("Caching resultset..."))
      }

      lowLevelCache(
        data = resultset,
        query = sql_statement
      )

      return(tibble::as_tibble(resultset))
    } else {
      if (verbose) {
        secretary::typewrite(secretary::magentaTxt("Cached resultset found..."))
      }

      return(tibble::as_tibble(resultset))
    }
  }


#' @title
#' Join a R dataframe to Athena's concept table
#' @description This function has better performance than the WHERE IN statement for larger searches. A table in the format of "v{unpunctuated timestamp}" is written to the local Athena. A join is performed with a concept table. The table is subsequently dropped. If a dataframe column is not provided as a the column to join the concept table on, the 1st column will be used by default.
#' @param data dataframe to join
#' @param column string of the column name to join on. If NULL, the 1st column is used.
#' @param vocab_table OMOP Vocabulary table to join to
#' @param vocab_column Field in the `vocab_table` to join on.
#' @param select_data_columns Vector of field names in the `data` to select for.
#' @param select_vocab_fields Vector of field names in the `vocab_table` to select for.
#' @param distinct Will the resultset contain only distinct rows?
#' @param write_schema Schema in the database with write permissions to write the
#' table containing `data` to. This table is dropped on exit.
#' @param vocab_schema Schema containing the OMOP Vocabulary tables.
#' @inheritParams pg13::join1
#' @export
#' @importFrom secretary typewrite
#' @importFrom pg13 write_staging_table draft_join1 query
#' @importFrom tibble as_tibble

join <-
  function(kind = c("LEFT", "RIGHT", "INNER", "FULL"),
           data,
           column = NULL,
           vocab_table,
           vocab_field,
           select_data_columns = "*",
           select_vocab_fields = "*",
           distinct = FALSE,
           write_schema = "patelm9",
           vocab_schema = "omop_vocabulary",
           where_in_vocab_field,
           where_in_vocab_field_value,
           where_not_in_vocab_field,
           where_not_in_vocab_field_value,
           where_is_null_vocab_field,
           where_is_not_null_vocab_field,
           case_insensitive = TRUE,
           conn,
           conn_fun = "connectAthena()",
           verbose = TRUE,
           render_sql = TRUE,
           render_only = FALSE) {

    if (is.null(column)) {
      column <- colnames(data)[1]
    }
    secretary::typewrite("Target column:", column)


    if (missing(conn)) {

      conn <- eval(rlang::parse_expr(conn_fun))
      on.exit(expr = dcAthena(),
              add = TRUE,
              after = TRUE)
    }


    staging_table <- pg13::write_staging_table(
                        conn = conn,
                        schema = write_schema,
                        data = data,
                        drop_existing = TRUE,
                        drop_on_exit = TRUE,
                        verbose = verbose,
                        render_sql = render_sql)



    sql_statement <-
      pg13::draft_join1(write_schema = write_schema,
                  data = data,
                  column = column,
                  select_table_fields = select_data_columns,
                  select_join_on_fields = select_vocab_fields,
                  join_on_schema = vocab_schema,
                  join_on_table = vocab_table,
                  join_on_column = vocab_field,
                  kind = kind,
                  where_in_join_on_field = where_in_vocab_field,
                  where_in_join_on_vector = where_in_vocab_field_value,
                  where_not_in_join_on_field = where_not_in_vocab_field,
                  where_not_in_join_on_vector = where_not_in_vocab_field_value,
                  where_is_null_join_on_field = where_is_null_vocab_field,
                  where_is_not_null_join_on_field = where_is_not_null_vocab_field,
                  case_insensitive = case_insensitive,
                  distinct = distinct,
                  verbose = verbose,
                  render_sql = render_sql,
                  render_only = render_only
      )


    pg13::query(
      conn = conn,
      sql_statement = sql_statement,
      verbose = verbose,
      render_sql = render_sql,
      render_only = render_only
    )

  }


#' @title
#' Send a SQL Statement
#' @description
#' Unlike the \code{\link{queryAthena}} function, the render_sql parameter provides a pause after rendering in case the user wants to copy and paste the rendered SQL into a client if the session is interactive. This is particularly useful with large operations that are better executed within a background client.
#' @param conn PARAM_DESCRIPTION, Default: NULL
#' @param sql_statement PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[pg13]{send}}
#' @rdname sendAthena
#' @export



sendAthena <-
  function(conn,
           conn_fun = "connectAthena()",
           sql_statement,
           verbose = TRUE,
           render_sql = TRUE,
           render_only = FALSE) {


    if (missing(conn)) {

            conn <- eval(expr = rlang::parse_expr(x = conn_fun))
            on.exit(expr = dcAthena(conn = conn,
                                    verbose = verbose),
                    add = TRUE,
                    after = TRUE)

    }

    pg13::send(
      conn = conn,
     # conn_fun = conn_fun,
      sql_statement = sql_statement,
      verbose = verbose,
      render_sql = render_sql,
      render_only = render_only
    )
  }
