#' @title List Table Fields
#' @param schema Schema of the Vocabulary Table
#' @param tableName Vocabulary Table
#' @param conn Connection object. If NULL, automatically queries the local Athena database. Default: NULL
#' @return
#' Vector of field names for the table.
#' @seealso
#'  \code{\link[pg13]{lsFields}}
#' @rdname list_fields
#' @export
#' @importFrom pg13 lsFields
list_fields <-
  function(vocab_schema,
           tableName,
           conn = NULL) {
    if (is.null(conn)) {
      conn <- connectAthena()

      resultset <-
        pg13::lsFields(
          conn = conn,
          tableName = tableName,
          schema = schema
        )

      dcAthena(conn = conn)
    } else {
      resultset <-
        pg13::lsFields(
          conn = conn,
          tableName = tableName,
          schema = schema
        )
    }

    resultset
  }



#' @title
#' List Hierarchical Relationships
#' @inheritParams queryAthena
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname get_hierarchical_relationships
#' @export
#' @importFrom SqlRender render
get_hierarchical_relationships <-
  function(vocab_schema,
           conn = NULL,
           conn_fun = "connectAthena()",
           cache_only = FALSE,
           skip_cache = FALSE,
           override_cache = FALSE,
           cache_resultset = TRUE,
           render_sql = TRUE,
           verbose = TRUE,
           sleepTime = 1) {
    queryAthena(
      sql_statement = SqlRender::render("SELECT * FROM @schema.relationship WHERE is_hierarchical <> '0';", schema = schema),
      conn = conn,
      conn_fun = conn_fun,
      cache_only = cache_only,
      skip_cache = skip_cache,
      override_cache = override_cache,
      cache_resultset = cache_resultset,
      render_sql = render_sql,
      verbose = verbose,
      sleepTime = sleepTime
    )
  }


#' @title
#' Get Ancestry Relationships
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname get_ancestor_relationships
#' @export
#' @importFrom SqlRender render
get_ancestor_relationships <-
  function(vocab_schema,
           conn = NULL,
           conn_fun = "connectAthena()",
           cache_only = FALSE,
           skip_cache = FALSE,
           override_cache = FALSE,
           cache_resultset = TRUE,
           render_sql = TRUE,
           verbose = TRUE,
           sleepTime = 1) {
    queryAthena(SqlRender::render("SELECT * FROM @schema.relationship WHERE defines_ancestry <> '0';", schema = schema),
      conn = conn,
      conn_fun = conn_fun,
      cache_only = cache_only,
      skip_cache = skip_cache,
      override_cache = override_cache,
      cache_resultset = cache_resultset,
      render_sql = render_sql,
      verbose = verbose,
      sleepTime = sleepTime
    )
  }

#' @title
#' Get Vocabulary Ids
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname get_vocabulary_ids
#' @export
#' @importFrom SqlRender render

get_vocabulary_ids <-
  function(vocab_schema = "omop_vocabulary",
           conn,
           conn_fun = "connectAthena()",
           cache_only = FALSE,
           skip_cache = FALSE,
           override_cache = FALSE,
           cache_resultset = TRUE,
           render_sql = TRUE,
           verbose = TRUE,
           sleepTime = 1) {
    queryAthena(
      sql_statement =
        SqlRender::render(
          "
                                        SELECT DISTINCT vocabulary_id
                                        FROM @vocab_schema.concept
                                        ",
          vocab_schema = vocab_schema
        ),
      conn = conn,
      conn_fun = conn_fun,
      cache_only = cache_only,
      skip_cache = skip_cache,
      override_cache = override_cache,
      cache_resultset = cache_resultset,
      render_sql = render_sql,
      verbose = verbose,
      sleepTime = sleepTime
    ) %>%
      unlist()
  }
