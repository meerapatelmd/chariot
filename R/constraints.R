#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname constraints
#' @export
#' @importFrom SqlRender render


constraints <-
  function(conn,
           conn_fun = "connectAthena()",
           vocab_schema,
           table,
           field,
           skip_cache = FALSE,
           override_cache = FALSE,
           render_sql = TRUE,
           verbose = TRUE,
           sleepTime = 1) {
    if (!missing(conn_fun)) {
      conn <- eval(expr = rlang::parse_expr(x = conn_fun))
      on.exit(
        expr = dcAthena(
          conn = conn,
          verbose = verbose
        ),
        add = TRUE,
        after = TRUE
      )
    }

    sql_statement <-
      SqlRender::render(
        "
                                         SELECT DISTINCT @field
                                         FROM @vocab_schema.@table
                                         ",
        field = field,
        table = table,
        vocab_schema = vocab_schema
      )

    queryAthena(
      sql_statement = sql_statement,
      conn = conn,
      skip_cache = skip_cache,
      override_cache = override_cache,
      render_sql = render_sql,
      verbose = verbose,
      sleepTime = sleepTime
    ) %>%
      unlist()
  }

#' @export

constraints_ff <-
  function(table,
           field,
           vocab_schema,
           conn_fun = "connectAthena()") {
    function(conn,
             skip_cache = FALSE,
             override_cache = FALSE,
             render_sql = TRUE,
             verbose = TRUE,
             sleepTime = 1) {
      constraints(
        conn = conn,
        conn_fun = conn_fun,
        vocab_schema = vocab_schema,
        table = table,
        field = field,
        skip_cache = skip_cache,
        override_cache = override_cache,
        render_sql = render_sql,
        verbose = verbose,
        sleepTime = sleepTime
      )
    }
  }

#' @export

vocabulary_id_constraints <-
  constraints_ff(
    table = "concept",
    field = "vocabulary_id",
    vocab_schema = "omop_vocabulary"
  )

#' @export

domain_id_constraints <-
  constraints_ff(
    table = "concept",
    field = "domain_id",
    vocab_schema = "omop_vocabulary"
  )

#' @export

concept_class_id_constraints <-
  constraints_ff(
    table = "concept",
    field = "concept_class_id",
    vocab_schema = "omop_vocabulary"
  )
