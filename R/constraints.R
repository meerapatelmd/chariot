#' @title
#' Get Constraints
#' @description
#' For any table in the given schema, a
#' named list of distinct values within the provided field
#' is returned.
#'
#' @return
#' A named list of unique values by given field.
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname get_constraints
#' @export
#' @importFrom SqlRender render


get_constraints <-
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
                                         SELECT
                                          '@table'  AS table,
                                          '@field'  AS field,
                                           @field   AS value,
                                           COUNT(*) AS value_count
                                         FROM @vocab_schema.@table
                                         GROUP BY @field
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
    )
  }

#' @export

get_vocab_table_constraints <-
  function(conn,
           tables   = list_vocab_table_names(),
           conn_fun = "connectAthena()",
           vocab_schema = "omop_vocabulary",
           skip_cache = FALSE,
           override_cache = FALSE,
           render_sql = TRUE,
           verbose = TRUE,
           sleepTime = 1) {

    cdm_wiki     <- read_cdm_wiki_table()

    vocab_tables <-
      match.arg(arg        = tables,
                choices    = list_vocab_table_names(),
                several.ok = TRUE)

    vocab_wiki <-
      cdm_wiki[vocab_tables] %>%
      dplyr::bind_rows(.id = "CDM Table")

    # Exclude Primary Keys
    vocab_wiki2 <-
      vocab_wiki %>%
      dplyr::filter(`Primary Key` == "No") %>%
      dplyr::filter_at(vars(`CDM Field`),
                       ~grepl("concept_id", .) == FALSE)

    # Exclude Float and Date datatypes
    vocab_wiki3 <-
      vocab_wiki2 %>%
      dplyr::filter(!(Datatype %in% c("date", "float", "varchar(255)", "varchar(1000)", "varchar(50)")))



    # Iteratively get the list object
    output <- list()
    for (i in 1:nrow(vocab_wiki3)) {

      table <- vocab_wiki3$`CDM Table`[i]
      field <- vocab_wiki3$`CDM Field`[i]

      if (!(table %in% names(output))) {

        output[[length(output)+1]]    <- list()
        names(output)[length(output)] <- table

      }

      output[[table]][[1+length( output[[table]])]] <-
        get_constraints(conn = conn,
                        conn_fun = conn_fun,
                        vocab_schema = vocab_schema,
                        table = table,
                        field = field,
                        skip_cache = skip_cache,
                        override_cache = override_cache,
                        render_sql = render_sql,
                        verbose = verbose,
                        sleepTime = sleepTime)

      names(output[[table]])[length(output[[table]])] <- field


    }

    output


  }
