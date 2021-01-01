#' @title
#' Search for a Code in the Concept Table
#'
#' @inheritParams queryAthena
#' @inheritParams grepl
#' @param code_pattern Regex pattern of code
#' @examples
#' if(interactive()){
#'  search_code(code_pattern = "^C40")
#'  }
#' @return resultset as a dataframe with all column types as character and trimmed white space
#' @export
#' @rdname search_code

search_code <-
  function(code_pattern,
           ignore.case = FALSE,
           perl = FALSE,
           fixed = FALSE,
           useBytes = FALSE,
           vocab_schema = "omop_vocabulary",
           limit = NULL,
           conn,
           conn_fun = "connectAthena()",
           cache_only = FALSE,
           skip_cache = FALSE,
           override_cache = FALSE,
           verbose = TRUE,
           render_sql = TRUE,
           sleepTime = 1) {

    code <- stringr::str_replace_all(
      string = code_pattern,
      pattern = "[^0-9A-Za-z]{1,}",
      replacement = "%"
    )

    sql_statement <-
      pg13::build_query_like(
        schema = vocab_schema,
        table = "concept",
        where_like_field = "concept_code",
        where_like_value = code,
        case_insensitive = TRUE,
        limit_n = limit
      )

    resultset <- queryAthena(
      sql_statement = sql_statement,
      conn = conn,
      conn_fun = conn_fun,
      cache_only = cache_only,
      skip_cache = skip_cache,
      override_cache = override_cache,
      render_sql = render_sql,
      verbose = verbose,
      sleepTime = sleepTime
    )

    resultset %>%
      dplyr::filter_at(
        dplyr::vars(concept_code),
        dplyr::all_vars(grepl(
          pattern = code_pattern,
          x = .,
          ignore.case = ignore.case,
          perl = perl,
          fixed = fixed,
          useBytes = useBytes
        ))
      )
  }


