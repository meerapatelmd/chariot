#' @title
#' Lookup a Concept Id
#'
#' @return
#' Concept Class object if the `concept_id` argument is found in the Concept table.
#' If it is not found, nothing is returned.
#'
#' @importFrom SqlRender render
#' @export
#' @rdname get_concept
#' @example inst/example/lookup_atc_class_ingredients.R

get_concept <-
  function(concept_id,
           vocab_schema = "omop_vocabulary",
           conn,
           conn_fun = "connectAthena()",
           cache_only = FALSE,
           skip_cache = FALSE,
           override_cache = FALSE,
           render_sql = FALSE,
           verbose = FALSE,
           sleepTime = 1) {

    cli::cli_h1("Is Concept Id Valid Integer?")
    check_concept_id(concept_id = concept_id)

    cli::cli_h1("Lookup Concept Id in Concept Table")
    df <- lookup_concept_id(
      concept_id = concept_id,
      vocab_schema = vocab_schema,
      conn = conn,
      conn_fun = conn_fun,
      cache_only = cache_only,
      skip_cache = skip_cache,
      override_cache = override_cache,
      render_sql = render_sql,
      verbose = verbose,
      sleepTime = sleepTime
    )

    if (nrow(df) == 0) {
      cli::cli_alert_danger("Concept Id `{concept_id}` not found in Concept Table")
      invisible(NULL)
    } else {
      cli::cli_alert_success("Concept Id `{concept_id}` found in Concept Table")
      cli::cli_h1("Lookup Synonyms")
      df2 <- lookup_synonyms(
        concept_id = concept_id,
        vocab_schema = vocab_schema,
        conn = conn,
        cache_only = cache_only,
        skip_cache = skip_cache,
        override_cache = override_cache,
        render_sql = render_sql,
        verbose = verbose,
        sleepTime = sleepTime
      )

    new(
      Class = "concept",
      concept_id = df$concept_id,
      concept_name = df$concept_name,
      concept_synonym_names = paste(df2$concept_synonym_name, collapse = "|"),
      domain_id = df$domain_id,
      vocabulary_id = df$vocabulary_id,
      concept_class_id = df$concept_class_id,
      standard_concept = df$standard_concept,
      concept_code = df$concept_code,
      valid_start_date = df$valid_start_date,
      valid_end_date = df$valid_end_date,
      invalid_reason = df$invalid_reason
    )

    }
  }

#' @title
#' Lookup a Concept Id
#' @noRd
#' @rdname lookup_concept_id


lookup_concept_id <-
  function(concept_id,
           vocab_schema = "omop_vocabulary",
           conn,
           conn_fun = "connectAthena()",
           cache_only = FALSE,
           skip_cache = FALSE,
           override_cache = FALSE,
           render_sql = TRUE,
           verbose = TRUE,
           sleepTime = 1) {
    sql_statement <-
      SqlRender::render("SELECT *
                                                        FROM @vocab_schema.concept c
                                                        WHERE c.concept_id IN (@concept_id)
                                                      ",
        vocab_schema = vocab_schema,
        concept_id = concept_id
      )

    queryAthena(
      sql_statement = sql_statement,
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
#' Lookup Synonyms
#' @noRd
#' @rdname lookup_synonyms


lookup_synonyms <-
  function(concept_id,
           vocab_schema = "omop_vocabulary",
           conn,
           conn_fun = "connectAthena()",
           cache_only = FALSE,
           skip_cache = FALSE,
           override_cache = FALSE,
           render_sql = TRUE,
           verbose = TRUE,
           sleepTime = 1) {
    sql <-
      SqlRender::render("WITH con AS (
                                                        SELECT *
                                                        FROM @vocab_schema.concept c
                                                        WHERE c.concept_id IN (@concept_id)
                                                )

                                            SELECT DISTINCT cs.concept_synonym_name
                                            FROM @vocab_schema.concept_synonym cs
                                            INNER JOIN con c
                                            ON c.concept_id = cs.concept_id
                                            WHERE c.concept_name <> cs.concept_synonym_name
                                                AND cs.language_concept_id = 4180186
                                                      ",
        vocab_schema = vocab_schema,
        concept_id = concept_id
      )

    queryAthena(
      sql_statement = sql,
      conn = conn,
      conn_fun = conn_fun,
      cache_only = cache_only,
      skip_cache = skip_cache,
      override_cache = override_cache,
      render_sql = render_sql,
      verbose = verbose,
      sleepTime = sleepTime
    )
  }
